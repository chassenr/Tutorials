# This script is an example of the basic dada2 workflow.
# See: https://benjjneb.github.io/dada2/tutorial.html
# It is not optimized for:
#   amplicons which are sequenced into the rev primer at the 5' end of the fwd read, and vice versa (e.g. ITS)
#   faster computing times at slightly lower output quality (e.g. big data)

# load packages
require(dada2)
require(ShortRead)
require(ggplot2)
require(gridExtra)

packageVersion("dada2")
# 1.16.0

# save and load workspace
setwd("/home/chh/Documents/Projects/UniHB_MOeP_dada2_workshop/Library_052020")
# save.image("dada2_separate.Rdata")

# specify path to input fastq files
path <- "Clipped"
fnFR_R1 <- sort(list.files(path, pattern="clip_fr_R1.fastq", full.names = TRUE))
fnFR_R2 <- sort(list.files(path, pattern="clip_fr_R2.fastq", full.names = TRUE))
fnRF_R1 <- sort(list.files(path, pattern="clip_rf_R1.fastq", full.names = TRUE))
fnRF_R2 <- sort(list.files(path, pattern="clip_rf_R2.fastq", full.names = TRUE))
# Extract sample names
sample.names <- sapply(strsplit(basename(fnFR_R1), "_"), `[`, 1)
# optional: use mixedsort of package gtools to get full alphanumeric sort

# quality check
source("../dada2_quality_check.R")
quality_check(
  c(fnFR_R1, fnRF_R1),
  c(fnFR_R2, fnRF_R2),
  file_base = "QualityProfile_separate"
)

# Place filtered files in Filtered/ subdirectory
filtFR_R1 <- file.path("Filtered", paste0(sample.names, "_FR_R1_filt.fastq"))
filtFR_R2 <- file.path("Filtered", paste0(sample.names, "_FR_R2_filt.fastq"))
filtRF_R1 <- file.path("Filtered", paste0(sample.names, "_RF_R1_filt.fastq"))
filtRF_R2 <- file.path("Filtered", paste0(sample.names, "_RF_R2_filt.fastq"))
names(filtFR_R1) <- sample.names
names(filtFR_R2) <- sample.names
names(filtRF_R1) <- sample.names
names(filtRF_R2) <- sample.names

# Considerations for trimming:
# expected max length: 252bp (?)
# min overlap: 30bp
# reads should be truncated so that rev primer is not included at end of fwd reads
# It is recommended to trim to just enough for the required length for sufficient overlap
# Caution: don't remove too much

# Define ranges for truncLen
# To save time and compare the outcome directly to the reorient data,
# use c(180, 160)

# Define ranges for truncLen
# Use c(2, 2) for now

# Run parameter optimization
# skip for now

# Run trimming with optimal parameters
filt_FR.out <- filterAndTrim(
  fwd = fnFR_R1, 
  filt = filtFR_R1, 
  rev = fnFR_R2, 
  filt.rev = filtFR_R2,
  truncLen = c(180, 160),
  maxN = 0,
  minQ = 2,
  maxEE = c(2, 2), 
  truncQ = 0, 
  rm.phix = TRUE,
  compress = F,
  multithread = 4
)
filt_FR.out <- filterAndTrim(
  fwd = fnRF_R1, 
  filt = filtRF_R1, 
  rev = fnRF_R2, 
  filt.rev = filtRF_R2,
  truncLen = c(180, 160),
  maxN = 0,
  minQ = 2,
  maxEE = c(2, 2), 
  truncQ = 0, 
  rm.phix = TRUE,
  compress = F,
  multithread = 4
)

# Repeat quality check after trimming
quality_check(
  c(filtFR_R1, filtRF_R1),
  c(filtFR_R2, filtRF_R2),
  file_base = "QualityProfileFiltered_separate"
)

# Learn error rates
# It is generally not necessary to increase the number of nbases used for the error estimation
# It is possible that with 10 rounds (MAX_CONSIST), the algorithm for learning the errors won't converge
# Increasing MAX_CONSIST will lead to longer run times, and may only marginally improve error estimation
# I would not recommend setting MAX_CONSIST higher than 15
errFR_R1 <- learnErrors(filtFR_R1, multithread = 20, randomize = TRUE, verbose = 1, MAX_CONSIST = 10)
errFR_R2 <- learnErrors(filtFR_R2, multithread = 20, randomize = TRUE, verbose = 1, MAX_CONSIST = 10)
errRF_R1 <- learnErrors(filtRF_R1, multithread = 20, randomize = TRUE, verbose = 1, MAX_CONSIST = 10)
errRF_R2 <- learnErrors(filtRF_R2, multithread = 20, randomize = TRUE, verbose = 1, MAX_CONSIST = 10)
# it is a good idea to save your workspace here

# check convergence of error estimation and plot error profiles
pdf("ErrorProfiles.pdf")
barplot(log10(dada2:::checkConvergence(errFR_R1) + 1), main = "Convergence_fwd")
barplot(log10(dada2:::checkConvergence(errFR_R2) + 1), main = "Convergence_rev")
barplot(log10(dada2:::checkConvergence(errRF_R1) + 1), main = "Convergence_fwd")
barplot(log10(dada2:::checkConvergence(errRF_R2) + 1), main = "Convergence_rev")
plotErrors(errFR_R1, nominalQ = TRUE)
plotErrors(errFR_R2, nominalQ = TRUE)
plotErrors(errRF_R1, nominalQ = TRUE)
plotErrors(errRF_R2, nominalQ = TRUE)
dev.off()

# correct error estimates because of binned quality scores?
# https://github.com/benjjneb/dada2/issues/791
# https://github.com/benjjneb/dada2/issues/938
# The dada2 team is working on making some good recommendations for binned quality scores
# For now, there are 2 options:
#   1) run error learning with modified loess function (maybe more elegant)
#   Hack the loessErrfun() of dada2 package (used in both learnErrors and dada): 
#   mod.lo <- loess(rlogp ~ q, df, weights = log10(tot), span = 2)
#   2) coerce any value lower than the Q40 probability to be the Q40 value in the learnErrors() output
#   We will do this here to avoid re-running the error learning
errFR_R1_mod <- errFR_R1
errFR_R2_mod <- errFR_R2
errRF_R1_mod <- errRF_R1
errRF_R2_mod <- errRF_R2
errFR_R1_mod$err_out <- t(apply(getErrors(errFR_R1), 1, function(x) ifelse(x < x[41], x[41], x)))
errFR_R2_mod$err_out <- t(apply(getErrors(errFR_R2), 1, function(x) ifelse(x < x[41], x[41], x)))
errRF_R1_mod$err_out <- t(apply(getErrors(errRF_R1), 1, function(x) ifelse(x < x[41], x[41], x)))
errRF_R2_mod$err_out <- t(apply(getErrors(errRF_R2), 1, function(x) ifelse(x < x[41], x[41], x)))
pdf("ErrorProfiles_separate_mod.pdf")
plotErrors(errFR_R1_mod, nominalQ = TRUE)
plotErrors(errFR_R2_mod, nominalQ = TRUE)
plotErrors(errRF_R1_mod, nominalQ = TRUE)
plotErrors(errRF_R2_mod, nominalQ = TRUE)
dev.off()

# Dereplicate and denoise samples
# This step takes a while...
# For large data set (e.g. full HiSeq lane), I strongly recommend pool = "pseudo"
# I would not use pool = FALSE as this will strongly impact (i.e. lower) your alpha diversity,
# which seems to be rather an artifact of the change in parameters than any true signal
dadaFR_R1 <- dada(filtFR_R1, err = errF_mod, multithread = 20, pool = TRUE)
dadaFR_R2 <- dada(filtFR_R2, err = errR_mod, multithread = 20, pool = TRUE)
dadaRF_R1 <- dada(filtRF_R1, err = errF_mod, multithread = 20, pool = TRUE)
dadaRF_R2 <- dada(filtRF_R2, err = errR_mod, multithread = 20, pool = TRUE)
# it is a good idea to save your workspace here

# Merge reads
mergers_FR <- mergePairs(
  dadaFR_R1,
  filtFR_R1, 
  dadaFR_R2, 
  filtFR_R2, 
  minOverlap = 10,
  verbose = TRUE
)
mergers_RF <- mergePairs(
  dadaRF_R1,
  filtRF_R1, 
  dadaRF_R2, 
  filtRF_R2, 
  minOverlap = 10,
  verbose = TRUE
)

# Create sequence table
seqtab_FR <- makeSequenceTable(mergers_FR)
seqtab_RF <- makeSequenceTable(mergers_RF)
dim(seqtab_FR)
dim(seqtab_RF)

# As with the tryRC option of mergeSequenceTables only those sequences which are duplicated
# will be turned, manually turn sequences of RF table
seqtab_RF_rc
colnames(seqtab_RF_rc) <- rc(colnames(seqtab_RF))

# Merge sequence tables
seqtab <- mergeSequenceTables(
  seqtab_FR,
  seqtab_RF,
  repeats = "sum"
)

# continue with reorient script