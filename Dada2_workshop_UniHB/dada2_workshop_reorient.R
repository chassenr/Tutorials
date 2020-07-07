# This script is an example of the basic dada2 workflow.
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
# save.image("dada2_reorient.Rdata")

# specify path to input fastq files
path <- "Clipped"
fnFs <- sort(list.files(path, pattern="clip_R1.fastq", full.names = TRUE))
fnRs <- sort(list.files(path, pattern="clip_R2.fastq", full.names = TRUE))
# Extract sample names
sample.names <- sapply(strsplit(basename(fnFs), "_"), `[`, 1)
# optional: use mixedsort of package gtools to get full alphanumeric sort

# quality check
source("../dada2_quality_check.R")
quality_check(
  fnFs,
  fnRs,
  file_base = "QualityProfile"
)

# Place filtered files in Filtered/ subdirectory
filtFs <- file.path("Filtered", paste0(sample.names, "_F_filt.fastq"))
filtRs <- file.path("Filtered", paste0(sample.names, "_R_filt.fastq"))
names(filtFs) <- sample.names
names(filtRs) <- sample.names

# Considerations for trimming:
# expected max length: 300
# min overlap: 30
# reads should be truncated so that rev primer is not included at end of fwd reads
# It is recommended to trim to just enough for the required length for sufficient overlap
# Caution: don't remove too much

# Define ranges for truncLen
range_truncLen <- matrix(
  c(170, 170,
    175, 165,
    180, 160,
    185, 155,
    190, 150),
  nrow = 5,
  ncol = 2,
  byrow = T
)

# Define ranges for truncLen
range_maxEE <- matrix(
  c(1, 1,
    1, 2,
    2, 2,
    2, 3,
    3, 3,
    3, 4,
    4, 4),
  nrow = 7,
  ncol = 2,
  byrow = T
)

# Run parameter optimization
# This is quite time consuming and should only be attempted on a server with as many cores as you have samples (or at least 20)
source("../dada2_screen_settings.R")
screen_filt_settings <- screen_settings(
  sample.names, 
  fnFs, 
  fnRs, 
  range_maxEE, 
  range_truncLen, 
  cpus = 10
)

# This is just a gut feeling, but I would optimize for the following criteria:
#   small difference between 10 and 90 percentile of retained reads
#   high total proportion of retained reads
#   most stringent maxEE that does not result in severe loss of reads
plot(
  screen_filt_settings[, "prop.total"],
  screen_filt_settings[, "q90"] - screen_filt_settings[, "q10"],
  col = rep(rainbow(nrow(range_maxEE)), nrow(range_truncLen)),
  pch = 16
)

# Run trimming with optimal parameters
tmp <- filterAndTrim(
  fwd = fnFs, 
  filt = filtFs, 
  rev = fnRs, 
  filt.rev = filtRs,
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
  filtFs,
  filtRs,
  file_base = "QualityProfileFiltered"
)

# Learn error rates
# It is generally not necessary to increase the number of nbases used for the error estimation
# I increased MAX_CONSIST to allow for more iterations of error learning to converge
# This may increase run time, but also make the error learning more robust
errF <- learnErrors(filtFs, multithread = 4, randomize = TRUE, verbose = 1, MAX_CONSIST = 20)
errR <- learnErrors(filtRs, multithread = 4, randomize = TRUE, verbose = 1, MAX_CONSIST = 20)
# it is a good idea to save your workspace here

# check convergence of error estimation and plot error profiles
pdf("ErrorProfiles.pdf")
barplot(dada2:::checkConvergence(errF), main = "Convergence_fwd")
barplot(dada2:::checkConvergence(errR), main = "Convergence_fwd")
plotErrors(errF, nominalQ = TRUE)
plotErrors(errR, nominalQ = TRUE)
dev.off()

# correct error estimates because of binned quality scores?
# https://github.com/benjjneb/dada2/issues/791
# https://github.com/benjjneb/dada2/issues/938


# Dereplicate and denoise samples
dadaFs <- dada(filtFs, err = errF, multithread = 4, pool = TRUE)
dadaRs <- dada(filtRs, err = errR, multithread = 4, pool = TRUE)



