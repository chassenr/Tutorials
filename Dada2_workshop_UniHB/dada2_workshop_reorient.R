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
  file_base = "QualityProfile_reorient"
)

# Place filtered files in Filtered/ subdirectory
filtFs <- file.path("Filtered", paste0(sample.names, "_F_filt.fastq"))
filtRs <- file.path("Filtered", paste0(sample.names, "_R_filt.fastq"))
names(filtFs) <- sample.names
names(filtRs) <- sample.names

# Considerations for trimming:
# expected max length: 252bp (?)
# min overlap: 30bp
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
# Based on the expected maximum fragment length, the trimming could be even stricter
# Feel free to adjust the parameter ranges further

# Define ranges for maxEE
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
filt.out <- filterAndTrim(
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
# It is possible that with 10 rounds (MAX_CONSIST), the algorithm for learning the errors won't converge
# Increasing MAX_CONSIST will lead to longer run times, and may only marginally improve error estimation
# I would not recommend setting MAX_CONSIST higher than 15
errF <- learnErrors(filtFs, multithread = 20, randomize = TRUE, verbose = 1, MAX_CONSIST = 10)
errR <- learnErrors(filtRs, multithread = 20, randomize = TRUE, verbose = 1, MAX_CONSIST = 10)
# it is a good idea to save your workspace here

# check convergence of error estimation and plot error profiles
pdf("ErrorProfiles.pdf")
barplot(log10(dada2:::checkConvergence(errF) + 1), main = "Convergence_fwd")
barplot(log10(dada2:::checkConvergence(errR) + 1), main = "Convergence_rev")
plotErrors(errF, nominalQ = TRUE)
plotErrors(errR, nominalQ = TRUE)
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
errF_mod <- errF
errR_mod <- errR
errF_mod$err_out <- t(apply(getErrors(errF), 1, function(x) ifelse(x < x[41], x[41], x)))
errR_mod$err_out <- t(apply(getErrors(errR), 1, function(x) ifelse(x < x[41], x[41], x)))
pdf("ErrorProfiles_mod.pdf")
plotErrors(errF_mod, nominalQ = TRUE)
plotErrors(errR_mod, nominalQ = TRUE)
dev.off()

# Dereplicate and denoise samples
# This step takes a while...
# For large data set (e.g. full HiSeq lane), I strongly recommend pool = "pseudo"
# I would not use pool = FALSE as this will strongly impact (i.e. lower) your alpha diversity,
# which seems to be rather an artifact of the change in parameters than any true signal
dadaFs <- dada(filtFs, err = errF_mod, multithread = 20, pool = TRUE)
dadaRs <- dada(filtRs, err = errR_mod, multithread = 20, pool = TRUE)
# it is a good idea to save your workspace here

# Merge reads
mergers <- mergePairs(
  dadaFs,
  filtFs, 
  dadaRs, 
  filtRs, 
  minOverlap = 10,
  verbose = TRUE
)

# Create sequence table
seqtab <- makeSequenceTable(mergers)
dim(seqtab)

# This is the step at which separate denoising runs should be combined
# (e.g. if data comes from different sequencer runs or lanes, 
# or if fwd-rev and rev-fwd orientation were processed separately) 
# See documentation for mergeSequenceTables (especially tryRC)

# Remove chimeras
# This may remove quite a bit of ASVs, but only a small fraction of your total sequences
seqtab.nochim <- removeBimeraDenovo(seqtab, method = "consensus", multithread = 20, verbose = TRUE)
ncol(seqtab.nochim)/ncol(seqtab)
summary(rowSums(seqtab.nochim)/rowSums(seqtab))

# Inspect ASV length distribution
table(nchar(colnames(seqtab.nochim)))
table(rep(nchar(colnames(seqtab.nochim)), colSums(seqtab.nochim)))

# Check unusual sequence lengths
uniquesToFasta(
  # seqtab.nochim[, sample(which(nchar(colnames(seqtab.nochim)) < 250 | nchar(colnames(seqtab.nochim)) > 300), 100)], 
  seqtab.nochim[, nchar(colnames(seqtab.nochim)) < 250 | nchar(colnames(seqtab.nochim)) > 252], 
  "check.fasta"
)
# Those have all very low similarity to known 16S
# Optional: exclude (conservative approach) or keep (see what happens in taxonomic classification)

# Remove potential junk sequences and singletons
# dada does not generate singletons, any singletons are introduced in the merging step
# Adjust range of sequence lengths based on expected length of marker gene fragment
seqtab.nochim2 <- seqtab.nochim[, nchar(colnames(seqtab.nochim)) %in% 250:252 & colSums(seqtab.nochim) > 1]
dim(seqtab.nochim2)
ncol(seqtab.nochim2)/ncol(seqtab)
summary(rowSums(seqtab.nochim2)/rowSums(seqtab))

# Get nSeqs summary
nSeqs <- read.table("nSeqs_all.txt", h = T, stringsAsFactors = F)
all.equal(nSeqs$SID, sample.names)
getN <- function(x) sum(getUniques(x))
track <- cbind(
  nSeqs$Demux,
  filt.out,
  sapply(dadaFs, getN), 
  sapply(dadaRs, getN), 
  sapply(mergers, getN), 
  rowSums(seqtab.nochim), 
  rowSums(seqtab.nochim2)
)
colnames(track) <- c("Demux", "Clipped", "Filtered", "Denoised_fwd", "Denoised_rev", "merged", "nochim", "tabled")
rownames(track) <- c(sample.names)
track <- data.frame(track)

# How many reads were lost at each analysis step?
track.perc <- data.frame(round(apply(track, 2, function(x) x/track$Demux) * 100, 2))

# Taxonomic classification
# Available options:
#   DECIPHER (IdTaxa), 
#   RDP
#   Blast
#   silvangs
#   etc.

# For now, we will use RDP
# I am disabling the bootstrap filtering, but saving the bootstrap values
# so that we can manually filter by bootstrap later
tax <- assignTaxonomy(
  seqtab.nochim2, 
  "/media/16TB/chh/UniHB_dada2_workshop/silva_nr_v138_train_set.fa.gz",
  multithread = 20,
  minBoot = 0,
  outputBootstraps = T
)
# remember to save your workspace regularly

# Remove unwanted lineages
tax.good <- lapply(tax, function(x) {
  x[tax$tax[, 1] %in% c("Archaea", "Bacteria") & 
      !grepl("[Cc]hloroplast", tax$tax[, 4]) &
      !grepl("[Mm]itochondria", tax$tax[, 5])
    , ] 
})

# Determine most suitable bootstrap cut-off
apply(tax.good$boot, 2, summary)
# How many sequences and OTUs will be unclassified at a minboot of 70?
tax.filt70 <- tax.good$tax
tax.filt70[tax.good$boot < 70] <- NA
tax.filt70 <- tax.filt70[!is.na(tax.filt70[, 1]), ]
apply(tax.filt70, 2, function(x) sum(is.na(x)) )
dim(tax.filt70)
# Which proportion of sequences is affected?
otu.filt70 <- seqtab.nochim2[, rownames(tax.filt70)]
for(i in 1:ncol(tax.filt70)) {
  print(colnames(tax.filt70)[i])
  print(sum(rowSums(otu.filt70[, is.na(tax.filt70[, i])]))/sum(colSums(otu.filt70)))
}

# Update track
track$Classified <- rowSums(otu.filt70)
track.perc$Classified <- round(track$Classified/track$Demux * 100, 2)

# Write output
write.table(track, "nSeq_dada2.txt", quote = F, sep = "\t")
otu.70.print <- t(otu.filt70)
rownames(otu.70.print) <- paste("sq", 1:ncol(otu.filt70), sep = "")
write.table(otu.70.print, "otu_tab_70.txt", quote = F, sep = "\t")
write.table(otu.filt70, "otu_tab_70_with_seqs.txt", quote = F, sep = "\t")
uniquesToFasta(otu.filt70, "dada2_unique_70.fasta")
tax.70.print <- tax.filt70
rownames(tax.70.print) <- paste("sq", 1:nrow(tax.filt70), sep = "")
all.equal(rownames(tax.70.print), rownames(otu.70.print))
write.table(tax.70.print, "tax_tab_70.txt", quote = F, sep = "\t")

# Further curation of taxonomic paths (optional)
# append unclassified to last classified level instead of NA
TAX <- read.table(
  "tax_tab_70.txt",
  h = T,
  sep = "\t",
  stringsAsFactors = F
)
Taxb <- TAX
Taxb[Taxb == "uncultured"] <- NA
k <- ncol(Taxb) - 1
for (i in 2:k) {
  if (sum(is.na(Taxb[, i])) > 1) {
    test <- Taxb[is.na(Taxb[, i]), ]
    for (j in 1:nrow(test)) {
      if (sum(is.na(test[j, i:(k + 1)])) == length(test[j, i:(k + 1)])) {
        test[j, i] <- paste(test[j, (i - 1)], "_unclassified", sep = "")
        test[j, (i + 1):(k + 1)] <- test[j, i]
      }
    }
    Taxb[is.na(Taxb[, i]), ] <- test
  }
  if (sum(is.na(Taxb[, i])) == 1) {
    test <- Taxb[is.na(Taxb[, i]), ]
    if (sum(is.na(test[i:(k + 1)])) == length(test[i:(k + 1)])) {
      test[i] <- paste(test[(i - 1)], "_unclassified", sep = "")
      test[(i + 1):(k + 1)] <- test[i]
    }
    Taxb[is.na(Taxb[, i]),] <- test
  }
}
Taxb[is.na(Taxb[, (k + 1)]), (k + 1)] <- paste(Taxb[is.na(Taxb[, (k + 1)]), k], "_unclassified", sep = "")
write.table(Taxb, "tax_tab_70_formatted.txt", quote = F, sep = "\t")

