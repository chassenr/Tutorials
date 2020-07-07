# This script is an example of the basic dada2 workflow.
# It is not optimized for:
#   amplicons which are sequenced into the rev primer at the 5' end of the fwd read, and vice versa (e.g. ITS)
#   faster computing times at slightly lower output quality (e.g. big data)

# load packages:
require(dada2)
require(ShortRead)
require(ggplot2)
require(gridExtra)

packageVersion("dada2")
# 1.16.0

# specify path to input fastq files
setwd("/home/chh/Documents/Projects/UniHB_MOeP_dada2_workshop/Library_052020")
path <- "Clipped"
fnFR_R1 <- sort(list.files(path, pattern="clip_fr_R1.fastq", full.names = TRUE))
fnFR_R2 <- sort(list.files(path, pattern="clip_fr_R2.fastq", full.names = TRUE))
fnRF_R1 <- sort(list.files(path, pattern="clip_rf_R1.fastq", full.names = TRUE))
fnRF_R2 <- sort(list.files(path, pattern="clip_rf_R2.fastq", full.names = TRUE))
# Extract sample names
sample.names <- sapply(strsplit(basename(fnFR_R1), "_"), `[`, 1)
# optional: use mixedsort of package gtools to get full alphanumeric sort

# quality check
QualityProfileFR_R1 <- list()
for(i in 1:length(fnFR_R1)) {
  QualityProfileFR_R1[[i]] <- list()
  QualityProfileFR_R1[[i]][[1]] <- plotQualityProfile(fnFR_R1[i])
}
pdf("QualityProfileFR_R1.pdf")
for(i in 1:length(fnFR_R1)) {
  do.call("grid.arrange", QualityProfileFR_R1[[i]])  
}
dev.off()
rm(QualityProfileFR_R1)

QualityProfileFR_R2 <- list()
for(i in 1:length(fnFR_R2)) {
  QualityProfileFR_R2[[i]] <- list()
  QualityProfileFR_R2[[i]][[1]] <- plotQualityProfile(fnFR_R2[i])
}
pdf("QualityProfileFR_R2.pdf")
for(i in 1:length(fnFR_R2)) {
  do.call("grid.arrange", QualityProfileFR_R2[[i]])  
}
dev.off()
rm(QualityProfileFR_R2)

# Place filtered files in filtered/ subdirectory
filtFs <- file.path("Filtered", paste0(sample.names, "_F_filt.fastq"))
filtRs <- file.path("Filtered", paste0(sample.names, "_R_filt.fastq"))
names(filtFs) <- sample.names
names(filtRs) <- sample.names