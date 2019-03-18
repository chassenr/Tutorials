# Analysis of bacterial V3V4 16S seqs
# R version 3.5.2 (or 3.5.1)
# following mostly the tutorial available on: https://benjjneb.github.io/dada2/tutorial.html

# start R
require(dada2)
require(ShortRead)
require(ggplot2)
require(gridExtra)

# save.image("bacteria_dada2.Rdata")
# load("bacteria_dada2.Rdata")

packageVersion("dada2")
# 1.10.1

# list files
path <- "./Clipped/"
fns <- list.files(path)
fns

# Sort ensures forward/reverse reads are in same order
fnFs <- sort(list.files(path, pattern="_R1.fastq"))
fnRs <- sort(list.files(path, pattern="_R2.fastq"))
# Extract sample names, assuming filenames have format: SAMPLENAME_XXX.fastq
sample.names <- sort(read.table("sample_names.txt", h = F, stringsAsFactors = F)$V1)
# Specify the full path to the fnFs and fnRs
fnFs <- file.path(path, fnFs)
fnRs <- file.path(path, fnRs)

# quality check
QualityProfileFs <- list()
for(i in 1:length(fnFs)) {
  QualityProfileFs[[i]] <- list()
  QualityProfileFs[[i]][[1]] <- plotQualityProfile(fnFs[i])
}
pdf("QualityProfileForward.pdf")
for(i in 1:length(fnFs)) {
  do.call("grid.arrange", QualityProfileFs[[i]])  
}
dev.off()
rm(QualityProfileFs)

QualityProfileRs <- list()
for(i in 1:length(fnRs)) {
  QualityProfileRs[[i]] <- list()
  QualityProfileRs[[i]][[1]] <- plotQualityProfile(fnRs[i])
}
pdf("QualityProfileReverse.pdf")
for(i in 1:length(fnRs)) {
  do.call("grid.arrange", QualityProfileRs[[i]])  
}
dev.off()
rm(QualityProfileRs)
# expected max length: 430
# min overlap: 30

# Make directory and filenames for the filtered fastqs
filt_path <- file.path(path, "../Filtered")
if(!file_test("-d", filt_path)) dir.create(filt_path)
filtFs <- file.path(filt_path, paste0(sample.names, "_F_filt.fastq"))
filtRs <- file.path(filt_path, paste0(sample.names, "_R_filt.fastq"))

# Filter
# depending on the expected overlap, the filtering parameters can be stricter
# calculate expected error rates:
# sum(10^(-x/10))
# where x is quality scores of each read
out <- filterAndTrim(
  fnFs, 
  filtFs, 
  fnRs, 
  filtRs,
  truncLen = c(255, 205),
  maxN = 0,
  minQ = 2,
  maxEE = c(3, 3), 
  truncQ = 0, 
  rm.phix = TRUE,
  compress = F,
  multithread = 6
)
head(out)
summary(out[, 2]/out[, 1])
# seems ok...

# quality check
QualityProfileFs.filt <- list()
for(i in 1:length(filtFs)) {
  QualityProfileFs.filt[[i]] <- list()
  QualityProfileFs.filt[[i]][[1]] <- plotQualityProfile(filtFs[i])
}
pdf("QualityProfileForwardFiltered.pdf")
for(i in 1:length(filtFs)) {
  do.call("grid.arrange", QualityProfileFs.filt[[i]])  
}
dev.off()
rm(QualityProfileFs.filt)

QualityProfileRs.filt <- list()
for(i in 1:length(filtRs)) {
  QualityProfileRs.filt[[i]] <- list()
  QualityProfileRs.filt[[i]][[1]] <- plotQualityProfile(filtRs[i])
}
pdf("QualityProfileReverseFiltered.pdf")
for(i in 1:length(filtRs)) {
  do.call("grid.arrange", QualityProfileRs.filt[[i]])  
}
dev.off()
rm(QualityProfileRs.filt)
# looks ok

# learn errors
# follow tutorial for big data
errF <- learnErrors(filtFs, multithread = 4, randomize = TRUE, verbose = 1, MAX_CONSIST = 10)
errR <- learnErrors(filtRs, multithread = 4, randomize = TRUE, verbose = 1, MAX_CONSIST = 10)

# plot error profiles
pdf("ErrorProfiles.pdf")
plotErrors(errF, nominalQ = TRUE)
plotErrors(errR, nominalQ = TRUE)
dev.off()

# dereplication
derepFs <- derepFastq(filtFs, verbose = TRUE)
derepRs <- derepFastq(filtRs, verbose = TRUE)
# Name the derep-class objects by the sample names
names(derepFs) <- sample.names
names(derepRs) <- sample.names

# denoising
dadaFs <- dada(derepFs, err = errF, multithread = 6, pool = TRUE)
dadaRs <- dada(derepRs, err = errR, multithread = 6, pool = TRUE)

# merging
# since one read will be preferred to the other for the overlap region,
# allowing mismatches may introduce error again?
mergers <- mergePairs(
  dadaFs, 
  derepFs, 
  dadaRs,
  derepRs,
  minOverlap = 10,
  verbose = TRUE,
  propagateCol = c("birth_fold", "birth_ham")
)

# create sequence table
seqtab <- makeSequenceTable(mergers)
dim(seqtab)

# removing chimeras
seqtab.nochim <- removeBimeraDenovo(seqtab, method = "consensus", multithread = 6, verbose = TRUE)
dim(seqtab.nochim)
summary(rowSums(seqtab.nochim)/rowSums(seqtab))
table(rep(nchar(colnames(seqtab.nochim)), colSums(seqtab.nochim)))

# inspect output
# remove singletons and 'junk' sequences
# there are quite a few unexpected sequence lengths --> only pick the first 50
uniquesToFasta(seqtab.nochim[, which(nchar(colnames(seqtab.nochim)) %in% c(383:388))[1:50]], "check.fasta")
# these are all low identity hits --> remove
seqtab.nochim2 <- seqtab.nochim[, nchar(colnames(seqtab.nochim)) %in% c(400:431) & colSums(seqtab.nochim) > 1]
dim(seqtab.nochim2)
summary(rowSums(seqtab.nochim2)/rowSums(seqtab.nochim))

# get summary nSeqs
getN <- function(x) sum(getUniques(x))
track <- cbind(out, sapply(dadaFs, getN), sapply(mergers, getN), rowSums(seqtab.nochim), rowSums(seqtab.nochim2))
colnames(track) <- c("input", "filtered", "denoised", "merged", "nochim", "tabled")
rownames(track) <- sample.names
track <- data.frame(track)
head(track)
summary(track$tabled/track$input) 
summary(track$filtered/track$input)
summary(track$denoised/track$filtered)
summary(track$merged/track$denoised)
summary(track$nochim/track$merged)
summary(track$tabled/track$nochim)
# ok
summary(rowSums(seqtab.nochim2))
# ok

# assign taxonomy
# download from https://zenodo.org/record/1172783#.XIadAWN7lhE
# wget https://zenodo.org/record/1172783/files/silva_nr_v132_train_set.fa.gz
tax <- assignTaxonomy(
  seqtab.nochim2, 
  "silva_nr_v132_train_set.fa.gz", 
  tryRC = TRUE,
  multithread = 10
)

# remove OTUs unclassified on phylum level, and non bacteria
table(tax[, 1])
sum(is.na(tax[, 2]))
tmp <- tax[!is.na(tax[, 2]) & tax[, 1] == "Bacteria", ]
tax.good <- tmp[-c(grep("Chloroplast", tmp[, 4]), grep("Mitochondria", tmp[, 5])), ]
seqtab.nochim2.good <- seqtab.nochim2[, rownames(tax.good)]
track$classified <- rowSums(seqtab.nochim2.good)
summary(track$classified/track$tabled)
summary(track$classified/track$input)
summary(rowSums(seqtab.nochim2.good))
# ok

# format output
seqtab.nochim2.print <- t(seqtab.nochim2.good)
tax.print <- tax.good
all.equal(rownames(seqtab.nochim2.print), rownames(tax.print))
rownames(seqtab.nochim2.print) <- paste("sq", 1:ncol(seqtab.nochim2.good), sep = "")
rownames(tax.print) <- rownames(seqtab.nochim2.print)

# write output
write.table(seqtab.nochim2.print, "bac_seqtab_nochim2.txt", quote = F, sep = "\t")
write.table(tax.print, "bac_taxonomy_table.txt", sep = "\t", quote = F)
uniquesToFasta(seqtab.nochim2.good, "bac_dada2_unique_nochim.fasta")
write.table(track, "bac_nSeq_dada2.txt", quote = F, sep = "\t")

# curate NAs in taxonomy table
Taxb <- tax.print
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


# to also classify the sequences from the swarm output (only non-singletons)
# this is not part of the regular dada2 workflow
# I gave up finding an R function to read a fasta file directly...
tmp <- scan(what = "character", "Swarm/amplicons_seeds_heavy.fasta")
seqs.accnos <- gsub(">", "", sapply(strsplit(tmp[seq(1, length(tmp), by = 2)], split = "_"), function(x) x[1]))
names(seqs.accnos) <- tmp[seq(2, length(tmp), by = 2)]
seqs.swarm <- as.integer(sapply(strsplit(tmp[seq(1, length(tmp), by = 2)], split = "_"), function(x) x[2]))
names(seqs.swarm) <- tmp[seq(2, length(tmp), by = 2)]
tax.swarm <- assignTaxonomy(
  seqs.swarm, 
  "silva_nr_v132_train_set.fa.gz", 
  tryRC = TRUE,
  multithread = 10
)

# remove OTUs unclassified on phylum level, and non bacteria
table(tax.swarm[, 1])
sum(is.na(tax.swarm[, 2]))
tmp <- tax.swarm[!is.na(tax.swarm[, 2]) & tax.swarm[, 1] == "Bacteria", ]
tax.swarm.good <- tmp[-c(grep("Chloroplast", tmp[, 4]), grep("Mitochondria", tmp[, 5])), ]
# ok

# write output
tax.swarm.print <- tax.swarm.good
seqs.accnos.good <- seqs.accnos[rownames(tax.swarm.good)]
all.equal(names(seqs.accnos.good), rownames(tax.swarm.print))
rownames(tax.swarm.print) <- seqs.accnos.good
write.table(tax.swarm.print, "bac_taxonomy_table_swarm.txt", sep = "\t", quote = F)

# have a closer look at chloroplast sequences
tax.chloro <- tax[grep("Chloroplast", tax[, 4]), ]
seqtab.nochim2.chloro <- seqtab.nochim2[, rownames(tax.chloro)]
seqtab.nochim2.chloro.print <- t(seqtab.nochim2.chloro)
all.equal(rownames(seqtab.nochim2.chloro.print), rownames(tax.chloro))
rownames(seqtab.nochim2.chloro.print) <- paste("sq", 1:ncol(seqtab.nochim2.chloro), sep = "")
write.table(seqtab.nochim2.chloro.print, "bac_seqtab_chloro.txt", quote = F, sep = "\t")
uniquesToFasta(seqtab.nochim2.chloro, "bac_dada2_unique_chloro.fasta")

# add blast output to chlorplast OTU table
blast.chloro <- read.table("dada2_chloro.blastout_best.txt", h = F, sep = "\t", stringsAsFactors = F)
colnames(blast.chloro) <- c(
  "qseqid",
  "sseqid",
  "pident",
  "length",
  "mismatch",
  "gapopen",
  "qstart",
  "qend",
  "sstart",
  "send",
  "evalue",
  "bitscore",
  "qcovs",
  "qlen",
  "gaps"
)
rownames(blast.chloro) <- sapply(blast.chloro$qseqid, function(x) { strsplit(x, ";", fixed = T)[[1]][1] })
blast.chloro.path <- read.table("chloro_path.txt", h = F, sep = "\t", stringsAsFactors = F)
for(i in 1:nrow(blast.chloro)) {
  blast.chloro$path[i] <- blast.chloro.path$V1[blast.chloro.path$V2 == blast.chloro$sseqid[i]]
}
seqtab.nochim2.chloro.classified <- seqtab.nochim2.chloro.print[rownames(blast.chloro), ]

# summarize per taxonomic path
Chloroplast <- aggregate(
  seqtab.nochim2.chloro.classified,
  by = list(blast.chloro$path),
  FUN = sum
)
rownames(Chloroplast) <- Chloroplast$Group.1
Chloroplast <- Chloroplast[, -1]
Chloroplast <- Chloroplast[rowSums(Chloroplast) > 1, ]
write.table(Chloroplast, "Chloroplast.txt", sep = "\t", quote = F)
