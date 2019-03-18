# data visualization
# requires R 3.5.1 or higher

### set up working environment ####

# set working directory
setwd("D:/Zmt_home/Workshops/Bioinf_ZMT_workshop_2019/")
# load("bacteria_16S_analysis.Rdata")
# save.image("bacteria_16S_analysis.Rdata")

# load packages
require(vegan)
require(car)
require(gplots)
require(OceanView)
require(reshape)
require(ALDEx2) # bioconductor
require(gtools)
require(randomForest)
require(caret)
require(rfUtilities)
require(SpiecEasi) # github
require(phyloseq) # bioconductor
require(igraph)
require(scales)
require(WGCNA)

# load custom functions
# https://github.com/chassenr/NGS
source("D:/Repos/chassenr/NGS/Plotting/PlotAbund.R")
source("D:/Repos/chassenr/ARISA/anosimPosthoc.R")
source("D:/Repos/chassenr/NGS/Plotting/SubsampleNGS.R")


### read data ####

# dada2 output
OTU <- read.table(
  "bac_seqtab_nochim2.txt",
  h = T,
  sep = "\t"
)

TAX <- read.table(
  "bac_taxonomy_table.txt",
  h = T, 
  sep = "\t", 
  stringsAsFactors = F, 
  row.names = 1
)

# replace NA with unclassified
TAX[is.na(TAX)] <- "unclassified"

# swarm output
# here we will have to do some further filtering to remove singleton OTUs
# and OTUs not kept after the taxonomic classification also from the OTU_contingency_table
# output from swarm
temp <- read.table(
  "OTU_contingency_table.csv",
  h = T,
  sep = "\t",
  stringsAsFactors = F
)
rownames(temp) <- temp$amplicon

TAX.swarm <- read.table(
  "bac_taxonomy_table_swarm.txt",
  h = T, 
  sep = "\t", 
  stringsAsFactors = F, 
  row.names = 1
)

# replace NA with unclassified
TAX.swarm[is.na(TAX.swarm)] <- "unclassified"

OTU.swarm <- temp[rownames(TAX.swarm), 3:(ncol(temp) -1)]
OTU.swarm.all <- temp[, 3:(ncol(temp) -1)]

# contextual data
# the sediment samples were collected along a natural pH gradient at a hydrothermal vent in a coral reef
# at each site, 3 replicate measurements of the environmental parameters were taken
# to analyze the sequence data, we will take the mean of each parameter per site
META <- read.table("ENV_table.txt", h = T, sep = "\t", stringsAsFactors = F)
str(META)
META$seep.influence <- factor(META$seep.influence, levels = c("reference", "medium", "high"))

# convert to long format
temp <- melt(META)
# calculate means in wide format
ENV <- cast(temp, "site + seep.influence ~ variable", fun.aggregate = mean, na.rm = T)
rownames(ENV) <- ENV$site

# order based on seep influence
ENV <- ENV[order(ENV$seep.influence), ]

# generate colors for seep influence
ENV$color <- ENV$seep.influence
levels(ENV$color) <- c("blue", "red", "orange")

# match site names
colnames(OTU) <- paste("S", sapply(strsplit(colnames(OTU), "_"), function(x) x[2]), sep = "")
colnames(OTU.swarm) <- paste("S", sapply(strsplit(colnames(OTU.swarm), "_"), function(x) x[2]), sep = "")
colnames(OTU.swarm.all) <- paste("S", sapply(strsplit(colnames(OTU.swarm.all), "_"), function(x) x[2]), sep = "")

# reorder OTU tables to match order of ENV
OTU <- OTU[, rownames(ENV)]
OTU.swarm <- OTU.swarm[, rownames(ENV)]
OTU.swarm.all <- OTU.swarm.all[, rownames(ENV)]

# calculate proportions
OTU.rel <- prop.table(as.matrix(OTU), 2) * 100
OTU.swarm.rel <- prop.table(as.matrix(OTU.swarm), 2) * 100
OTU.swarm.all.rel <- prop.table(as.matrix(OTU.swarm.all), 2) * 100

# for easier processing, put all OTU tables in a list
OTU.list <- list(OTU, OTU.swarm, OTU.swarm.all)
names(OTU.list) <- c("OTU_dada2", "OTU_swarm", "OTU_swarm_all")
OTU.rel.list <- list(OTU.rel, OTU.swarm.rel, OTU.swarm.all.rel)
names(OTU.rel.list) <- c("OTU_dada2_rel", "OTU_swarm_rel", "OTU_swarm_all_rel")


### rarefaction curves ####

# comparing output from dada2 and swarm
# generate 3 panel plot with rarefaction curves for each OTU table
# first, let's have a look at the number of OTUs
par(mfrow = c(1, 3), mar = c(4, 4, 4, 1))
for(i in 1:length(OTU.list)) {
  maxIndex <- colSums(decostand(OTU.list[[i]], "pa"))
  plot(
    0, 0,
    type = "n",
    xlim = c(0, max(colSums(OTU.list[[i]]))),
    ylim = c(0, max(maxIndex)),
    xlab = "seq depth",
    ylab = "nOTU",
    axes = T,
    cex.axis = 0.7,
    mgp = c(2, 0.5, 0),
    main = names(OTU.list)[i]
  )
  for(k in 1:ncol(OTU.list[[i]])) {
    temp <- rarefy(
      OTU.list[[i]][, k],
      sample = round(seq(0, sum(OTU.list[[i]][, k]), length.out = 50))
    )
    lines(
      round(seq(0, sum(OTU.list[[i]][, k]), length.out = 50)),
      temp,
      lwd = 0.5,
      col = as.character(ENV$color)[k]
    )
  }
}

# also look at and alpha diversity index which is less affected by rare OTUs
par(mfrow = c(1, 3), mar = c(4, 4, 4, 1))
for(i in 1:length(OTU.list)) {
  maxIndex <- vegan::diversity(t(OTU.list[[i]]), "invsimpson")
  plot(
    0, 0,
    type = "n",
    xlim = c(0, max(colSums(OTU.list[[i]]))),
    ylim = c(0, max(maxIndex)),
    xlab = "seq depth",
    ylab = "invS",
    axes = T,
    cex.axis = 0.7,
    mgp = c(2, 0.5, 0),
    main = names(OTU.list)[i]
  )
  for(k in 1:ncol(OTU.list[[i]])) {
    subsample <- round(seq(0, sum(OTU.list[[i]][, k]), length.out = 50))
    temp <- c()
    # for each sequencing depth
    for (j in 1:length(subsample)) {
      temp[j] <- vegan::diversity(
        rrarefy(
          OTU.list[[i]][, k],
          subsample[j]
        ),
        "invsimpson"
      )
    }
    temp[1] <- 0
    lines(
      subsample,
      temp,
      lwd = 0.5,
      col = as.character(ENV$color)[k]
    )
  }
}


### Mantel test ####
mantel(
  vegdist(t(OTU.rel.list$OTU_dada2_rel)),
  vegdist(t(OTU.rel.list$OTU_swarm_rel))
)
mantel(
  vegdist(t(OTU.rel.list$OTU_dada2_rel)),
  vegdist(t(OTU.rel.list$OTU_swarm_all_rel))
)
mantel(
  vegdist(t(OTU.rel.list$OTU_swarm_all_rel)),
  vegdist(t(OTU.rel.list$OTU_swarm_rel))
)


### NMDS ####

# generate NMDS ordinations for each OTU table
NMDS.list <- vector("list", length = length(OTU.rel.list))
names(NMDS.list) <- c("OTU_dada2_nmds", "OTU_swarm_nmds", "OTU_swarm_all_nmds")
for(i in 1:length(OTU.rel.list)) {
  NMDS.list[[i]] <- metaMDS(t(OTU.rel.list[[i]]))
}


### Procrustes ####
protest(NMDS.list$OTU_dada2_nmds, NMDS.list$OTU_swarm_nmds)
protest(NMDS.list$OTU_dada2_nmds, NMDS.list$OTU_swarm_all_nmds)
protest(NMDS.list$OTU_swarm_all_nmds, NMDS.list$OTU_swarm_nmds)

plot(procrustes(NMDS.list$OTU_dada2_nmds, NMDS.list$OTU_swarm_nmds))
plot(procrustes(NMDS.list$OTU_dada2_nmds, NMDS.list$OTU_swarm_all_nmds))
plot(procrustes(NMDS.list$OTU_swarm_all_nmds, NMDS.list$OTU_swarm_nmds))

# you can run more extensive comparisons
# we will continue with the dada2 output


### alpha diversity ####

# repeatedly randomly subsample to the minimum library size
Alpha <- SubsampleNGS(OTU, n = 100, sub = min(colSums(OTU)))

# compare raw vs. subsampled alpha diversity indices
# nOTU
plot(
  Alpha$summaryHillRaw[1, ],
  Alpha$summaryHill[1, ]
)
abline(0, 1)

# invS
plot(
  Alpha$summaryHillRaw[3, ],
  Alpha$summaryHill[3, ]
)
abline(0, 1)

# invS per seep influence
plot(
  Alpha$summaryHillRaw[3, ] ~ ENV$seep.influence,
  col = levels(ENV$color),
  xlab = "Seep influence",
  ylab = "Inverse Simpson Index"
)
points(
  as.numeric(ENV$seep.influence),
  Alpha$summaryHillRaw[3, ],
  pch = 22,
  cex = 1.5,
  bg = as.character(ENV$color)
)


### taxonomic composition ####

# what is the proportion of unclassified sequences for each taxonomic level
for(i in 1:ncol(TAX)) {
  print(colnames(TAX)[i])
  print(sum(colSums(OTU[TAX[, i] == "unclassified", ]))/sum(colSums(OTU)))
}
# already on family level there is a high proportion of unclassified sequence
# choose order level or higher for visualization and OTU level for statistical analysis

# summarize by taxon
TAX.pooled <- vector(
  mode = "list", 
  length = 6
)
names(TAX.pooled) <- colnames(TAX)
for (i in 1:6) { 
  temp <- aggregate(
    OTU,
    by = list(TAX[, i]),
    FUN = sum 
  )
  rownames(temp) <- temp$Group.1
  TAX.pooled[[i]] <- temp[, -1]
  rm(temp)
}
sapply(TAX.pooled, nrow)

TAX.pooled.rel <- lapply(
  TAX.pooled,
  function(x) {
    prop.table(as.matrix(x), 2) * 100
  }
)

# write to file
for(i in 2:length(TAX.pooled)) {
  write.table(
    TAX.pooled[[i]],
    paste("bac", names(TAX.pooled)[i], ".txt", sep = ""),
    quote = F,
    sep = "\t"
  )
}

# generate a stacked bar plot for an overview of the taxonomic composition
PlotAbund(
  TAX.pooled.rel$Class,
  abund = 5,
  method = "percentage",
  open.window = F,
  sort.taxa = T
)


### cluster diagram ####

OTU.clust <- hclust(vegdist(t(OTU.rel)))
plot(OTU.clust)


### NMDS plot ####

# extract NMDS of dada2 data from NMDS list
OTU.nmds <- NMDS.list$OTU_dada2_nmds

# to format plotting device, generate emtpy plot
plot(OTU.nmds, display = "sites", type = "n")

# add hull around samples from the same seep category
ordihull(OTU.nmds, groups = ENV$seep.influence)

# plot points
points(
  OTU.nmds$points,
  pch = 15,
  col = as.character(ENV$color),
  cex = 1.3
)

# add legend
legend(
  "topright",
  legend = levels(ENV$seep.influence),
  col = levels(ENV$color),
  pch = 15,
  pt.cex = 1.3
)

# stress value
text(
  par("usr")[1] + (par("usr")[2] - par("usr")[1]) * 0.05,
  par("usr")[3] + (par("usr")[4] - par("usr")[3]) * 0.05,
  labels = paste("2D stress:", round(OTU.nmds$stress, 2)),
  adj = 0
)


### ANOSIM ####

OTU.anosim <- anosim(t(OTU.rel), grouping = ENV$seep.influence)
OTU.anosim.posthoc <- ANOSIMposthoc(t(OTU.rel), ENV$seep.influence)


### PERMANOVA ####

# significance of seep influence
OTU.permanova <- adonis2(t(OTU.rel) ~ seep.influence, data = ENV, sqrt.dist = T)

# significance of environmental parameters
adonis2(t(OTU.rel) ~ ., data = ENV[, c(3:7, 10, 11)], sqrt.dist = T)
adonis2(t(OTU.rel) ~ ., data = ENV[, c(3:7, 10, 11)], sqrt.dist = T, by = "margin")
# why is there a difference in the output of the above 2 commands?
# why should you not run the above for a 'real' analysis? (at least not without further data exploration)


### Betadispersion ####

OTU.betadispersion <- betadisper(vegdist(t(OTU.rel)), group = ENV$seep.influence, sqrt.dist = T)
boxplot(OTU.betadispersion)


### RDA ####

# here the analysis gets a little bit more complicated
# first we should remove rare and low coverage OTUs
# i.e. OTU with only a few sequences, and OTUs occurring only in a few samples
# the last criterion should only be applied when you don't expect outliers in your data set
# (each sampled condition should have several representative replicates)
# here an OTU should occur with at least 0.01% in at least 2 samples
OTU.pruned <- OTU[apply(OTU.rel, 1, function(x) sum(x >= 0.01) >= 2), ]
dim(OTU.pruned)
OTU.pruned <- OTU[apply(OTU.rel, 1, function(x) sum(x >= 0.1) >= 2), ]
dim(OTU.pruned)

# how many sequences were removed this way?
summary(colSums(OTU.pruned)/colSums(OTU))
# you should try to keep more than 50%
summary(colSums(OTU.pruned))

# does this removal alter betadiversity patterns?
mantel(
  vegdist(prop.table(t(OTU.pruned), 1) * 100),
  vegdist(t(OTU.rel))
)
plot(
  vegdist(prop.table(t(OTU.pruned), 1) * 100),
  vegdist(t(OTU.rel))
)
abline(0, 1)
# ok

# clr transformation
OTU.clr <- clr(OTU.pruned + 0.5, 2)

# RDA for seep influence
RDA.seep <- rda(t(OTU.clr) ~ seep.influence, data = ENV)
RsquareAdj(RDA.seep)
anova(RDA.seep, by = "margin")

# model selection for environmental parameters
pairs(ENV[, c(3:7, 10, 11)])
cor(data.frame(ENV[, c(3:7, 10, 11)]))
# pick one of pH, DIC, TA

# full RDA model for environmental parameters
RDA.env <- rda(t(OTU.clr) ~ ., data = ENV[, 3:7])
vif(RDA.env)
# ok
anova(RDA.env, by = "margin")

# alterantively use automatic model selection
step.forward <- ordiR2step(
  rda(t(OTU.clr) ~ 1, data = ENV[, 3:7]),
  scope = formula(RDA.env),
  direction = "forward",
  pstep = 1000
)
# since nothing significant --> no parameter selected


### ALDEx2 ####

# differential OTU proportions between seep influence categories
OTU.aldex <- aldex(
  OTU.pruned,
  conditions = as.character(ENV$seep.influence),
  test = "glm",
  mc.samples = 16 # for a real analysis set this much higher (default of 128 is good)
)

# inspect output
head(OTU.aldex)
hist(OTU.aldex$glm.eBH)
hist(OTU.aldex$kw.ep)
sum(OTU.aldex$glm.eBH < 0.05)
sum(OTU.aldex$glm.eBH < 0.05 & OTU.aldex$kw.ep < 0.05)

# names of significant OTUs
OTU.aldex.sign <- rownames(OTU.aldex)[OTU.aldex$glm.eBH < 0.05 & OTU.aldex$kw.ep < 0.05]
colSums(OTU.rel[OTU.aldex.sign, ])

# further narrow down selection
sum(apply(OTU.rel[OTU.aldex.sign, ], 1, function(x) max(x) >= 1))
OTU.aldex.sign.abund <- OTU.aldex.sign[apply(OTU.rel[OTU.aldex.sign, ], 1, function(x) max(x) >= 1)]

# heatmap
heatmap.2(
  OTU.clr[OTU.aldex.sign.abund, ],
  col = bluered(50),
  Colv = F,
  Rowv = T,
  keysize = 0.8, 
  margins = c(4, 10),
  labRow = paste(TAX[OTU.aldex.sign.abund, "Class"], OTU.aldex.sign.abund),
  cexRow = 0.7,
  cexCol = 1,
  trace = "none",
  dendrogram = "none",
  density.info = "none",
  key.title = "",
  key.xlab = ""
)

# alternatively, heatmap with proportions
heatmap.2(
  sqrt(OTU.rel[OTU.aldex.sign.abund, ]),
  col = colorRampPalette(c("grey95", "darkred"))(50),
  Colv = F,
  Rowv = T,
  keysize = 0.8, 
  margins = c(4, 10),
  labRow = paste(TAX[OTU.aldex.sign.abund, "Class"], OTU.aldex.sign.abund),
  cexRow = 0.7,
  cexCol = 1,
  trace = "none",
  dendrogram = "none",
  density.info = "none",
  key.title = "",
  key.xlab = ""
)


### Random Forests ####

# https://github.com/LangilleLab/microbiome_helper/wiki/Random-Forest-Tutorial
# use as alternative to ALDEx2

# running model
randomForest.otu <- randomForest(
  x = data.frame(t(OTU.clr)),
  y = ENV$seep.influence, 
  ntree = 1001, # use 10001 for real data
  importance = TRUE, 
  proximities = TRUE
)
randomForest.otu

# model significance
randomForest.otu.sig <- rf.significance(
  x = randomForest.otu,  
  xdata = data.frame(t(OTU.clr)),
  nperm = 200, # use 1000
  ntree = 1001 # use 10001 for real data
)

# cross-validation
RF_state_classify_loocv.otu <- train( 
  data.frame(t(OTU.clr)), 
  y = ENV$seep.influence,
  method = "rf", 
  ntree = 1001, # use 10001 for real data
  tuneGrid = data.frame(mtry = randomForest.otu$mtry),
  trControl = trainControl(method = "LOOCV") 
)
RF_state_classify_loocv.otu$results

# importance
randomForest.otu.imp <- data.frame(randomForest.otu$importance)
randomForest.otu.imp <- randomForest.otu.imp[order(randomForest.otu.imp[, "MeanDecreaseAccuracy"], decreasing = T), ]
barplot(randomForest.otu.imp$MeanDecreaseAccuracy[1:200], las = 2, cex.names = 0.8)
abline(h = 0.0036)
# choose cut-off at 0.0036
randomForest.otu.names <- rownames(randomForest.otu.imp)[randomForest.otu.imp$MeanDecreaseAccuracy > 0.0036]

# heatmap
heatmap.2(
  OTU.clr[randomForest.otu.names, ],
  col = bluered(50),
  Colv = F,
  Rowv = T,
  keysize = 0.8, 
  margins = c(8, 12),
  labRow = paste(TAX[randomForest.otu.names, "Class"], randomForest.otu.names),
  cexRow = 0.7,
  cexCol = 1,
  trace = "none",
  dendrogram = "none",
  density.info = "none",
  key.title = "",
  key.xlab = ""
)
# not the best approach for this data set
# works much better with less OTUs
# try repeating this with a more conservative cut-off for OTU.pruned
# e.g.:
# OTU.pruned <- OTU[apply(OTU.rel, 1, function(x) sum(x >= 0.1) >= 2), ]


### co-occurrence network ####

# this code is mostly from a tutorial that I attended in 2018:
# EBAME Workshop on Computational Microbial Ecogenomics
# 21-28 October 2017
# IUEM Brest

# summarize data in phyloseq object
otus.bac <- otu_table(OTU.pruned, taxa_are_rows = T)
taxa.bac <- tax_table(as.matrix(TAX[rownames(OTU.pruned), ]))
physeqo.bac <- phyloseq(otus.bac, taxa.bac)

# spieceasi parameters
nc <- 1
lambda.min.ratio <- 1e-2
nlambda <- 20
rep.num <- 10 # maybe choose 20
stars.thresh <- 0.05  # we can use 0.05 or 0.1 (for details see function huge.select from R package huge)

# spieceasi run
physeqo.bac.mb <- spiec.easi(
  physeqo.bac, 
  method = 'mb', 
  lambda.min.ratio = lambda.min.ratio, 
  nlambda = nlambda, 
  pulsar.params = list(thresh = stars.thresh, rep.num = rep.num, ncores = nc)
)
print(physeqo.bac.mb)

# visualization as network
adj.bac <- physeqo.bac.mb
adj.bac.g <- adj2igraph(symBeta(getOptBeta(adj.bac), mode = 'maxabs'), vertex.attr = list(name = taxa_names(physeqo.bac)))
adj.bac.g.pos <- adj.bac.g - E(adj.bac.g)[E(adj.bac.g)$weight <= 0]
hist(E(adj.bac.g.pos)$weight, breaks = 50)
hist(E(adj.bac.g)$weight, breaks = 50)

# network layout
# am.coord.bac <- layout_with_graphopt(adj.bac.g)
am.coord.bac <- layout_with_graphopt(adj.bac.g.pos)
# temp.color <- as.factor(TAX[names(V(adj.bac.g)), "Class"])
temp.color <- as.factor(TAX[names(V(adj.bac.g.pos)), "Class"])
levels(temp.color) <- rainbow(length(levels(temp.color)))
# V(adj.bac.g)$color <- as.character(temp.color)
V(adj.bac.g.pos)$color <- as.character(temp.color)

# first look
plot(
  adj.bac.g.pos, 
  layout = am.coord.bac, 
  vertex.size = 5, 
  vertex.label = NA, 
  vertex.color = V(adj.bac.g.pos)$color,
  edge.color = ifelse(E(adj.bac.g.pos)$weight > 0, "red", "blue"), 
  edge.width = abs(E(adj.bac.g.pos)$weight) * 5,
  main = ""
)
legend(
  'right',
  legend = levels(as.factor(TAX[names(V(adj.bac.g.pos)), "Class"])),
  col = rainbow(length(levels(as.factor(TAX[names(V(adj.bac.g.pos)), "Class"])))),
  pch = 15,
  cex = 0.7
)

# cluster modules
graph.modules.bac <- cluster_louvain(adj.bac.g.pos, weights = E(adj.bac.g.pos)$weight)
sizes(graph.modules.bac)
plot(
  adj.bac.g.pos, 
  layout = am.coord.bac, 
  vertex.size = 5, 
  vertex.label = NA, 
  vertex.color = jet.col(length(sizes(graph.modules.bac)))[graph.modules.bac$membership],
  edge.color = "grey", 
  edge.width = E(adj.bac.g.pos)$weight * 5,
  main = ""
)
legend(
  'right',
  legend = 1:length(sizes(graph.modules.bac)),
  col = jet.col(length(sizes(graph.modules.bac))),
  pch = 15,
  cex = 0.7
)

# summarize sequence counts by module
TAX$module.grouping <- c()
TAX[names(membership(graph.modules.bac)), "module.grouping"] <- paste("M", c(membership(graph.modules.bac)), sep = "")
TAX$module.grouping[is.na(TAX$module.grouping)] <- "other"

OTU.modules <- aggregate(
  OTU,
  by = list(TAX$module.grouping),
  sum
)
rownames(OTU.modules) <- OTU.modules$Group.1
OTU.modules <- OTU.modules[, -1]
OTU.modules.rel <- prop.table(as.matrix(OTU.modules), 2) * 100
all.equal(colnames(OTU.modules.rel), rownames(ENV))

# first look at module proportions
barplot(
  OTU.modules.rel, 
  col = c(rainbow(length(sizes(graph.modules.bac))), "grey"),
  las = 2,
  names.arg = rownames(ENV)
)
legend(
  'left',
  legend = rownames(OTU.modules.rel),
  col = c(rainbow(length(sizes(graph.modules.bac))), "grey"),
  pch = 15,
  cex = 0.7
)

# eigengene analysis
cluster.ME <- moduleEigengenes(
  physeqo.bac.mb$est$data,
  membership(graph.modules.bac)
)
cluster.ME$eigengenes
cluster.ME$varExplained

# correlation with environmental parameters
cor(cluster.ME$eigengenes, data.frame(ENV[, 3:7]))

# which taxa are in module 11
TAX[TAX$module.grouping == "M11", ]


### Chlorplast sequences
