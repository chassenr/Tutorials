# R roundtable: Sequence analysis in R
# by Christiane Hassenrück
# ZMT Bremen, 21.1.2016
# R functions not provided on http://tinyurl.com/j8cvd4e
# can be found here: https://github.com/chassenr/NGS

setwd("H:/Sequence_analysis - R roundtable")

#load("SequenceAnalysis.Rdata")

### Loading packages
require(vegan)

#### Reading input data:
#  Data.txt: Sample-by-OTU table
#  Tax.txt: taxonomic paths
#  Env.txt: contextual data

# Sample-by-OTU table
Data <- read.table("Data.txt",   # file name
                   sep = "\t",   # tab-separated
                   h = T,        # first row contains column names
                   row.names = 1 # first column contains row names
                   )

# Taxonomic paths
#  sometimes taxonomic paths can contain ", ' and/or #,
#  which are used by default to detect quotes and comments in files
#  SILVAtaxopathProkaryotes.R to parse incomplete or truncated taxonomic paths
Tax <- read.table("Tax.txt", sep = "\t", row.names = 1, h = T, 
                  quote = "",       # do not use any special characters to detect quotes
                  comment.char = "" # do not use any special characters to detect comments
                  )

# Contextual data
Env <- read.table("Env.txt", sep = "\t", h = T, row.names = 1)
#  order factor levels by impact
Env$CO2 <- ordered(Env$CO2, levels = c("Ref", "Med", "High"))
#  create color vector for environmental conditions
Env$CO2col <- Env$CO2
levels(Env$CO2col) <- c("darkblue", "red", "orange")

# Check order of samples
all.equal(rownames(Data), rownames(Tax))
all.equal(colnames(Data), rownames(Env))

# If order doesn't match
#Tax_ordered <- Tax[match(rownames(Data), rownames(Tax)), ]
#Env_ordered <- Env[match(colnames(Data), rownames(Env)), ]

# Prune dataset
#  E.g. remove rare types, which will not be relevant for beta diversity calculation 
#  or differential OTU abundance
#  This can reduce your dataset considerable and speed up calculations
#  Common approaches are:
#  Remove singletons
#  Remove OTUs, which are not present in at least n samples (phyloseq)
#  Multivariate Cut-off Analyis (MultiCoLA: http://www.mpi-bremen.de/en/Software_4.html)

#  Here: an OTU has to be present with more than 2 sequences in more than 10% of the samples
Data_pruned <- Data[apply(Data, 1, function(x) {sum(x > 2) > (0.1 * length(x))}), ]
dim(Data_pruned)                            # checking dimensions
Tax_pruned <- Tax[rownames(Data_pruned), ]  # subsetting Tax accordingly
dim(Tax_pruned)
all.equal(rownames(Tax_pruned), rownames(Data_pruned))

#  How many sequences were retained
hist(colSums(Data_pruned) / colSums(Data), xlab = "% retained Sequnces", main = "")

# Convert sequence counts to relatve abundance (proportions)
relData <- prop.table(as.matrix(Data), # only works on matrices
                      margin = 2       # calculate percentages over columns
                      ) * 100          # convert to percentages
relData_pruned <- prop.table(as.matrix(Data_pruned),  margin = 2) * 100

# Did pruning of the dataset have an effect on beta diversity pattern?
mantel(vegdist(t(relData_pruned), method = "bray"),
       vegdist(t(relData), method = "bray"))               # Bray-Curtis dissimilarity
mantel(vegdist(t(relData_pruned), method = "jaccard", binary = T),
       vegdist(t(relData),method = "jaccard", binary = T)) # Jaccard dissimilarity (presence/absence)

# Calculating abundance tables for each taxonomic level
source("taxa.pooler.1.4.r")
M <- data.frame(Data_pruned, Tax_pruned[, 2:6])
Data_taxapooled <- taxa.pooler(M)
#  number of samples:          13
#  number of taxonomic levels:  5

Phylum <- t(Data_taxapooled$phylum)
relPhylum <- prop.table(Phylum, 2) * 100
Class <- t(Data_taxapooled$class)
relClass <- prop.table(Class, 2) * 100
Order <- t(Data_taxapooled$order)
relOrder <- prop.table(Order, 2) * 100
Family <- t(Data_taxapooled$family)
relFamily <- prop.table(Family, 2) * 100
Genus <- t(Data_taxapooled$genus)
relGenus <- prop.table(Genus, 2) * 100


### Alpha diversity

# Rarefaction curves
#  There is a built-in vegan function to plot rarefaction curves (rarecurve)
#  However, here is an example how to create the plot yourself
nSeq <- colSums(Data)                                  # number of Sequences per sample
maxOTU <- max(colSums(decostand(Data, method = "pa"))) # maximum number of OTUs per sample
par(mar = c(5,5,4,2))
plot(0, 0,                         # any coordinates would do
     type="n",                     # don't plot any data yet
     xlim=c(0,max(nSeq)+10000),          # range of x axis
     ylim=c(0,maxOTU),             # range of y axis
     xlab = "Number of sequences", # x axis title
     ylab = "OTU number",          # y axis title
     las = 1,                      # all tick labels horizontal
     mgp = c(4,1,0)                # position of axis label, ticks and axis
     )               
for(i in 1:ncol(Data)){    
  temp <- rarefy(Data[, i],                     # for each sample
                 sample = seq(0, nSeq[i], 1000) # subsample randomly every 1000 sequences
                 )
  lines(seq(0, nSeq[i], 1000), temp, col = as.character(Env$CO2col)[i])
  text(nSeq[i], max(temp), label = colnames(Data)[i], cex = 0.5, pos = 4)
}

# random subsampling
#  loading function
source("C:/Users/User/Documents/githubRepos/NGS/trunk/AMPLICON/SubsampleNGS.R")

#  Subsampling
Alpha <- SubsampleNGS(Data,           # input data
                      n = 100,        # repeat subsamping 100 times
                      sub = min(nSeq) # subsample to the minimum library size in the dataset
                      )

#save.image("SequenceAnalysis.Rdata")

# plotting alpha diversity indices and percentage rare biosphere
windows()                                             # open separate window (for mac: quartz)
par(mfrow = c(2, 4))                                  # arrange plots in 2 row
for (i in 1:nrow(Alpha$summaryAlpha)) {               # for each index
  plot(Alpha$summaryAlpha[i, ] ~ as.numeric(Env$CO2), # scatter plot  
       ylim = c(0, max(Alpha$summaryAlpha[i, ])), 
       type = "n", 
       xlim = c(0.5, length(levels(Env$CO2)) + 0.5),
       xlab = "",
       ylab = "",
       main = rownames(Alpha$summaryAlpha)[i],
       axes = F                                       # don't plot axes yet
       )
  axis(1,                              # x axis
       at = 1:length(levels(Env$CO2)), # ticks at each level
       labels = levels(Env$CO2))       # use level names and not numbers as tick labels
  axis(2)
  # plot median
  segments(c(1:length(levels(Env$CO2))) - 0.3,  
           by(Alpha$summaryAlpha[i, ], as.numeric(Env$CO2), median),
           c(1:length(levels(Env$CO2))) + 0.3,
           by(Alpha$summaryAlpha[i, ], as.numeric(Env$CO2), median),
           lwd = 2
           ) 
  # plot values
  points(Alpha$summaryAlpha[i, ] ~ as.numeric(Env$CO2), 
         pch = 22, 
         cex = 2, 
         col = "black",
         bg = as.character(Env$CO2col)
         )
}

# plotting Hill numbers
source("C:/Users/User/Documents/githubRepos/NGS/trunk/AMPLICON/PlotHillNGS.R")

PlotHillNGS(Alpha,                    # input data
            per.sample = F,           # plot lines per condition
            cond = Env$CO2,
            col = levels(Env$CO2col), # color for each condition
            method = "median",        # plot median lines...
            CI = T,                   # ...with interquartile range
            raw = F                   # don't add non-subsampled values
            )


### Beta diversity

# Plotting the n most abundant taxa per sample
source("C:/Users/User/Documents/githubRepos/NGS/trunk/PlotAbund.R")

PlotAbund(relPhylum[, order(Env$CO2)], 7)
PlotAbund(relClass[, order(Env$CO2)], 7)
PlotAbund(relOrder[, order(Env$CO2)], 7)
PlotAbund(relFamily[, order(Env$CO2)], 7)
PlotAbund(relGenus[, order(Env$CO2)], 7)

# Plotting OTU network
source("C:/Users/User/Documents/githubRepos/NGS/trunk/PlotOTUnet.R")

net.coord <- PlotOTUnet(Data_pruned, Tax_pruned,             # input OTU and taxonomy table
                        abund = 20,                          # use the 20 most abundant OTUs per sample
                        transparent = F,                     # don't use transparency as abundance indicator
                        sample.names = as.character(Env$CO2) # use environmental condition as sample labels
                        )

# Ordination plots
#  Non-metric multidimensional scaling
#  Reduce multidemsional dissimilarity table to 2 dimensions
#  while retaining as much information on the true community patterns as possible
NMDS <- metaMDS(t(relData_pruned), # OTU table (samples in rows)
                k= 2,              # calculate for 2 dimensions
                trymax = 50        # run a maximum of 50 iterations
                )
NMDS$stress  # display stress value

Data_clust <- hclust(vegdist(t(relData_pruned))) # create cluster diagram
plot(Data_clust)                                 # plot cluster diagram
rect.hclust(Data_clust, h = 0.8)                 # define groups at 80% dissimilarity
group_0.8 <- cutree(Data_clust, h = 0.8)         # save group designation

plot(NMDS, display = "sites",                              # plot the samples (sites) and not the OTUs (species)
     type = "n", axes = F, xlab = "", ylab = "")
ordihull(NMDS, display = "sites", groups = group_0.8)      # plot hulls around samples which as less than 80% dissimilar
points(NMDS, display = "sites", 
       pch = 22, cex = 2, col = "black", bg = as.character(Env$CO2col))
legend('bottomright',                                # add legend in bottomright corner
       legend = c(levels(Env$CO2), "Stress = 0.05"), # legend text
       pch = 22,
       pt.cex = 2,                                   # size of points
       col = c(rep("black", 3), "white"),
       pt.bg = c(levels(Env$CO2col), "white"),       # color of points
       bty = "o"                                     # draw box around legend
       )


### Multivariate statistics

# Analysis of similarity: ANOSIM
#  Are samples within groups more similar than between groups?
#  Based on dissimilarity matrix
anosim(t(relData_pruned), Env$CO2)

source("anosimPosthoc.R")
ANOSIMposthoc(t(relData_pruned), Env$CO2)

# PERMANOVA
#  Non-parametric multivariate analysis of variance
#  Based on any dissimilarity measure
adonis(t(relData_pruned) ~ Env$CO2)

# Redundancy analysos: RDA
#  parametric test
#  use clr or hellinger transformed data
temp_clr <- Data_pruned + 0.5                   # add prior to avoid problems with zeros
Data_clr <- temp_clr
for (i in 1:ncol(temp_clr)) {                   # for each sample
  log2gm <- mean(log2(temp_clr[, i]))           # calculate log2 of geometric mean
  Data_clr[, i] <- log2(temp_clr[, i]) - log2gm
}

RDA <- rda(t(Data_clr) ~ Env$CO2) # running the model
anova(RDA)                        # testing the model
RsquareAdj(RDA)                   # calculating adjusted R²
ordiresids(RDA)                   # checking assumptions


### Path analysis
require(plspm)
# not applicable to this dataset


### Differential OTU abundance
detach("package:igraph", unload = TRUE) # some functions are masked
require(ALDEx2)

# original aldex functions
#  only 1 environmental parameter
Data_clr_aldex <- aldex.clr(data.frame(Data_pruned), mc.samples = 2)
Data_glm_aldex <- aldex.glm(Data_clr_aldex, Env$CO2)
#save.image("SequenceAnalysis.Rdata")
OTU_sign <- Data_glm_aldex[Data_glm_aldex$glm.eBH < 0.05 & Data_glm_aldex$kw.ep < 0.05, ] 

# modified aldex functions
#  for any glm or lmer
source("aldex.glm2.R")
source("aldex.lmer.R")

Test_data <- read.table("Test_data.txt", h = T, sep = "\t")
Test_env <- read.table("Test_env.txt", h = T, sep = "\t")
all.equal(colnames(Test_data), rownames(Test_env))

Test_clr <- aldex.clr(data.frame(Test_data), mc.samples = 2)
Test_glm <- aldex.glm2(Test_clr, "~ type + location", Test_env)
head(Test_glm$BH.glm)
boxplot(prop.table(as.matrix(Test_data), 2)[2, ] ~ interaction(Test_env$type, Test_env$location),
        las = 2)

Test_lmer <- aldex.lmer(Test_clr, "~ type + (1|location)", Test_env)
head(Test_lmer$BH.lmer)


### Estimating bacterial functions
# dependencies: qiimer, biom
# download .zip and install from .zip
# also download and unzip SILVA 119 reference files
require(Tax4Fun)

# reading taxonomic path
Path <- read.table("Path4Fun.txt",sep = "\t", h = T, row.names = 1,
                   quote = "", comment.char = "", stringsAsFactors = F)
all.equal(rownames(Data), rownames(Path))

# preparing input for Tax4Fun
#  sum up sequence counts per taxonomic path
Data_path <- aggregate(Data, 
                       by = list(as.factor(Path$Tax.path)), # grouping vector(s)
                       FUN = sum                        # sum per variable and group
                       )
rownames(Data_path) <- Data_path$Group.1
Data_path <- Data_path[, -1]

input_tax4fun <- list(sampleNames = colnames(Data_path), 
                      otuTable = Data_path)

# Running Tax4Fun
tax4fun_res=Tax4Fun(input_tax4fun,
                    "C:/Users/User/Documents/R/win-library/3.2/Tax4Fun/SILVA119", #location of reference database
                    fctProfiling = TRUE,                                          # using  the  pre-computed KEGG Ortholog reference profiles
                    refProfile = "UProC",                                         # method for pre-computing reference profiles
                    shortReadMode = FALSE,                                        # 400bp reads
                    normCopyNo = TRUE                                             # adjust for rRNA gene copy number
                    )
str(tax4fun_res)
Data_fun <- t(tax4fun_res$Tax4FunProfile) * 100
head(Data_fun[order(rowSums(Data_fun), decreasing = T), ])
SRB <- Data_fun[grep("EC:1.8.99", rownames(Data_fun)),][-3, ]

windows()  
par(mfrow = c(2, 2)) 
for (i in 1:nrow(SRB)) { 
  plot(SRB[i, ] ~ as.numeric(Env$CO2), 
       ylim = c(0, max(SRB[i, ])), xlim = c(0.5, length(levels(Env$CO2)) + 0.5),
       type = "n", xlab = "", ylab = "",  main = c("aprA","aprB","dsrA","dsrB")[i], axes = F)
  axis(1, at = 1:length(levels(Env$CO2)), labels = levels(Env$CO2))
  axis(2)
  # plot median
  segments(c(1:length(levels(Env$CO2))) - 0.3, by(SRB[i, ], as.numeric(Env$CO2), median),
           c(1:length(levels(Env$CO2))) + 0.3, by(SRB[i, ], as.numeric(Env$CO2), median), lwd = 2) 
  # plot values
  points(SRB[i, ] ~ as.numeric(Env$CO2), pch = 22, cex = 2, col = "black", bg = as.character(Env$CO2col))
}
