#########################################################
# R course: Day 3 (Wednesday, 14.09.2016)               #
#########################################################

# the examples below are only intended to demonstrate how to run statistical tests in R
# some tests are shown, which may not be the most appropriate ones for the data
# also for demonstrative purposes, several tests will be run on the same data set


# let's go back to the vent data set
setwd("E:/PhD/courses/R_course_MPI/ExampleData/")

# load workspace with vent data
# load("Vent.Rdata")

# before you run any statistical test: ALWAYS HAVE A LOOK AT YOUR DATA FIRST
# this will give you an idea of the shape of your data, and which tests may be appropriate

# checking assumptions of data are difficult when there are too few observations
# results may not be meaningful


### t test ####
# compare pH values between reference and medium sites
Y <- ENV.all$pH[ENV.all$seep.influence != "high"]
X <- droplevels(ENV.all$seep.influence[ENV.all$seep.influence != "high"])

# look at data
boxplot(Y ~ X)
by(Y, X, summary)

# check assumptions
# normality
by(Y, X, shapiro.test)
# homoscedacity
require(car)
leveneTest(Y, group = X)

# run t test
pH.T <- t.test(Y ~ X)

# confidence intervals of the mean difference
# useful for plotting error bars
pH.T$conf.int


### Wilcoxon test ####
# 'non-parametric t test'
# same example as for t test

pH.wilcox <- wilcox.test(Y ~ X)


### ANOVA ####
# comparing more than 2 groups
# unifactorial and multifactorial designs
# categorical and continuous explanatory variables
# same as lm()
Y <- ENV.all$pH
X <- ENV.all$seep.influence

# oav
pH.aov <- aov(Y ~ X)
# get p-value
summary(pH.aov)

# lm
pH.lm <- lm(Y ~ X)
# get p-value
anova(pH.lm)

# check assumptions
plot(pH.aov)
# that doesn't really look good....


# permutation test ###
# randomly reshuffle data set
# i.e. sampling without replacement
# this is not bootstrapping (sampling with replacement)

# extract F values
F.X <- summary(pH.aov)[[1]]$'F value'[1]

# create vectors for random F values
F.X.random <- c()

# calculate 999 F values based on random permutations
require(permute)
for(i in 1:999) {
  F.X.random[i] <- summary(aov(Y[shuffle(length(Y))] ~ X))[[1]]$'F value'[1]
}

# calculate p-values for permutation tests
sum(F.X.random > F.X)/(length(F.X.random) + 1)

# plot permutation F and theoretical F distribution
hist(
  F.X.random, 
  las = 1, 
  xlim = c(0, 10),
  breaks = 50, # number of bins
  prob = TRUE, # display probability and not frequency
  ylim = c(0, 1)
)
abline(v = F.X)

# theoretical F distribution
F.X.dist <- df( # calculate density/probability for the specified quantiles
  seq(0, 10, length = 100), # quantiles (x-values in plot)
  df1 = 2, # numerator df
  df2 = 9 # denominator df
)
lines( # add lines to plot
  seq(0, 10, length = 100), 
  F.X.dist,
  col = "red"
)

# post-hoc tests
# pairwise comparisons between seep influence
TukeyHSD(pH.aov, which = "X")


### Kruskal-Wallis test ####
# non-parametric ANOVA
# only unifactorial

pH.kruskal <- kruskal.test(Y ~ X)

# there are R packages, which offer post-hoc tests for kruskal.test
# or use p-value adjusted wilcoxon tests
pH.kuskal.posthoc <- p.adjust( # adjust p values
  c(
    wilcox.test(Y[X != "high"] ~ droplevels(X[X != "high"]))$p.value, # comparison reference - medium
    wilcox.test(Y[X != "medium"] ~ droplevels(X[X != "medium"]))$p.value, # comparison reference - high
    wilcox.test(Y[X != "reference"] ~ droplevels(X[X != "reference"]))$p.value # comparison medium - high
  ),
  method = "fdr", # false discovery rate, other options: e.g. Bonferroni
)


### Mixed-effects model ####
# using ENV as example data
# difference in pH between seep influence
# accounting for site as random variable
lmer.input <- data.frame(Y = ENV$pH, X = ENV$seep.influence, Z = ENV$site)

# mixed model
# random factor specified with (1|random.factor)
require(lme4)
pH.lmer <- lmer(Y ~ X + (1|Z), data = lmer.input)
# or:
require(nlme)
pH.lmer <- lme(Y ~ X, data = lmer.input, random = ~ 1|Z)

# get P values
require(lmerTest)
anova(pH.lmer)

# run post-hoc tests
require(multcomp)
summary(
  glht(
    pH.lmer, # model
    linfct = mcp(X = "Tukey") # specify on which model term to calculate pairwise comparisons
  )
)

# residuals vs. fitted plot
plot(pH.lmer)
plot(residuals(pH.lmer) ~ fitted(pH.lmer))
# looks kind of ok...


### PCA plot ####
# unconstrained ordination
# reduce dimensions of numerical data
# based on euclidean distances

# there are several implementations of PCA in R
# we use rda here
# rda doesn't like NAs, so only bottom water values will be used
require(vegan)
ENV.pca <- rda(ENV.num, scale = T) 

# inspect output
str(ENV.pca)

# cumulative percentage variation displayed on PCs
cumsum(ENV.pca$CA$eig/sum(ENV.pca$CA$eig) * 100)

# create PCA plot
ENV.pca.scaling2 <- plot(ENV.pca, scaling = 2)

# build plot element by element
plot(
  0, 0,
  type = "n",
  las = 1,
  xlim = c(min(c(ENV.pca.scaling2$species[, 1],
                 ENV.pca.scaling2$sites[, 1])),
           max(c(ENV.pca.scaling2$species[, 1],
                 ENV.pca.scaling2$sites[, 1]))),
  ylim = c(min(c(ENV.pca.scaling2$species[, 2],
                 ENV.pca.scaling2$sites[, 2])),
           max(c(ENV.pca.scaling2$species[, 2],
                 ENV.pca.scaling2$sites[, 2]))),
  xlab = paste("PC1 (", 
               round(ENV.pca$CA$eig[1]/sum(ENV.pca$CA$eig) * 100, 2),
               "%)",
               sep = ""),
  ylab = paste("PC2 (", 
               round(ENV.pca$CA$eig[2]/sum(ENV.pca$CA$eig) * 100, 2),
               "%)",
               sep = "")
)
abline(v = 0, lty = 2) # vertical line
abline(h = 0, lty = 2) # horizontal line

# sites (samples)
points(
  ENV.pca.scaling2$sites,
  pch = 15,
  col = ENV.all$seep.color
)

# species (variables)
arrows(
  rep(0, nrow(ENV.pca.scaling2$species)), # start x
  rep(0, nrow(ENV.pca.scaling2$species)), # start y
  ENV.pca.scaling2$species[, 1], # end x
  ENV.pca.scaling2$species[, 2], # end y
  length = 0.15, # length of arrow head
  angle = 20 # angle of arrow head
)
text(
  ENV.pca.scaling2$species,
  cex = 0.8,
  labels = rownames(ENV.pca.scaling2$species),
  pos = 3
)


### NMDS plots ####
# non-metric multidimensional scaling
# based on Bray-Curtis dissimilarity
# show maximum variation in 2 dimensions

OTU.nmds <- metaMDS(t(OTU.rel))

# stress value: distortion of true distances in NMDS plot 
# should be less than 0.2
OTU.nmds$stress

# default plot (only showing sites)
plot(OTU.nmds, display = "sites")

# build plot element by element
plot(
  0, 0,
  type = "n",
  axes = F,
  asp = 1,
  xlim = c(min(OTU.nmds$points[, 1]),
           max(OTU.nmds$points[, 1])),
  ylim = c(min(OTU.nmds$points[, 2]),
           max(OTU.nmds$points[, 2])),
  xlab = "NMDS1",
  ylab = "NMDS2"
)
box("plot")

# hulls by seep influence
ordihull(OTU.nmds, groups = ENV.all$seep.influence)

# sites
points(
  OTU.nmds$points,
  pch = 15,
  col = ENV.all$seep.color
)
text(
  OTU.nmds$points,
  cex = 0.8,
  labels = colnames(OTU),
  pos = 4
)


### ANOSIM ####
# analysis of similarity
# only unifactorial
# based on triangular dissimilarity matrix between observations (samples)
Y <- t(OTU.rel) # transpose since vegan expects samples in rows
X <- ENV.all$seep.influence

require(vegan)
OTU.anosim <- anosim(Y, grouping = X)

# there is no post-hoc function for anosim in R
OTU.anosim.posthoc <- p.adjust(
  c(
    anosim(Y[X != "high", ], droplevels(X[X != "high"]))$signif, # comparison reference - medium
    anosim(Y[X != "medium", ], droplevels(X[X != "medium"]))$signif, # comparison reference - high
    anosim(Y[X != "reference", ], droplevels(X[X != "reference"]))$signif # comparison medium - high
  ),
  method = "fdr", # false discovery rate, other options: e.g. Bonferroni
)


### PERMANOVA ####
# permutational ANOVA, non-metric multivariate ANOVA
# also multifactial sampling designs
# based on triangular dissimilarity matrix between observations (samples)

OTU.adonis <- adonis(Y ~ X)


### RDA ####
# constrained ordination
# same function as for PCA, but with explanatory variables
# linear technique
# transformation of Y necessary?

OTU.rda <- rda(Y ~ X)
ordiresids(OTU.rda)

# hellinger transformation
Y.hell <- decostand(Y, "hellinger")
OTU.rda <- rda(Y.hell ~ X)
ordiresids(OTU.rda)

# centered log ratio transformation
# clr = log(x / geometric mean (x))
# geometric mean: exp(mean(log(x)))
# http://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
Y.clr <- t(
  apply(
    OTU + 0.5, # add prior of half the detection limit to remove zeros, i.e. 0.5 for sequence counts
    2, # apply to columns (samples)
    function(x) {
      log2(x) - mean(log2(x)) # clr transformation
    }
  )
)
OTU.rda <- rda(Y.clr ~ X)
ordiresids(OTU.rda)

# significance
OTU.anova <- anova(OTU.rda)

# explained variation
RsquareAdj(OTU.rda)

# variation partitioning
# define additional explanatory variable
Z <- ENV.all$SiO4

# include conditioning variable
# account for variation explained by Z when assessing X
OTU.rda.condition <- rda(Y.clr ~ X + Condition(Z))
OTU.anova.condition <- anova(OTU.rda.condition)
RsquareAdj(OTU.rda.condition)

# visualize variation partitioning
OTU.varpart <- varpart(
  Y.clr, # response variable
  model.matrix(~X)[,-1], # factors have to be converted to a dummy matrix
  Z # dummy matrix not necessary for numerical data
)
showvarparts(2) # show venn diagram
plot(OTU.varpart) # plot contribution to explaining variation


### SIMPER ####
# to find out which taxa are responsible for the differences between seep influence
# maybe not the best method for this
# see ?simper

OTU.simper <- simper(
  t(relOTU),
  ENV.all$seep.influence
)

# inspect simper output
str(OTU.simper)
# there are 3 tables for each pairwise comparison

# write simper output to file
# one file for each comparison
# only look at OTUs contributing at least to 70% of the differences between groups
for(i in 1:length(OTU.simper)) {
  write.table(
    ,
    paste(),
    sep = "\t",
    quote = F
  )
}


### Differential OTU abundance ####
# use variance-stabilizing transformation
# run multiple univariate tests (one for each OTU)
# correct p-values
# using ALDEx2

require(ALDEx2)


### Mantel test ####
# compare the OTU table of bacterial and archaeal communities at the CO2 vents
# based on Bray-Curtis dissimilarity

# read archaeal OTU table
OTU.arch <- read.table(
  "OTU_arch_table.txt", 
  h = T, # first line contains column names
  sep = "\t", # values separated by tabstops
  row.names = 1 # first row contains row names, i.e. OTU names
)
head(OTU.arch)

# match order of samples in OTU
OTU.arch <- OTU.arch[, colnames(OTU)]

# calculate realtive abundances
OTU.rel.arch <- prop.table(as.matrix(OTU.arch), 2) * 100

# load R package
require(vegan)

OTU.mantel <- mantel(
  vegdist(t(OTU.rel)), # triangular dissimilarity matrix 1
  vegdist(t(OTU.rel.arch)) # triangular dissimilarity matrix 2
)
plot(
  vegdist(t(OTU.rel)), 
  vegdist(t(OTU.rel.arch)), 
  las = 1, 
  xlab = "Bray-Curtis dissimilarity bacteria",
  ylab = "Bray-Curtis dissimilarity archaea"
)
abline(lm(vegdist(t(OTU.rel.arch)) ~ vegdist(t(OTU.rel))))


### Procrustes test ####
# compare 2 ordination objects

# create NMDS plot for archaeal data set
OTU.nmds.arch <- metaMDS(t(OTU.rel.arch))

# plot procrustes rotation
plot(procrustes(OTU.nmds, OTU.nmds.arch), las = 1)

# significance test
protest(OTU.nmds, OTU.nmds.arch)

# save workspace
# save.image("Vent.Rdata")
