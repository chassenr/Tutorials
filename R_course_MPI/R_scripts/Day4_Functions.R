#########################################################
# R course: Day 4 (Thursday, 14.09.2016)                #
#########################################################

# we have written a lot of R code in the last few days
# most of it for plotting
# often you need to produce several similar plots
# to avoid writing endless scripts, a certain block of code can be stored in a function
# executing that function will only take one line of code
# e.g. you need to perform the same task several times for different data sets
# if you can generalize the code, you can save it as function to be applied to different data sets
# you have already written functions
# in every instance of apply(x, margin, FUN) you specified a function

# annotation:
function.name <- function(x) {
  # what you want to do with x
}
# usage
function.name(x = yourData)

# Now we will look at:
# 1) the plotting script to produce stacked bar plots with the taxonomic composition
# 2) a post-hoc function for anosim
# 3) plotting gene annotation output

setwd("C:/Users/User/Documents/githubRepos/Tutorials/trunk/R_course_MPI/Example_data")
# load("Vent.Rdata")


### taxonomic composition ####

# previous code: ####
relData <- TAX.pooled.rel$class

# we only want to plot the 10 most abundant classes per sample
abund <- 10

# specify color palette
colorPalette <- c("violet", "purple", "blue", "white", "darkgreen", "yellow", "red", "darkred")

# extract names of the 10 most abundant taxa per sample
abund_names <- c()
for (i in 1:ncol(relData)) {
  abund_names <- unique( # remove duplicates
    c(
      abund_names, # abundant taxa from previous sample
      rownames(relData)[order(relData[, i], decreasing = T)][1:abund] # order taxa by relative abundance 
    )
  )
}

# select abundant taxa from input
abund_rel0 <- relData[abund_names, ]

# add row for remaining taxa, pooled as 'other'
abund_rel <- rbind(abund_rel0, 100 - colSums(abund_rel0))
rownames(abund_rel)[nrow(abund_rel)] <- "other"

# convert to matrix
abund_rel <- as.matrix(abund_rel)

# open new graphics device
windows(width = 8, height = 6)
par(
  mfrow = c(1, 2), # 2 plots in 1 row and 2 column
  xpd = NA # allow plotting on margins
)

# first plot: stacked bar plot
barplot(
  abund_rel, 
  col = c(colorRampPalette(colorPalette)(nrow(abund_rel) - 1), "darkgrey"), # add darkgrey to color character for 'other'
  ylim = c(0, 100), 
  las = 2,
  ylab = "Relative sequence abundance [%]"
)

# second plot: legend
plot.new()
legend(
  "center", 
  pch = 22, # filled squares with black border
  col = "black", # border of squares
  pt.bg = rev(c(colorRampPalette(colorPalette)(nrow(abund_rel) - 1), "darkgrey")), # color of squares
  legend = rev(rownames(abund_rel)), # legend labels, reversed order, to match order of occurrence in barplot (plotted bottom up)
  pt.cex = 1.5, 
  cex = 0.8
) 
### end previous code ####

# we needed 3 R objects as input for that script:
# relData (data.frame with percentages of taxa (rows) per sample (columns))
# abund (integer value, how many taxa should be plotted)
# colorPalette (character vector, color range for taxa)

# if we have these 3 R objects,
# we can repeat the script without changing anything else

# define function:
PlotAbund <- function(relData, # list arguments to function to be provided by user
                      abund, 
                      colorPalette = c("violet", "purple", "blue", "white", "darkgreen", "yellow", "red", "darkred") # include default
                      ) { 
  # copy paste previous code here:
  # extract names of the 10 most abundant taxa per sample
  abund_names <- c()
  for (i in 1:ncol(relData)) {
    abund_names <- unique( # remove duplicates
      c(
        abund_names, # abundant taxa from previous sample
        rownames(relData)[order(relData[, i], decreasing = T)][1:abund] # order taxa by relative abundance 
      )
    )
  }
  
  # select abundant taxa from input
  abund_rel0 <- relData[abund_names, ]
  
  # add row for remaining taxa, pooled as 'other'
  abund_rel <- rbind(abund_rel0, 100 - colSums(abund_rel0))
  rownames(abund_rel)[nrow(abund_rel)] <- "other"
  
  # convert to matrix
  abund_rel <- as.matrix(abund_rel)
  
  # open new graphics device
  windows(width = 8, height = 6)
  par(
    mfrow = c(1, 2), # 2 plots in 1 row and 2 column
    xpd = NA # allow plotting on margins
  )
  
  # first plot: stacked bar plot
  barplot(
    abund_rel, 
    col = c(colorRampPalette(colorPalette)(nrow(abund_rel) - 1), "darkgrey"), # add darkgrey to color character for 'other'
    ylim = c(0, 100), 
    las = 2,
    ylab = "Relative sequence abundance [%]"
  )
  
  # second plot: legend
  plot.new()
  legend(
    "center", 
    pch = 22, # filled squares with black border
    col = "black", # border of squares
    pt.bg = rev(c(colorRampPalette(colorPalette)(nrow(abund_rel) - 1), "darkgrey")), # color of squares
    legend = rev(rownames(abund_rel)), # legend labels, reversed order, to match order of occurrence in barplot (plotted bottom up)
    pt.cex = 1.5, 
    cex = 0.8
  )
}

# if you run that function script, PlotAbund will be listed as function in your environment
# if you save the function script in a separate R file, the function can be imported with:
source("PlotAbund.R")

# to run the function you don't have to rename your objects to match the function arguments
# the function can be run several times, e.g. for each taxonomic level
# like any other R function provided by an R package you can run:
PlotAbund(relData = TAX.pooled.rel$phylum, abund = 10, colorPalette = c("white", "red", "blue"))
PlotAbund(TAX.pooled.rel$class, 10)
PlotAbund(TAX.pooled.rel$order, 10)
PlotAbund(TAX.pooled.rel$family, 7)
PlotAbund(TAX.pooled.rel$genus, 7)
PlotAbund(OTU.rel, 5)


### anosim posthoc function ####
# the problem with writing R functions is that they should be universally executable
# i.e. they should work with more than 1 data set
# sometime a lot of extra code is necessary to accommodate different scenarios

require(vegan)

# to get adjusted p-values for anosim we used the following code yesterday:
OTU.anosim.posthoc <- p.adjust(
  c(
    anosim(Y[X != "high", ], droplevels(X[X != "high"]))$signif, # comparison reference - medium
    anosim(Y[X != "medium", ], droplevels(X[X != "medium"]))$signif, # comparison reference - high
    anosim(Y[X != "reference", ], droplevels(X[X != "reference"]))$signif # comparison medium - high
  ),
  method = "fdr" # false discovery rate, other options: e.g. Bonferroni
)

# to generalize this code and to add some more parameters from the anosim output we need to:
# account for a different number of levels in the explanatory variable
# account for different dissimilarity measures
# include ANOSIM R and unadjusted p-value
# include the option to chose the adjustment method for the p-values
# here is an example, how such a function could look like:

ANOSIMposthoc <- function(M, # community data (samples in rows)
                          E, # explanatory variable (factor)
                          distance = "bray", # dissimilarity measure (default: Bray-Curtis)
                          padj = "fdr") { # p-value adjustment method (default: fdr)
  # remove empty levels from explanatory variable
  E <- droplevels(E) 
  
  # create subsets of community table for each level of explanatory variable
  Mlist <- list()
  for (i in 1:length(levels(E))) {
    Mlist[[i]] <- M[E == levels(E)[i], ]
  }
  
  # create subset of explanatory variable for each level, store as numeric
  Elist <- list()
  for (i in 1:length(levels(E))) {
    Elist[[i]] <- as.numeric(E[E == levels(E)[i]])
  }
  
  # create results object
  # list with 3 elements: ANOSIM R, unadjusted p, adusted p
  result <- list(
    anosimR = matrix(NA, length(levels(E)), length(levels(E))),
    anosimP = matrix(NA, length(levels(E)), length(levels(E))),
    anosimPadj = matrix(NA, length(levels(E)), length(levels(E)))
  )
  # row and column names correspond to names of levels in explanatory variable
  colnames(result$anosimR) = colnames(result$anosimP) = levels(E)
  rownames(result$anosimR) = rownames(result$anosimP) = levels(E)
  
  # run pairwise comparisons
  # level 1: compare to levels 2 to n
  # level 2: compare to levels 3 to n
  # etc...
  # level n -1: compare to level n
  
  # for each level except the last
  for (i in 1:(length(levels(E)) - 1)) {
    # for each level that is one higher than i
    for (j in (i + 1):length(levels(E))) {
      # create temporary object with anosim result for each comparison
      temp <- anosim(
        rbind(Mlist[[i]], # fuse community tables for 2 conditions
              Mlist[[j]]),
        c(Elist[[i]], # fuse explanatory variable for 2 conditions
          Elist[[j]]),
        distance = distance) # specify diisimilarity measure
      result$anosimR[j, i] <- temp$statistic # extract ANOSIM R
      result$anosimP[j, i] <- temp$signif # extract p-value
    }
  }
  
  # adjust p-value
  result$anosimPadj <- matrix( # save output as matrix
    p.adjust(as.vector(result$anosimP), # transform matrix to vector
             method = padj,
             n = sum(!is.na(as.vector(result$anosimP)))), # specify length, since NA in input
    length(levels(E)),
    length(levels(E))
  )
  colnames(result$anosimPadj) <- levels(E)
  rownames(result$anosimPadj) <- levels(E)
  
  # final result of function
  return(result)
}

# save as separate script and source
source("AnosimPosthoc.R")
require(vegan)

# run with bacterial and archaeal OTU matrices
ANOSIMposthoc(t(OTU.rel), ENV.all$seep.influence)
ANOSIMposthoc(t(OTU.rel.arch), ENV.all$seep.influence)

# on a different taxonomic level
ANOSIMposthoc(t(TAX.pooled.rel$genus), ENV.all$seep.influence)

# note that (sometimes) the p-values for the comparison reference-high are lower than for the other 2 comparisons
# even though the ANOSIM R is comparable
# this is an artefact of the permutation test used by anosim to test for significance
# for a low level of replication, the number of possible permutations is too low

# save.image("Vent.Rdata")


### gene annotation map ####
# there is a package called genoPlotR which should do more or less the same thing
# if you are familiar with plotting in R
# creating your own function and using it to plot gene annotation data may be more intuitive

# usually gene annotations are shown with horizontal filled arrows
# to create such an arrow you need to:
# use polygon() for plotting
# make length of arrow adjustable
# make length of arrow head adjustable
# make width of arrow adjustable
# specify on which horizontal line to plot
# add labels for annotation, if available
# account for reverse orientation

gene <- read.table("gene_annotation.txt", h = T, sep = "\t")

horiz.arrow <- function(start, # start base position
                        end, # end base position
                        width, # width of arrow
                        head, # length of arrow head
                        border = "black", # border color of arrow
                        fill = "orange", # color of arrow
                        horiz = 0, # which horizontal line
                        rev = F, # reverse orienation (default: FALSE)
                        annotation = NULL) { # don't inlcude feature names
    # reverse orientation
  if (rev == T) {
    polygon(x = c(start, # lower left
                  end + head, # begin head lower
                  end + head, # head lower
                  end, # point
                  end + head, # head upper
                  end + head, # end head upper
                  start), # upper left
            y = c(horiz - width/2, # lower left
                  horiz - width/2, # begin head lower
                  horiz - width, # head lower
                  horiz, # point
                  horiz + width, # head upper
                  horiz + width/2, # end head upper
                  horiz + width/2), # upper left
            border = border,
            col = fill
    )
  } 
  
  # forward orientation
  if (rev == F) {
    polygon(x = c(start, # lower left
                  end - head, # begin head lower
                  end - head, # head lower
                  end, # point
                  end - head, # head upper
                  end - head, # end head upper
                  start), # upper left
            y = c(horiz - width/2, # lower left
                  horiz - width/2, # begin head lower
                  horiz - width, # head lower
                  horiz, # point
                  horiz + width, # head upper
                  horiz + width/2, # end head upper
                  horiz + width/2), # upper left
            border = border,
            col = fill
    )
  }
  
  # plot labels if provided
  if (!is.null(annotation)) {
    text(mean(c(start, end)),
         horiz + width,
         labels = annotation,
         cex = 0.5,
         pos = 3
    )
  }
}

# if saved as separate R script
source("HorizArrow.R")

plot(
  0, 0,
  ylim = c(-1, 2),
  xlim = c(22400, 27600),
  type = "n",
  axes = F,
  ylab = "",
  xlab = "bp"
)
axis(
  1,
  pos = 0,
  at = seq(22400, 27600, 1000),
  labels = paste(seq(22400, 27600, 1000)/1000,
                 "kb", sep = "")
)

for(i in 1:nrow(gene)) {
  horiz.arrow(start = gene$Stop[i],
              end = gene$Start[i],
              width = 0.1,
              head = 100,
              horiz = 0.5,
              rev = T,
              annotation = gene$Feature[i])
}



