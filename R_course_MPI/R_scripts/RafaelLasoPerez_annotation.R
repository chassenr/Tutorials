# genome annotation map
# input data with at least the following columns
# shortName: gene name (shouldn't be too long, otherwise labels will overlap)
# start: start position of gene
# stop: stop position of gene
# contig: name of contig that gene belongs to
# Horiz: factor (or number) giving horizontal line for annotation plots

setwd("C:/Users/User/Documents/programming/Tutorials/R_course_MPI/Example_data/")

# load function
source("../R_scripts/HorizArrow.R")

# read input data
gene <- read.table(
  "gene_contigs.txt",
  h = T,
  sep = "\t"
)

# convert Horiz to factor
gene$Horiz <- as.factor(gene$Horiz)

# add column for orientation
gene$orientation <- gene$Start > gene$Stop

# quick and dirty plot
# for each (partial) contig a new plot is created
# for each gene a new arrow is plotted
# forward and reverse genes are on different horitontal lines

# open graphics device
windows(width = 7, height = 5)

# specify margings and number of plots
par(mar = c(2, 2, 2, 2), mfrow = c(length(levels(gene$Horiz)), 1))

# for each (partial) contig
for(i in 1:length(unique(gene$Horiz))) {
  
  # for convenience subset table to contig
  temp <- droplevels(gene[gene$Horiz == levels(gene$Horiz)[i], ])
  
  # create plot (no data plotted yet)
  plot(
    0, 0,
    ylim = c(-0.5, 0.5),
    # define x-axis range larger and min and max of start and end position
    xlim = c(min(temp[, 2:3]) - diff(range(temp[, 2:3])) * 0.1, 
             max(temp[, 2:3]) + diff(range(temp[, 2:3])) * 0.1),
    type = "n",
    axes = F,
    ylab = "",
    xlab = "",
    main = levels(temp$contig),
    cex.main = 1,
    font.main = 1
  )
  
  # plot x-axis
  axis(
    1,
    # guess pretty positions for axis ticks
    at = pretty(c(min(temp[, 2:3]), 
                  max(temp[, 2:3])),
                n = 6), # show n intervals on axis
    pos = -0.5
  )
  
  # plot arrow for each gene
  for(j in 1:nrow(temp)) {
    horiz.arrow(
      start = temp$Start[j],
      end = temp$Stop[j],
      width = 0.08,
      head = 80,
      # if forward orientation plot slightly higher
      horiz = if (temp$orientation[j] == F) { 0.2 } else { -0.2 },
      rev = temp$orientation[j],
      annotation = temp$shortName[j]
    )
  }
  
  # remove temporary subset of data for each i
  rm(temp)
}


