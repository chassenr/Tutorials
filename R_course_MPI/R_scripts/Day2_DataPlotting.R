#########################################################
# R course: Day 2 (Tuesday, 13.09.2016)                 #
#########################################################

# set working directory
setwd("C:/Users/User/Documents/githubRepos/Tutorials/trunk/R_course_MPI/Example_data")

# load workspace with vent data
# load("Vent.Rdata")


### building plot element by element ####
# plotting mean pH values per condition (treatment, time interval, sampling time) 
# whiskers show range (min/max), but can also show error bars (e.g. standard error)

# format input
# using do.call() and by()
# with by() we create summaries of pH per site
# the output of by() is a list
# do.call() runs function using data supplied in the list
pH.summary <- do.call(
  rbind, # concatonate rows, function that uses elements of by() as input
  by(ENV$pH, # vector
     ENV$site, # grouping variable
     summary) # function to be applied
)

# also add seep.influence information and color
# we have this data in ENV.all
pH.plot <- data.frame(
  pH.summary, 
  seep.influence = ENV.all[ rownames(pH.summary), "seep.influence"], # use rownames to ensure the right order
  seep.color = ENV.all[ rownames(pH.summary), "seep.color"]
)

# order input for plot by seep influence
pH.plot <- pH.plot[order(pH.plot$seep.influence), ]

# check data types
str(pH.plot)
# seep.color has been converted to factor
# convert to character
pH.plot$seep.color <- as.character(pH.plot$seep.color)

# open separate graphics device
windows(width = 8, height = 6) # width and height of graphics device in inches
# on Mac: use quartz()
# on linux: X11()

# set margins of plot
par(mar = c(5, 5, 2, 2))
# there are many, many more parameters that can be specified for plotting
# see ?par for more details

# create empty plot
plot(
  0, 0, # since no data is plotted yet, x and y coordinates are irrelevant
  type = "n", # don't plot anything
  xlim = c(1, nrow(pH.plot) + 5), # range of x-axis, give some extra space for legend
  ylim = c(min(c(ENV$pH, ENV.all$pH.sed), na.rm = T), # range of y-axis
           max(c(ENV$pH, ENV.all$pH.sed), na.rm = T)), # consider both bottom water and profile pH
  xlab = "", # don't plot x-axis label
  ylab = "", # don't plot y-axis label
  axes = F, # don't plot axes
  xpd = NA # allow plotting on margins of plot
)

# add axes to plot
# check range of pH values
range(c(ENV$pH, ENV.all$pH.sed), na.rm = T)

axis(
  2, # add axis at side = 2, i.e. first y-axis
  at = seq(6.6, 8.3, 0.2), # define position of ticks, from pH 6.6 to 8.3 every 0.2 units
  las = 2, # labels perpendicular to axis
  cex.axis = 0.8 # size of tick labels
)

axis(
  1, # add x-axis
  at = 1:nrow(pH.plot),
  labels = rownames(pH.plot), # specify tick labels
  cex.axis = 0.8,
  las = 1
)

# add axis labels
mtext(
  side = 1,
  text = "Sampling sites", 
  line = 3, # margin line
  at = 7 # position along x-axis
)

mtext(
  2, 
  text = "pH", 
  line = 3,
  at = 7.4
)

# plot whiskers
# showing range
segments(
  x0 = 1:nrow(pH.plot), # draw lines from x ...
  y0 = pH.plot$Min., # ... and from y ...
  x1 = 1:nrow(pH.plot), # ... to x ...
  y1 = pH.plot$Max. # ... and y
)
# lower end of whiskers
segments(
  x0 = 1:nrow(pH.plot) - 0.3, 
  y0 = pH.plot$Min., 
  x1 = 1:nrow(pH.plot) + 0.3, 
  y1 = pH.plot$Min.
)
# upper end of whiskers
segments(
  x0 = 1:nrow(pH.plot) - 0.3, 
  y0 = pH.plot$Max., 
  x1 = 1:nrow(pH.plot) + 0.3, 
  y1 = pH.plot$Max.
)

# plot mean pH values
points(
  1:nrow(pH.plot), # x values
  pH.plot$Mean, # y values
  pch = 18, # point character (symbol)
  cex = 2, # character expansion (point size)
  col = pH.plot$seep.color # color
)

# add profile pH values for reference
points(
  1:nrow(pH.plot),
  ENV.all[rownames(pH.plot), "pH.sed"], # use rownames(pH.plot) to select and order at the same time
  pch = 5,
  cex = 1,
  lwd = 2, # line width
  col = pH.plot$seep.color
)

# add legend
# there is some space on the right side of the plot
text(
  c(15, 17), # x values
  c(6.9, 6.9), # y values
  labels = c("Bottom water", "Sediment"), # text to be plotted
  cex = 0.8
)
text(
  c(15, 15, 15),
  c(6.8, 6.7, 6.6),
  labels = c("Reference", "Medium", "High"),
  cex = 0.8,
  pos = 2 # plot the left of the point
)
points(
  c(15, 15, 15),
  c(6.8, 6.7, 6.6),
  cex = 2,
  pch = 18,
  col = unique(pH.plot$seep.color)
)
points(
  c(17, 17, 17),
  c(6.8, 6.7, 6.6),
  cex = 1,
  pch = 5,
  lwd = 2,
  col = unique(pH.plot$seep.color)
)


### rarefaction curves ####
# assessing alpha diversity, i.e. the number of OTUs at differing sequencing depths

# load R package
require(vegan)

# library size
nSeq <- colSums(OTU)

# maximum number of OTUs in the data set
# to define y-axis range
maxOTU <- max( 
  colSums(decostand(OTU, # transform OTU table
                    method = "pa")) # to presence/absence
)

# without vegan
OTU01 <- OTU
OTU01[OTU01 > 0] <- 1
maxOTU <- max(colSums(OTU01))

# open new graphics device
windows(width = 8, height = 6)

# create empty plot
plot(
  0, 0, 
  type = "n", 
  xlim = c(0, max(nSeq) + 10000), # add space for labels
  ylim = c(0, maxOTU), 
  xlab = "Number of sequences", 
  ylab = "OTU number",
  las = 1, 
  mgp = c(3,1,0)
)
# for each sample
for (i in 1:ncol(OTU)) {    
  temp <- rarefy(
    OTU[, i],
    sample = seq(0, # subsample from 0
                 nSeq[i], # to library size
                 1000) # every 1000 sequences
  )
  # draw lines
  lines(
    seq(0, nSeq[i], 1000), 
    temp, 
    col = ENV.all$seep.color[i]
  )
  # add labels
  text(
    nSeq[i], 
    max(temp), 
    label = colnames(OTU)[i],
    cex = 0.5, 
    pos = 4
  )
}
# highlight minimum library size
abline(v = min(nSeq))


### stacked barplots ####
# usefull to display taxonomic composition
# we use class-level taxonomy
relData <- TAX.pooled.rel$class

# we only want to plot the 10 most abundant classes per sample
abund = 10

# spedicy color palette
colorPalette = c("violet", "purple", "blue", "white", "darkgreen", "yellow", "red", "darkred")

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
  col = c(colorRampPalette(colorPalette)(nrow(abund_rel) - 1), "darkgrey"), # add darkgrey to color vector for 'other'
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


### correlation heatmap ####
# spearman rank correlation between the relative sequence abundance of OTUs and numeric environmental parameters
# this is just an example of how to create such a plot
# relative sequence abundances, i.e. percentages SHOULD NOT BE USED for this kind of analysis
# also no statistical test will be applied, we are only interested in the correlation coefficients
# other input options are, e.g. species counts (marcoecology)

# subset OTU table to only contain OTUs present in 10 out of 13 samples
# with at least 1% relative abundance in at least 1 sample
OTU.abund <- OTU.rel[apply(
  OTU.rel, 
  1, 
  function(x) { 
    sum(x > 0) >= 10 & max(x) >= 1 
    }
  ), ]
TAX.abund <- TAX[rownames(OTU.abund), ]
  
# only use numerical bottom water parameters for correlations
ENV.num <- ENV.all[, c(5:9, 12:14)]

# check order
all.equal(colnames(OTU.abund), rownames(ENV.num))

# create matrix with spearman correlation coefficients for each OTU (rows) and each environmental variable (columns)
# the input to calculate spearman correlation coefficients are 2 matrices with samples in rows
correlation.heatmap <- cor(
  t(OTU.abund), # OTU matrix, t() automatically changes the data type to matrix since OTU table only contained numbers
  as.matrix(ENV.num), # matrix with environmental information
  method = "spearman" # use spearman correlation coefficients
)

# the heatmap is a graphical representation of the correlation matrix
# correlation coefficients are coded as color
# blue: negative correlation
# red: positive correlation
# white: no correlation

# we will also use class-level affiliation the draw a color bar along the rows of the heatmap
row.color <- as.factor(TAX.abund$class)
levels(row.color) <- rainbow(length(levels(row.color))) # use existing R color palette

# or us customized color palette
row.color <- as.factor(TAX.abund$class)
levels(row.color) <- colorRampPalette(c("firebrick4", "gold", "darkolivegreen"))(length(levels(row.color))) # use existing R color palette

# set names of color vector to OTU names
names(row.color) <- rownames(TAX.abund)

# load R package
require(gplots)

# draw heatmap
heatmap.2(
  correlation.heatmap, # input matrix
  col = bluered(20), # color palette for heatmap
  margins = c(5, 10),
  scale = "none", # don't rescale data
  key = TRUE, # add color legend
  symkey = FALSE, # disable symmetric color legend (already specified in breaks)
  density.info = "none", # don't overlay color legend with histogram 
  trace = "none", # don't show data value as line per cell
  cexCol = 0.8, # size of column labels
  cexRow = 0.8, # size of row labels
  RowSideColors = as.character(row.color), # bar with colors for taxonomy
  labRow = TAX.abund$class, # use class-level taxonomy as row labels
  keysize = 1.3, # size of color legend
  breaks = seq(-1, 1, 0.1), # define breaks for color palette used in heatmap
  dendrogram = "row" # only draw row dendrogram
)


### taxonomy tree ####
# visualize taxonomic levels on a phylogenetic tree-like structure
# let's have a look at the OTUs used for the heatmap

# the TAX.abund table contains also unclassified taxonomic levels, which we don't want to show in the plot
# we also need the taxonomy table as factor
taxon.tree <- as.matrix(TAX.abund)
taxon.tree <- gsub("_unclassified", "", taxon.tree) # replace '_unclassified' with nothing
taxon.tree <- data.frame(taxon.tree)

for (i in 1:ncol(taxon.tree)) {
  taxon.tree[, i] <- as.factor(taxon.tree[, i])
}

# we want the tip labels to be colored by class using the same colors as in the heatmap
taxon.tree$color <- as.character(row.color)

# load R package
require(ape)

# create tree object
otu.tree <- as.phylo(
  ~phylum/class/order/family/genus, # specify order of taxonomic ranks (column names in taxon.tree)
  data = taxon.tree # input taxonomy file
)

plot(
  otu.tree, # tree to plot
  cex = 0.8, # size of tip labels
  show.tip.label = T, # plot tip labels
  tip.color = taxon.tree$color[match(otu.tree$tip.label, taxon.tree$genus)], # color of tip labels, order is the same as in otu.tree$tipl.label
  type = "unrooted", # create unrooted tree
  no.margin = T # create plot without margin
)


### networks ####

# OTU co-occurrence network ####
# this is an example of an OTU co-occurrence network based on absence/presence of OTUs
# size of OTU nodes defined by total abundance
# OTUs which occur together will cluster around samples with their highest abundance

# load R packages
require(reshape)
require(igraph)
require(scales)

# input needs to be sorted by increasing total abundance of OTUs
# otherwise larger nodes may cover smaller nodes in network plot
OTU.abund.sorted <- OTU.abund[order(rowSums(OTU.abund)), ]
TAX.abund.sorted <- TAX.abund[rownames(OTU.abund.sorted), ]

# input format for networks
# transform the sample by OTU table from a contigency matrix (samples by OTUs)
# to a long table with 3 columns:
# otu name, sample name, abundance, i.e. molten data.frame
OTU.network <- data.frame(
  otu = rownames(OTU.abund.sorted), 
  OTU.abund.sorted
)
OTU.network.molten <- melt(OTU.network) 
head(OTU.network.molten)
OTU.network.molten <- OTU.network.molten[OTU.network.molten$value > 0, ]

# create network graph
network.graph <- graph.data.frame(OTU.network.molten, directed = F) # create an undirected network (edges without arrows)

# set edge weight to ranked total abundance
E(network.graph)$weights <- rank(OTU.network.molten$value)

# create table with OTU vertex (node) attributes
network.vertex <- data.frame(
  otu = rownames(OTU.abund.sorted),
  sum.abund = rowSums(OTU.abund.sorted),
  tax = TAX.abund.sorted$class
)
# color OTU verteces by class taxonomy
network.vertex$color <- as.character(row.color)[match(TAX.abund.sorted$class, TAX.abund$class)]

# sort table with vertex attributes to match order in network graph
# only OTU verteces are of interest
# the order of verteces is: all OTU vertices, then all sample verteces
network.vertex <- network.vertex[match(
  V(network.graph)$name[-c((nrow(OTU.abund.sorted) + 1):length(V(network.graph)$name))],
  as.character(network.vertex$otu)
  ), ]

# save vertex attributes as part of network graph
# horizontal extension of verteces
V(network.graph)$size <- c(
  rescale(network.vertex$sum.abund, # first OTUs
          to = c(1, 25)), 
  nchar(colnames(OTU.abund.sorted)) * 3 # then sample names (label size depending on length of sample name)
)
# vertical extension of verteces
V(network.graph)$size2 <- c(
  rescale(network.vertex$sum.abund, # same as above
          to = c(1, 25)), 
  rep(4.5, ncol(OTU.abund.sorted)) # fixed height for sample verteces
)
# vertex color
V(network.graph)$color <- c(
  network.vertex$color, # OTU color
  rep("white", ncol(OTU.abund.sorted)) # sample verteces are white
)
# vertex name
V(network.graph)$label <- c(
  rep(NA, nrow(OTU.abund.sorted)), # don't label OTUs
  colnames(OTU.abund.sorted) # only include sample labels
)
# vertex shape
V(network.graph)$shape <- c(
  rep("circle", nrow(OTU.abund.sorted)), # OTU verteces are circles
  rep("rectangle", ncol(OTU.abund.sorted)) # sample verteces are rectangles
)

# create plot
# open new graphics device
windows(width = 10, height = 8)
# specify layout of graphics device
# we want 2 plots in 1 row, the first plot taking upe twice as much space as the second one
layout(
  matrix(c(1, 2), 1, 2), 
  widths = c(3, 1)
)
# set plot margins
par(mar = c(1, 1, 1, 1))
# plot network
plot.igraph(
  network.graph, # network graph
  vertex.size = V(network.graph)$size, # horizontal extension of verteces
  vertex.size2 = V(network.graph)$size2, # vertical extension of verteces
  vertex.label = V(network.graph)$label, # vertex name
  vertex.label.cex = 0.7, # size of vertex label
  vertex.label.color = "black", # color of vertex label
  vertex.label.family =  "sans", # font family
  vertex.shape = V(network.graph)$shape, # vertex shape
  edge.color = "lightgrey", # color of edges
  edge.width = 0.5, # line width of edges
  layout = layout.fruchterman.reingold(network.graph, # algorithm to create network topology
                                       weights = E(network.graph)$weights) # OTU abundance used as weights 
)

# to save the layout (coordinates of the nodes)
# net.topology <- layout.fruchterman.reingold(network.graph, weights = E(network.graph)$weights)
# 
# plot.igraph(
#   network.graph, # network graph
#   vertex.size = V(network.graph)$size, # horizontal extension of verteces
#   vertex.size2 = V(network.graph)$size2, # vertical extension of verteces
#   vertex.label = V(network.graph)$label, # vertex name
#   vertex.label.cex = 0.7, # size of vertex label
#   vertex.label.color = "black", # color of vertex label
#   vertex.label.family =  "sans", # font family
#   vertex.shape = V(network.graph)$shape, # vertex shape
#   edge.color = "lightgrey", # color of edges
#   edge.width = 0.5, # line width of edges
#   layout = net.topology
# )

# call new plot to add legend
plot.new()
par(mar=c(1,1,1,4))
legend(
  "left",
  pch = 22,
  col = "black",
  pt.bg = unique(as.character(row.color)),
  legend = unique(TAX.abund$class),
  pt.cex = 2,
  cex = 1,
  bty = "n"
)


# correlation network ####

# calculate pairwise spearman correlations between OTUs and environmental parameters
# only for demonstrative purposes: relative OTU abundances (Percentages) should not be used for this kind of analysis
# also more than at least 20 (better 40) samples are required

# convert triangular distance matrix to full matrix with '0' in diagonal
# unlike with correlation heatmap earlier, we want all pairwise correlations (also between OTUs)
correlation.mat <- as.matrix(
  as.dist(cor(data.frame(t(OTU.abund), ENV.num), 
              method = "spearman"), 
          upper = TRUE, 
          diag = TRUE)
)
# melt matrix to have one pairwise correlation per row
# http://stackoverflow.com/questions/5813156/convert-and-save-distance-matrix-to-a-specific-format
# convert distance matrix to logical
# upper half of matrix is TRUE
# melt logical matrix
# select only TRUE rows in molten data frame
correlation.mat.molten <- melt(correlation.mat)[melt(upper.tri(correlation.mat))$value, ]
# filter by absolute spearman correlation coefficient of at least 0.6
correlation.mat.molten.06 <- correlation.mat.molten[abs(correlation.mat.molten$value) >= 0.6, ]

# create network graph
correlation.graph <- graph.data.frame(correlation.mat.molten.06, directed = F)
# set edge weight to correlation coefficient (rescaled to 0 - 1)
E(correlation.graph)$weights <- rescale(correlation.mat.molten.06$value, to = c(0, 1))
# specify color for edges
E(correlation.graph)$color <- c(NA)
E(correlation.graph)$color[correlation.mat.molten.06$value > 0] <- "red" # positive correlation
E(correlation.graph)$color[correlation.mat.molten.06$value < 0] <- "blue" # negative correlation
# specify color for verteces
# first all NA
V(correlation.graph)$color <- c(NA)
# color for OTUs
V(correlation.graph)$color[grep("otu", V(correlation.graph)$name)] <- 
  as.character(row.color[grep("otu", V(correlation.graph)$name, value = T)])
# remaining NAs (non-OTUs) are white
V(correlation.graph)$color[is.na(V(correlation.graph)$color)] <- "white"

# plot network
windows(width = 10, height = 8)
layout(matrix(c(1, 2), 1, 2), 
       widths = c(3, 1))
par(mar = c(1, 1, 1, 1))

plot.igraph(
  correlation.graph,
  edge.color = E(correlation.graph)$color,
  vertex.label.cex = 0.7, 
  vertex.label.color = "black", 
  vertex.label.family =  "sans",
  layout = layout.fruchterman.reingold(correlation.graph, weights = E(correlation.graph)$weights)
)

# call new plot to add legend
plot.new()
par(mar=c(1,1,1,4))
legend(
  "left",
  pch = 22,
  col = "black",
  pt.bg = unique(as.character(row.color)),
  legend = unique(TAX.abund$class),
  pt.cex = 2,
  cex = 1,
  bty = "n"
)


### map: coastlines ####
# maps in R can be generated using the normal plot() function
# here we will produce a large-scale and zoom-in map of the sampling area for the vent data set
# thanks to Pier Buttigieg for parts of the code

require(sp)
require(maps)

# download coastlines/country borders at: http://www.gadm.org/country
pngAdm0 <- readRDS("../Example_data/PNG_adm0.rds")

# open new graphics device
windows(height = 12, width = 7.5)
par(
  mar = c(2,2,1,1), 
  mfrow = c(2,1) # 2 plots in 2 rows and 1 column, plotting area has the same size
)

# first plot: large-scale map
plot(
  pngAdm0, # country borders
  col = 'lightgrey', # 'land' color
  border = 'darkgrey', # 'land' border
  xlim = c(149.5, 151.5),
  ylim = c(-10.8, -9),
  asp = 1 # aspect ratio
)

# add axes
axis(
  1, 
  tck = 0.01, # length of ticks, draw inside plot
  mgp = c(1,-1.5,0), # margin lines for the axis title, tick labels and axis line, plot ticks inside plot
  cex.axis = 0.8, # size of tick labels
  at = seq(149.5, 151.5, 0.5), # position of tick labels
  labels = paste(seq(149.5, 151.5, 0.5), "°", sep = "") # tick labels
)
axis(
  2, 
  tck = 0.01, 
  mgp = c(1,-0.8,0), 
  cex.axis = 0.8, 
  las = 2,
  at = seq(-10.5, -9, 0.5), labels = paste(seq(-10.5, -9, 0.5), "°", sep = ""), 
  hadj = 0 # align right (horizontal labels)
)

# add axis titles
mtext(
  side = 1, # below plot (x-axis)
  text = "Longitude", # title
  line = 1 # margin line
)
mtext(
  side = 2, # left of plot (y-axis)
  text = "Latitude", 
  line = 1
)

# draw box around plotting area
box(
  which = "plot", 
  lty = "solid"
)

# draw box around sampling area
rect(150.73, -9.85, 150.93, -9.7)

# add some geographic information
points(
  c(150.43, 150.8177),
  c(-10.32, -9.824), 
  pch = 16, 
  cex = 1
)
text(
  c(150.43, 150.9),
  c(-10.32, -9.9),
  labels = c("Alotau", "Upa Upasina"),
  cex = 1,
  pos = c(2, 2, 4)
)
text(
  c(149.3, 151, 150.6, 149.9),
  c(-10.15, -9.87, -10.42, -10),
  cex = 0.8, 
  col = "gray27", 
  labels = c("Papua New Guinea", "Normanby Island", "Milne Bay", "Goodenough Bay"), 
  pos = 4
)

# add scale bar
map.scale(
  150.73, # position (x)
  -9.1, # position (y)
  relwidth = 0.2, # proportion of width of display to be used for the scale
  metric = TRUE, # units in km
  ratio = FALSE, # don't display scale ratio of map
  cex = 0.8 # size of labels
)

# add north arrow
arrows(
  149.5, # beginning of arrow (x)
  -9.2, # beginning of arrow (y)
  149.5, # end of arrow (x)
  -9, # end of arrow (y)
  length = 0.08, # length of the "arrow roof"
  angle = 30, # angle of the "arrow roof"
  col = "black" # colour of arrow
)
text(149.5, -9.1 , "N", font = 1, cex = 1, col = "black")

# second plot: zoom-in sampling area
plot(
  pngAdm0,
  col = 'lightgrey',
  border = 'darkgrey',
  xlim = c(150.73, 150.93),
  ylim = c(-9.85, -9.7),
  asp = 1
)
axis(
  1, 
  tck = 0.01,  
  mgp = c(1,-1.5,0),
  cex.axis = 0.8,
  at = seq(150.75, 150.90, 0.05), 
  labels = paste(seq(150.75, 150.90, 0.05), "°", sep = "")
)
axis(
  2,
  tck = 0.01,  
  mgp = c(1,-0.8,0), 
  cex.axis = 0.8, 
  las = 2,
  at = seq(-9.85, -9.7, 0.05), 
  labels = paste(seq(-9.85, -9.7, 0.05), "°", sep = ""), 
  hadj = 0
)
mtext(
  side = 1, 
  text = "Longitude", 
  line = 1
)
mtext(
  side = 2,
  text = "Latitude",
  line = 1
)
box(which = "plot", lty = "solid")

# sampling site
text(
  150.818,
  -9.813,
  labels = "Upa Upasina",
  cex = 1.3, 
  pos = 2,
  font = 2
)

# reference and seep locations
points(
  c(150.8177, 150.82),
  c(-9.824, -9.828),
  pch = 16, 
  cex = 1
)
text(
  c(150.8177,150.82),
  c(-9.821, -9.83),
  labels = c("seep", "reference"),
  cex = 1, 
  pos = 2
)

# other geographic information
text(
  c(150.85, 150.76),
  c(-9.77, -9.75),
  cex = 0.8, 
  col = "gray27",
  labels = c("Dobu Island", "Normanby Island"),
  pos = 4
)

# scale bar
map.scale(
  150.87,
  -9.71,
  relwidth = 0.2, 
  metric = TRUE, 
  ratio = FALSE,
  cex = 0.8
)

# north arrow
arrows(
  150.93, 
  -9.82,
  150.93, 
  -9.8,
  length = 0.08,
  angle = 30,
  col = "black"
)
text(150.93, -9.81 , "N", font = 1, cex = 1, col = "black")


# at this point we will switch to another data set
# save.image("Vent.Rdata")


### bathymetric map ####
# bathymetric map of the Galapagos Islands
# code from: https://cran.r-project.org/web/packages/marmap/vignettes/marmap.pdf

# load("Bathy.Rdata")

# load R package
require(marmap)

# get input data
# download from NOAA (internet connection required)
galapagos <- getNOAA.bathy(
  lon1 = -92, # define coordinates of area to download bathymetric grid data for
  lon2 = -88,
  lat1 = -2,
  lat2 = 2,
  resolution = 1 # resolution of grid: 1min (highest resolution)
)

# have a look at the data
# contains also elevation values
summary(galapagos)

# customize color palettes
blues <- colorRampPalette(c("purple", "darkblue", "blue", "cadetblue1", "white"))
greys <- colorRampPalette(c("cornsilk2", "goldenrod4"))

# open new graphics device
windows(height = 10, width = 10)

# create plot
plot.bathy(
  galapagos, # input data
  image = T, # plot bathymetry as color
  land = T, # include isobars above sea level
  axes = F, # don't plot axes
  xlab = "", # don't plot x-axis label
  ylab = "", # don't plot y-axis label
  asp = 1, # aspect ratio
  drawlabels = c(F, F, F, F), # don't plot labels for isobars, define for each range of isobars
  # plot isobars with different intervals, colors, and line widths for deep, shallow, coast line, land
  deep = c(-4000, -1000, 0, 100), # define lower border of isobar range
  shallow = c(-1000, -100, 0, 1500), # define upper border of isobar range
  step = c(500, 200, 0, 500), # define intervals at which to plot isobars for each range
  lwd = c(0.1, 0.1, 2, 0.1), # define line width for each isobar range
  lty = c(1, 1, 1, 1), # line type: solid lines
  col = c("grey", "grey", "black", "grey"), # define line color for each isobar range
  bpal = list(c(0, max(galapagos), greys(50)), # use one color palette for land (0m to max)
              c(min(galapagos), 0, blues(100))) # use second color palette for ocean (min to 0)
)

# add axes
axis(1, pos = -2)
axis(2, pos = -92, las = 2)

# draw box around plot
rect(-92, -2, -88, 2)

# add axis titles
mtext(text = "Longitude", side = 1, line = 2)
mtext(text = "Latitude", side = 2, line = 1)

# save.image("Bathy.Rdata")


### interpolated map ####
# create interpolated depth profile of Chlorophyll data across the Atlantic
# similar to ocean data view
# warning: the kriging method used may not be the most appropriate one

# load("Interpolation.Rdata")

# read input data
# downloaded at http://doi.pangaea.de/10.1594/PANGAEA.854028
atlantic <- read.table(
  "M96_hydrochemistry.tab", 
  h = T,
  sep = "\t",
  skip = 56 # skip the first 56 lines of the file
)

# have a look at the data
head(atlantic)

# rename columns
colnames(atlantic)[6:ncol(atlantic)] <- c(
  "elevation", 
  "depth", 
  "temp", 
  "cond",
  "chla", 
  "O2", 
  "sal", 
  "density",
  "PO4", 
  "NO3", 
  "NO2", 
  "NH4"
)

# change sign of depth
atlantic$water.depth <- -atlantic$depth

# plot location of chla values
# x-axis is Longitude
# y-axis is water.depth
plot(
  atlantic$Longitude,
  atlantic$water.depth,
  pch = "+",
)

# load R packages
require(sp)
require(automap)

# create grid
# longitude (x) coordinates of grid every 0.1 degrees
grid.x <- seq(from = min(atlantic$Longitude), to = max(atlantic$Longitude), by = 0.1)
# longitude (y) coordinates of grid every 1m
grid.y <- seq(from = min(atlantic$water.depth), to = max(atlantic$water.depth), by = 1)
# calculate all points in grid
# i.e. create coordinates for each combination on grid.x and grid.y
grid <- expand.grid(grid.x,grid.y) 
# save grid as data frame
grid.df <- as.data.frame(grid)

# have a look at grid
# check if spacing sufficient
head(grid.df)
plot(grid.df, pch = ".")

# format input for interpolation
# we only need Longitude (x), water depth (y), and chl a
chla <- atlantic[, c("Longitude", "water.depth", "chla")]
head(chla)
# are there any NAs in data set?
sum(is.na(chla$chla))

# convert to different R object class specific to sp package: SpatialPoints
chla.coord <- chla # next line will convert sites to class SpatialPoints and overwrite chla.coord
coordinates(chla.coord) <- ~ Longitude + water.depth
grid.coord <- grid.df # next line will convert sites to class SpatialPoints and overwrite grid.coord
coordinates(grid.coord) <- c("Var1","Var2")

# interpolation using default kriging
# may have to be adjusted!
chla.kriging <- autoKrige(
  chla.coord, # input data
  new_data = grid.coord # grid coordinates for interpolation
)

# extract chla values for better plotting
chla.predicted <- cbind( # column bind (works only for same data type, otherwise use data.frame)
  grid.df, # xy coordinates
  chla.kriging$krige_output@data$var1.pred # predicted chl a values
)
colnames(chla.predicted) <- c("Longitude", "water.depth", "chla")
# are there any NAs in data set?
sum(is.na(chla.predicted$chla))

# create color scheme for chla concentrations
# http://stackoverflow.com/questions/9946630/colour-points-in-a-plot-differently-depending-on-a-vector-of-values
chla.predicted$chla.color <- colorRampPalette(c("white", "darkolivegreen"))(100)[as.numeric(cut(chla.predicted$chla, breaks = 100))]

# open new graphics device
windows(height = 8, width = 10)
layout(
  matrix(c(1, 2), 1, 2), 
  widths = c(6, 1)
)
# create plot
plot(
  0, 0,
  type = "n",
  axes = F,
  xlim = c(min(chla.predicted[, 1]), max(chla.predicted[, 1])),
  ylim = c(min(chla.predicted[, 2]), 0),
  xlab = "Longitude",
  ylab = "Water depth [m]",
  xaxs="i", # remove 4% margins along the x-axis within the plotting area
  yaxs="i" # remove 4% margins along the y-axis within the plotting area
)
 
# add points
points(
  chla.predicted[, 1:2], 
  pch = 16, 
  cex = 0.7, 
  col = chla.predicted$chla.color
)

# add axes
axis(1)
axis(2, las = 2)

# draw baox around plot
box(
  which = "plot", 
  lty = "solid"
)

# add color legend
source("colorBar.R")
color.bar(
  lut = colorRampPalette(c("white", "darkolivegreen"))(100), 
  min = min(chla.predicted$chla), 
  max = max(chla.predicted$chla),
  ticks = seq(0.02, 0.2, 0.02), 
  title = "Chl a"
)


### time and dates in R ####
# in the atlantic data set there is also one column called Date.Time
# more info on: http://www.stat.berkeley.edu/classes/s133/dates.html

# first split Date.Time into date and time, and retain date
atlantic$date <- sapply(
  strsplit(
    as.character(atlantic$Date.Time), 
    split = "T"
  ), 
  function(x) { 
    x[1] 
  }
)
str(atlantic$date)

# convert to 'Date' type
atlantic$date <- as.Date(
  atlantic$date, # input dates
  format='%Y-%m-%d'# specify format of dates, here: YYYY-MM-DD
)
str(atlantic$date)

# have a look at temperature per day at 5m water depth
temp.date <- atlantic[atlantic$water.depth == -5, c("date", "temp")]
plot(temp.date$date, temp.date$temp)
# time interval considered in plot

# save.image("Interpolation.Rdata")

