# R roundtable 21.09.2017
# Plot()-ting in R

# 1: data handling
# 2: par()
# 3: layout()
# 4: plotting elements
# 5: scatter plot (PCA)
# 6: barplot with error bars
# 7: heatmap (correlations)
# 8: heatmap (percentages)
# 9: bathymetric map 
# 10: depth profiles 1
# 11: depth profiles 2
# 12: networks

# packages
require(scales)
require(reshape)
require(vegan)
require(gplots)
require(marmap)
require(OceanView)
require(automap)
require(sp)
require(maps)
require(raster)
require(igraph)
require(TeachingDemos)

# set working directory
# change path to your copy of the R roundtable files
setwd("D:/Repos/chassenr/Tutorials/R_roundtable_plotting/")
load("Plotting_tutorial.Rdata")
# save.image("Plotting_tutorial.Rdata")

### data handling ####

# calculating mean/median, standard deviation, etc.
# these values will then be used for plotting
# conversion between long and wide data formats
# R package: reshape
# alternative packages: plyr, dplyr, data.table, etc.

# read data
ENV <- read.table("ENV_sim.txt", h = T, sep = "\t")

# long data format
# one data point per line (+ metadata)
ENV.long <- melt(ENV)
head(ENV.long)

dim(ENV.long)
dim(ENV)
# all the values in ENV[, 2:9] stacked in one column, 
# with an additional column specifying the measurement it came from
# makes is easier to calculate summary statistics

# mean and standard deviation
ENV.mean <- cast(ENV.long, "SITE ~ variable", fun.aggregate = mean)
ENV.sd <- cast(ENV.long, "SITE ~ variable", fun.aggregate = sd)

# you can also write your own function:
ENV.se <- cast(ENV.long, "SITE ~ variable", fun.aggregate = function(x) { sd(x)/sqrt(length(x)) })

### data handling end ####


### plotting parameters ####

# layout of plotting devices
# figure area
# plot area
plot.new()
box("plot")
box("figure", col = "red")

# margins
par("mar")
for(i in 1:4) {
  mtext(
    text = paste("line", 0:(floor(par("mar")[i]) - 1)), 
    side = i, 
    line = 0:(floor(par("mar")[i]) - 1)
  )
} 

# outer margins
par("oma")
par(oma = c(4, 0, 4, 0))
plot.new()
box("plot")
box("figure", col = "red")
box("inner", col = "blue")
box("outer", col = "green")
for(i in 1:4) {
  mtext(
    text = paste("line", 0:(floor(par("mar")[i]) - 1)), 
    side = i, 
    line = 0:(floor(par("mar")[i]) - 1)
  )
} 
for(i in c(1, 3)) {
  mtext(
    text = paste("line", 0:3), 
    side = i, 
    line = 0:3,
    outer = T
  )
}

# plotting area in user coordinates
par("usr")

# plotting on margins allowed?
par("xpd")

# multipanel figures
par("mfrow")
par("mfcol")

# position of axis title, axis tick marks, axis
par("mgp")

# plot titles?
par("ann")

# extra spacing ad beginning of axes?
par("xaxs")
par("yaxs")

# plotting parameters can be set before the plot to defin the plotting area, etc.
# there are some parameters which are then used by the plotting commands
# those can also be specified later (as part of the command)
# please refer to the examples in the plotting commands

# to delete any changes to default plotting parameters
# close all plotting devices
dev.off()

# or same default settings 
op <- par()
# and reset
par(op)

# lets's have a look at a tyipcal plotting area of a simple 2 panel figure
par(
  mfrow = c(1, 2),
  oma = c(4, 0, 4, 0)
)

# for each plot in the multipanel layout
for(j in 1:2) {
  # open new plot
  plot.new()
  
  # draw box around plotting region defined by user coordinates
  box("plot")
  
  # box around panel
  box("figure")
  
  # allow plotting on margins
  par(xpd = NA)
  
  # show inner margin lines
  for(i in 1:4) {
    mtext(
      text = paste("line", 0:(floor(par("mar")[i]) - 1)), 
      side = i, 
      line = 0:(floor(par("mar")[i]) - 1)
    )
  } 
}

# show outer margin lines
for(i in c(1, 3)) {
  mtext(
    text = paste("line", 0:3), 
    side = i, 
    line = 0:3,
    outer = T
  )
}

# you can navigate to any point in the plotting device
# as long as you know (guess) the coordinates!

### plotting parameters end ####


### layout ####

# more versatile plot arrangements
layout(
  mat = matrix(
    c(1, 1, 1,
      2, 2, 3,
      4, 5, 6),
    3,
    3,
    byrow = T
  ), 
  widths = c(1, 1, 0.5),
  heights = c(1.3, 1, 1)
)
# this will produce a the following multipanel plot
# 6 plots in 3 rows
# row 1: first plot over full width of plotting device and one third as high as the others
# row 2: second and third plot, the second taking up 4 times as much space as the third
# row 3: the last 3 plots, with plot 4 and 5 each taking up twice as much space as plot 6

# Let's have a look:
# use smaller margins
par(mar = c(1, 1, 1, 1))
for(i in 1:6) {
  plot.new()
  box("plot")
}

### layout end ####


### plotting elements ####

# we will be using some of the following commands to add sepcific elements to a plot

# open new plot
dev.off()
par(mar = c(4, 4, 1, 1), ann = F)
# plot.new()

plot(
  0,
  0,
  type = "n",
  ylim = c(-0.5, 1.5),
  xlim = c(0, 1),
  axes = F
)

# get plot dimensions in user coordinates
par("usr")

# individual data points, requires x and y coordinates
points(
  x = seq(0, 1, 0.1), 
  y = seq(0, 1, 0.1),
  pch = 16, # point character, i.e. symbol
  cex = seq(0.5, 2, length.out = 11), # point size
  col = rainbow(11) # point color
)

# individual text labels (e.g. for data points), requires x and y coordinates
text(
  x = seq(0, 1, 0.1), 
  y = seq(0, 1, 0.1),
  labels = LETTERS[1:11],
  pos = c(rep(3, 5), rep(1, 6)) # position of label relativ to xy
)
text(
  0.5, # x
  0.8, # y
  labels = "value without point",
  font = 2 # bold text
)

# connect individual data points (in the order of input), requires x and y coordinates
lines(
  x = seq(0, 1, 0.1), 
  y = seq(0, 1, 0.1),
  lty = 3, # line type dashed
  lwd = 3 # line width
)

# line connectors between 2 points, requires start and end x and y coordinates
segments(
  x0 = seq(0, 1, 0.1), 
  y0 = seq(0, 1, 0.1) - 0.2, 
  x1 = seq(0, 1, 0.1), 
  y1 = seq(0, 1, 0.1) + 0.2
)

# rectangular box, requires x and y coordinates of lower left and upper right hand corner
rect(
  0.3,
  0.75,
  0.7,
  0.85,
  border = "red",
  lty = 2,
  lwd = 2
)

# connect individual data points (in the order of input) to form a polygon (fill optional),
# requires x and y coordinates
# similar to lines()
polygon(
  x = c(0, 0.2, 0.2, 0.3, 0.2, 0.2, 0), 
  y = c(0.75, 0.75, 0.7, 0.8, 0.9, 0.85, 0.85),
  col = "red"
)

# like segments, but with arrow head
arrows(
  x0 = rep(0, 3), 
  y0 = rep(0, 3), 
  x1 = c(0.5, 0.6, 0.7), 
  y1 = c(0.1, 0.2, 0.3), 
  length = 0.1 # length of arrowhead
)

# draw box (rectangle) around plotting area (defined by user coordinates)
box("plot", lwd = 0.5)

# add axis to plot
# specify which side of plot to add axis
axis(
  2,
  mgp = c(2, 0.7, 0),
  las = 1 # always horizontal
)
axis(
  1, 
  mgp = c(2, 0.7, 0), 
  at = c(0.2, 0.5, 0.8), 
  labels = c("small", "medium", "large")
)

# add text labels on margin
mtext(
  text = "Size",
  side = 1, # side of plot (underneath)
  line = 2 # margin line
)

# also works with
title(ylab = "Value")

# add linear curve to plot, 
# e.g. linear fit, horizontal or vertical lines with specific intersection with axis
abline(a = 0.2, b = 0.5) # intercept and slope
abline(h = 0.5)
abline(v = 0.85)

### plotting elements end ####


### scatter plot ####

# build your own plot element by element
# example: PCA 
ENV.pca <- rda(ENV[, -1], scale = T)

# choose scaling 2
# save coordinates of default plot as speparate R object
ENV.pca.scaling2 <- plot(ENV.pca)

# plot settings
# points colored by site
# point character by site
# aspect ratio of 1 
# variable loadings as arrows in blue with labels
# gridlines for origin of coordinate system 
# hulls around points from same site with transparent color
# display explained variation for each axis

# save point color and symbol as separate R object
# since both col and pch are based on SITE
# duplicate SITE factor and redefine factor levels
ENV.plot.pars <- data.frame(
  pch = ENV$SITE,
  col = ENV$SITE
)
levels(ENV.plot.pars$pch) <- c(15, 16, 17)
levels(ENV.plot.pars$col) <- c("darkblue", "orange", "darkred")

# close revious devices
dev.off()

# save directly to pdf
# the sizes and dimensions of the individual plotting elements will vary
# when plotting to pdf or to a new plotting window
pdf("PCA_plot.pdf", width = 7, height = 7)

# set margins
# don't plot any titles on the plot yet
par(mar = c(4, 4, 1, 1), ann = F)

# start with empty plot
# this will define the range for x and y axis automatically
plot(
  ENV.pca.scaling2$sites[, 1], # x
  ENV.pca.scaling2$sites[, 2], # y
  type = "n", # don't plot anything yet
  axes = F, # don't plot axes
  asp = 1 # aspect ration of 1
)

# use abline to show origin of coordinate system
abline(h = 0, lty = 3, lwd = 0.5)
abline(v = 0, lty = 3, lwd = 0.5)

# add axes
# tick labels horizontal
axis(1, las = 1)
axis(2, las = 1)

# get coordinates to define hulls around points from the same site
# the ordihull function can also draw the hulls directly
# however the plotting settings are more flexible if you draw them yourself
# e.g. adding transparency
# the R object is a list, with a separate data.frame of polygon coordinates for each SITE
ENV.poly <- ordihull(
  ENV.pca,
  groups = ENV$SITE,
  draw = "none"
)

# only 1 polygon can be plotted at a time
for(i in 1:3) {
  polygon(
    ENV.poly[[i]],
    col = alpha(levels(ENV.plot.pars$col)[i], alpha = 0.3), # adjust transparency
    border = NA # don't draw border of polygon
  )
}

# add points
points(
  ENV.pca.scaling2$sites[, 1],
  ENV.pca.scaling2$sites[, 2],
  pch = as.numeric(as.character(ENV.plot.pars$pch)),
  col = as.character(ENV.plot.pars$col)
)

# add variables
arrows(
  rep(0, nrow(ENV.pca.scaling2$species)), # start x
  rep(0, nrow(ENV.pca.scaling2$species)), # start y
  ENV.pca.scaling2$species[, 1], # end x
  ENV.pca.scaling2$species[, 2], # end y
  length = 0.15, # length of arrow head
  angle = 20 # angle of arrow head
)

# add variable labels
# the function expression() can be used to format labels with sub- and superscript
text(
  ENV.pca.scaling2$species[, 1],
  ENV.pca.scaling2$species[, 2],
  cex = 0.8,
  labels = c(
    "Temperature",
    "pH",
    expression('O'[2]),
    "Chl a",
    expression('PO'[4]^"3-"),
    expression('NO'["x"]^"-"),
    "DOC",
    "POC"
  ),
  # plot labels in lower half of plot below the arrows, otherwise above
  pos = ifelse(ENV.pca.scaling2$species[, 2] < 0, 1, 3) 
)

# add box around plotting area
box("plot", lwd = 0.5)

# add axis titles that will include explained variation
# eigenvalues of PCA axes
title(
  xlab = paste("PC1 (", 
               round(ENV.pca$CA$eig[1]/sum(ENV.pca$CA$eig) * 100, 2),
               "%)",
               sep = ""),
  ylab = paste("PC2 (", 
               round(ENV.pca$CA$eig[2]/sum(ENV.pca$CA$eig) * 100, 2),
               "%)",
               sep = "")
)

# add legend to plot
legend(
  'bottomright',
  legend = paste("Site", levels(ENV$SITE), sep = " "),
  pch = as.numeric(levels(ENV.plot.pars$pch)),
  col = levels(ENV.plot.pars$col),
  pt.cex = 1.3 # only increase size of points
)

# close plotting device
# that will save the plot to pdf
dev.off()


# other scatter plot applications
# show individual data points instead of mean values or boxplot
# suitable for very small data sets with low replication
# multipanel figure

# save formatted parameter names (including units)
params <- c(
  "Temperature [°C]",
  "pH",
  expression('O'[2]*" [mg L"^-1*"]"),
  expression("Chl a [µg L"^-1*"]"),
  expression('PO'[4]^"3-"*' [µmol L'^-1*']'),
  expression('NO'["x"]^"-"*' [µmol L'^-1*']'),
  expression('DOC [µmol L'^-1*']'),
  expression("POC [µg L"^-1*"]")
)

# write plot to file
pdf("scatter_plot.pdf" ,width = 7, height = 10)

# define number and layout of panels
# set margins and outer margins (more space at the bottom and top of plot)
# adjust position of axis tick labels
# define line width
par(
  mfrow = c(4, 2), 
  mar = c(1, 5, 1, 1), 
  mgp = c(3.2, 1, 0), 
  oma = c(3, 0, 2, 0),
  lwd = 0.5
)

# plot one parameter at a time
for(i in 2:ncol(ENV)) {
  
  # use factor with SITE categories as x coordinates
  # factor levels will be converted to running number starting with one
  # add some noise to x coordinates so that points from one site do not overlap
  # add some more space on either end of the x axis
  plot(
    jitter(as.numeric(ENV$SITE)), 
    ENV[, i],
    col = as.character(ENV.plot.pars$col),
    pch = as.numeric(as.character(ENV.plot.pars$pch)),
    xlim = c(0.5, 3.5),
    xlab = "Site",
    ylab = params[i - 1],
    axes = F
  )
  
  # add y axis to plot
  axis(2, las = 1, lwd = 0.5)
  
  # add x axis to plot
  # since tick labels identical for all plots, only show for plots in last row
  if(i %in% c(8, 9)) {
    axis(1, at = 1:3, labels = levels(ENV$SITE), lwd = 0.5)
  } else {
    axis(1, at = 1:3, labels = NA, lwd = 0.5)
  }
  
  # add horizontal segment for mean
  segments(
    1:3 - 0.3,
    ENV.mean[, i],
    1:3 + 0.3,
    ENV.mean[, i]
  )
  
  # draw box around plot
  box(which = "plot", lty = "solid", lwd = 0.5)
  
  # add panel number
  par(xpd = NA) # allow plotting on margins
  text(
    par("usr")[1] - (par("usr")[2] - par("usr")[1]) * 0.1,
    par("usr")[4] + (par("usr")[4] - par("usr")[3]) * 0.05,
    labels = paste("(", LETTERS[i - 1], ")", sep = ""),
    font = 2,
    cex = 1.3
  )
  par(xpd = F) # reset xpd
}

# add common x axis label for all plots
mtext("Site", side = 1, line = 1, outer = T)

# close plotting device
dev.off()

### scatter plot end ####


### barplot ####

# create the typical mean +/- error bar plot
pdf("barplot.pdf", width = 7, height = 7)

# save center position of bars on x axis
bp <- barplot(
  ENV.mean$pH,
  col = levels(ENV.plot.pars$col),
  las = 1,
  names.arg = levels(ENV$SITE), # define labels for bars
  xlab = "Site",
  ylab = "pH",
  ylim = c(0, max(ENV.mean$pH_units + ENV.sd$pH_units)),
  cex.axis = 1.3,
  cex.names = 1.3,
  cex.lab = 1.5
)

# draw error bars (range)
segments(
  bp[, 1],
  ENV.mean$pH_units - ENV.sd$pH_units,
  bp[, 1],
  ENV.mean$pH_units + ENV.sd$pH_units
)

# draw whiskers
segments(
  bp[, 1] - 0.3,
  ENV.mean$pH_units - ENV.sd$pH_units,
  bp[, 1] + 0.3,
  ENV.mean$pH_units - ENV.sd$pH_units
)
segments(
  bp[, 1] - 0.3,
  ENV.mean$pH_units + ENV.sd$pH_units,
  bp[, 1] + 0.3,
  ENV.mean$pH_units + ENV.sd$pH_units
)

dev.off()

### barplot end ####


### heatmap (gplots) ####

# we will plot a correlation heatmap,
# however any matrix can be visualized as a heatmap
# blue colors for negative correlation coefficients,
# red colors for positive correlation coefficients (symmetric color code)
# intensity of color corresponds to strength of correlation

ENV.cor <- cor(ENV[, -1])

pdf("heatmap_plot.pdf", width = 6, height = 6)
heat <- heatmap.2(
  ENV.cor, # input matrix
  mar = c(0, 0), # bottom and right margins for central part of heatmap
  col = bluered(30), # color palette
  scale = "none", # don't stadardize (scale) data
  key = TRUE, # show color key
  symkey = F, # don't draw symmetric key (bug if TRUE)
  density.info = "none", # don't add histogram to color key
  trace = "none", # don't show offset of value per cell to mean
  cexCol = 1, # size of column labels
  cexRow = 1, # size of row labels
  # use formatted parameter names
  labCol = c(
    "Temperature",
    "pH",
    expression('O'[2]),
    "Chl a",
    expression('PO'[4]^"3-"),
    expression('NO'["x"]^"-"),
    "DOC",
    "POC"
  ),
  labRow = c(
    "Temperature",
    "pH",
    expression('O'[2]),
    "Chl a",
    expression('PO'[4]^"3-"),
    expression('NO'["x"]^"-"),
    "DOC",
    "POC"
  ),
  keysize = 1, # size of color key
  key.title = NA, # don't plot title for color key 
  key.par = list(mar = c(5, 1, 1, 1), cex = 0.6), # plotting parameters for color key
  key.xlab = "Pearson correlation", # name of color key
  dendrogram = "none", # don't show dendrograms
  Colv = T, # cluster columns
  Rowv = T, # cluster rows, i.e. group by similar values
  breaks = seq(-1, 1, length.out = 31), # range of color key
  # the next few parameters will determine the position of each of the elements of the heatmap
  # by default the color key is in the topleft plot, and the heatmap in the bottomright plot
  # of a multipanel figure with 4 plots: heatmap (1), dendrograms (2+3), color key (4)
  # the default layout (width and height and order of the default heatmap can be changed)
  lhei = c(5, 1),
  lwid = c(4, 1),
  lmat = matrix(c(1, 2, 3, 4), 2, 2, byrow = T)
)
dev.off()

### heatmap (gplots) end ####


### heatmap (image) ####
# if the heatmap.2 function is too rigid for more complicated figures,
# you can also build your own heatmap from scratch using the image() function

SPECIES <- read.table("example_data_heatmap.txt", h = T, sep = "\t", row.names = 1)
META <- read.table("example_metadata_heatmap.txt", h = T, sep = "\t", stringsAsFactors = F, row.names = 1)
META$Station <- factor(META$Station, levels = c("R1", "F1", "F2", "F3", "F4"))
META$Tidal.phase <- as.factor(META$Tidal.phase)
META$Pore.size <- as.factor(META$Pore.size)
levels(META$Pore.size) <- c("FL", "PA")
META.species <- read.table("example_species_metadata_heatmap.txt", h = T, sep = "\t", row.names = 1)

# order observations for plot
META <- META[order(META$Pore.size, META$sampling.unit, META$Tidal.phase), ]
SPECIES <- SPECIES[, rownames(META)]
all.equal(rownames(META), colnames(SPECIES))
all.equal(rownames(SPECIES), rownames(META.species))

# barplot and heatmap of potential pathogens
# 2 panel figure
layout(
  matrix(c(1:6), nrow = 3, ncol = 2, byrow = T),
  heights = c(1.2, 2, 0.8),
  widths = c(4, 1)
)
par(
  mar = c(1, 4, 1, 1),
  xaxs = "i",
  yaxs = "i"
)
# plot.new()
# box("plot")
barplot(
  colSums(SPECIES),
  names.arg = "",
  ylab = "",
  axes = F
)
axis(
  2,
  cex.axis = 0.7,
  las = 2,
  mgp = c(2, 0.5, 0.1),
  tcl = -0.3
)
mtext(2, text = "Proportion of potential", line = 3, cex = 0.9)
mtext(2, text = "pathogens [%]", line = 1.5, cex = 0.9)
plot.new()
par(
  mar = c(0, 4, 0, 1),
  xpd = NA
)
# plot.new()
# box("plot")
image(
  x = 1:ncol(SPECIES),
  y = 1:nrow(SPECIES),
  z = sqrt(t(SPECIES)),
  col = colorRampPalette(c("white", "darkred"))(30),
  axes = F,
  xlab = "",
  ylab = ""
)
axis(
  1,
  at = 1:ncol(SPECIES),
  labels = paste(META$Station, META$Tidal.phase, META$sampling.unit, META$Pore.size, sep = "_"),
  cex.axis = 0.7,
  las = 2,
  mgp = c(2, 0.5, 0),
  tcl = -0.3
)
axis(
  4,
  at = 1:nrow(SPECIES),
  labels = rownames(SPECIES),
  cex.axis = 1,
  las = 1,
  mgp = c(2, 0.5, 0),
  tcl = -0.3
)
axis(
  2,
  at = 1:nrow(SPECIES),
  labels = META.species$host,
  cex.axis = 1,
  las = 1,
  mgp = c(2, 0.5, 0),
  tcl = -0.3
)
box("plot")
mtext(1, text = "Station-Tide-Event-Fraction", line = 5.5)
plot.new()
plot.new()
par(
  mar = c(4, 1, 2, 1),
  xpd = NA, 
  ann = F
)
image(
  matrix(
    seq(0, sqrt(max(SPECIES)), length.out = 30),
    nrow = 30,
    ncol = 1
  ),
  x = seq(0, sqrt(max(SPECIES)), length.out = 30),
  y = 1,
  axes = F,
  col = colorRampPalette(c("white", "darkred"))(30)
)
box("plot", lwd = 0.5)
axis(
  1, 
  at = sqrt(c(0, 1, 2, 5, 10)), 
  labels = c(0, 1, 2, 5, 10),
  mgp = c(1, 0.1, 0),
  tcl = -0.1,
  cex.axis = 0.7
)
title(xlab = "Sequence proportion [%]", cex.lab = 0.8, mgp = c(1, 0.1, 0))

### heatmap (image) end ####


### bathymetric map ####

# bathymetric map of the Galapagos Islands
# with scale bar showing distance in kilometers
# in the upper right hand corner an overview map (without bathymetry)
# will be plotted showing part of south and middle America

# get input data for bathymetric map
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

# to get input data for overview map:
# here we will use the coastlines and country border provided by the maps package
# for specific countries (and a higher resolution), 
# you can download the data from www.gadm.org (global administrative areas)
# R should be able to handle both spatial polygon files as we as shape files

# open new graphics device
pdf("bathy_coarse.pdf", height = 7, width = 7)

# define margins
par(mar = c(3, 3, 1, 1))

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

source("scalebar.R")
map.scale2(
  -91.7, # left x ...
  -1.7, # ... and y of scale bar
  ratio = F, # do not show scaling ratio on map
  col = "white", # white text and scale bar
  font = 2, # bold text
  lwd = 2 # thicker lines for scale bar
)

# clear area for overview map
# IMPORTANT: preserve aspect ratio
# we will plot from -95 to -55 (Longitude), and -7, 15 (Latitude)
# which means the plot is 1.8 times as wide as high
plot.ratio <- (-55 - (-95))/(15 - (-7))

rect(
  -88 - (1 * plot.ratio), 
  1, 
  -88, 
  2, 
  col = "white",
  border = NA, 
  lwd = 0.5
)

# here we need to cheat a little bit
# the map() function doesn't seem to work with subplot
# so we will save the coastlines in a separate R object 
# and use the regular plot function
overview.plot <- map(
  "world", # world coastlines
  resolution = 0, # highest resolution
  xlim = c(-95, -55), # area to be plotted
  ylim = c(-7, 15), 
  plot = F, # don't plot
  fill = T, # fill land masses
  col = "grey" # land color
)

# since we need to add several elements to the subplot,
# we include the nessecary plotting commands in a separate function
# which is then called in subplot()
myplot <- function() {
  # main plot call
  plot(
    overview.plot$x, 
    overview.plot$y, 
    type = "n", # don't plot anything yet
    asp = 1, 
    xlim = c(-95, -55), 
    ylim = c(-7, 15),
    axes = F
  )
  # add land masses
  polygon(
    overview.plot$x, 
    overview.plot$y,
    col = "grey" # fill with grey color
  )
  # show sampling area
  rect(
    -92,
    -2,
    -88,
    2,
    border = "red",
    lwd = 2
  )
}

# add overview map
subplot(
  myplot(),
  x = c(-88 - (1 * plot.ratio), -88), # x coordinates for lower left and upper right corner of subplot
  y = c(1, 2), # y coordinates for lower left and upper right corner of subplot
  pars = list(mar = c(0, 0, 0, 0), xaxs="i", yaxs="i", xpd = F, ann = F) # list of plotting paramters for subplot
)

# draw border around subplot
rect(
  -88 - (1 * plot.ratio), 
  1, 
  -88, 
  2, 
  col = NA,
  border = "black", 
  lwd = 0.5
)

# add axes
axis(1, pos = -2)
axis(2, pos = -92, las = 2)

# only draw box around plot, if plotting area conforms with aspect ration
# otherwise par("usr") is not equal to axis range
box("plot")

# add axis titles
mtext(text = "Longitude", side = 1, line = 2)
mtext(text = "Latitude", side = 2, line = 2)

# close plotting device
dev.off()


# if you want to zoom into a smaller area,
# a 1 minute resolution for the bathymetry may not be sufficient
# if you don't have more detailed data available
# you can use interpolation to increase the resolution of the plot
# CAUTION: this will only create smaller pixels based on the coarse data
# it will not show small scale variability that was not included in the original data set

galapagos.small <- getNOAA.bathy(
  lon1 = -92, # define coordinates of area to download bathymetric grid data for
  lon2 = -91.2,
  lat1 = 0,
  lat2 = 0.8,
  resolution = 1 # resolution of grid: 1min (highest resolution)
)

# we will need quite a few reformatting steps to get what we want
# format to 3 column data.frame: x, y, z
galapagos.small.df <- fortify.bathy(galapagos.small)

# convert to crosstable suitable for changeres function of OceanView package
galapagos.small.cross <- db2cross(
  galapagos.small.df, 
  col = "y",
  row = "x", 
  val = "z"
)

# change resolution by adding additional points to grid
# simple linear interpolation
galapagos.small.highres <- changeres(
  var = galapagos.small.cross$z,
  x = galapagos.small.cross$x, 
  y = galapagos.small.cross$y, 
  resfac = 10 # increase resolution 10 times
)

# rename elements of list
names(galapagos.small.highres) <- c("z", "x", "y")

# convert to raster object
tmp <- raster(galapagos.small.highres)

# convert to bathy object, which can be used in plot.bathy
galapagos.small.bathy <- as.bathy(tmp)
summary(galapagos.small.bathy)
rm(tmp)

# create plot
pdf("bathy_highres.pdf", height = 7, width = 7)

par(mar = c(3, 3, 1, 1))

# to ensure that we are using the same color space
# save previously used color palette
galapagos.bpal <- palette.bathy(
  mat = galapagos, # larger data set
  # originally used colors
  layers = list(c(0, max(galapagos), greys(50)),
                c(min(galapagos), 0, blues(100))),
  land = T # include land (same settings as before for coarse plot)
)

# bathymetry
plot.bathy(
  galapagos.small.bathy, # input data
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
  bpal = galapagos.bpal, # use same color palette as for coarse plot
  zlim = c(min(galapagos), max(galapagos)) # use same depth range as for coarse plot
)

map.scale2(-91.93, 0.07, ratio = F, col = "white", font = 2, lwd = 2)

axis(1)
axis(2, las = 2)

box("plot")

mtext(text = "Longitude", side = 1, line = 2)
mtext(text = "Latitude", side = 2, line = 2)

dev.off()

### bathymetric map end ####


### depth profile 1 ####


manta.1m <- read.table("example_data_manta.txt", sep = "\t", h = T)
manta.1m$time <- as.POSIXlt(paste(manta.1m$DATE, manta.1m$t.start), format = "%m.%d.%Y %H:%M:%S")

pdf("Manta_plot.pdf", height = 10, width = 7)
par(
  mfrow = c(7, 1),
  mar = c(0, 3, 3, 5),
  oma = c(5, 2, 0, 0),
  mgp = c(2, 0.7, 0),
  xpd = NA,
  lwd = 0.5
)
for(i in 3:9) {
  # subset data set to parameter of interest
  temp <- manta.1m[, c(12, 2, i)]
  temp$Depth_1m <- -(as.numeric(temp$Depth_1m))
  temp$time.num <- as.numeric(temp$time)
  
  # rearrange values
  temp.cross <- db2cross(
    temp,
    col = "Depth_1m",
    row = "time.num", 
    val = colnames(temp)[3]
  )
  
  # interpolation and plot
  image2D(
    y = temp.cross$y, 
    x = temp.cross$x, 
    z = temp.cross$z, 
    resfac = 8, 
    axes = F,
    xlab = "",
    ylab = "",
    clab = colnames(temp)[3],
    colkey = list(lwd = 0.5, adj.clab = 0, lwd.ticks = 0.5, mgp = c(2, 0.7, 0)),
    xlim = as.numeric(c(min(manta.1m$time), max(manta.1m$time))),
    lwd = 0.5
  )
  
  # add x-axis (sampling times as labels only for last plot)
  if(i == 9) {
    axis.POSIXct(
      1, 
      at = seq(
        min(as.Date(as.character(manta.1m$DATE), format = '%m.%d.%Y')), 
        max(as.Date(as.character(manta.1m$DATE), format = '%m.%d.%Y')),
        by = "1 day"
      ),
      format= "%m/%d",
      las = 2,
      lwd = 0.5
    )
  } else {
    axis.POSIXct(
      1, 
      at = seq(
        min(as.Date(as.character(manta.1m$DATE), format = '%m.%d.%Y')), 
        max(as.Date(as.character(manta.1m$DATE), format = '%m.%d.%Y')),
        by = "1 day"
      ),
      format= "%m/%d",
      labels = "",
      las = 2,
      lwd = 0.5
    )
  }
  
  # add y-axis
  axis(2, at = seq(-5, -20, -5), las = 1, lwd = 0.5)
  
  # add information about tidal phase (only first plot)
  if(i == 3) {
    text(
      unique(manta.1m$time),
      rep(0, 13),
      labels = ifelse(unique(manta.1m[, c("time", "tide")])$tide == "incoming", "I", "O"),
      pos = 3
    )
  }
  
  # add figure panel
  text(
    par("usr")[1] - (par("usr")[2]- par("usr")[1]) * 0.05,
    par("usr")[4] + (par("usr")[4]- par("usr")[3]) * 0.05,
    labels = paste("(", LETTERS[i-2], ")", sep = ""),
    font = 2,
    cex = 1.3
  )
}
mtext(side = 1, line = 3.5, outer = T, text = "Date")
mtext(side = 2, line = 0, outer = T, text = "Depth [m]")
dev.off()


### depth profile 1 end ####


### depth profile 2 ####

# we will create a 2D plot of depth profiles of Chla along a longitudinal transect
# ordinary kriging will be used to fill in gaps in cruise track

# read data
atlantic <- read.table("Atlantic_Chla.txt", sep = "\t", h = T)
head(atlantic)

# show coordinates of measurements
plot(atlantic$Longitude, atlantic$Depth, ylim = c(max(atlantic$Depth), 0), pch = "+")

# there are several options of how to create a grid in R
# here, we will use the function expand.grid() to create a 
# rectangular grid with a higher x resolution
# it is also possible to create irregular shaped grids
# for this, have a look at: griddify (marmap), over (sp)

# create grid
# longitude (x) coordinates of grid every 0.5 degrees
grid.x <- seq(from = floor(min(atlantic$Longitude)), to = ceiling(max(atlantic$Longitude)), by = 0.5)
# keep depth resolution, already regularly spaced (5m)
grid.y <- unique(sort(atlantic$Depth))
# calculate all points in grid
# i.e. create coordinates for each combination on grid.x and grid.y
grid <- expand.grid(grid.x,grid.y) 
# save grid as data frame
grid.df <- as.data.frame(grid)
# show resolution on previous plot
points(grid.df, pch = ".")

# convert to different R object class specific to sp package: SpatialPoints
chla.coord <- atlantic # next line will convert sites to class SpatialPoints and overwrite chla.coord
coordinates(chla.coord) <- ~ Longitude + Depth
grid.coord <- grid.df # next line will convert sites to class SpatialPoints and overwrite grid.coord
coordinates(grid.coord) <- c("Var1","Var2")

# interpolation using default kriging
# may have to be adjusted!
# this part of the script is intended as tutorial about how to visualize kriging output
# it is NOT a tutorial on kriging
chla.kriging <- autoKrige(
  chla.coord, # input data
  new_data = grid.coord # grid coordinates for interpolation
)
# Warning message:
# In sqrt(krige_result$var1.var) : NaNs produced

# extract chla values
chla.predicted <- cbind(
  grid.df,
  chla.kriging$krige_output@data$var1.pred # predicted chl a values
)
colnames(chla.predicted) <- c("Longitude", "Depth", "Chla")

# change to crosstabular format of OceanView package
atlantic.cross <- db2cross(
  chla.predicted, 
  col = "Depth",
  row = "Longitude", 
  val = "Chla"
)

# define Chla ranges to also include original values
Chla.min <- min(c(atlantic.cross$z, atlantic$Chla))
Chla.max <- max(c(atlantic.cross$z, atlantic$Chla))

# plot
pdf("Chla_interpolation.pdf", width = 7, height = 5)

image2D(
  y = atlantic.cross$y, 
  x = atlantic.cross$x, 
  z = atlantic.cross$z, 
  resfac = 1, # don't change resolution
  ylim = c(max(atlantic.cross$y), 0),
  xlim = c(min(atlantic.cross$x), max(atlantic.cross$x)), 
  zlim = c(Chla.min, Chla.max),
  axes = F,
  xlab = "",
  ylab = "",
  clab = "Chla",
  colkey = T,
  col = colorRampPalette(c("white", "darkgreen"))(100)
)

# define color for original data
Chla.col <- cut(
  atlantic$Chla, 
  breaks = seq(
    Chla.min, # include full range of values
    Chla.max, 
    length.out = 101 # need one more breaks than levels that will be created (similar to heatmap.2 argument)
  )
)

levels(Chla.col) <- colorRampPalette(c("white", "darkgreen"))(100)

# add points for original data
points(
  atlantic$Longitude,
  atlantic$Depth,
  col = as.character(Chla.col),
  pch = 16,
  cex = 0.8
)

# add axes
axis(2, las = 1, at = seq(200, 0, -50))
axis(1, las = 1, at = seq(-60, -25, 5))

# add box around plot
# don't use box() since color key included in par("usr")
rect(min(atlantic.cross$x), max(atlantic.cross$y), max(atlantic.cross$x), 0)

# add axis labels
title(xlab = "Longitude", ylab = "Depth")

# close plotting device
dev.off()

### depth profile 2 end ####


### networks ####

# use pairwise correlations as input for non-directed network
# parameters (vertices) will be connected by edges based on the
# strength of the correlation

# input: 3 colum long data format
# column 1 and 2: vertex names, each pair connected by edge
# only take the upper triangle of the correlation matrix
# otherwise values would repeat, since A --> B is the same as B --> A
ENV.cor.melt <- melt(ENV.cor)[melt(upper.tri(ENV.cor))$value, ]

# create network graph
correlation.graph <- graph.data.frame(ENV.cor.melt, directed = F)

# set edge weight to absolute correlation coefficient
E(correlation.graph)$weights <- abs(ENV.cor.melt$value)

# specify color for edges based on correlation coefficient
tmp <- cut(
  ENV.cor.melt$value, 
  breaks = seq(-1, 1, length.out = 31)
)
levels(tmp) <- bluered(30)
E(correlation.graph)$color <- as.character(tmp)
rm(tmp)

# format vertex labels
V(correlation.graph)$label <- c(
  "Temp",
  "pH",
  expression('O'[2]),
  "Chl a",
  expression('PO'[4]^"3-"),
  expression('NO'["x"]^"-"),
  "DOC",
  "POC"
)

# plot network
pdf("cor_network.pdf", width = 7, height = 7)
par(mar = c(1, 1, 1, 1))

# specify network layout
# this will create a force-directed network 
# where verteces connected by strong correlation sohuld be closer together
net.layout <- layout.fruchterman.reingold(
  correlation.graph, # graph object
  weights = E(correlation.graph)$weights # weight of edges based on correlation coefficients
)

plot.igraph(
  correlation.graph,
  edge.color = as.character(E(correlation.graph)$color),
  vertex.color = "white",
  vertex.label.cex = 1, 
  vertex.label.color = "black", 
  vertex.label.family =  "sans",
  vertex.label = V(correlation.graph)$label,
  vertex.size = 30,
  vertex.size2 = 15,
  vertex.shape = "rectangle",
  edge.width = (E(correlation.graph)$weights * 5) + 0.1,
  layout = net.layout
)

dev.off()

### network end ####