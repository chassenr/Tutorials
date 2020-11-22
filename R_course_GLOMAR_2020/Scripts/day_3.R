#################################
#         DAY 3                 #
#################################

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

# plotting parameters can be set before the plot to define the plotting area, etc.
# there are some parameters which are then used by the plotting commands
# those can also be specified later (as part of the command)
# please refer to the examples in the plotting commands

# to delete any changes to default plotting parameters
# close all plotting devices
dev.off()

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

#####


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

#####


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

#####

require(vegan)
require(Hmisc)
require(OceanView)
require(rgdal)
require(raster)
require(marmap)
require(TeachingDemos)
require(maps)


### PCA (scatter plot) ####

# build your own plot element by element
# example: PCA
env.params <- c("pH", "SiO4", "PO4", "NH4", "NO3", "NO2")
ENV.pca <- rda(ENV[, env.params], scale = T)

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
  groups = ENV$seep.category,
  draw = "none"
)

# only 1 polygon can be plotted at a time
for(i in 1:length(levels(ENV$seep.category))) {
  polygon(
    ENV.poly[[i]],
    col = alpha(levels(ENV$color)[i], alpha = 0.3), # adjust transparency
    border = NA # don't draw border of polygon
  )
}

# add points
points(
  ENV.pca.scaling2$sites[, 1],
  ENV.pca.scaling2$sites[, 2],
  pch = 21,
  cex = 2,
  col = "black",
  bg = as.character(ENV$color)
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
    "pH",
    expression('SiO'[4]^"4-"),
    expression('PO'[4]^"3-"),
    expression('NH'[4]^"+"),
    expression('NO'[3]^"-"),
    expression('NO'[2]^"-")
  ),
  # plot labels in lower half of plot below the arrows, otherwise above
  pos = ifelse(ENV.pca.scaling2$species[, 1] < 1, 3, 4) 
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
  'topright',
  title = "Seep influence",
  legend = levels(ENV$seep.category),
  pch = 21,
  pt.bg = levels(ENV$color),
  pt.cex = 2 # only increase size of points
)

# close plotting device
# that will save the plot to pdf
dev.off()

#####


### multipanel line plot mean with sd ####

# getting data into shape
ENV.mean <- cast(ENV.melt, "seep.category ~ variable", mean, na.rm = T)
ENV.sd <- cast(ENV.melt, "seep.category ~ variable", sd, na.rm = T)
env.labels <- c(
  "pH",
  expression('SiO'[4]^"4-"*" [然]"),
  expression('PO'[4]^"3-"*" [然]"),
  expression('NH'[4]^"+"*" [然]"),
  expression('NO'[3]^"-"*" [然]"),
  expression('NO'[2]^"-"*" [然]")
)
names(env.labels) <- env.params

# preparing plot
pdf("Line_plot.pdf", width = 7, height = 7)
par(
  mfcol = c(ceiling(length(env.params)/2), 2),
  mar = c(0.5, 3, 0.5, 0.5),
  oma = c(3, 0.5, 0, 0),
  xpd = NA
)

# run plot in loop
for(i in env.params) {
  
  # prepare empty plot
  plot(
    0, 0,
    type = "n", # don't plot anything yet
    axes = F, # don't plot axes
    xlim = c(1, length(levels(ENV$seep.category))),
    ylim = c(min(ENV.mean[, i] - ENV.sd[, i]), max(ENV.mean[, i] + ENV.sd[, i])),
    mgp = c(2, 0.5, 0),
    ylab = env.labels[i],
    xlab = ""
  )
  
  # add segments for sd range
  segments(
    1:length(levels(ENV$seep.category)),
    ENV.mean[, i] + ENV.sd[, i],
    1:length(levels(ENV$seep.category)),
    ENV.mean[, i] - ENV.sd[, i]
  )
  
  # add line connecting means
  lines(
    1:length(levels(ENV$seep.category)),
    ENV.mean[, i],
    lty = 1
  )
  
  # add points for mean
  points(
    1:length(levels(ENV$seep.category)),
    ENV.mean[, i],
    pch = 21,
    bg = levels(ENV$color),
    cex = 2
  )
  
  # add axes
  axis(2, mgp = c(2, 0.5, 0), tcl = -0.3, las = 1, cex.axis = 0.7)
  if(i %in% env.params[c(ceiling(length(env.params)/2), length(env.params))]) {
    axis(1, at = 1:length(levels(ENV$seep.category)), labels = levels(ENV$seep.category), mgp = c(2, 0.5, 0), tcl = -0.3, las = 1, cex.axis = 1)
  } else {
    axis(1, at = 1:length(levels(ENV$seep.category)), labels = NA, mgp = c(2, 0.5, 0), tcl = -0.3)
  }
  
}

# add title for x axis
mtext("Seep influence", side = 1, line = 1.5, outer = T)

dev.off()

#####


### taxonomic composition barplot ####
# useful to display taxonomic composition

# summarize per site (done yesterday)
# this is not needed to avoid hardcoding the number of samples per seep category
ENV.mean <- cast(ENV.melt, "seep.category + site ~ variable", mean, na.rm = T)
all.equal(as.character(ENV.mean$site), colnames(OTU))

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
pdf("Tax_plot.pdf", width = 8, height = 6)
layout(
  matrix(c(1, 2), nrow = 1, ncol = 2), # 2 plots in 1 row and 2 column
  widths = c(1.5, 1)
)

# first plot: stacked bar plot
par(mar = c(5, 4, 1, 1), xpd = NA)
bp <- barplot(
  abund_rel, 
  col = c(colorRampPalette(colorPalette)(nrow(abund_rel) - 1), "darkgrey"), # add darkgrey to color vector for 'other'
  ylim = c(0, 100), 
  las = 2,
  ylab = "Sequence proportion [%]",
  cex.axis = 0.7,
  space = ifelse(1:ncol(OTU) %in% (cumsum(table(ENV.mean$seep.category))[-length(levels(ENV.mean$seep.category))] + 1), 0.5, 0.2)
)

# add boxes for seep categories
rect(
  (bp - 0.5)[c(1, cumsum(table(ENV.mean$seep.category))[-length(levels(ENV.mean$seep.category))] + 1)],
  par("usr")[4] * -0.18,
  (bp + 0.5)[cumsum(table(ENV.mean$seep.category))],
  par("usr")[4] * -0.14,
  col = levels(ENV$color)
)
text(
  apply(
    data.frame(
      (bp - 0.5)[c(1, cumsum(table(ENV.mean$seep.category))[-length(levels(ENV.mean$seep.category))] + 1)],
      (bp + 0.5)[cumsum(table(ENV.mean$seep.category))]
    ),
    1,
    mean
  ),
  par("usr")[4] * -0.16,
  labels = levels(ENV$seep.category),
  col = c("white", "black", "black"),
  cex = 0.7
)

# second plot: legend
plot.new()
legend(
  "center", 
  pch = 22, # filled squares with black border
  col = "black", # border of squares
  pt.bg = rev(c(colorRampPalette(colorPalette)(nrow(abund_rel) - 1), "darkgrey")), # color of squares
  legend = gsub(
    "_unclassified",
    " (uncl.)",
    rev(rownames(abund_rel)) # legend labels, reversed order, to match order of occurrence in barplot (plotted bottom up)
  ),
  pt.cex = 1.5, 
  cex = 0.8
) 

dev.off()

#####


### Heatmap of dominant classes ####
# instead of barplot, let's plot a heatmap

pdf("Heatmap_plot.pdf", width = 7, height = 7)
layout(
  matrix(
    1:4, 
    nrow = 2,
    ncol = 2,
    byrow = T
  ),
  heights = c(1, 0.25),
  widths = c(3, 1)
)

# plot heatmap using image
par(mar = c(0.5, 0.5, 0.5, 0.5), xaxs = "i", yaxs = "i", xpd = NA, lwd = 0.5, ann = F)
temp.heat <- t(sqrt(abund_rel0)) # remove others as taxon
plot(
  0, 0,
  type = "n",
  xlim = c(0.5, nrow(temp.heat) + (length(levels(ENV$seep.category)) - 1) + 0.5),
  ylim = c(0.5, ncol(temp.heat) + 0.5),
  axes = F
)
for(i in 1:length(levels(ENV$seep.category))) {
  image(
    x = (1:nrow(temp.heat))[ENV.mean$seep.category == levels(ENV$seep.category)[i]] + (i - 1),
    y = 1:ncol(temp.heat),
    z = temp.heat[ENV.mean$seep.category == levels(ENV$seep.category)[i], ],
    zlim = c(0, max(temp.heat)),
    add = T,
    col = colorRampPalette(c("white", "grey", "yellow", "orange", "red", "darkred"))(30),
    axes = F
  )
  rect(
    min((1:nrow(temp.heat))[ENV.mean$seep.category == levels(ENV$seep.category)[i]] + (i - 1)) - 0.5,
    0.5,
    max((1:nrow(temp.heat))[ENV.mean$seep.category == levels(ENV$seep.category)[i]] + (i - 1)) + 0.5,
    ncol(temp.heat) + 0.5,
    lwd = 0.5
  )
  axis(
    1, 
    at = (1:nrow(temp.heat))[ENV.mean$seep.category == levels(ENV$seep.category)[i]] + (i - 1),
    labels = NA,
    lwd = 0.5,
    tcl = -0.3
  )
  text(
    (1:nrow(temp.heat))[ENV.mean$seep.category == levels(ENV$seep.category)[i]] + (i - 1), 
    rep(0, sum(ENV.mean$seep.category == levels(ENV$seep.category)[i])),
    labels = ENV.mean$site[ENV.mean$seep.category == levels(ENV$seep.category)[i]],
    cex = 1,
    pos = 1,
    srt = 90
  )
}

# taxon names
temp.tax <- gsub("_unclassified", " (uncl.)", capitalize(rownames(abund_rel0)))
text(
  rep(par("usr")[2], ncol(temp.heat)),
  1:ncol(temp.heat),
  labels = temp.tax,
  pos = 4,
  cex = 0.8
)

# plot 6: spacer
par(mar = rep(0, 4))
plot.new()

# plot 7: spacer
par(mar = rep(0, 4))
plot.new()

# plot 8: legend for heatmap
par(mar = c(2.5, 0, 0.5, 1), xaxs = "i", yaxs = "i", xpd = NA, lwd = 0.5, ann = F)
image(
  matrix(
    seq(0, max(temp.heat), length.out = 30),
    nrow = 30,
    ncol = 1
  ),
  x = seq(0, max(temp.heat), length.out = 30),
  y = 1,
  axes = F,
  col = colorRampPalette(c("white", "grey", "yellow", "orange", "red", "darkred"))(30)
)
box("plot", lwd = 0.5)
axis(
  1, 
  at = sqrt(c(0, 1, 2, 5, 10, 25)), 
  labels = c(0, 1, 2, 5, 10, 25),
  mgp = c(1, 0.1, 0),
  tcl = -0.1,
  cex.axis = 0.7,
  lwd = 0.5
)
title(xlab = "Sequence proportion [%]", cex.lab = 0.8, mgp = c(1, 0.1, 0))

dev.off()

#####


### bathymetric map of Galapagos ####
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
pdf("Bathy_plot.pdf", height = 7, width = 7)

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
  bpal = list(
    c(0, max(galapagos), greys(50)), # use one color palette for land (0m to max)
    c(min(galapagos), 0, blues(100)) # use second color palette for ocean (min to 0)
  ),
  xlim = c(-92, -88),
  ylim = c(-2, 2)
)

source("../Scripts/scalebar.R")
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

# only draw box around plot, if plotting area conforms with aspect ratio
# otherwise par("usr") is not equal to axis range
box("plot")

# add axis titles
mtext(text = "Longitude", side = 1, line = 2)
mtext(text = "Latitude", side = 2, line = 2)

# close plotting device
dev.off()

#####


### interpolated Chla profile of Atlantic ####
manta.1m <- read.table("../../../Plotting-in-R/example_data_manta.txt", sep = "\t", h = T)
manta.1m$time <- as.POSIXlt(paste(manta.1m$DATE, manta.1m$t.start), format = "%m.%d.%Y %H:%M:%S")
atlantic <- read.table("../../../Plotting-in-R/Atlantic_Chla.txt", sep = "\t", h = T)
head(atlantic)

pdf("Interpolation_plot.pdf", height = 5, width = 7)

# rearrange values
temp.cross <- db2cross(
  atlantic,
  col = "Depth",
  row = "Longitude", 
  val = "Chla"
)

# interpolation and plot
image2D(
  y = temp.cross$y, 
  x = temp.cross$x, 
  z = temp.cross$z, 
  resfac = 10, # play around with resolution factor
  axes = T,
  xlab = "Longitude",
  ylab = "Depth [m]",
  clab = "Chla",
  colkey = list(lwd = 0.5, adj.clab = 0, lwd.ticks = 0.5, mgp = c(2, 0.7, 0), cex.axis = 0.7),
  lwd = 0.5,
  ylim = c(max(atlantic$Depth), 0),
  las = 1,
  cex.axis = 0.7,
  mgp = c(2.5, 0.5, 0)
)

dev.off()

#####


### phylogenetic tree ####
# This is just a very rudimentary example
# The input is a .tree file as e.g. produced by FastTree

require(ape)
require(tree)

tlh.tree <- read.tree("Vibrio_tlh_fasttree.tree")
tlh.map <- read.table("Vibrio_tlh_species_map.txt", h = F, sep = "\t", stringsAsFactors = F, row.names = 1)
tlh.tree$tip.label <- paste(
  gsub("genomosp", "sp", gsub(";", "", gsub("organism=Vibrio ", "V.", tlh.map[tlh.tree$tip.label, 1]))),
  gsub("\\.[0-9]*$", "", tlh.tree$tip.label)
)

# quick and dirty unrooted tree
plot(
  tlh.tree, 
  show.node.label = F, 
  show.tip.label = T,
  cex = 0.3,
  no.margin = T,
  edge.width = 0.3,
  type = "unrooted"
)
nodelabels(
  text = tlh.tree$node.label, 
  frame = "none",
  col = "blue",
  cex = 0.3
)

# root tree
plot(tlh.tree, cex = 0.5, no.margin = T)
tlh.rooted.tree <- root(tlh.tree, interactive = T, resolve.root = T)
plot(
  tlh.rooted.tree,
  cex = 0.5,
  show.node.label = F,
  no.margin = T,
  edge.width = 0.3
)

# subset tree
tlh.subset.tree <- extract.clade(tlh.rooted.tree, interactive = T)
pdf("Phylo_plot.pdf", height = 7, width = 5)
plot(
  tlh.subset.tree, 
  cex = 0.5, 
  show.node.label = F, 
  no.margin = T,
  edge.width = 0.3
)
dev.off()

#####