# creating 3 panel plot with ####
# cluster dendrogram based on euclidean distances of environmental data
# line plot with carbon fixation rates
# line plot with nitrogen fixation rates

setwd("C:/Users/User/Documents/githubRepos/Tutorials/trunk/R_course_MPI/Example_data")


# load data
M96 <- read.table(
  "Martinez-Perez_Supplementary_Table_1_forR_renamed.csv",
  h = T,
  sep = "\t"
)
head(M96)

range(M96$CO2.fix)
range(M96$N.fix)

# create depth category in R
M96$depth.type <- M96$depth > 100

# specify rownames as unique combunation of station and depth
rownames(M96) <- paste(M96$CRUISE, M96$STATION, M96$depth, sep = ".")

# add color for stations
M96$STATION.color <- as.factor(M96$STATION)
levels(M96$STATION.color) <- rainbow(length(levels(M96$STATION.color)))
M96$STATION.color <- as.character(M96$STATION.color)

# extract numerical variables to be used for cluster diagram
M96.num <- M96[, c(4, 6, 18:21)] # longitude, depth, nutrients
str(M96.num)

# z-score transform M96.num
M96.num.z <- scale(M96.num)

# calculate distance and cluster diagram
require(vegan)
M96.num.dist <- vegdist(M96.num.z, method = "euclidean")
M96.num.dend <- as.dendrogram(hclust(M96.num.dist))
order.dendrogram(M96.num.dend)

# reorder dendrogram based on depth.type
M96.num.dend.ord <- reorder(M96.num.dend, as.numeric(!M96$depth.type), agglo.FUN = mean)
order.dendrogram(M96.num.dend.ord)

# plotting
# worth a look: http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning
windows(width = 15, height = 10)
par(
  mar = c(5, 2, 2, 0),
  mfrow = c(1, 3),
  xpd = NA # plot on margins
)

# first plot: cluster diagram
plot(
  M96.num.dend.ord, 
  horiz = T, 
  leaflab = "none", 
  axes = F
)

# second plot: carbon fixation
par(mar = c(5, 5, 2, 2))
plot(
  0, 0,
  type = "n",
  axes = F,
  xlab = "Carbon fixation",
  ylab = "",
  ylim = c(1, nrow(M96)),
  xlim = c(0, 2000)
)
axis(
  1,
  at = seq(0, 2000, 500)
)
axis(
  2,
  at = 1:nrow(M96),
  labels = paste("St", M96$STATION, " ", M96$depth, "m", sep = "")[order.dendrogram(M96.num.dend.ord)],
  las = 2,
  cex.axis = 0.8
)
lines(
  M96$CO2.fix[order.dendrogram(M96.num.dend.ord)],
  1:nrow(M96)
)
points(
  rep(-600, nrow(M96)),
  1:nrow(M96),
  col = M96$STATION.color[order.dendrogram(M96.num.dend.ord)],
  pch = 15
)

# third plot: nitrogen fixation
par(mar = c(5, 5, 2, 2))
plot(
  0, 0,
  type = "n",
  axes = F,
  xlab = "Nitrogen fixation",
  ylab = "",
  ylim = c(1, nrow(M96)),
  xlim = c(0, 40)
)
axis(
  1,
  at = seq(0, 40, 10)
)
axis(
  2,
  at = 1:nrow(M96),
  labels = paste("St", M96$STATION, " ", M96$depth, "m", sep = "")[order.dendrogram(M96.num.dend.ord)],
  las = 2,
  cex.axis = 0.8
)
lines(
  M96$N.fix[order.dendrogram(M96.num.dend.ord)],
  1:nrow(M96)
)