#=============================================================================
# Multivariate data
# 
# R has many functions for multivariate plotting; however keep in mind that
# you can use the 'basic' plotting skills to make multivariate plots yourself.
# In fact, our customised scatterplot in Walkthrough 1 was a multivariate plot.
# Plotting characters, point sizes, and point colours were all dependent on our
# data.
#
# Here we'll look at a few examples of multivariate plotting functions and also
# work with some ordination and cluster analysis plotting functions.
#
#=============================================================================

# Keep in mind that "graphing" need not be exclusively visual...
# http://fms.csvsoundsystem.com/
# soundscapes are being used from genomics to astrophysics to represent both
# raw data and the results of analyses.

# Now back to it...

# R has many forms of multivariate data plots built in and its packages have
# even more. This is just a sample of some of its capability.

## Star plot examples from ?stars

stars(
	mtcars[, 1:7],
 	key.loc = c(14, 2),
	main = "Motor Trend Cars",
 	full = FALSE
	)

stars(
	mtcars[, 1:7],
 	key.loc = c(14, 1.5),
	main = "Motor Trend Cars",
 	flip.labels = FALSE
	)

# 'Spider' or 'Radar' plot
stars(
	mtcars[, 1:7],
 	locations = c(0, 0),
 	radius = FALSE,
	key.loc = c(0, 0),
 	main = "Motor Trend Cars",
 	lty = 2
	)

# Segment plots

palette(rainbow(12, s = 0.6, v = 0.75))

stars(
	mtcars[, 1:7],
 	len = 0.8,
 	key.loc = c(12, 1.5),
	main = "Motor Trend Cars",
 	draw.segments = TRUE
	)

## Ternary plot examples from ?triangle.plot

require("ade4")

data(euro123)
head(euro123)

opar <- par(mfrow = c(1,1))

triangle.plot(
	euro123$in78,
 	clab = 0,		# 0 = don't label characters
 	cpoint = 2,		# point type
 	addmean = TRUE, # add the mean of the triangle scatter to the plot
	show = FALSE	# Should the "used" triangle's position be shown 
					# in the complete data range? i.e. the "big" triangle?
	)


triangle.plot(
	euro123$in86,
 	label = row.names(euro123$in78),
 	clab = 0.8 # size of the labels
	)

# To compare the relative positions of points between two triangle plots
# use triangle.biplot. The arrows represent the shift in point position
# between two data sets.
triangle.biplot(
	euro123$in78,
 	euro123$in86
	)

# You can even show the principal axes of the scatter (i.e. orthogonal axes,
# drawn successively, through directions of maximal point scatter...
triangle.plot(
	rbind.data.frame(euro123$in78, euro123$in86),
 	clab = 1, 
	addaxes = TRUE, # Add principal axes
	
	# The subtitle and its position are used to label the axis that is the
	# first principal axis. You'd usually do this after you've plotted the
	# triangle and determined where to put the label.
 	sub = "Principal axis", 
 	csub = 2,
 	possub = "topright"
	)


## Chernoff faces
# The human brain is very good at facial recognition. The traits magnitudes of
# Chernoff faces can be set to correspond to specific data values.
# See ?faces for more
require(TeachingDemos)
faces(mtcars)


## Ordinations
# Vegan has a suite of useful functions for working with ordination plots
# However, many of the low-level skills you've seen today can do the same
# There's a vegan manual and vignette available. Consult that for more info
# on the functions used here.
require(vegan)

# First we import some ecological data
data(dune)
data(dune.env)

# Then perform an NMDS analysis with metaMDS
dune.nmds <- metaMDS(dune)
dune.nmds

# Don't forget we can pull anything out of this object to plot, for example:
hist(dune.nmds$points)
plot(dune.nmds$species)

# Once vegan is loaded, however, R's plot() function can handle metaMDS objects
# intelligently.
plot(dune.nmds, type = "t")

# vegan features ordination functions that can make life easier, for example:
fig <- ordiplot(dune.nmds, type = "n")
points(fig, "sites", pch = 21, col = "red", bg = "yellow")
text(fig, "species", col = "blue", cex = 0.9)

identify(fig, "sites")

# The ordi*() functions are described in the vegan intro and are generally 
# useful.
# http://cran.r-project.org/web/packages/vegan/vignettes/intro-vegan.pdf

# For what follows, we need the environmental data that comes with dune...
attach(dune.env) # Generally not a good idea to attach things unless you're
				 # VERY SURE they won't cause trouble.
				 # if the variable names match other object names, things
				 # can get confusing.

# Lay down the basic plot with no characters
plot(dune.nmds, disp = "sites", type = "n")

# Add hulls linking sites with similar Management with ordihull()
# Management is a variable in the dune.env data we attached.
# if you didn't attach it, you'd say: dune.env$Management
ordihull(dune.nmds, Management, col = "blue", lwd = 2)

# Similarly, add ellipses (these are usually calculated from standard 
# deviations or errors, but may be derived from other measures...
ordiellipse(dune.nmds, Management, col = "forestgreen",lwd = 2)

# A spider connecting points to their group centroid can also be added
ordispider(dune.nmds, Management, col="red", label = TRUE)

# Finally, we can add some points to the plot...
points(
	dune.nmds,
 	disp = "sites",
 	pch = 21,
 	col = "black",
 	bg = "green",
 	cex = rowSums(dune)/10	# To make it interesting, let's make the cex
							# depend on the total abundance at a site
							# The rowSums() function gives us that number
							# and we divide by 10 to make the cex values
							# reasonable (a cex value of 40 would fill up
							# the plot!).
	)

# Cool - but some points have been plotted over our Management type labels
# placed by our previous ordispider() call. Running the ordispider() after
# the points have been placed would plot it "in front of" the points.

# Routines for adding the results of cluster analyses with ordicluster()
# surfaces with ordisurf() and other functions are also available.


## Clustering

# To perform hierarchical clustering, we can use the hclust() function on a
# distance matrix produced by dist() or vegdist() from vegan. vegdist() is
# preferred here as we can use the Bray-Curtis dissimilarity measure by
# default.
dune.dist <- vegdist(dune)
dune.clust <- hclust(dune.dist, method = "average")	# a few common clustering
													# methods are available in
													# hclust()
plot(dune.clust, main = "Dune dendrogram")	# plot recognises an hclust object
											# and plots a dendrogram

# Is there a merge level with three groups defined?
cutree(dune.clust, k = 3)

# Let's visualise that with rect.hclust(), which draws a rectangle over a
# dendrogram...

rect.hclust(
			dune.clust,
 			h = 0.4,
            border = rainbow(3)
			)

# As usual, we can modify the object and hence modify the plot
# Let's see what we can access:
str(dune.clust)

# Let's mess with the labels
dune.clust$labels  <- sample(LETTERS, 20) # just some random labels 
plot(dune.clust)


# Using our low-level skills, we can edit dendrograms extensively
# http://gastonsanchez.wordpress.com/2012/10/03/7-ways-to-plot-dendrograms-in-r/
plot(
	dune.clust,
 	col = "#487AA1",
 	col.main = "#45ADA8",
 	col.lab = "#7C8071",
    col.axis = "#F38630",
 	lwd = 3,
 	lty = 3,
 	sub = '',
 	hang = 1,
 	axes = FALSE
	)

# add axis
axis(
	side = 2,
 	at = seq(0, .8, .1),
 	col = "#F38630",	
    labels = FALSE, 
	lwd = 2
	)

# add text in margin
mtext(
	seq(0, .8, .1), 
	side = 2,
 	at = seq(0, .8, .1),
	line = 1,
 	col = "#A38630",
 	las = 2
	)

# More customisations are possible, such as adding values or other plots to
# merge/split points on the tree, although other packages like phylotools can
# make this easier.


# The ape package allows dendrogram plotting in phylogenetic paradigms
# the phylotools package is also worth learning about.
# http://gastonsanchez.wordpress.com/2012/10/03/7-ways-to-plot-dendrograms-in-r/
require(ape)

hc  <-  hclust(dist(mtcars))	# Here we use dist() rather than vegdist()
								# as Euclidean distances are fine
								# Notice the distance matrix is created and
								# then fed into hclust() in one line.
	
# We can cut our dendrogram into 5 clusters...
clus.5  <-  cutree(hc, 5)

# ...and assign a colour to each cluster
clus.cols  <-  c("#556270", "#4ECDC4", "#1B676B", "#FF6B6B", "#C44D58")

# Now we call plot() with the ape loaded. This means that plot() will
# be able to handle the "phylo" format from ape
par(mar = rep(0,4))

# You could use the tiff() device to export the plot too.
# just uncomment the line below as well as "dev.off()".
#tiff("ape.tif")
plot(
	as.phylo(hc), # We use ape's phlyo class
 	type="fan",
 	tip.color = clus.cols[clus.5],
 	label.offset = 1,
	cex = log(mtcars$mpg, 15), # The bigger the label, the better the mpg
 	col="red"
	)
#dev.off()

# The plotting area is not perfect, but this can be tweaked a little more so
# everything looks good. Try using some low-level settings to make this plot
# fit the device well. 


