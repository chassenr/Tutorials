#=============================================================================
# Popular graphics packages
#
# A number of graphics packages are becoming quite popular in the R community.
# These have their own conventions and coding rules and require some time to
# get used to. If you like what you see, dedicate some time to familiarising
# yourself with their peculiarities.
#=============================================================================


#=============================================================================
# Lattice graphics
#=============================================================================

require(lattice)

# http://csg.sph.umich.edu/docs/R/graphics-1.pdf
# Lattice has its own conventions and requires some getting used to, but
# many people seem to find it useful. Let's look at the lattice version of plot
# xyplot()

data(Formaldehyde)
Formaldehyde

xyplot(
	optden ~ carb,
 	data = Formaldehyde,
 	xlab = "Carbohydrate (ml)",
	ylab = "Optical density", 
	main = "Default panel function",
	pch = 16,
	cex = 2
	)

# So far it looks pretty standard. Lattice requires you to specify 
# everything in one go and has the "panel" argument to let you do that:
xyplot(
	optden ~ carb,
 	data = Formaldehyde,
 	xlab = "Carbohydrate (ml)",
	ylab = "Optical density",
	panel = function(x, y) {panel.xyplot(x,y); panel.lmline(x,y)}
	)

# Where lattice is most useful is in creating trellis graphics

require(nlme) # we just need this package for the Oxboys data
data(Oxboys) # Height data of Oxfordian boys

xyplot(
	height ~ age | Subject, # height as a function of age given subject
 	data = Oxboys,
 	ylab = "Height (cm)",
	aspect = "xy", # calculate an optimal aspect ratio for the trellis
	panel = function(x,y) {panel.grid(); panel.xyplot(x,y)}
	)
# The panel argument allows you to add elements to each panel of the plot
# type "panel." and hit TAB (or whatever your autocomplete key(s) is (are).
# to see the range of functions that can be used to modify panels.

# The trellis layout can be useful in situations we've already seen:
# https://www.stat.auckland.ac.nz/~ihaka/787/lectures-trellis.pdf

data(VADeaths)
rate  <-  as.vector(VADeaths)
age   <-  row(VADeaths, as.factor = TRUE)
group <-  col(VADeaths, as.factor = TRUE)

# barchart() is lattice's answer to barplot()
barchart(
	age ~ rate | group,
	xlab = "Death Rate (per 1000)",
	layout = c(2, 2, 1)
	)


# Trellis panels can be quite versatile, for example...
# http://gallery.r-enthusiasts.com/graph/Scatter_plot_3D_41
data(iris)

cloud(
	Sepal.Length ~ Petal.Length * Petal.Width,
	data = iris,
	groups = Species,
	screen = list(z = 20, x = -70),
	perspective = FALSE,
	key = list(title = "Iris Data", x = .05, y=.95, corner = c(0,1),
	border = FALSE, 
	points = Rows(trellis.par.get("superpose.symbol"), 1:3),
	text = list(levels(iris$Species)))
)

# If you think that these kinds of graphics are useful to you, spend some time
# getting to know lattice's capabilities

#=============================================================================
# Gplots
#=============================================================================
require(gplots)

# gplots is a powerful library to prettify graphics. Let's revist the VADeaths
# data and recreate a bar plot.
# http://gallery.r-enthusiasts.com/graph/barplot2_:_Enhanced_Bar_Plots_54

# First we rearrange the data into a form that gplots likes using transpose,
# t(), and reversing the transposed columns
hh <- t(VADeaths)[, 5:1]

hh

# Let's set a colour
mybarcol <- "gray20"

# and some imaginary confidence intervals for error bars.
ci.l <- hh * 0.85
ci.u <- hh * 1.15


# the barplot2() function is from gplots. It's similar to barplot, but has some
# enhancements

mp <- barplot2(
	hh, 
	beside = TRUE,
	col = c("lightblue", "mistyrose", "lightcyan", "lavender"),
	legend = colnames(VADeaths),
 	ylim = c(0, 100),
	main = "Death Rates in Virginia",
 	font.main = 4,
	sub = "Faked 95 percent error bars",
 	col.sub = mybarcol,
	cex.names = 1.5,
	# This is a handy barplot2() way of dealing with confidence intervals
 	plot.ci = TRUE,
 	ci.l = ci.l,
 	ci.u = ci.u,
	# And you can add a grid...
	plot.grid = TRUE
	)

mtext(
	side = 1,
 	at = colMeans(mp),
 	line = 2,
    text = paste("Mean", formatC(colMeans(hh))),
 	col = "red"
	)

## 2D histograms are also gplottable...

# example data, bivariate normal, no correlation
x <- rnorm(2000, sd=4)
y <- rnorm(2000, sd=1)

# separate scales for each axis, this looks circular
hist2d(x,y, nbins=50, col = c("white",heat.colors(16)))

# A rug plot is a nice addition to a histogram or scatter
# rug() is a standard R function from R's graphics package
rug(x,side=1)
rug(y,side=2)
box()

# Balloon plots are also implmented in gplots
# http://gallery.r-enthusiasts.com/graph/Balloon_plot_60

# Make some data
carnames <- c("bmw","renault","mercedes","seat")
carcolors <- c("red","white","silver","green")
datavals <- round(rnorm(16, mean=100, sd=60),1)
data <- data.frame(Car=rep(carnames,4),
                   Color=rep(carcolors, c(4,4,4,4) ),
                   Value=datavals )
		   
levels(data$Car) <- c("BMW: \nHigh End,\n German","Renault: \nMedium End,\n French",
 "Mercedes:\n High End,\n German", "Seat:\n Imaginary,\n Unknown Producer")

# generate balloon plot with default scaling

balloonplot(
	data$Car,	# X conditional
 	data$Color, # Y conditional
 	data$Value,	# Bubble size
 	ylab ="Color",
 	xlab="Car"
	)


#=============================================================================
# Grammar of graphics
#=============================================================================

# ggplot2 is an increasingly popular graphing package in R, but it takes some
# getting used to, much like lattice.

# install.packages("ggplot2")
require(ggplot2)
# The following example is from: http://had.co.nz/ggplot2

# qplot examples
# qplot() is the basic plotting function of ggplot2

qplot(diamonds$cut, diamonds$carat)
qplot(carat, price, data = diamonds)

# Once data is declared, you can assign other arguments in a more compact form
# for example, the colour:
qplot(carat, price, data = diamonds, colour = clarity)

# You can define geometric elements or "geom"s and execute statistical methods
# such as lm() for linear modelling:
qplot(carat, price, data = diamonds, geom = c("point", "smooth"), method = lm)

# The geom you declare will determine what plot emerges from qplot
qplot(carat, data = diamonds, geom = "histogram")

# each geom then has it's own arguments like "binwidth" for histograms
qplot(carat, data = diamonds, geom = "histogram", binwidth = 1)
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.1)
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.01)

# using ggplot()
# This approach allows you to create a dynamically modifiable ggplot2 object
# "aes", or aesthetics, define how variables are mapped to the visual elements
# of a geom or set of geoms. It's a bit strange at first, but one gets the hang
# of it. Let's create an object and tell it to look at a couple of variables in
# the diamonds data:

d <- ggplot(diamonds, aes(x = carat, y = price))

# Right now, d doesn't know how to visualise the data we just told it to handle
# To try to plot it, just type "d"
d

# You should get an error complaining that there are no layers in the plot.
# without geoms, ggplot2 doesn't know how you'd like the data visualised

# Here's the dynamic part: add a geom to object "d", allowing
# visualisation...

d + geom_point()

# This should visualise the aes elements carat and price using a point plot
# Note that "d" is not changed. If you just typed "d" without adding
# geoms, it would still not plot.

# we can add more complex instructions, for example, we'd like a point plot
# geom, but we use aes to tell ggplot2 to map point colours to the value of
# carat:

d + geom_point(aes(colour = carat))

# legends are automatically added.

# Let's run ggplot on the whole diamond data set 
d.all <- ggplot(diamonds)

# as before, without geoms or aes maps, we can't plot yet
d.all

# Let's add a histogram geom and use aes to feed this geom "price" data:
d.all + geom_histogram(aes(x = price))


# Separation of statistcs and geometric elements
# For these examples, let's create a ggplot2 object which handles price data
# from the diamonds data set
p <- ggplot(diamonds, aes(x = price))

# we can then use a histogram geom to visualise it. Without any parameters in
# geom_histogram, a set of defaults are used.
p + geom_histogram()

# ggplots also have a set of stat_ transformations which will statistically 
# transform your data prior to plotting. stat_bin, for example, will bin your
# data. It's worth checking out what you can do here:http://docs.ggplot2.org/
# the stat_ transformations typically have a geom argument to let the package
# know how to plot the results of the transformations without you having to 
# add the geom separately:

p + stat_bin(geom = "area")
p + stat_bin(geom = "point")
p + stat_bin(geom = "line")

# You can do this the other way round too, using aes to define a statistical
# transformation for a geom function to plot. Here, we run a density function
# on the data in "p" (price data) and map that (using aes) to geom_histogram...
p + geom_histogram(aes(y = ..density..))



# Setting vs mapping
# Let's modify "p" and tell it to handle both carat and price
p <- ggplot(diamonds, aes(x=carat,y=price))

# Now we can use a point plot geom and set the point colour to green:
p + geom_point(colour = "green")

# Why wouldn't this work? 
p + geom_point(aes(colour = "green"))

# Remember, aes() is used to map data, not to set
# parameters. Above, we used aes(colour = carat) which makes sense, as we're
# mapping data (carat) to a parameter, rather than simply setting that 
# paramter to a fixed value.

# Find out more at: http://docs.ggplot2.org/

# Let's see some more http://www.r-bloggers.com/basic-introduction-to-ggplot2/

# We're going to get some taxonomic tribe data from the web
filepath <- "http://bit.ly/wBBTQO"

#read in the tab delimited text file using the url() function
myData <- read.table(
					file = url(filepath),
 					header = TRUE,
 					sep="\t"
					)
myData

# Let's try a simple scatter
qplot(
	data = myData,
	x = BM,
	y = var1,
	log = "xy",
	color = Tribe
	) + theme_bw()

# Adding "theme_bw()" removes the grey background. There are more themes
# available too, and you could probably define your own.

# Now we can use a boxplot geom in ggplot2's catch-all qplot() function
qplot(
	data = myData,
	x = Hab, # One boxplot per habitat type
	y = var1,
	geom = "boxplot"
	) + theme_bw()

# Faceting is a trellis-like approach to breaking down data visually
# it's similar to coplot() thinking...
qplot(
	data = myData,
	x = BM, # Body mass
	y = var1,
	log = "xy",
	color = Tribe,
	facets = Hab ~ Tribe # We're faceting the data set on Habitat and Tribe
	) + theme_bw()

# We can add statistical results to the plot (which is not difficult in the
# normal graphics functions either)
# First we create a plot
myGG <- qplot(
	data = myData,
	x = BM,
	y = var1,
	color = Tribe,
	facets = ~Tribe # This time we just facet by Tribe
	) + theme_bw()

# We can then conveniently add a "layer" that contains a statistical operation
# (in this case linear OLS regression) that will be applied to the contents of
# the graph object.
myGG <- myGG + stat_smooth(method = "lm")

myGG

# Confidence regions have been added automatically.
# To save ggplot2 objects, see ?ggsave


#=============================================================================
# Network graphics
#=============================================================================

require(igraph)

# code lines from Csardi, G. (n.d.). Practical statistical network analysis
# (with R and igraph). Vienna.

# creating graphs
g <- graph( c(1,2, 1,3, 1,4, 2,4, 4,5), n=5 )
plot(g)

# with loops
g <- graph( c(1,1, 1,2, 1,3, 2,3, 4,5, 5,4, 5,1), n=5 )
plot(g)

# A simple undirected graph 
g <- graph.formula(
					Alice-Bob-Cecil-Alice,
					Daniel-Cecil-Eugene,
 					Cecil-Gordon
					)
				
plot(g, vertex.size = 50)

# Another undirected graph, ":" notation for more compact linkage statements
# look at the graphs that result to figure out what it does
g <- graph.formula(Alice-Bob:Cecil:Daniel, Cecil:Daniel-Eugene:Gordon)
plot(g, vertex.size = 50)

# A directed graph 
g <- graph.formula(
	Alice +-+ Bob --+ Cecil +-- Daniel, Eugene --+ Gordon:Helen
	)
plot(g, vertex.size = 40)

# Graphs may be created from result sets like correlation tables
# or dissimilarity matrices (see ?graph.adjacency et al.), but you can also
# pull data out of graphs... 
# Extraction of data.frame frome igraph object

get.data.frame(g, what="vertices")
get.data.frame(g, what="edges")

# Extract adjacency matrix
get.adjacency(g)
adj <- get.adjacency(g)

graph.adjacency(adj)


# Assigning attributes: set/get.graph/vertex/edge.attribute.

# First we create a random graph with 100 vertices using an Erdos-Renyi game
g <- erdos.renyi.game(100, 1/100) 
plot(g)

# We can use the V() iterator to set vertex attributes
# if a vertex or edge attribute (like $color) doesn't exist yet, it will be 
# created using the data you're feeding into it

# Running the line below should give a NULL result
V(g)$color

# We can set the colours for the 100 vertices.
V(g)$color <- sample(
					c("red", "black"),
					vcount(g),	# vcount just counts the number of vertices in
 								# a graph
					rep = TRUE
					)

plot(g)

# We can edit edge colours similarly, here we set all edge colours to grey
E(g)$color <- "grey" 
plot(g, layout = layout.circle) # layouts are used to plot your graph in
								# a specifid format or using an algorithm

# We can use V() and E() to create lists of vertices or edges which meet
# a specific condition...
red <- V(g)[ color == "red" ]
bl  <- V(g)[ color == "black" ] 

# We can use those lists to access those edges for specific manipulations
# Now we'll set the edges going from vertices of the same colour to the
# colour of the vertices.
E(g)[ red %--% red ]$color <- "red" 
E(g)[ bl %--% bl ]$color <- "black" 

plot(g, vertex.size=10, layout= layout.circle)

# Naming vertices can be done using the same logic
g <- graph.ring(10) 
V(g)$name <- sample(letters, vcount(g))
plot(g)

# Using layouts
g <- graph.tree(40, 4) 
plot(g) 
plot(g, layout=layout.circle)

# Force directed layouts  
plot(g, layout=layout.fruchterman.reingold)

# Interactive 
tkplot(g, layout=layout.kamada.kawai) 

# 3D
rglplot(g, layout=layout.kamada.kawai)

# Visual properties can be edited in bulk when calling the plot() function
plot(g, layout=layout.kamada.kawai, vertex.color="cyan")


# There are many more packages to explore. Remember to be task-oriented and to
# use CRAN's task views to reduce search time and prevent yourself getting lost
# in crowd. Find packages that address your specific needs when possible.