#=============================================================================
# Maps (geographic, topographic, data)
#
# R has a variety of methods for drawing maps. The default map databases are
# nothing special, so if you need to use maps, find out where to get shapefiles
# or other compatible file formats for your region(s) of interest. An example
# of a custom, high resolution shapefile is shown below ("coast").
# Topographic and surface plotting functions (shown later) can be fed with "xyz"
# data, such as bathymetric data that can sourced from a variety of sources 
# such as: http://topex.ucsd.edu/cgi-bin/get_srtm30.cgi
#
#=============================================================================

require(maps)
require(mapdata)
require(mapproj)
require(scales)
require(maptools)

map()
map("usa")

map("usa")
map("rivers", add = TRUE)

# We can also use spatial data to interact with map
data(ozone)
head(ozone)

map(
	"state",				# This defines the granularity, we're on the 
							# "state" level and will see state borders
 	xlim = range(ozone$x),	# And now we define lat/lon range we're
							# interested in
 	ylim = range(ozone$y)
	)

text(ozone$x, ozone$y, ozone$median)

# We could also use points
points(
	ozone$x,
 	ozone$y,
 	cex = ozone$median / 10, # Size by median
 	pch = 21,
 	bg = "blue"
	)


# This can get pretty sophisticated as this example from ?map shows
# BEGIN EXAMPLE...
# color US county map by 2009 unemployment rate
# match counties to map using FIPS county codes
# Based on J's solution to the "Choropleth Challenge"
# http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html

# load data
# unemp includes data for some counties not on the "lower 48 states" county
# map, such as those in Alaska, Hawaii, Puerto Rico, and some tiny Virginia
#  cities
data(unemp)
data(county.fips)

# Define color buckets
# First define some colours...
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
# Next, we use cut() to put the continous unemp value into categories which
# we can use to index our colours...
unemp$colorBuckets <- as.numeric(
								cut(
									unemp$unemp,
 									c(0, 2, 4, 6, 8, 10, 100)
									)
								)

# define some legend text that fits our cut unemp range
leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")

# align data with map definitions by matching FIPS codes
# works much better than trying to match the state, county names
# which also include multiple polygons for some counties

colorsmatched <- unemp$colorBuckets[match(county.fips$fips, unemp$fips)]

  # Draw county maps
  map(
	"county",
 	col = colors[colorsmatched],
 	fill = TRUE,
 	resolution = 0,
    lty = 0,
 	projection = "polyconic"
	)

# Add state maps
  map(
	"state",
 	col = "white",
 	fill = FALSE,
 	add = TRUE,
 	lty = 1,
 	lwd = 0.2,
    projection="polyconic"
	)

  title("unemployment by county, 2009")
  legend("topright", leg.txt, horiz = TRUE, fill = colors)

# ... END EXAMPLE

# The above is not very useful for marine scientists, but shows the logic of
# mapping in R.

# You can also define your own coordinate range and projection type.
# Let's make an Arctic plot...

xlim = c(-180,180)	# Longitude, all around the world
ylim = c(60,90)		# Latitude, just high latitudes
 
map(
	xlim = xlim,
 	ylim = ylim,
 	plot = TRUE,
	col="lightgreen",
 	fill=TRUE,
 	projection="gnomonic",
	orientation=c(90, 0, 225)	# This defines where the North Pole should be
								# and the rotation of the map.
	)

# Now we can add points at specific lat/lon coords...
lon <- c(0,-72, -66, -107, -154)	# fake longitude vector
lat <- c(90,81.7, 64.6, 68.3, 60)	# fake latitude vector
value <- c(20,10,20,30,40)			# values at each coord pair

# mapproject() will make sure that your coordinates will be compatible with
# the projection you've chosen...

coord <- mapproject(
	lon,
 	lat,
 	proj = "gnomonic",
 	orientation = c(90, 0, 225) # This should match your map's orientation
	)

# Just setting some point colours
point.cols <- rainbow(length(value))

#plot converted points
points(
	coord, 
	pch=20, 
	cex= 5, 
	col=point.cols
	)  

# We can use other low-level functions like arrows() we learnt on maps too
#http://www.stat.auckland.ac.nz/~paul/RGraphics/examples-map.R

par(mar = rep(0, 4)) # no margins

map(
	"nz",
 	fill = TRUE,
 	col = "green"
	)

points(
	174.75, # long
 	-36.87, # lat
 	pch = 16,
 	cex = 2,
 	col = "red"
	)

arrows(
	# From
	172,
 	-36.87,
	# To
 	174,
 	-36.87,
 	lwd=3
	)

text(
	172, # long
 	-36.87, # lat
 	"Auckland  ",
 	adj = 1,
 	cex = 2
	)


# Sometimes, the built in maps are not high-resolution enough, at this stage
# you should look for shapefiles to import into R
# Here's a well known "Gadoid landings" example

require(mapplots)
data(landings)
data(coast)

par(mfrow = c(1,1))

# Set the long and lat limits
xlim <- c(-15, 0)
ylim <- c(50, 56)

# Turn landings data into an xyz file
xyz <- make.xyz(
	landings$Lon, # x
	landings$Lat, # y
	# Anything after x and y is considered "z"
	landings$LiveWeight,  # z
	landings$Species	  # z
	)

# set colours for our legend, we have 5 species so...
col <- rainbow(5) 

# First we set down the base map
basemap(
	xlim, 
	ylim, 
	main = 'Species composition of gadoid landings'
	)

# Then we use our shapefile data to draw a high res coast
# As we have a basemap already in the device, only the coast in the range
# of the device will be plotted...

draw.shape(
	coast, 
	col='cornsilk'
	)

# Now we can add some barplots to convey the species composition
draw.barplot2D(
	xyz$x, 
	xyz$y, 
	xyz$z,		# The "z" contains both the weight and species types
	width = 0.8, 
	height = 0.4, 
	col = col, 
	scale = TRUE
	)

legend('topright', colnames(xyz$z), fill=col, bg='lightblue', inset=0.02)

## There are several biodiversity packages that can be used with ggplot2-like
## interfaces. They can call Google maps when shapefiles or xyz data is scarce.
# rgbif http://www.r-bloggers.com/map-biodiversity-records-with-rgbif-and-ggmap-packages-in-r/

require(rgbif)
Dan_chr <- occurrencelist(
					scientificname = 'Danaus chrysippus',
                    coordinatestatus = TRUE,
                    maxresults = 1000,
                    latlongdf = TRUE,
 					removeZeros = TRUE
					)
				
library(ggmap)
library(ggplot2)

wmap1 <- qmap('India',zoom=2)

wmap1 + geom_jitter(
				data = Dan_chr,
                aes(
					decimalLongitude,
 					decimalLatitude
					),
                alpha=0.6,			# This is the transparency level
				size = 4, 
				color = "red"
				) +
                    ggtitle("Danaus chrysippus")

# We can use some of ggplot2's features to add more summary visuals to
# the map...
wmap1 + stat_density2d(
				aes(
					x = decimalLongitude, 
					y = decimalLatitude,
                    fill = ..level.., 
					alpha = ..level..
					),
				size = 4, 
				bins = 6,
				data = Dan_chr, 
				geom = 'line'
				) +
		geom_jitter(
				data = Dan_chr,
				aes(
					decimalLongitude,
 					decimalLatitude
					),
				alpha=0.6,
 				size = 4,
 				color = "red"
				) 



#=============================================================================
# Contour and Perspective Relief plots
#=============================================================================
#http://gallery.r-enthusiasts.com/graph/contour_plot_%3Cbr%3EMaunga_Whau_Volcano_22

data(volcano)
volcano

# image() is a handy function to summarise a matrix visually
image(volcano)

# Well, that looks like a volcano, but we can do better...

# image() can process x,y,z coordinates, let's create an x and y
# that boost the volcano data's dimesions for smoother plotting
# x.at and y.at will be used for the axes later...

x <- 10 * (1:nrow(volcano))
x.at <- seq(100, 800, by = 100)

y <- 10 * (1:ncol(volcano))
y.at <- seq(100, 600, by = 100)

# Using Terrain Colors and the volcano data as "z"

image(x, y, volcano, col = terrain.colors(100), axes = FALSE)

# We can then add some contours with contour()
contour(
	x,
 	y,
 	volcano,
 	levels=seq(90, 200, by=5), # define how many steps the contours have
 	add=TRUE,
	col="brown"
	)

# Now we can give the axes more sensible intervals...
axis(1, at = x.at)
axis(2, at = y.at)

title(
	main = "Maunga Whau Volcano",
 	sub = "col=terrain.colors(100)",
 	font.main = 4
	)

# Well that's satisfying, but can we go 3D?
# http://gallery.r-enthusiasts.com/graph/Perspective_plot.%3Cbr%3ESimple_DEM_model_24

z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)

# Flesh out and equalise the margins
par(mar=rep(.5,4))

# The perspective function
persp(
	x,
 	y,
 	z,
 	theta = 120, # theta and phi determine the perspective angle
 	phi = 15,
 	scale = FALSE,
 	axes = FALSE
	)

# We can combine the best of both worlds. There's a lot of data manipulation
# here to get the volcano data in the right 'shape' and getting the colours
# right. The actual plotting is quite simple (just using persp)

# Reset the data

z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)


# here we just add some buffer room for the plot...
z0 <- min(z) - 20
z <- rbind(z0, cbind(z0, z, z0), z0)
x <- c(min(x) - 1e-10, x, max(x) + 1e-10)
y <- c(min(y) - 1e-10, y, max(y) + 1e-10)

# Now we define the colours of the fill. We 
fill <- matrix("green3", nr = nrow(z)-1, nc = ncol(z)-1)

# We also set some predefined rows and columns to grey
fill[ , i2 <- c(1,ncol(fill))] <- "gray"
fill[i1 <- c(1,nrow(fill)), ] <- "gray"

fcol <- fill
zi <-	volcano[ -1,-1] +
		volcano[ -1,-61] +
		volcano[-87,-1] + 
		volcano[-87,-61]

fcol[-i1,-i2] <- terrain.colors(20)[
									cut(zi,
 										quantile(zi, seq(0,1, len = 21)
										),
									include.lowest = TRUE)
									]
par(mar=rep(.5,4))
persp(x, y, 2*z, theta = 110, phi = 40, col = fcol, scale = FALSE,
      ltheta = -120, shade = 0.4, border = NA, box = FALSE)


## Heatmaps

# heatmaps require matrices to work
car.mat <- as.matrix(mtcars)

heatmap(car.mat, margins = c(5, 5))

#Why's that so unexciting? 
heatmap(car.mat, scale = c("column"))

# One dendrogram or both dendrograms can be omitted to control the reordering
heatmap(car.mat, Colv = NA, scale = c("column"), margins = c(5, 5))
heatmap(car.mat, Rowv = NA,  margins = c(5, 5))
heatmap(car.mat, Colv = NA, Rowv = NA, scale = c("column"),  margins = c(5, 5))

# gplots offers an "enhanced" heatmap

require(gplots)
par(mar = rep(10, 4))
heatmap.2(
	car.mat,
 	scale = c("column"),
	margins = c(5, 10)
	)

