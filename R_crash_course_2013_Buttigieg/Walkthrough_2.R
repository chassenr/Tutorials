#=============================================================================
# Generating standard graphics
#
# This walkthrough is more of a demonstration of basic graphing functions.
# If you see a function that is relevant to you, find out more about it
# using both R's help files and online - it's likely people have more
# interesting ways of using and customising these plots which they have
# shared. 
#
#=============================================================================

#### Smoothed scatter ####
# We've already seen a scatter plot, but what if we have lots and lots of
# data points? The function smoothScatter() is useful here. This example
# is from ?smoothScatter

# First, we create a biggish data set
n <- 10000
x1  <- matrix(rnorm(n), ncol = 2) # make a matrix with a 10k point
								  # normal distribution
x2  <- matrix(
	rnorm(n, mean = 3, 	sd = 1.5),
	ncol = 2
	)							# Do the same, but define the mean and sd of
								# the distribution.
								
x   <- rbind(x1, x2)	# Stick the rows of x1 and x2 together

# let's take a look at x
dim(x)
head(x)
tail(x)

par(mfrow = c(2, 2)) # prep a 4 panel device

# smoothScatter() has a bunch of arguments, for customisation, you can
# explore them using the help file.

smoothScatter(
	x,	# This is our two column matrix, the first column is "x" and the second
 		# is "y"
 	nrpoints = 0 # here we tell the function not to plot any points. You can
				 # set this to an integer value (e.g. 100) and regions where
				 # the point density is low enough will have points in them.
				 # If you want all points plotted, set this to "Inf"
	)


smoothScatter(x) # this uses all the defaults

# Let's use a different color scheme:
# colorRampPalette() will create a smooth colour range (a "ramp") which can
# used with some parameters like "colramp ="

Lab.palette <- colorRampPalette(
	c("blue", "orange", "red"),		# Low = blue, mid = orange, high = red
 	space = "Lab"					# The colour space to use, handy if
									# journals are picky.
	)

smoothScatter(x, colramp = Lab.palette) # use our new palette

# R has many ways to do similar stuff, for example:
# You can use plot() in combination with densCols() to create a smooth scatter
# too, but this can be less efficient than smoothScatter.

plot(
	x,
 	col = densCols(x),	# densCols produces a vector containing colours which
 						# encode the local densities at each point in a 
						# scatterplot. 
 	pch = 20
	)


#### 3D Scatter plots ####

#http://gallery.r-enthusiasts.com/graph/Scatter_plot_3D_44
# More examples in ?scatterplot3d
# If you don't have the package, uncomment the line below this one:
#install.packages("scatterplot3d")

require(scatterplot3d)
data(trees)
par(mfrow = c(1,1))

s3d <- scatterplot3d(
					trees,				
 					type = "h",			# Type of scatter
 					highlight.3d = TRUE,
                    angle = 55,
 					scale.y = 0.7,
 					pch = 16,
 					main = "scatterplot3d - 5"
					)
        
# We add points to the "scatterplot3d" object like so:

s3d$points3d(
	seq(10,20,2),		# x coordinates (Just a sequence of numbers)
 	seq(85,60,-5),		# y coordinates (Just a sequence of numbers)
 	seq(60,10,-10),		# z coordinates (Just a sequence of numbers)
	col = "blue", 
	type = "h", 
	pch = 16
	)        

# We could do the same with a regression plane.

my.lm <- lm(Volume ~ Girth + Height, data = trees) # make a linear model
s3d$plane3d(my.lm)	# Add the linear model to the $plane3d component of our
					# s3d object. You can find out more about scatterplot3d
 					# objects to find out what more can be done to these 
					# objects.



#### Pair plots ####

# Pair plots are useful if you want to look at a range of variables in a
# scatter-like way

par(mfrow = c(1, 1)) # set up a 1 plot device

# create some data: here a normal distribution has been split between 4
# rows of a matrix and another Gaussian has been added to each column.
# This will create some correlated variables

y <- matrix(
		rnorm(40000),
 		ncol = 4
		) + 3 * rnorm(10000)

# The following command inverts the values second and fourth column of 
# the matrix on the number line.
y[, c(2,4)] <-  -y[, c(2,4)]

# Now we can use pairs() to plot each variable (matrix column) against one
# another

pairs(y)

# Wouldn't it be nice to use smoothScatter IN pairs? Well, guess what...
# The "panel" argument lets you define what kind of plot should be added
# to each panel of the pair plot. You can even have different plots in the
# upper and lower panels (upper.panel, and lower.panel)

#pairs(y, lower.panel = barplot, upper.panel = points)

pairs(
	y,
	panel = function(...) smoothScatter(..., nrpoints = 0, add = TRUE)
	)

# We did something special here, we defined a function IN an argument using
# function(). The elipses (...) mean that arguments will be "passed" to
# smoothScatter from "above". That is, "y" will be passed to smoothScatter.
# The "add = TRUE" argument prevents smoothScatter() from trying to create
# a new plot and, instead, will plot in the existing device.

# Combining what we saw above...
pairs(
	y,
 	upper.panel = function(...) smoothScatter(
								..., 
								nrpoints = 0, 
								add = TRUE, 
								coloramp = Lab.palette
								),
 	lower.panel = points
	)


#### HISTOGRAMS ####

# Histograms can be created using the hist() function. For exaple,
# Let's look at the distribution of the miles per gallon from our mtcars data

hist(mtcars$mpg)

# You can customise the number of intervals or set an algorithm to compute
# them. Here, let's use 10 rather than 5 intervals.

hist(
	mtcars$mpg,
	breaks = 10,
	)

# You'll notice the x-axis doesn't cover the whole range. Actually, the max
# mpg value is "33.9". so it makes sense the axis doesn't go all the way,
# but we'd like a cosmetic enhancement for the x-axis to go to the end of the
# last bar. We know how to fix this. Remember:

# Suppress x-axis

hist(
	mtcars$mpg,
	breaks = 10,
	xaxt = "n"
	)

# Add our custom axis. # First just add the axis line
axis(
	1,
 	at = c(10,40),
 	labels = c("",""),
 	lwd.ticks = 0 # No tick marks
	)  

# then the ticks where you need them lwd = 0 prevents another line being drawn
axis(
	1,
 	at = seq(10 , 40, by = 5),
 	lwd = 0, # No axis line...
 	lwd.ticks = 1 # ... but tickmarks will be plotted
	)	

# Moving on...
# Rather than counts, we could look at a probability density distribution
hist(
	mtcars$mpg,
	prob = TRUE
	)

# You can also customise how the intervals of the histogram work. By default,
# these are right-closed (left open) intervals. Let's compare them:

par(mfrow=c(2,1))

hist(
	mtcars$mpg,
	right = TRUE
	)

hist(
	mtcars$mpg,
	right	= FALSE
	)

# We can use some low-level skills we've learnt earlier to enhance the plot

par(mfrow = c(1,1))
hist(
	mtcars$mpg,
	breaks = 10,
	prob = TRUE,
	las = 1,
	col = "cornflowerblue",
	main = "Miles per Gallon distribution",
	xlab = "Miles per gallon"
	)

lines(
	density(mtcars$mpg), # density() makes mpg values compatible with probability
						 # density values used in the histogram above
	col = "red"
	)

#### BARPLOTS ####

# barplot() minimally needs a vector of heights

bar.heights <- c(1,3,2,4,1)
barplot(bar.heights)

# All the usual modifications apply...
bar.cols <- c("cornsilk",3,2,4,5)
bar.cats <- c("one", "two", "three", "four", "five")
barplot(
	bar.heights, 
	col = bar.cols,
	legend = bar.cats	# notice you can call legend right in the barplot args
						# this is useful as it "knows" what the colours etc
						# "should" be from the other args.
						# it may be inconveniently placed and may need manual
						# placing as we've seen before.
	)

# Modified from example(barplot)
# We're going to use some mortality data from Virginia that's already in R 

head(VADeaths)

# first let's set some custom margins for more plot room
par(mar=c(2, 3.1, 2, 2.1))

# As a high-level function, barplot() will try to find the "best" form of
# barplot to suit the data it receives. For example...
barplot(VADeaths, border = "white")

# This stacked format is not very useful without a legend... 
# Let's add some shading while we're at it...
barplot(
	VADeaths,
 	angle = 15+10*1:5,	# This defines the shading angle. The math
						# just creates five numbers: 25, 35, ... 65
						# that are used as shading angles
						# you could do this with seq() too
 	density = 20,	# How dense should the shading be?
 	col = "black",
 	legend = rownames(VADeaths)
	)

# We can low-level the approach to do some cool labelling...

# Here, we create an object as a "side effect" of plotting.
# The midpoints are the middle of the bars.
midpts <- barplot(
	VADeaths, 
	col = grey(0.5 + 1:5/12),	# use the grey() function to produce a list of
 								# grey colours defined by the math in the 
								# parentheses
	names=rep("", 4)
	)

midpts

# We can use the midpoints object to tell R where to add bar labels in the 
# margins...			
mtext(
	sub(" ", "\n", colnames(VADeaths)), # substitute blanks with new lines
										# in the names of the VADeaths columns
										# This "stacks" the words in a label
	at = midpts,	# Use the midpoints to position the labels
 	side = 1,		# target the x-axis
 	line = 0.5,
 	cex = 0.8
	)

# Now we think numerically to add text to the middle of each range...
text(
	rep(midpts, each = 5),	# Each stacked bar has five segments, so we need
 							# five labels per bar. Thus we repeat the midpt
							# coordinates five times. Remember, this is the
							# "x =" argument of text()
 	
	  # Now we need "y =" coordinates, these should be in the middle of
	  # each segment. We apply the cummulative sum [cumsum()] function
	  # to the columns (MARGIN = 2) of our data set and subtract half of
	  # the original value to get to the centre of each bar...
	
	apply(VADeaths, 2, cumsum) - VADeaths/2,
	VADeaths, # This is the "labels" argument 
	
	# Now we can choose an appropriate colour for each label
	col = rep(
			c("white", "black"),
 			times=2:3,
 			cex=0.8
			)
	)

par(mar=c(5.1, 4.1, 4.1, 2.1))	# Here we restore the default margins. Closing
							    # the device window often does the same.	

# We can use barplot()'s arguments to customise the kind of plotting
# if we're not happy with barplot()'s default choice...

barplot(VADeaths, beside = TRUE, legend = row.names(VADeaths))

# And use our low-level skills to pretty it up


barplot(
	VADeaths,
 	beside = TRUE,
    col = c("lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk"),
    legend = rownames(VADeaths),
 	ylim = c(0, 100)
	)

title(main = "Death Rates in Virginia", font.main = 4)

# What about error bars? Let's make a simple data set
# From: http://strata.uga.edu/6370/rtips/barPlotErrorbars.html

means <- c(23, 28, 19)
names <- c("squirrel", "rabbit", "chipmunk")

# We create a vector with the standard errors that may come from some
# statistical test. Here, they're just made up...
standard.errors <- c(1.2, 1.7, 0.9)

# Now we find out the highest point in the bar plot, including the error bars
# We'll need this to define how high the y-axis will go
plot.top <- max(means + standard.errors * 2)

# Prep for plotting
par(mfrow = c(1,2))

# We use the side-effect object assignment we saw before while barplotting...
bar.centers <- barplot(
						means,
 						names.arg = names,
 						col = "gray",
 						las = 1,
						ylim = c(0,plot.top)	# Here we use plot.top to make
 												# sure the device is big enough
						)

# Now we use segments() to add a line using the coordinates of our bar centres
# and the values from our standard errors...
			
segments(
	# The first segment goes from the centre of the bar
	# to the bottom of the CI
	bar.centers,
 	means - standard.errors * 2,
	# The second segment goes from the centre of the bar
	# to the top of the CI
 	bar.centers,
 	means + standard.errors * 2,
 	lwd = 2
	)

# For the more traditional error bars, use arrows()
bar.centers <- barplot(
	means,
 	names.arg = names,
 	col = "gray",
 	las = 1,
 	ylim = c(0,plot.top)
	)

arrows(
	bar.centers,
 	means - standard.errors * 2,
 	bar.centers,
 	means + standard.errors * 2,
 	lwd = 2,
	# angle and code define the 'flat bar' terminus
 	angle = 90,
 	code = 3
	)

# You can make a function to do this automatically...
# from http://monkeysuncle.stanford.edu/?p=485
error.bar <- function(
	x,
 	y,
 	upper,
 	lower = upper,
 	length=0.1,
	...
	){
		if(length(x) != length(y) |
 			length(y) !=length(lower) |
 			length(lower) != length(upper)
			)
				stop("vectors must be same length")
				
		arrows(
			x,
			y + upper,
 			x,
 			y - lower,
 			angle = 90,
 			code = 3,
 			length = length,
 			...
			)
}


# Now let's use that function...
# First plot the barplot...
bar.centers <- barplot(
	means,
 	names.arg = names,
 	col = "gray",
 	las = 1,
 	ylim = c(0,plot.top)
	)

# and now run the function
error.bar(bar.centers, means, standard.errors)

# Adding error bars to scatter plots or any other plot can be done similarly

#### Boxplots ####
# Modified from example(boxplot) - itself from suggestion by Roger Bivand

par(mar=c(5, 4.1, 2, 0))

# We're now going to use some tooth growth data to check out boxplot()
# first, let's check out the data

data(ToothGrowth)
head(ToothGrowth)
tail(ToothGrowth)

# Let's say we're interested in the length of the tooth according to the
# dosage level of the treatment (orange juice or pure Vit C)
summary(ToothGrowth$len)
summary(ToothGrowth$dose)

# We can boxplot both of these in the same device
boxplot(
	ToothGrowth$len,
	ToothGrowth$dose
	)

# This is not really informative, what I really want is length as a function
# of the dose. Remember that R has a formula syntax which can give you this

boxplot(
	len ~ dose, # Length as a function of dose
	data = ToothGrowth
	)

# That's rather nice, I get one boxplot for each "level" of dose.
# It looks like the higher the dose, the greather the length.
# If you had # a continous numeric variable, you could use cut() to make
# 'clean' levels.

# Can there be a difference between orange juice and pure Vit C?
# Now let's compare two data series from the same data set. We're going to
# move the boxes over a little, to make room for the second series.
boxplot(
	len ~ dose, 
	data = ToothGrowth,
    boxwex = 0.25,		# Here, the box width expansion has been shrunk to 1/4
	at = 1:3 - 0.2,		# Using at, boxplot is told to place the boxes at each
						# of the three levels, but a little bit (0.2 units) to
						# the left.
    subset = supp == "VC", # Using subset, boxplot will only plot the VC values
 	col="yellow",
    xlab="",
    ylab="tooth length",
 	ylim=c(0,35)
	)

# Add the x-axis label in the margin
mtext("Vitamin C dose (mg)", side=1, line=2.5, cex=0.8)

# We can then add to this plot The orange juice data...
boxplot(
	len ~ dose,
 	data = ToothGrowth,
 	add = TRUE,			# This prevents a new device being started
    boxwex = 0.25, 
	at = 1:3 + 0.2,		# This will be plotted to the right of the dose tick
    subset= supp == "OJ", 
	col="orange"
	)

# Add a legend
legend(
	1.5, # we don't have to define which arg we're using if it's in the order
		 # expected by the function. Here, legend was expecting x = 
 	9,	 # and here it was expected y=	
 	c("Ascorbic acid", "Orange juice"),
 	bty="n",	# Now we start defining because we're not necessarily defining
				# the args in order.
    fill = c("yellow", "orange")
	)

par(mar=c(5.1, 4.1, 4.1, 2.1))

#### Piechart ####
# Example 4 from help(pie)

# Set some parameters
par(mar=c(0, 2, 1, 2), xpd=FALSE, cex=0.5)

# Create a percentage/proportion vector
pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
sum(pie.sales)

# name the vector values
names(pie.sales) <- c("Blueberry", "Cherry", "Apple", "Boston Cream", "Other",
 					"Vanilla")
				
# Plot a pie chart and use rainbow() to autogenerate some colours...
pie(pie.sales, col = rainbow(6))

#### Stripchart ####

# This is an alternative to a boxplot when sample sizes are small.
# Let's look at the OrchardSprays data set example from ?stripchart

stripchart(
	decrease ~ treatment,
	data = OrchardSprays,
    main = "stripchart(OrchardSprays)", 
    vertical = FALSE, 
	log = "x"
	)

#### Conditioning plot ####

# Like in the boxplot example, you often want to 'condition' one variable on
# another as you expect there to be some sort of effect. A "co-plot" is a
# good tool to help you visualise this. We're going to use some data

data(quakes)
dim(quakes)
head(quakes)

# Say we'd like to see the relationship between latitude and longitude, but
# conditioned on ('controlled for', 'given') depth...

coplot(lat ~ long | depth, data = quakes)

# Looks interesting, but what if we're not happy with the standard intervals...
given.depth <- co.intervals(quakes$depth, number = 5, overlap = .1)

# Using the custom intervals defined above, a more coarse-grained summary can
# be plotted...
coplot(lat ~ long | depth, data = quakes, given.v = given.depth, rows = 1)

# What if we wish to condition on two variables?
ll.dm <- lat ~ long | depth * mag	# Here's the formula
coplot(ll.dm, data = quakes)		# and the coplot call

# As always, there's more to explore in ?coplot

#### Mosaic plots ####

data(HairEyeColor)
mosaicplot(HairEyeColor, shade = TRUE)

# ?mosaicplot:
# Mosaicplot graph represents a contingency table, each cell corresponding
# to a piece of the plot, which size is proportional to cell entry. Extended
# mosaic displays show the standardized residuals of a loglinear model of the
# counts by the color and outline of the mosaic's tiles.
# (Standardized residuals are often referred to a standard normal distribution)
# Negative residuals are drawn in shaded of red and with broken outlines; positive
# ones are drawn in blue with solid outlines. Thus, mosaicplot are perfect to 
# visualize associations within a table and to detect cells which create 
# dependancies.

#### Time series plots ####

# R has a special class for time series data. Let's look at some turkey prices.
require(nutshell)
data(turkey.price.ts)
class(turkey.price.ts)
turkey.price.ts

# Fortunately, plot() detects and has special handling of time series objects
plot(turkey.price.ts)

# Looks like there's some autocorrelation to worry about, acf() allows one to
# assess autocorrelation visually...

acf(turkey.price.ts)

# Prices are positively correlated over 12 month cycles (1.0 suggests one
# cycle, in this case 1 year) and inversely correlated over 6 month cycles.

#### Corrgrams ####

# Correlation can be visualised with corrgram(). The content of the 
# upper, lower, and text (diagonal) panels can be customised

require(corrgram)
corrgram(
	mtcars,
 	order = TRUE,
 	lower.panel = panel.shade,
    upper.panel = panel.pie,
 	text.panel = panel.txt,
    main = "Car Milage Data in PC2/PC1 Order"
	)

# Less pretty, but more informative panels can be chosen
corrgram(
	mtcars,
 	order = TRUE,
 	lower.panel = panel.ellipse,
	upper.panel = panel.pts, 
	text.panel = panel.txt,
	diag.panel = panel.minmax, 
 	main = "Car Milage Data in PC2/PC1 Order"
	)

# It's worth checking out the panel.* functions to see what you can do
# If you examine them, you can even write your own.


#=============================================================================
# Exporting graphics
#=============================================================================

# you can export graphics 'on the fly' with the "Save as..." functions or 
# "Copy" functions. Sometimes, however, this won't export graphics in a format
# that a journal will be happy with (DPI too low, etc).

# Here's the simple way. Make sure you know where your working directory is!

tiff(filename = "mpg_vs_wt.tif")

plot(mtcars$mpg, mtcars$wt)

dev.off()

# It's pretty small and low resolution, let's fix that

tiff(
	filename = "mpg_vs_wt.tif",
 	width = 20,
 	height = 20,
  	units = "cm",
	res = 300 # boost the resolution to 300 ppi
	)

plot(mtcars$mpg, mtcars$wt)

dev.off()

# that's better.
# Devices exist for PNGs, JPEGs, BMPs et al.

# This short tour covered a few of R's main plotting routines, there are many
# more, browse R Gallery (http://gallery.r-enthusiasts.com/) for inspiration



#=============================================================================
# Demonstrations (if time permits)
#=============================================================================


#### HISTOGRAMS WITH SCATTER ####
# What if we wanted to fuse histograms with scatter plots?
# http://gallery.r-enthusiasts.com/graph/Scatterplot_with_marginal_histograms_78

def.par <- par(no.readonly = TRUE) # save default, for resetting...

# Create some data
x <- pmin(3, pmax(-3, rnorm(50)))
y <- pmin(3, pmax(-3, rnorm(50)))

# Create some histograms that summarise our data
xhist <- hist(x, breaks=seq(-3,3,0.5), plot=FALSE)
yhist <- hist(y, breaks=seq(-3,3,0.5), plot=FALSE)

# Find the limits and range of the histogram bar height for pretty graphing
top <- max(c(xhist$counts, yhist$counts))
xrange <- c(-3,3)
yrange <- c(-3,3)

# set up a layout to plot our various graph components...
nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
layout.show(nf)

# set the margins of the scatter plot and plot it
par(mar=c(3,3,1,1))
plot(x, y, xlim=xrange, ylim=yrange, xlab="", ylab="")

# set the margins of the x histogram and plot it...
par(mar=c(0,3,1,1))
barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)

# Do the same for the y histogram
par(mar=c(3,0,1,1))
barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)

par(def.par)


## Fourfold plots http://gallery.r-enthusiasts.com/graph/Fourfold_Display_32

# Fourfold display: The fourfold display depicts frequencies by quarter circles,
# whose radius is proportional to `\sqrt{n_{ij}}` , so the area is proportional
# to the cell count . The cell frequencies are usually scaled to equate the
# marginal totals, and so that the ratio of diagonally opposite segments depicts
# the odds ratio. Confidence rings for the observed odd ratio allow a visual
# test of the hypothesis `H_0: \theta=1` corresponding to no association. They
# have the property that the rings for adjacent quadrants overlap iff the 
# observed counts are consistent with the null hypothesis.

data(UCBAdmissions)
x <- aperm(UCBAdmissions, c(2, 1, 3))
dimnames(x)[[2]] <- c("Yes", "No")
names(dimnames(x)) <- c("Sex", "Admit?", "Department")

fourfoldplot(x[,,1:4])



## Forecasting
# http://gallery.r-enthusiasts.com/graph/Times_Series_:_Forecasting_51
# A forecasting plot combining many of the things we've seen. This seems very
# complicated, but really is just built element by element.

require(gplots)

# Data generation BEGIN
set.seed(120)

# simulate an AR(1) process
coefs  <- 0.95
series <- arima.sim(list(ar=coefs),n=250)

# fit AR(1) with the 200 first data
model  <- arima(series[1:200],c(1 ,   # AR part
                                0,    #  I order
			        0))   # MA part

# make forecast from the model
forecast <- predict(model,80)

# Data generation END

# compute the limits of the graph
ylim <- c( min(series[1:200],forecast$pred - 1.96 * forecast$se),
           max(series[1:200],forecast$pred + 1.96 * forecast$se))

# prepare the space where to plot
opar <- par(mar=c(4,4,2,2),las=1)
plot(series,ylim=ylim,type="n",xlim=c(1,250))
usr <- par("usr")

# split the figure in two parts
#   - the part used to fit the model
rect(usr[1],usr[3],201   ,usr[4],border=NA,col="lemonchiffon")

#   - the part used to make the forecast
rect(201   ,usr[3],usr[2],usr[4],border=NA,col="lavender")

abline(h= (-3:3)*2 , col ="gray" , lty =3)


# draw a 95% confidence band
polygon( c(201:280,280:201),
         c(forecast$pred - 1.96*forecast$se,rev(forecast$pred + 1.96*forecast$se)),
	 col = "orange",
	 lty=2,border=NA)

	     
lines( 201:280 , forecast$pred - 1.96*forecast$se , lty=2)
lines( 201:280 , forecast$pred + 1.96*forecast$se , lty=2)



lines( series , lwd=2 )
lines(201:280 , forecast$pred , lwd=2 , col ="white")

smartlegend( x="left", y= "top", inset=0,                             #smartlegend parameters
             legend = c("series","prediction","95% confidence band"), #legend parameters
	     fill=c("black","white","orange"),                        #legend parameters
	     bg = "gray")                                             #legend parameters


box()
par(opar) # reset the par
