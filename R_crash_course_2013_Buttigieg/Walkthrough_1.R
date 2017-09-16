#=============================================================================
# Introduction
#
# Most of the material here has been adapted from the help files associated 
# with each function used or is circulating on the internet. It's been commented
# to help "narrate" the code. There are many good introductions to graphics
# in R which are worth reading, for example: 
# https://www.stat.auckland.ac.nz/~paul/RGraphics/
#
#=============================================================================


# Before we begin, you may want to install the fortunes package

install.packages("fortunes")

# This will provide motivation, for example:
#
# Evelyn Hall: I would like to know how (if) I can extract some of
# the information from the summary of my nlme.
#
# Simon Blomberg: This is R. There is no if. Only how.
#  -- Evelyn Hall and Simon 'Yoda' Blomberg
#      R-help (April 2005)

# and also words of wisdom when interacting with the R community:
fortunes::fortune(14)


#=============================================================================
# R's understanding of graphics
#=============================================================================

# R has a bunch of "high-level" plotting functions that will attempt to 
# visualise data as appropriately as possible. However, R can only work with 
# the information provided to it.

# First, let's get some data that's preloaded into R
data(mtcars)

# As most things, the data is stored as an object in the working environment
# Let's find out a bit about the data

?mtcars

# To plot some data, we can call the very-high-level function plot()
# Let's say we were interested in the mpg vs the weight of the car
# To find out how R is going to "see" these variables, we can use the class()
# function

mtcars$qsec
class(mtcars$mpg)
class(mtcars$wt)

# These will be interpreted as two numeric variables

plot(mtcars$mpg, mtcars$wt)

# plot() detected two vectors of numeric data and defaulted to a scatter plot
# What if our variable was not numeric? The "am" variable is a factor where
# 0 = automatic and 1 = manual transmission, but does R know this?

class(mtcars$am)

# Apparently not. As it thinks "am" is numeric, plot() will probably behave
# as above...
plot(mtcars$am)

# Not a totally useless plot, but probably not the most effective for this kind
# of data. We can tell R that this is a factor rather than a numeric variable
# and then see how plot() behaves...

plot(
	as.factor(mtcars$am),
	)

# plot() now understands that it's dealing with a factor and attempts a more
# effective visualisation.


#=============================================================================
# The elements of an R graphic
#=============================================================================

# Understanding the anatomy of R graphics can help you customise and edit them
# in very flexible ways. R's graphical parameters can be queried and set by 
# the par() function.

par()

# The help file on par() is also very informative, but quite a read.

?par

# We'll get into many of these later, but, for now, we're just going to mess
# with a few to illustrate the main components of an R graphic.

# Before customising or experimenting with parameters, it's a good idea to 
# back up the default parameter list in an object, such as "op", standing for
# "original parameters".

op <- par()

# Now let's start stripping down a standard plot. We'll use a scatter plot
# we generated above as an example.
plot(mtcars$mpg, mtcars$wt)

# The actual plotting region, or plot, is placed in a "device" region. The
# device is the whole window or figure region you're using to plot in.
# To illustrate, resize the plot window you generated above to a rectangle.
# You'll notice the plot itself also becomes rectangular. We can force the plot
# region to be square to help us see that the plot and device area are different.
par(pty = "s")
plot(mtcars$mpg, mtcars$wt)

# Now, resizing the device window will not stretch out the plot region.

# Standard graphics have a background layer. Using par(), we can set the colour
# of the "bg".
par(bg = "blue")
plot(mtcars$mpg, mtcars$wt)

# If there's a background, there's probably a foreground...
par(fg = "white")
plot(mtcars$mpg, mtcars$wt)

# We see that some, but not all of the elements are included in the foreground
# There are other parameters which can be used to access and manipulate them.
# We'll see these when dealing with low-level interactions, for now, just note
# that these are different elements.

# Graphics generally have annotations like titles and axis labels. Let's use
# par to disable the "ann"otations.
par(ann = FALSE)
plot(mtcars$mpg, mtcars$wt)

# Generally, graphics have boxes drawn around the plot area. We can mess with
# this using the "box type" or "bty" parameter...
par(bty = "n")
plot(mtcars$mpg, mtcars$wt)

# By removing the box, we can now see that the axes are different entities too.
# 

# Each axis may be addressed separately by, e.g., "[x,y]axt" or 
# "[x,y] axis type"
par(xaxt = "n")
plot(mtcars$mpg, mtcars$wt)

par(yaxt = "n")
plot(mtcars$mpg, mtcars$wt)

# The margins of the plot area are also a separate entity and can be adjusted
# in a number of ways. Let's add a box to the plot to make this clearer

par(bty = "o")

par(
	mar = c(
			17, # Bottom
			7, # Left
			2, # Top
			2  # Right
			)
	)
		
plot(mtcars$mpg, mtcars$wt)		

# Finally, the points, bars, lines, etc that are in the plotting area can be 
# modified. In our scatterplot, we have points so let's use an appropriate 
# parameter to modify them
par(pch = "m")
plot(mtcars$mpg, mtcars$wt)

# Finally, you may add multiple plots to a device
par(mfrow = c(2,2)) # A 2 x 2 plot array

# Let's fill the array with four identical plots, with all the manipulations
# we've performed on the parameters
plot(mtcars$mpg, mtcars$wt)
plot(as.factor(mtcars$am))
plot(mtcars$mpg, mtcars$wt)
plot(mtcars$mpg, mtcars$wt)

# There are more subtleties to the anatomy of R graphics, but these should give
# you an idea of how to work with them. In the next section, we'll go through
# some low-level interactions which will allow you to customise your graphics.

# Before we end, let's restore the orignal parameter set
par(op)

# You'll get a few warnings here, saying that some parameters could not be set.
# This is expected, as some, likc "cin" and "cra", are read-only. You should 
# also close the device window to refresh the parameter list completely before
# creating a new plot...

plot(mtcars$mpg, mtcars$wt)

# Let's use plot() to show us the device regions. This code is a little
# involved, so feel free to run it 'blindly' if you don't wish to delve into
# it. From:
# http://www.stat.auckland.ac.nz/~paul/RGraphics/custombase-baseregionsnfig.R

par(oma = rep(3, 4), mfrow = c(3,2), bg = "grey80")

for (i in 1:6) {
    if (i == 3) {
		omar <- par(mar = c(2, 2, 2, 1))  
		plot(
			c(0, 1),
 			c(0, 1),
 			type = "n",
 			ann = FALSE,
 			axes = FALSE
			)
		par(xpd = TRUE)
		rect(-1, -1, 2, 2, col = "grey90")
		box("figure")
		par(xpd = FALSE)
		rect(-1, -1, 2, 2, col = "grey80")
		box("plot", lty = "dashed")
		text(.5, .5, "Current Plot Region", cex = 1.5)
		mtext("Current Figure Region", side = 3)
		par(omar)
		
    } else {
		omar <- par(mar = rep(0, 4))  
		plot(
			c(0, 1),
 			c(0, 1),
 			type = "n",
 			ann = FALSE,
 			axes = FALSE
			)
		par(xpd = TRUE)
		rect(-1, -1, 2, 2, col = "grey90")
		box("figure")
		text(.5, .5, paste("Figure", i), cex = 1.5)
		par(omar)
		}
}

box("outer", col="grey")

for (i in 1:4){
    mtext(
		paste("Outer margin", i),
 		side = i,
 		line = 1,
 		outer = TRUE
		)
}


#=============================================================================
# Low-level interaction with graphics
#=============================================================================

# As we've seen with par() there are many ways we can customise R graphics
# after or while they're being processed by a function like plot(). This sort 
# of activity is referred to as "low-level" interaction.

# If you haven't already done so, bring up our mpg vs wt scatterplot
plot(mtcars$mpg, mtcars$wt)

# Rather than running par() all the time, we can also set parameters in a call
# to plot() or many other graphing functions. In R lingo, the arguments will be
# "passed" on to par() from plot() to save some typing.

# Let's try something familiar...

plot(
	mtcars$mpg, 
	mtcars$wt,
	pch = 6
	)

# Now let's use lines and points, re-label the axes, and add a title and 
# subtitle.

plot(
	mtcars$mpg, 
	mtcars$wt,
	pch = 6,
	main = "Weight vs fuel consumption",
	sub = "Heavy car, light wallet",
	xlab = "Miles per gallon",
	ylab = "Weight in thousands of lbs",
	col = "red"
	)

# Many low-level manipulations can be done directly in a plotting function.
# However, when things start getting complicated, it's sometimes better to
# gradually build your plot. This gives you more control over the end result
# and prevents the code for one function becoming too complicated.

# Let's explore this alernative. First, we'd want to create a relatively
# empty plot in our device...

plot(
	mtcars$mpg, 
	mtcars$wt,
	type = "n",		# we supress plotting of symbols 
	ann = FALSE,	# and prevent any annotation of labels or titles
	bty = "n",		# no box
	axes = FALSE	# no axes, individual axes can be disabled, see below:
	#xaxt = "n",		# no x-axis
	#yaxt = "n",		# no y-axis
	)

# That's certainly relatively empty. Now we can add the elements interactively,
# in a low-level approach. Keep in mind, you don't need to write code defining
# the various parameters if you're already happy with the defaults. If you know
# you like a certain kind of plot element, make it a default using the par()
# function as described above. you can save your preferences as an object.

# Titles for the plot, the axes, and other elements can be added with title()
title(
	main = "Weight vs fuel consumption",
	sub = "Heavy car, light wallet",
	xlab = "Miles per gallon",
	ylab = "Weight in thousands of lbs",
	# you can set the fonts of the various labels too
	font.main = 11,
	font.sub = 12,
	font.lab = 3
	)

axis(
	side = 1, # 1=below, 2=left, 3=above and 4=right
	lty = "dotdash", # the type of line
	lwd = 3, # the width of the line
	lwd.ticks = 4,
	col = "blue",
	col.ticks = "darkblue",
	las = 3, # label style: 0=parallel, 1=horizontal, 2=perpendicular, 
			 # 3=vertical
	font = 15
	)


axis(
	side = 2, # 1=below, 2=left, 3=above and 4=right
	lty = "longdash", # the type of line
	lwd = 3, # the width of the line
	lwd.ticks = 4,
	col = "red",
	col.ticks = "darkred",
	las = 2
	)

# We can add another axis with different tick marks using "at"
# This can be used to provide an axis for a different data series plotted
# on the same graph as long as the coordinates are made compatible.

axis(
	side = 3, # 1=below, 2=left, 3=above and 4=right
	at = c(12.5, 17.5, 22.5, 27.5),
	lty = "dashed", # the type of line
	lwd = 3, # the width of the line
	lwd.ticks = 4,
	col = "green",
	col.ticks = "darkgreen",
	las = 0, # label style: 0=parallel, 1=horizontal, 2=perpendicular, 
			 # 3=vertical
	font = 15
	)

# We can add our points to the plot with, surprise surprise, the points()
# function. There are other functions for different kinds of plots like
# lines() and segments(). 

points(
	mtcars$mpg, # X and ...
	mtcars$wt,	# ... Y values, must match initial plot call
	pch = mtcars$cyl + 17,	# Choose a plotting character according
							# to the number of cylinders
							# See ?points to see different symbol types for pch
							# The "+ 17" is just to convert the cylinder number
							# (4, 6, 8) into numbers that correspond to filled
							# plotting characters (21, 23, 25)
	cex = (mtcars$hp/max(mtcars$hp)) * 6,	
									# Character expansion: the pch will be 
									# scaled according
									# to horsepower.
	lwd = 1,	# How thick should the pch's border be?
	col = "cornflowerblue", # what colour should the pch border be?
	bg = mtcars$gear		# fill colours match number of forward gears
	)


# Once the points are there, we can interactively find out which object
# (i.e. row) they correspond to using the identify() function. Press
# Esc when you're done identifying interesting points.

identify(mtcars$mpg, mtcars$wt, labels = row.names(mtcars), pos = 4)

# Object 17 looks pretty interesting. It's a heavy car that's relatively
# fuel efficient. Let's celebrate this fact with text()

# First we need its coordinates
mtcars[17, c("mpg","wt")]

# It's the Chrysler Imperial with and mpg of 14.7 and wt of 5.345 klbs


text(
	mtcars[17, c("mpg","wt")],	# coordinates of where to put text
								# These can be individual or a whole column
	labels = row.names(mtcars)[17], # What text should go there?
	pos = 4, # position the test to the right of the coords 
			 # 1=below 2=left 3=top 4=right
			
	offset = 2, # Give it more of a nudge to the right
	cex = 1.4,	# Slightly bigger characters
	col = "forestgreen"
	)
	
# we can even add some math text to the plot using expression()
text(
	mtcars[25, c("mpg","wt")],
 	expression(hat(beta) == (X^t * X)^{-1} * X^t * y),
	pos = 4,
	offset = 2,
	cex = 1.4,
	col = "orange"
	)

# you can add text to the margins using mtext()	
mtext(
	"«Latin-1 accented chars»: éè øØ å<Å æ<Æ", 
	side = 4, # Right side
	line = 0  # initial line of the margin
	)

mtext(
	"«Latin-1 accented chars»: éè øØ å<Å æ<Æ", 
	side = 3, # Top side
	line = 3, # "third" line of the margin
	adj = 1,  # Right alignment in reading direction
	cex = 1.4,  # bigger characters
	col = "green"
	)

# Legends can be plotted using legend() or smartlegend() from the gplots package
# Let's leave our scatter plot to illustrate how to create a legend. This example
# is from the ?legend material and will put a bunch of skills you've seen to use

# First we define some legend text
leg.txt <- c("Setosa Petals", "Setosa Sepals", "Versicolor Petals", "Versicolor Sepals")

# We're going to plot 5 legends at the same x coordinate, but different
# y coordinates, so we'll define a vector of y coords
y.leg <- c(4.5, 3, 2.1, 1.4, .7)

# We also want different character expansion factors to be used for each legend
cexv  <- c(1.2, 1, 4/5, 2/3, 1/2)

# We use matplot() or "matrix plot" to lay down a basic plot space
matplot(
	c(1, 8),	# range of X
 	c(0, 4.5),	# range of Y
 	type = "n",
 	xlab = "Length",
 	ylab = "Width",
	main = "Petal and Sepal Dimensions in Iris Blossoms"
	)

# We'll use a for loop to automate the plotting. If this is too complicated for
# now, see the legend() command below the for loop for a simper example. The
# for loop is contained within braces "{}"
for (i in seq(cexv)){
	# First, we'll paste some text on the graph to describe the cex of the
	# legends
	text(
		1, # X coordinate
		y.leg[i] - 0.1, # Y coords in our y.leg vector minus a small correction
 		paste(			# paste in labels
			"cex=",
 			cexv[i]		# loop through and paste in different cex values
			),
 		cex = 0.8,		# set a constant cex for our labels
 		adj = 0			# text right at coords defined
		)
	
	# Now we use legend() to plot a bunch of legends
    legend(
		x = 3,				# Fixed X coord of top-left corner of the legend
 		y = y.leg[i],		# loop through our y.leg coords to define corner
 		legend = leg.txt,	# define our legend text, each string will 
							# be printed on a new line
 		pch = "sSvV",		# We can define a symbol for each line, here
							# we used real characters rather than numeric codes
							# notice that each character will be used 
							# sequentially
 		col = c(1, 3),		# Two colours will be used. As there are four lines
							# this vector will be recycled. In this context, 
							# this makes sense.
 		cex = cexv[i]		# The character expansion to be used for each 
							# legend
		)
}

# Let's plot an individual legend a little differently...
legend(
	"topright",		# Instead of coordinates, legend can accept positions
					# that reference the plot area
	legend = leg.txt,
	col = c(1,3),
	fill = c(1,3),	# Instead of text symbols, we use "fill" to define colour
					# coded rectangles for the plot
	cex = 1
	)
	


# Mathematical symbols and functions can be added to the plot area using
# functions like expression(). We can look at an example from the ?title help.
# See ?plotmath for more information on how to add mathematical symbols to R 
# graphics.

# First, create some data to plot a sine and cosine function across a range
x <- seq(-4, 4, len = 101)
y <- cbind(sin(x), cos(x)) # The cbind function allows use to create a matrix

# Now we'll call the function matplot() or "matrix plot" that will allow us to
# plot one column of a matrix against another...

matplot(
	x,
 	y, 
	type = "l", # lines will be used instead of points
				# matplot will be smart about differentiating between the two
				# columns.
	xaxt = "n", # no x axis will be plotted
	main = expression(			# We're building the main title
		paste(					# paste can be used to "build" a label.
			plain(sin) * phi,	# plain() just returns normal text rather
								# than an expression
 			"  and  ",			# the quotes allow literal text to be entered
			plain(cos) * phi)
		),
    ylab = expression("sin" * phi),
    xlab = expression(paste("Phase Angle ", phi)),	# paste is needed here
													# otherwise expression()
													# will ignore anything
													# after the comma
    col.main = "blue" # A blue title would be nice
	)

# Now we can add the x-axis labels. As long as the values in "at" line up with
# the values in your x variable, there should be issues...
axis(
	1, # defines the side. 1=below, 2=left, 3=above and 4=right.
 	at = c(-pi, -pi/2, 0, pi/2, pi), # at defines where tickmarks should be
    labels = expression(
		-pi, 
		-pi/2, 
		0, 
		pi/2, 
		pi
		) 
	)

# We can some simple straight lines to the plot
abline(
	h = 0, # we want a horizontal line at a y-value of 0
 	v = pi/2 * c(-1,1), # and vertical lines at pi/2 on either side of 0
 	lty = 2, # Dashed lines would be nice
 	lwd = .1, # skinny too
 	col = "gray70" # and a light shade of grey
	)


# There's a lot more you can do to customise your plots, but this tutorial
# have given you an introduction to working with base graphics in R.
# See the help files associated with par() and high-level plotting functions
# to go deeper. There are also online tutorials and entire books dedicated to
# graphics in R.