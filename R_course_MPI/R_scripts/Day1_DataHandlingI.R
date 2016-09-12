#########################################################
# R course: Day 1 (Monday, 12.09.2016)                  #
#########################################################

### R data and object types ####
# also have a look at: http://www.statmethods.net/input/datatypes.html

# we will make up some data to look at common data and object types in R

# vectors
x <- c(1:10) # integer
y <- round(jitter(x), 2) # numeric
a <- rep(c("A", "B"), 5) # character
b <- as.factor(a) # factor
c <- a == "A" # logical

# data type conversions
# factor to numeric
n <- as.numeric(b)
as.numeric(as.factor(a))
d <- rep(c("1", "B"), 5) # character
as.numeric(d)

# numeric to factor
m <- as.factor(x)

# factor to character
as.character(b)

# factor with numeric levels to numeric
m <- as.factor(c(1,1,3,7,3,8,9))
as.numeric(m)
as.numeric(as.character(m))


# matrices: "vectors with line breaks"
# same data type
X <- matrix(
  data = 1:50, # vector is input for matrix command
  nrow = 10, # how many rows
  ncol = 5, # how many column
  byrow = F # fill by rows  (TRUE) or fill by column
)
X
X1 <- matrix(
  data = 1:50,
  nrow = 10, 
  ncol = 5, 
  byrow = T
)
X1

A <- matrix(
  rep(c("A","B"), 25), 
  nrow = 10,
  ncol = 5, 
  byrow = F
)
A
A1 <- matrix(
  rep(c("A","B"), 25), 
  nrow = 10, 
  ncol = 5, 
  byrow = T
)
A1

# convert to vector
c(X)


# data frames: concatonated vectors
# different data types
DF <- data.frame(x, y, a, b, c)
DF

# what happens when we transpose a data.frame consisting of different data types?
tDF <- t(DF)
tDF
str(tDF)
# result is a character matrix, since difference data types in one column are not allowed
# everything is converted to character


# lists: vectors consisting of R objects, including other lists
L <- list(x, y, a, b, c, X, A, DF)


### R object attributes ####
# names, column names, row names
# need to be unique identifiers

# vectors and lists
names(x) <- c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10")
str(x)
# remove names
names(x) <- NULL

# name elements of a list
names(L) <- c("int", "num", "chr", "fac", "log", "mat.int", "mat.chr", "df")

# matrices and data frames
rownames(X) <- c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10")
colnames(X) <- c("V1", "V2", "V3", "V4", "V5")

rownames(DF) <- c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10")
colnames(DF) # already exist, based on names of input vector

# avoid starting row and column names with numbers!


#### navigating R objects ####
# square brackets: [rows, columns] or if only one dimension [position]

# vectors
# select 3rd element in vector
x[3]
# select 3rd to 5th elements in vector
x[c3:5]
# select 2st, 3rd, 7th element in vector
x[c(2, 3, 7)]
# select everything except 3rd element
x[-3]
x[-c(3:5)]
# selection based on attributes
x[c("S3", "S5")]

# matrices and data frames
# select first 3 rows
X[1:3, ]
# select first 3 column
X[, 1:3]
# select second element in 3rd row
X.sub <- X[3, 2]
# selection based on attributes
X["S1", c("V3", "V5")]
DF["S1", c("x", "c")]
DF[, colnames(DF) %in% c("c", "a", "x")]
# use attributes to select and sort at the same time
DF[, c("c", "a", "x")]

# using logical operators
DF[DF$y > 3, ]
# same as line143, selection of column y with []
DF[DF[, "y"] > 3, ]
# just y larger than 3
DF$y[DF$y > 3]

# match of character
DF[DF$a == "A", ]

# other logical operators are:
# != does not equal
# >= more or equal than
# | or
# & and
DF[DF$y > 3 | DF$a == "A", ]

# use dollar '$' to extract columns in data frame by column name
DF$x

# use logical variable to subset data frame
DF[DF$c, ]

# navigating lists
# return a subset of the list: single square brackets
# the output will still be a list
L.sub <- L[1]
str(L[1])
# return an element of a list: '$' or double square brackets
# the output will have the object and data type of the element of the list
L.int <- L$int
str(L$int)
str(L[[1]])
str(x)

L.df <- L$df


### R errors ####
# most R syntax errors are related to data and object types

X["s1", "V3"]
# Error in X["s1", "V3"] : subscript out of bounds
# typo in "S1"
X["S1", "V3"]

DF["S1", c("int", "log")]
# Error in `[.data.frame`(DF, "S1", c("int", "log")) : undefined columns selected
# there are no column names "int" and "log" in DF
DF["S1", c("x", "c")]


# calculate percentages over columns of numeric data frame
DF.num <- data.frame(X)
DF.rel <- prop.table(
  DF.num, # input table
  2 # margin (1: rows; 2: columns)
)
# Error in margin.table(x, margin) : 'x' is not an array
# matrix input required
DF.rel <- prop.table(as.matrix(DF.num), 2)
# numeric data frames can be easily converted to a matrix

# to check data and object type:
str(x)
str(X)
str(DF)
str(L)
str(X["S1", c("V3", "V5")]) # not matrix anymore, but vector
str(DF["S1", c("x", "c")]) # still a data.frame
str(DF$x) # not data frame anymore, but vector

# special case: factors
levels(b)
# subsetting factor
b[c(1,3,5,7,9)]
# levels of subset of factor
levels(b[c(1,3,5,7,9)])
# although there is no "A" value anymore in the vector, the factor level is retained
# use droplevels to remove
levels(droplevels(b[c(1,3,5,7,9)]))

# adding new levels to factor
b[2] <- "C"
# this doesn't work
# Warning message: In `[<-.factor`(`*tmp*`, 2, value = "C") : invalid factor level, NA generated
# restore b
b <- as.factor(a)
# copy b to make modification
b1 <- b
# first add factor level
levels(b1) <- c(levels(b), "C")
b1[2] <- "C"
b1


### Let's have a look at some 'real' data ####

# which directory are you working in
getwd()

# tell R where to find the files
setwd("C:/Users/User/Documents/githubRepos/Tutorials/trunk/R_course_MPI/Example_data")

# clear workspace
rm(list = ls())

### reading input data (data frames) ####
# we have tab-separated data
# R also reads csv and space-separated
# since commas and spaces may also occur in the data, I prefer tabstops
# decimal separator is '.' by default, but can also be changed
# there are many more file types that can be read by R, mostly using package-specific functions

# bacterial community data
OTU <- read.table(
  "OTU_table.txt", # file name
  h = T, # first line contains column names
  sep = "\t", # values separated by tabstops
  row.names = 1 # first row contains row names, i.e. OTU names
)
head(OTU)

# bottom water data (carbonate chemistry, nutrients)
ENV <- read.table(
  "ENV_table.txt",
  h = T, 
  sep = "\t"
)

# taxonomy of microbial community
TAX <- read.table(
  "TAX_table.txt",
  h = T, 
  sep = "\t", 
  row.names = 1, # first row contains row names, i.e. OTU names
  stringsAsFactors = F, # to prevent character strings to be interpreted as factors, for taxonomy files factors sometimes also useful
  comment.char = "", # to account for weird symbols (#) in taxon names
  quote = "" # to account for weird symbols (", ') in taxon names
)
head(TAX)

# for more info, use help option
?read.table

# inspect data frames
dim(OTU)
dim(ENV)
dim(TAX)

head(OTU)
head(ENV)
head(TAX)

# more information on content of data frames
str(OTU)
str(ENV)
str(TAX)

# compare the order of OTUs in OTU and TAX tables
# row names are OTU names
all.equal(
  rownames(OTU), 
  rownames(TAX)
)


### data exploration ####

# get some summary information for the environmental parameters
# for one variable
summary(ENV$pH)
table(ENV$seep.influence)

# for one variable but for separate conditions
by( # calculate per grouping factor
  ENV$pH, # vector
  ENV$seep.influence, # grouping variable
  summary # function to be applied
)

# for several variable
apply( # apply to several (in this case) columns
  ENV[, 4:ncol(ENV)], # for all numeric variables in ENV
  2, # calculate per column (table margin 2)
  summary # function to be applied
)


# basic plots
plot(
  ENV$SiO4, # x
  ENV$pH #y
)
# or:
plot(ENV$pH ~ ENV$SiO4) # use formula y ~ x

# let's add some color
# create color vector for pH categories
ENV$seep.color <- ENV$seep.influence
# renaming factor levels automatically changes values
levels(ENV$seep.influence)
levels(ENV$seep.color) <- c("orange", "red", "darkblue") 
levels(ENV$seep.color)
# color argument in plot expects character input
ENV$seep.color <- as.character(ENV$seep.color)
# repeat scatter plot
plot(
  ENV$pH ~ ENV$SiO4,
  col = ENV$seep.color, # specify color for each point
  pch = 16 # point character: use filled circles
)
# when plotting for publications, avoid filled circles
# filled circles in R consists of line stacks

# boxplots are created when the explanatory variable in the formula is a factor
# or by specifying boxplot
plot(
  ENV$pH ~ ENV$site, 
  las = 2 # plot tick labels perpendicular to axis
)
boxplot(
  ENV$pH ~ ENV$seep.influence, 
  las = 2,
  col = c("orange", "red", "darkblue") # here the color argument is per box in the plot
)

# it would be nice to have the seep influence categories ordered by strength of impact
# the can be done by ordering/ranking factor levels of seep.influence
levels(ENV$seep.influence) # original factor levels
seep.influence.ordered <- ordered(
  ENV$seep.influence, # input vector
  levels = c("reference", "medium", "high") # new order of levels
)
levels(seep.influence.ordered) # new factor levels, order has changed
# the values of the vector were not changed
all.equal(
  as.character(seep.influence.ordered), # compare content of factor before and after ordering
  as.character(ENV$seep.influence) # it is still identical
)
ENV$seep.influence <- seep.influence.ordered # replace original variable with ordered variable
rm(seep.influence.ordered) # remove vector

# repeat boxplot
boxplot(
  ENV$pH ~ ENV$seep.influence, 
  las = 2,
  col = c("darkblue", "red", "orange") # adjust order of colors to new order of seep.influence
)

# other plotting options
# how many sequences were generated for each sample, i.e. library size
nSeq <- colSums(OTU) # calculate sum per column
barplot(
  nSeq[order(nSeq, decreasing = T)], # sort library size from largest to smallest
  las = 2
)

# taxonomic compostion on phylum level
pie(
  c(by(rowSums(OTU), TAX$phylum, sum)), # sum up sequence counts over all samples and for each phylum
  radius = 0.9, #size of pie
  cex = 0.8, # size of taxon labels
  col = rainbow(length(unique(TAX$phylum))), # color of pie slices
  clockwise = T # plot clockwise
)


# grep
# are there genera involved in sulfur cycling?
# typically containing "sulf" or "thio" in their genus names
# look for regular expressions
# more info about regular expressions: https://rstudio-pubs-static.s3.amazonaws.com/74603_76cd14d5983f47408fdf0b323550b846.html
length( # return length of vector
  grep("[sS]ulf", # pattern to be matched, case sensitive, allow both 's' and 'S'
       TAX$genus, # vector to be searched
       value = T) # show value of result, not just element number in vector
)

# grep without regular expressions
grep(
  "[sS]ulf", 
  TAX$genus, 
  value = T,
  fixed = T # disable regular expressions, if fixed = T current search pattern will not be found
)

# look for multiple patterns at the same time
length(
  grep("[sS]ulf|[tT]hio", # separate search patterns with '|' inside quotation marks
       TAX$genus, 
       value = T)
)


### calculations on data tables ####

# calculate proton concentration from pH values and append as separate column in ENV
ENV$proton <- 10^(-ENV$pH) # write formula using vector with input data and assign to new variable
plot(
  ENV$pH, 
  ENV$proton, 
  col = ENV$seep.color,
  pch = 16
)

# convert OTU table to relative abundances
OTU.rel <- prop.table(OTU, 2) * 100
# Error in margin.table(x, margin) : 'x' ist kein Array
# only works with matrices
OTU.rel <- prop.table(as.matrix(OTU), 2) * 100

# subset data
# only OTUs classified on genus level
OTU.classified <- OTU[-grep("unclassified", TAX$genus), ]
TAX.classified <- TAX[rownames(OTU.classified), ]
head(TAX.classified)
head(OTU.classified)

# how many sequences were retained [%]?
colSums(OTU.classified)/nSeq * 100


### write to file ####
# write OTU.classified and TAX.classified to file
write.table(
  OTU.classified, # R object
  "OTU_classified_table.txt", # file name
  sep = "\t", # separator
  quote = F # don't keep quotes around character strings
)
write.table(
  TAX.classified, 
  "TAX_classified_table.txt", 
  sep = "\t", 
  quote = F
)

# save workspace to continue working without having to import data again
# save.image("Vent.Rdata")


### work with your own data ####

# read data into R
# generate summaries
# generate plots
# collect error messages