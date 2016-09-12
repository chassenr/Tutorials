#########################################################
# R course: Day 2 (Tuesday, 13.09.2016)                 #
#########################################################

# more advaced example for data manipulation

# set working directory
setwd("C:/Users/User/Documents/githubRepos/Tutorials/trunk/R_course_MPI/Example_data")

# load workspace with vent data
# load("Vent.Rdata")


### reshaping data tables ####

# sequence data only available for one observation per site,
# but environmental information measured in triplicates
# calculate mean per site for environmental information
# similar to pivot table function in excel

ENV.mean <- aggregate(
  ENV[, 4:13], # numerical variables in ENV
  by = list(reef = ENV$reef, # group by reef,
            site = ENV$site, # site,
            seep.influence = ENV$seep.influence, # seep.influence
            seep.color = ENV$seep.color), # seep.color (this may seem redundant, but we want to keep the variable in the new data.frame)
  FUN = mean, # function to be applied
  na.rm = T # further arguments to mean
)

# now 'sites' can be used as unique identifier of the rows
rownames(ENV.mean) <- ENV.mean$site

# additional to bottom water measurements,
# sediment profiles are available for some of the sites

PROFILE <- read.table(
  "PROFILE_table.txt", 
  h = T, 
  sep = "\t"
)

# the profile data consists of several hundreds of data values,
# measuring several parameters every 100 µm in the sediment
# we are interested in the median value within the first 2cm of the sediment
# the data table organisation of PROFILE is slightly different
# there are only 4 column in the data frame: site, sediment.depth, measurement, value
# we will use the package reshape to create a data frame with sites in rows 
# and the different measured parameters in columns

# install reshape (command line)
# install.packages("reshape")
# or use option in 'Packages' tab in Rstudio

# load the package
require(reshape)

# since we have categories for sediment depth
# order them in a meaningful manner
PROFILE$depth <- ordered(
  PROFILE$depth,
  levels = c("water","2","4","6","8","10")
)

# reshape data
PROFILE.median <- cast(
  PROFILE, # molten data frame (long format)
  site + depth ~ measurement, # formula (similar to rows and columns selection for pivot table in excel)
  median, na.rm = T # function to aggregate data (remove NAs before calculation)
)

# we need the values for the first 2 cm in the sediment
PROFILE.median.2cm <- PROFILE.median[PROFILE.median$depth == "2", ] # "2" is referring to the factor level, not the number

# now we have 2 tables with environmental information
# not all sites are common to both tables
# %in% can be used to search for exact matches of values
# here we want to know which sites in ENV.mean also occur in PROFILE.median.2cm
# the output is a logical vector the length of the first argument (ENV.mean$site)
# sum() can be used to count the occurrences of TRUE in a logical vector
sum(ENV.mean$site %in% PROFILE.median.2cm$site)
# only 8 sites have profile data available

# we can combine both tables into one

# load R package
require(plyr)

# combine data frames
ENV.all <- join(
  ENV.mean, # first table
  PROFILE.median.2cm[, -2], # second table (without the depth column)
  by = "site", # variable used to match rows
  type = "full", # keep all data
  match = "all" # keep both rows in case of duplicates
)
rownames(ENV.all) <- ENV.all$site # keep site also as rownames

# where there is no profile data available, NAs will be generated

# make sure that ENV.all and OTU have the same order of samples/sites
# let's order ENV.all by seep-influence and then site
ENV.all <- ENV.all[order(ENV.all$seep.influence, ENV.all$site), ]

# now match the order of samples in OTU the order of sites in ENV.all
OTU <- OTU[, match(rownames(ENV.all), # target vector
                   colnames(OTU))] # vector to be rearranged
all.equal(rownames(ENV.all), colnames(OTU))

# easier option
# applicable if row and column names are the same
# use names to select rows and/or columns
# the output will automatically be in the order of the names used for the selection
OTU.rel <- OTU.rel[, rownames(ENV.all)]
OTU.classified <- OTU.classified[, colnames(OTU)]


### for loops ####
# useful to repeatly apply the same function/command
# e.g. here we will use a for loop the generate sample-by-taxon tables for each taxonomic level
# the results will be stored in a list

# create an empty list
TAX.pooled <- vector(
  mode = "list", # type of vector
  length = 5 # length of list, i.e. how many different taxonomic levels
)
names(TAX.pooled) <- colnames(TAX)[2:6] # phylum to genus

# fill elements of list with data frames with taxon abundance, 
# one taxonomic level at a time
for (i in 1:5) { # curly brackets are used to define what is done for each iteration of the loop
  temp <- aggregate(
    OTU, # take sequence counts from OTU
    by = list(TAX[, i + 1]), # for each taxon of the taxonomic level i + 1 (we don't want domain, which is in column 1)
    FUN = sum # sum up sequence counts
  )
  rownames(temp) <- temp$Group.1 # name rows based on taxon names
  TAX.pooled[[i]] <- temp[, -1] # since rows contain taxon names, Group.1 variable not needed anymore as part of the data frame
  rm(temp) # remove temporary object
}
str(TAX.pooled)

# check the number of sequences
all.equal(colSums(OTU), colSums(TAX.pooled$class))


### apply funtions ####
# there is another reason, why we only wanted numerical data in TAX.pooled
# we will calculate the percentage sequence counts for each taxonomic level

# option 1: also using a for loop
TAX.pooled.rel <- vector(mode = "list", length = 5)
names(TAX.pooled) <- names(TAX.pooled)
for (i in 1:length(TAX.pooled)) {
  TAX.pooled[[i]] <- prop.table(as.matrix(TAX.pooled[[i]]), 2) * 100
}

# option 2: using lappy
TAX.pooled.rel <- lapply( # 'list apply'
  TAX.pooled, # apply function to each element of TAX.pooled
  function(x) { # define the function to be applied
    prop.table(as.matrix(x), 2) * 100 # same function as above, x is a placeholder for each element in TAX.pooled
  }
)

# apply, sapply, lapply are very handy to apply the same function to...
# ... all rows (margin = 1) or column (margin = 2) of a 2-dimensional object (apply)
# ... each element of a list, creating another list (lapply)
# ... each element of a list, trying to simplify the output by e.g. creating a vector (sapply)

# instead of using the R command colSums() we could have used:
apply(OTU, 2, sum)
all.equal(colSums(OTU), apply(OTU, 2, sum))


# another example of working with lists
# working with taxonomic paths
ALIGN <- read.table(
  "ALIGN_table.txt", 
  h = T, 
  sep = "\t", 
  row.names = 1, 
  stringsAsFactors = F, 
  comment.char = "", 
  quote = ""
)

# check that rownames are in the same order as in OTU
all.equal(rownames(OTU), rownames(ALIGN))

# split taxonomic path into separate taxonomic levels
# similar to 'data to columns' option in excel
PATH.split <- strsplit(
  ALIGN$path, # input character vector
  split = ";", # separator (can also be more than one character)
  fixed = T # don't use regular expressions
)

# the output is a list with a separate character vector for each taxonomic path
# let's select the last element of each taxonomic path
# i.e. the last classified taxonomic level of an OTU
# since only one element is selected from each vector in the list,
# the output is concatenated into a vector
last.classified <- sapply(
  PATH.split, # input list
  function(x) { 
    x[length(x)] # select in each vector the element with the number equal to the length of the vector
  }
)
head(last.classified, 30)

# there is still something like 'uncultured' in the output
# we can use an if statement to select the level before the last in the taxonomic path
last.classified <- sapply(
  PATH.split, 
  function(x) { 
    if (x[length(x)] == "uncultured") { # logical condition: if the last level of the taxonomic path is 'uncultured'
      x[length(x) - 1] # take the level before the last
    } else { 
      x[length(x)] # otherwise take the last level of the taxonomic path
    }
  }
)
head(last.classified, 30)

# save workspace to continue working without having to import data again
# save.image("Vent.Rdata")

### you can do eveything that you are used to in excel ####
