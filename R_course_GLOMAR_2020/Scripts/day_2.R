#################################
#         DAY 2                 #
#################################


### Data exploration ####

# calculate proton concentration from pH
ENV$proton <- 10^(-ENV$pH) # write formula using vector with input data and assign to new variable

# calculate NO3 concentration based on NOx and NO2

# data summaries (all samples)
summary(ENV$pH)
summary(ENV$proton)

# data summaries (per seep category)
by(ENV$pH, ENV$seep.category, summary)
do.call("rbind", by(ENV$pH, ENV$seep.category, summary))

# number of sequences per sample
colSums(OTU)

# boxplots
boxplot(ENV$pH ~ ENV$seep.category)
# Add color to the boxplot

# pairwise correlations
pairs(ENV[, c(5:10, 13, 14)])

#####


### Filter data set ####

# Only select environmental parameters from reference sites
ENV[ENV$seep.category == "reference", ]

# How many OTUs are affiliated with Chloroflexi?

# Are there genera involved in sulfur cycling?
# typically containing "sulf" or "thio" in their genus names
# look for regular expressions
# more info about regular expressions: https://rstudio-pubs-static.s3.amazonaws.com/74603_76cd14d5983f47408fdf0b323550b846.html
length( # return length of vector
  grep(
    "[sS]ulf", # pattern to be matched, case sensitive, allow both 's' and 'S'
    TAX$genus, # vector to be searched
    value = T # show value of result, not just element number in vector
  ) 
)

#####


### Converting wide/long tables and summarizing data ####

# calculate mean for each parameter directly from ENV
ENV.mean <- aggregate(
  ENV[, 4:13], # numerical variables in ENV
  by = list(
    reef = ENV$reef,
    site = ENV$site,
    seep.category = ENV$seep.category,
    color = ENV$color # seep.color (this may seem redundant, but we want to keep the variable in the new data.frame)
  ),
  FUN = mean, # function to be applied
  na.rm = T # further arguments to mean
)

# or use melt and cast from the package reshape
require(reshape)
ENV.melt <- melt(ENV) # long format
ENV.mean <- cast(ENV.melt, "site + seep.category ~ variable", fun.aggregate = mean, na.rm = T)

# Calculate summary statitics and combine tables:
#   Calculate number of sequences and OTUs per sample and store values in a data.frame
#   Calculate median values for environmental parameters
#   Combine sequencing and environmental information into 1 data.frame
# There are many ways to to this...

#####


### Loops and apply ####

# before we looked at each variable separately to calculate summary statistics
# let's use a loop to look at each variable sequentially
for(i in c(5:10, 13, 14)) { # curly brackets are used to define what is done for each iteration of the loop
  print(summary(ENV[, i]))
}
# this is not very informative

# a better example is maybe to apply a glm to each variable, testing for differences between seep categories
# the output of a loop can be stored in a list
env.params <- colnames(ENV)[c(5:10, 13, 14)] 
# Why am I storing the names of the variables that I want to look at in a separate vector

ENV.glm <- vector("list", length = length(env.params))
names(ENV.glm) <- env.params
for(i in 1:length(env.params)) {
  ENV.glm[[i]] <- aov(ENV[, env.params[i]] ~ ENV$seep.category) 
}

# now that we have a list, let's look at the model summary
lapply(ENV.glm, summary)

# Another example:
# generate sample-by-taxon tables for each taxonomic level

# create an empty list
TAX.pooled <- vector(
  mode = "list", # type of vector
  length = 5 # length of list, i.e. how many different taxonomic levels
)
names(TAX.pooled) <- colnames(TAX)[2:6] # phylum to genus

# fill elements of list with data frames with taxon abundance, 
# one taxonomic level at a time
for (i in 1:5) { 
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

# calculate the percentage sequence counts for each taxonomic level using lapply
TAX.pooled.rel <- lapply( 
  TAX.pooled, 
  function(x) { 
    prop.table(as.matrix(x), 2) * 100 
  }
)

# apply, sapply, lapply are very handy to apply the same function to...
# ... all rows (margin = 1) or column (margin = 2) of a 2-dimensional object (apply)
# ... each element of a list, creating another list (lapply)
# ... each element of a list, trying to simplify the output by e.g. creating a vector (sapply)

#####


### Writing functions ####

# basically we already wrote our own functions in the previous examples

# Here is another example: function for standard error
se <- std <- function(x) {
  sd(x)/sqrt(length(x))
}

# Calculate standard error per seep category for all environmental parameters

# More complicated example: reading grain size data into R
# code validated for windows
source("../Scripts/read_grainsize.R")
read.grainsize(path = "Grain_size")

#####


### tidyverse ####

# load packages
require(tidyverse)

# tibble: tidyverse equivalent to data.frame
# but: tibbles behave differently than data.frames
ENV.tb <- as_tibble(ENV)
# What is the different between the following 2 commands?
ENV[, 5]
ENV.tb[, 5]

# pipe operator:
# instead of nested commands you can pipe the output of one command to the next
ENV.tb %>% 
  select(env.params) %>% 
  summary()

# filter, select, mutate, relocate, rename
ENV.tb %>% filter(seep.category == "reference")
ENV.tb <- ENV.tb %>% 
  relocate(proton, .before = SiO4) %>% 
  mutate(NO3 = NOx - NO2, .before = longitude) %>% 
  rename(location = site)
# Who of you get an error message here?
# What do you think is the reason?
# How would you go about solving it?

# group_by
# handy way to execute functions for groups of samples
ENV.tb %>% 
  group_by(seep.category) %>% 
  select(env.params) %>% 
  summarise_all(mean, na.rm = T)

# map
# similar to loops and apply
ENV.glm <- map(env.params, function(x) {
  aov(ENV[, x] ~ ENV$seep.category)
})
names(ENV.glm) <- env.params

# There is a multitude of other functions avaiable in tidyverse, which may make your life easier

#####


### Time to play around with your data! ####