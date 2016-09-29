# function to subset an OTU table (or similar) 
# to only display the samples of a certain condition (envData)
# and the OTUs that are unique to this condition, 
# i.e. not found in any of the other samples in the data set
# as input a sample-by-OTU table (samples in rows) and a grouping variable (factor) are required
# the function can also write the subsetted tables to file (one file for each condition)

EnvironmentSpecific <- function(input, # sample-by-OTU table (samples in rows)
                                envData, # grouping factor
                                write.files = T) { # should table subsets be written to file? (default: TRUE)
  
  # make sure that there are no empty factor levels in grouping variable
  envData <- droplevels(envData)
  
  # create empty list for table subsets
  output.list <- vector(
    mode = "list", # type of vector
    length = length(levels(envData)) # length of list, i.e. number of groups (levels) of grouning factor
  )
  names(output.list) <- levels(envData) # name elements of list based on factor levels
  
  # generate subset
  for (i in 1:length(output.list)) { # for each group
    output.list[[i]] <- input[envData == names(output.list)[i], # take only samples from group
                              colSums(input[envData == names(output.list)[i], ]) > 0 & # take only OTUs which are present in at least one sample of group
                                colSums(input[envData != names(output.list)[i], ]) == 0] # and which are not present anywhere else in the data set
  }
  
  # write files
  if (write.files == T) {
    for (i in 1:length(output.list)) { # for each group
      write.table(output.list[[i]], # take table subset
                  file = paste(names(output.list)[[i]], "txt", sep = "."), # write to file with name of group as file name 
                  quote = F,
                  sep = "\t")
    }
  }
  
  # output of function is list with table subsets
  # will be written to console (standard output) or saved as R object in workspace (if assigned)
  return(output.list)
}
