# add pie charts at specific coordinates
pie.coord <- ENV.all[, c("Longitute", "Latitude")]

# use phylum taxonomy

# use bioconductor R package
source("https://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

# load R package
require(Rgraphviz)

