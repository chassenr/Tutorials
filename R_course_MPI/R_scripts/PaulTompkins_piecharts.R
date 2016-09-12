# add pie charts at specific coordinates
pie.coord <- ENV.all[, c("Longitute", "Latitude")]
all.equal(rownames(pie.coord), rownames(seagrass))

# use bioconductor R package
source("https://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

# load R package
require(Rgraphviz)

# open new graphics device
plot.new()

# plot coastlines

# add pie charts
for(i in 1:nrow(pie.coord)){
  pieGlyph(
    seagrass[i, ],
    pie.coord[i, 2],
    pie.coord[i, 1], 
    labels = colnames(seagrass), 
    col = rainbow(ncol(seagrass)), 
    cex = 0.3
  )
}
