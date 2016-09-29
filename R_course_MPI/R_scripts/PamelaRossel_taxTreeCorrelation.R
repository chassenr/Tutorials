# generate taxonomy tree of OTU table
# each tip representing unique taxonomic paths in the data set
# dubplicate taxonomic paths allowed if OTUs of the same taxonomic paths show both positive and negative correlations to environmental data
# tip color based on positive and negative correlations
# the size of the tip label corresponding to the number of OTUs which are positively or negatively correlated

# input:
# a table with the taxonomy (taxonomic levels in different columns) and the correlation coefficient for each OTU
# colnames e.g.: "domain" "phylum" "class" "order" "family" "genus" "correlation"
# R object called: taxonomy

# remove "_unclassified" from taxon names
# "_unclassified" would only increase the size of the tip labels without adding any additional information to the tree
# we use gsub to remove "_unclassified" (i.e. replace it with nothing), which only works on matrices
taxonomy[, 1:6] <- gsub("_unclassified", "", as.matrix(taxonomy[, 1:6]))

# further preparation of input to tree
# we don't just want unique taxonomic paths on the tree
# we want to have the same taxon twice, of OTUs of that taxon are both positively and negatively correlated with some environmental parameter
# to retain this information about the direction of the correlation
# we first add a logical vector to the data.frame (TRUE for positive correlations)
taxonomy$posneg <- taxonomy$correlation > 0 

# we then combine the logical vector with the genus name
# interaction provides all possible combinations for genus pos and neg correlations
# we use droplevels to get rid of empty factor levels
taxonomy$taxoncorr <- droplevels(interaction(taxonomy$genus, taxonomy$posneg))

# now we only want the unique combinations of the complete taxonomic path (including taxoncorr and posneg)
taxonomy.unique <- unique(taxonomy[, c("domain", "phylum", "class", "order", "family", "genus", "taxoncorr", "posneg")])

# we reorder the table based on taxoncorr
taxonomy.unique <- taxonomy.unique[order(taxonomy.unique$taxoncorr), ]

# because we reordered the order of taxonomy.unique$taxoncorr will be alphabetical
# just like the order of the names in table(taxonomy.unique$taxoncorr)
all.equal(names(table(taxonomy$taxoncorr)), 
          as.character(taxonomy.unique$taxoncorr))
# that is why we cann append the number of occurrences of taxonomy$taxoncorr to taxonomy.unique,
# i.e. the number of positively and negatively correlated OTUs per taxonomic path in the original data set
taxonomy.unique$number <- c(table(taxonomy$taxoncorr))
# range of OTU numbers
range(taxonomy.unique$number)

# for plotting we have to convert the taxon names to factors
for (i in 1:6) { # assuming that the first 6 columns in the data are domain to genus
  taxonomy.unique[, i] <- as.factor(taxonomy.unique[, i])
}

# create the taxonomy tree using taxoncorr as genus name
otu.tree <- as.phylo(~phylum/class/order/family/taxoncorr, data = taxonomy.unique)

# reorder taxonomy.unique to match tip labels in otu.tree
# mathc will only work if the target vector consists of unique elements
# this is why it was important to use taxoncorr (which is unique for each tip) as genus name
taxonomy.unique <- taxonomy.unique[match(otu.tree$tip.label, taxonomy.unique$taxoncorr), ]
# check that the range of OTUs per unique taxoncorr has not changed
range(taxonomy.unique$number)

# now we remove the TRU and FALSE from the tip labels
# because we don't want this information in the plot
otu.tree$tip.label <- sapply(strsplit(otu.tree$tip.label, ".", fixed = T),
                             function(x) { x[1] })

# we need the package scales to convert OTU numbers to a range of values suitable for plotting
require(scales)

# create tree
plot.phylo(
  otu.tree,
  cex = rescale(taxonomy.unique$number, to = c(0.2, 2)),
  show.node.label = TRUE,
  # tip color based on posneg
  # if posneg is a factor, positive will be 2 and negative will be 1
  # if posneg is still logical, positive will be 1 and negative will be 0
  tip.color = as.numeric(taxonomy.unique$posneg), 
  main = "Title of plot", 
  type = "unrooted"
)
