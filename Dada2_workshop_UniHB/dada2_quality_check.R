# Wrapper function to create multi-page pdf with base quality plots
# Argument 'file_base' will be extended by '_fwd.pdf' and '_rev.pdf'

quality_check <- function(fwd, rev, file_base = "QualityProfile") {
  QualityProfileFs <- list()
  for(i in 1:length(fwd)) {
    QualityProfileFs[[i]] <- list()
    QualityProfileFs[[i]][[1]] <- plotQualityProfile(fwd[i])
  }
  pdf(paste0(file_base, "_fwd.pdf"))
  for(i in 1:length(fwd)) {
    do.call("grid.arrange", QualityProfileFs[[i]])  
  }
  dev.off()

  QualityProfileRs <- list()
  for(i in 1:length(rev)) {
    QualityProfileRs[[i]] <- list()
    QualityProfileRs[[i]][[1]] <- plotQualityProfile(rev[i])
  }
  pdf(paste0(file_base, "_rev.pdf"))
  for(i in 1:length(rev)) {
    do.call("grid.arrange", QualityProfileRs[[i]])  
  }
  dev.off()
}