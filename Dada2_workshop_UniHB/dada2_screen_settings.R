# Wrapper to screen settings for FilterAndTrim command in dada2 workflow
# This function will try to optimize best maxEE and truncLen settings

screen_settings <- function(sample.names, fnFs, fnRs, range_maxEE, range_truncLen, cpus = 4) {
 
  # construct output
  # proportion of total sequences retained, and mean
  # quantiles from min to max in 10% increments
  out <- matrix(NA, ncol = 17, nrow = nrow(range_maxEE) * nrow(range_truncLen))
  colnames(out) <- c(
    "truncLen.fwd",
    "truncLen.rev",
    "maxEE.fwd",
    "maxEE.rev",
    "prop.total",
    "prop.mean",
    "min",
    paste0("q", seq(10, 90, 10)),
    "max"
  )
  
  # fill first 4 columns
  out[, 1] <- rep(range_truncLen[, 1], each = nrow(range_maxEE))
  out[, 2] <- rep(range_truncLen[, 2], each = nrow(range_maxEE))
  out[, 3] <- rep(range_maxEE[, 1], times = nrow(range_truncLen))
  out[, 4] <- rep(range_maxEE[, 2], times = nrow(range_truncLen))
  
  # create tmp folder for output
  tmp_filtFs <- file.path("tmp", paste0(sample.names, "_F_filt.fastq"))
  tmp_filtRs <- file.path("tmp", paste0(sample.names, "_R_filt.fastq"))
  names(tmp_filtFs) <- sample.names
  names(tmp_filtRs) <- sample.names
  
  # loop through each combination of truncLen and maxEE
  for(i in 1:nrow(out)) {
    print(i)
    tmp <- filterAndTrim(
      fwd = fnFs, 
      filt = tmp_filtFs, 
      rev = fnRs, 
      filt.rev = tmp_filtRs,
      truncLen = c(out[i, 1], out[i, 2]),
      maxN = 0,
      minQ = 2,
      maxEE = c(out[i, 3], out[i, 4]), 
      truncQ = 0, 
      rm.phix = TRUE,
      compress = F,
      multithread = cpus
    )
    
    # fill output table
    out[i, "prop.total"] <- round(sum(tmp[, 2])/sum(tmp[, 1]), 4)
    out[i, "prop.mean"] <- round(mean(tmp[, 2]/tmp[, 1]), 4)
    out[i, 7:17] <- round(quantile(tmp[, 2]/tmp[, 1], seq(0, 1, 0.1)), 4)
  }
  
  # remove tmp folder and return output table
  system("rm -rf tmp/")
  return(out)
}