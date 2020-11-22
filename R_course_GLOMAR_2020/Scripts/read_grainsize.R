# reading grain size data from particle sizer instrument
# dependency: reshape

read.grainsize <- function(
  path,
  files = list.files(path, pattern = ".csv"),
  meta.out = "grainsize_metadata.txt",
  gs.out = "grainsize_parsed.txt"
) {
  
  # required metadata information
  info <- c(
    "Transmittance",
    "Circulation Speed", 
    "Agitation Speed",
    "Refractive Index",
    "Iteration Number"
  )
  
  # extract metadata
  input <- file.path(path, files)
  metadata <- vector("list", length = length(input))
  for(i in 1:length(metadata)) {
    metadata[[i]] <- scan(input[i], what = character(), sep = "\n", blank.lines.skip = T, strip.white = T, skipNul = T)
  }
  metadata.parsed <- do.call(
    "rbind",
    lapply(
      metadata,
      function(x) {
        do.call(
          "rbind",
          strsplit(
            gsub(" ", "", x[grep(paste(info, collapse = "|"), x)]),
            "\t"
          )
        )[, 2]
      }
    )
  )
  rownames(metadata.parsed) <- files
  colnames(metadata.parsed) <- c(
    "Transmittance(R)",
    "Transmittance(B)",
    "Circulation Speed", 
    "Agitation Speed",
    "Refractive Index(R)",
    "Refractive Index(B)",
    "Iteration Number"
  )
  
  # write to file
  write.table(metadata.parsed, meta.out, sep = "\t", quote = F)
  
  # parse grainsize data
  gs.data <- lapply(
    metadata, 
    function(x) {
      x[grep("Diameter(µm)\tq(%)\tUnderSize(%)", x, fixed = T):length(x)]
    }
  )
  gs.data.split <- lapply(
    gs.data,
    function(x) { apply(do.call("rbind", strsplit(x[-1], "\t")), 2, as.numeric) }
  )
  gs.data.parsed <- data.frame(do.call("rbind", gs.data.split))[, 1:2]
  colnames(gs.data.parsed) <- c("diameter.um", "percentage")
  gs.data.parsed$file.name <- rep(files, sapply(gs.data.split, nrow))
  
  # write to file
  write.table(gs.data.parsed, gs.out, sep = "\t", quote = F, row.names = F)
}
