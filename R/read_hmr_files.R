#' Read data from HMR txt files
#'
#' See HMR vignette for examples.
#'
#'@param folder - the folder where the HMR files are
#'@param prefix - the prefix for the set of hmr files
#'@param suffix - the suffix for the set of hmr files
#'@export
load_HMR <- function(folder, prefix, suffix = ".hmr_txt") {
  # 'global' variables
  Pt <- NULL; X <- NULL;
  # retrieve HMR data
  df <- data.frame(stringsAsFactors=F)
  pattern <- paste0(prefix,"(.*)", suffix) # file pattern
  for (file in dir(folder, pattern = pattern)) {
    ion <- sub(pattern, "\\1",file)
    info <- read.table(file.path(folder, file), header= FALSE, skip=1, fill= TRUE, comment.char = "", sep="\t")
    data <- read.table(file.path(folder, file), header= TRUE, skip=9, fill= TRUE, comment.char = "", sep="\t")
    df <- rbind(df,
                mutate(data, 
                       ion = ion,
                       prefix = prefix,
                       filename = sub("File name : (.*)", "\\1", info$V1[1]),
                       date = as.POSIXct(sub("Saving Time : (.*)", "\\1", info$V1[2]), format = "%m.%d.%y - %H:%M"),
                       trolley = sub("Trolley Id : (.*)", "\\1", info$V1[3]),
                       B = sub("B: ([0-9.]+) / R: ([0-9.]+) / M: ([0-9.]+)", "\\1", info$V1[4]),
                       R = sub("B: ([0-9.]+) / R: ([0-9.]+) / M: ([0-9.]+)", "\\2", info$V1[4]),
                       M = sub("B: ([0-9.]+) / R: ([0-9.]+) / M: ([0-9.]+)", "\\3", info$V1[4]),
                       step = sub("([0-9]+) : .*", "\\1", Pt),
                       voltage = as.numeric(sub("([0-9]+) : ((\\+|\\-)?[0-9E])+", "\\2", Pt)),
                       cts = X)[-(1:4)])
  }
  return(df)
}
