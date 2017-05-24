# not exported, just internal helper to make sure columns exist
col_check <- function(col, data, fun = sys.call(-1)) {
  if (!is.null(col) && !col %in% names(data)) 
    stop("column not in dataset: '", col, "'. Plese check your call to ", fun, call. = FALSE)
}