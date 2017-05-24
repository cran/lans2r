#' @keywords internal
"_PACKAGE"

#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import lazyeval
#' @importFrom stats setNames sigma var
#' @importFrom utils read.table
#' @importFrom R.matlab readMat
#' @importFrom reshape2 melt
NULL

# quiets concerns of R CMD check about . that appears in pipelines
# and the standard column names used in non-standard evaluation
utils::globalVariables(c(".", "variable", "value", "analysis", 
                         "x.um", "y.um", "ROI", "plane", "data_type"))


# release questions 
release_questions <- function() {
  c(
    "Is it passing travis, appveyor and win-builder?"
  )
}