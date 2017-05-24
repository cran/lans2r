#' Load LANS summary data
#' 
#' Load the ROI summary information exported from LANS analyses and attach additional information to each analysis. 
#' Uses \link{read_roi_data} to read individual files.
#' 
#' @param analysis - vector of LANS analysis folder names
#' @param ... - vectors of additional information to attach to each analysis, each argument has to have the same length as the 
#'    'analysis' parameter (or length 1)
#' @param base_dir - the directory where all the analysis folders are located (defaults to current directory)
#' @param ion_data_only - whether to import only ion data [TRUE by default], rather than any derived files (e.g. ratios calculated 
#'    within LANS). Recommend using \link{calculate_ratios} and \link{calculate_abundances} to process the raw ion counts in easy
#'    format and good error propagation.
#' @param load_zstacks - whether to load the planes data (ion-z.dat files need to be exported from 
#' LANS for this to be possible - they are created when the "Display depth profiles in ROI" is checked
#' during "Display masses")
#' @param quiet - whether to report information on the loaded data or not
#' @export
load_LANS_summary <- function(analysis, ..., base_dir = ".", ion_data_only = TRUE, load_zstacks = TRUE, quiet = F) {
  
  if(!dir.exists(base_dir))
    stop("The base directory does not exist: ", base_dir, call. = FALSE)
  
  info <- data_frame(analysis = analysis, ...)
  data <- lapply(analysis, function(i) {
    data_folder <- file.path(base_dir, i, "dat")
    read_roi_data(data_folder, ion_data_only = ion_data_only, load_zstacks = load_zstacks, quiet = quiet) %>% 
      mutate(analysis = i)
  }) %>% bind_rows()
  
  full_join(info, data, by = "analysis")
}

#' Load LANS ion map data
#' 
#' Load the full ion map data (incl. ROI locations) exported from LANS analyses and attach additional information to each analysis. 
#' Uses \link{read_map_data} to read individual matlab export files.
#' 
#' @param analysis - vector of LANS analysis folder names
#' @param ... - vectors of additional information to attach to each analysis, each argument has to have the same length as the 
#'    'analysis' parameter (or length 1)
#' @param base_dir - the directory where all the analysis folders are located (defaults to current directory)
#' @param ion_data_only - whether to import only ion data [TRUE by default], rather than any derived files (e.g. ratios calculated 
#'    within LANS). Recommend using \link{calculate_ratios} and \link{calculate_abundances} to process the raw ion counts in easy
#'    format and good error propagation.
#' @param quiet - whether to report information on the loaded data or not
#' @export
load_LANS_maps <- function(analysis, ..., base_dir = ".", ion_data_only = TRUE, quiet = F) {
  
  if(!dir.exists(base_dir))
    stop("The base directory does not exist: ", base_dir, call. = FALSE)
  
  info <- data_frame(analysis = analysis, ...)
  data <- lapply(analysis, function(i) {
    data_folder <- file.path(base_dir, i, "mat")
    read_map_data(data_folder, ion_data_only = ion_data_only, quiet = quiet) %>% 
      mutate(analysis = i)
  }) %>% bind_rows()
  
  full_join(info, data, by = "analysis")
}


