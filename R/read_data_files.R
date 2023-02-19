# ROI summary (.dac/.dat) files ================

#' Read LANS data summary (.dac/.dat) files
#'
#' Reads the ion data (.dac/.dat) files for the given LANS analysis folder
#' and returns the ROIs data in a concatenated data frame with identifier
#' columns 'ROI' and 'variable' (=ions). Note that this only reads ion data files by
#' default and not any derived data files (any ratio or other formulas
#' evaluated by LANS). If zstacks (i.e. individual planes) are exported from LANS,
#' they can be loaded as well (and are by default). The resulting data frame
#' has a 'plane' column that keeps track of the plane, the value 'all' identifies
#' the combined data for the ROI from all planes.
#'
#' @param dat_folder - the LANS dat directory with the ions' .dac files
#' @param ion_data_only - by default TRUE, i.e. ignores all non-ion data files
#' @param load_zstacks - whether to load any z_stacks found, by default TRUE
#' @param quiet - whether to report information on the loaded data or not
#' @return concatenated data_frame with all the ROIs' data, with identifier columns 'plane', 'ROI' and 'variable'
#' @export
read_roi_data <- function(dat_folder, ion_data_only = TRUE, load_zstacks = TRUE, quiet = FALSE) {
  # checks
  if (!dir.exists(dat_folder))
    stop("directory does not exist: ", dat_folder, call. =FALSE)
  
  # pattern
  if (ion_data_only) {
    dac_pattern <- "^([0-9A-Z]+)\\.dac$"
    zstack_pattern <- "^([0-9A-Z]+)\\-z.dat$"
  } else {
    dac_pattern <- "^(.+)\\.dac$"
    zstack_pattern <- "^(.+)\\-z.dat$"
  }
  
  # files
  dac_files <- list.files(dat_folder, pattern = dac_pattern, full.names = TRUE)
  zstack_files <- list.files(dat_folder, pattern = zstack_pattern, full.names = TRUE)
  if (length(dac_files) == 0)
    stop("there are no ion .dac files in this folder: ", dat_folder)
  if (length(zstack_files) == 0)
    stop("there are no z-stack ion .dat files in this folder: ", dat_folder)
  
  # load all files
  roi_data <- lapply(dac_files, read_roi_ion_data_file) %>% bind_rows()
  
  if (load_zstacks) {
    zstack_data <- 
      lapply(zstack_files, read_roi_ion_zstack_data_file) %>% bind_rows() %>% 
      left_join(roi_data %>% select(-"plane", -"value", -"sigma"), by = c("ROI", "data_type", "variable"))
  } else {
    zstack_data <- tibble()
  }
  
  if (!quiet) {
    sprintf(
      paste0(
        "INFO: folder '%s' read successfully.",
        "\n      Data for %d ROIs with %d ions recovered: %s.",
        "\n      Z-stacks were %sloaded.%s"),
      basename(dirname(dat_folder)),
      roi_data$ROI %>% unique() %>% length(),
      roi_data$variable %>% unique() %>% length(),
      roi_data$variable %>% unique() %>% paste(collapse = ", "),
      if (load_zstacks) "" else "not ",
      if (load_zstacks) paste(" Recovered", zstack_data$plane %>% unique() %>% length(), "planes.") else ""
    ) %>% message()
  }
  
  bind_rows(roi_data, zstack_data) %>% return()
}

# Read an ion data (.dac) file
read_roi_ion_data_file <- function (file) {
  stopifnot(file.exists(file))
  ion <- sub("^(.+)\\.dac$", "\\1", basename(file))
  data <- read.table(file, header = TRUE, skip=1, fill = TRUE, comment.char = "", sep="\t", check.names = FALSE)
  
  # name checks (what is expected in dac files)
  names_exp <- c("# i", "Xi", "Yi", "MEANi", "Poiss_Ei", "Poiss_%Ei", "SIZEi", "PIXELSi", "LWratio")
  if ( length(missing <- setdiff(names_exp, names(data))) > 0 ) {
    stop("error reading file '", file, "'\nseveral columns expected in regular .dac files seem to be missing: ", 
         missing %>% paste(collapse = ", "))
  }
  
  data %>% 
    mutate(
      plane = "all", 
      data_type = "ion_count",
      variable = ion,
      sigma = iso.errN(.data$MEANi) 
    ) %>% # recalculating it to be more precise
    select(
      "plane",
      ROI = "# i",
      "data_type",
      "variable",
      value = "MEANi",
      "sigma",
      coord_x = "Xi", 
      coord_y = "Yi", 
      size = "SIZEi", 
      pixels = "PIXELSi", 
      LW_ratio = "LWratio"
    ) %>% as_tibble()
}


# Read an ion data (.dac) file
read_roi_ion_zstack_data_file <- function (file) {
  stopifnot(file.exists(file))
  V1 <- NULL # global variable definition
  ion <- sub("^([0-9A-Z]+)\\-z.dat$", "\\1", basename(file))
  read.table(file, header = FALSE, skip = 3, comment.char = "", sep = "\t") %>%
    tidyr::gather(var, value, -V1) %>% 
    rename(plane = "V1") %>% 
    group_by(.data$plane) %>% 
    mutate(
      data_type = "ion_count",
      variable = ion,
      ROI = rep(seq(1, n()/2), each = 2),
      col = sub("V(\\d+)", "\\1", var) %>% as.numeric,
      var = ifelse(col %% 2 == 0, "value", "sigma")) %>% 
    select(-"col") %>% 
    ungroup() %>% 
    tidyr::spread(var, value) %>% 
    mutate(plane = as.character(.data$plane), # to fit with 'all' plane
           sigma = iso.errN(.data$value)) # recalculating it to be more precise
}

# Raw data (.mat) files ============

#' Read LANS full ion map data (.mat) files
#'
#' Reads the full matlab data files (.mat) that contain the complete ion maps and ROI outlines 
#' for the given LANS analysis folder and returns the data in a concatenated data frame with
#' identifier column 'variable' (=ion), data columns value (ion count) and sigma (error based on
#' counting statistics). Additionally, the column 'ROI' indicates which ROI each pixel belongs to
#' with a value of 0 indicating that it does not belong to any ROI. Note that this only reads ion 
#' data files by default and not any derived data files (any ratio or other formulas
#' evaluated by LANS). It does also not currently support z-stacks yet.
#'
#' @param mat_folder - the LANS mat directory with the ions' .mat files
#' @param ion_data_only - by default TRUE, i.e. ignores all non-ion data files
#' @param quiet - whether to report information on the loaded data or not
#' @return concatenated data_frame with the full ion maps data
#' @export
read_map_data <- function(mat_folder, ion_data_only = TRUE, quiet = FALSE) {
  # checks
  if (!dir.exists(mat_folder))
    stop("directory does not exist: ", mat_folder, call.=FALSE)
  
  # pattern
  if (ion_data_only) {
    mat_pattern <- "^([0-9A-Z]+)\\.mat$"
  } else {
    mat_pattern <- "^(.+)\\.mat$"
  }
  
  # files
  mat_files <- list.files(mat_folder, pattern = mat_pattern, full.names = TRUE)
  if (length(mat_files) == 0)
    stop("there are no ion .mat files in this folder: ", mat_folder)
  
  # load all files
  ion_map_data <- lapply(mat_files, read_full_ion_data_file) %>% bind_rows()
  
  if (!quiet) {
    sprintf(
      paste0(
        "INFO: folder '%s' read successfully.",
        "\n      Ion map data for %s x %s pixel frame (%s microm^2) for %s ions recovered: %s.",
        "\n      %s ROIs identified in the frame."),
      basename(dirname(mat_folder)),
      ion_map_data$x.px %>% max(), ion_map_data$y.px %>% max(), ion_map_data$frame_size.um[1],
      ion_map_data$variable %>% unique() %>% length(),
      ion_map_data$variable %>% unique() %>% paste(collapse = ", "),
      (ion_map_data$ROI %>% unique() %>% length() - 1)
    ) %>% message()
  }
  
  ion_map_data %>% return()
}

# Read a full ion data (.mat) file
read_full_ion_data_file <- function (file) {
  stopifnot(file.exists(file))
  ion <- sub("^(.+)\\.mat$", "\\1", basename(file))
  mat <- R.matlab::readMat(file)
  rois <- mat$CELLS %>% reshape2::melt() %>% 
    as_tibble() %>% rename(ROI = "value")
    # NOTE: could replace reshape2::melt with this but not clear it'd be faster
    # tibble::as_tibble() %>% 
    # tibble::rowid_to_column(var = "Var1") %>% 
    # tidyr::pivot_longer(cols = -Var1, names_to = "Var2", values_to = "ROI") %>% 
    # dplyr::mutate(Var2 = as.integer(gsub("V", "", Var2)))
  mat$IM %>% 
    # melt is significnatly faster than gather for this kind of matrix calculation
    reshape2::melt() %>% as_tibble() %>% 
    left_join(rois, by = c("Var1", "Var2")) %>% 
    mutate(
      variable = ion,
      data_type = "ion_count",
      sigma = iso.errN(.data$value),
      x.px = .data$Var2,
      y.px = max(.data$Var1) - .data$Var1 + 1,
      frame_size.px = max(.data$x.px),
      frame_size.um = mat$xyscale[1,1],
      x.um = .data$x.px/.data$frame_size.px * .data$frame_size.um,
      y.um = .data$y.px/.data$frame_size.px * .data$frame_size.um
    ) %>% 
    select(c("x.px", "y.px", "frame_size.px", "x.um", "y.um", 
             "frame_size.um", "variable", "data_type", "value", 
             "sigma", "ROI")) %>% 
    arrange(.data$x.px, .data$y.px)
}

