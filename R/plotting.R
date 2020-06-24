
#' Plot NanoSIMS ion maps
#'
#' Helps to plot the ion maps exported and loaded from LANS. Can overlay the ROI boundaries
#' for clarity as well. Note that this does not currently support any smoothing yet so plotting
#' ratios or abundances will most likely not work well because individual pixels have extreme
#' values.
#'
#' @param data the ion maps data frame
#' @param draw_ROIs whether to draw in the ROIs or not (default TRUE)
#' @param normalize whether to normalize the intensity scale for each panel (default TRUE)
#' @param color_scale what color scale to use for high and low intensity, default is black & white
#' @export
plot_maps <- function(data, draw_ROIs = TRUE, normalize = TRUE, color_scale = c("black", "white")) {
  if (nrow(data) == 0)
    stop("no rows in data frame")
  
  if (!"analysis" %in% names(data)) data$analysis <- "data"
  rois <- data %>% group_by(analysis) %>% extract_roi_boundaries()
  
  if (normalize) {
    data <- data %>% group_by(analysis, variable) %>% 
      mutate(value = value/max(value))
  }
  
  p <- data %>% 
    ggplot() + 
    aes(x.um, y.um) + 
    geom_raster(aes(fill = value)) +
    scale_y_continuous(expression("y ["*mu*"m]"), expand = c(0,0)) +
    scale_x_continuous(expression("x ["*mu*"m]"), expand = c(0,0)) +
    scale_fill_continuous(low=color_scale[1], high=color_scale[2]) +
    expand_limits(x = c(0, data$frame_size.um[1]), y = c(0, data$frame_size.um[1])) +
    theme_bw() + 
    theme(panel.background = element_rect(fill = "black"),
          panel.spacing = unit(0, "mm"),
          strip.text = element_text(size = 20),
          legend.position = "bottom", 
          strip.background = element_blank()) +
    guides(color = guide_legend(override.aes = list(size=8, shape = 15))) +
    facet_grid(analysis~variable) +
    labs(x = expression("x ["*mu*"m]"), y = expression("y ["*mu*"m]"), color = "ROI") + 
    coord_equal()
  
  if (draw_ROIs && nrow(rois) > 0) {
    p <- p +
      geom_point(
        data = rois, 
        aes(color = as.factor(ROI)), size = 0.5, shape = 15) +   
      labs(color = "ROI")
  }
  
  return(p)
}


# figure out the ROI boundaries from LANS map export
# @param data data_frame from LANS map, can already be grouped
extract_roi_boundaries <- function(data) {
  
  # checks
  sapply(c("ROI", "variable", "x.px", "y.px"), col_check, data, sys.call())

  # function to determine if on border
  is_on_border <- function(x, y) {
    is_pixel_on_border <- function(xi, yi) {
      if (
        !any(x == xi - 1 & y == yi) || !any(x == xi + 1 & y == yi) ||
        !any(x == xi & y == yi - 1) || !any(x == xi & y == yi + 1))
        return(TRUE)
      return(FALSE)
    }
    mapply(is_pixel_on_border, x, y)
  }
  
  # calculate border
  suppressMessages(
    data %>% filter(ROI > 0) %>% 
      group_by(ROI, add = TRUE) %>% 
      filter(variable == .data$variable[1]) %>%  # calculate for just one variable, for speed
      mutate(roi_border = is_on_border(.data$x.px, .data$y.px)) %>% 
      ungroup() %>% 
      filter(.data$roi_border) %>% 
      select(-.data$variable) %>% 
      inner_join(data %>% group_by(.data$ROI, .data$variable, add = TRUE) %>% 
                   select(.data$variable) %>% distinct()) %>% 
      arrange(.data$x.px, .data$y.px) # merge variables back in
  )
}