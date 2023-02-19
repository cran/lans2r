#' Spread data into wide format
#' 
#' This function allows easy spreading into wide format.
#' 
#' @param data a data frame with lans2r data
#' @param values whether to include the values in wide format
#' @param errors whether to include the errors in wide format
#' @return the original data frame but in wide format
#' @export
spread_data <- function(data, values = TRUE, errors = TRUE) {
  # checks
  sapply(c("variable", "value", "sigma", "data_type"), col_check, data, sys.call())
  
  # spread data into wide format
  if (values)
    val_df <- data %>% 
      select(-"sigma", -"data_type") %>% 
      tidyr::spread(variable, value)
  
  if (errors)
    err_df <- data %>% 
      mutate(variable = paste(variable, "sigma")) %>% 
      select(-"value", -"data_type") %>% 
      tidyr::spread(variable, sigma)
  
  # combine
  if (values && errors)
    df <- suppressMessages(left_join(val_df, err_df))
  else if (values)
    df <- val_df
  else if (errors)
    df <- err_df
  else
    stop("spreading neither errors nor values, need to include at least one of the two", call. = FALSE)
  
  return(df)
}