#' Calculate derived data
#' 
#' This function allows easy calculation of any quantities derived from other variables. The new quantities can be assigned to a specific data_type and values, errors as well as the resulting variable names are calculated/constructed based on custom functions that can be provided via the function parameters. 
#' \link{calculate_sums}, \link{calculate_ratios} and \link{calculate_abundances} are all based on this and provide an easy way for common standard calculations.
#' 
#' @param data a data frame with lans2r data, can be grouped to do calculations within individual groups
#' @param data_type what to call the new data type
#' @param ... the parameters to send to the value, error and naming function for each derived value. These are always expressions that can include references to variable columns, arithmetic and constants, e.g. c(`12C`, `13C`) or c("test", 100*(`12C`+`13C`)). The number of parameters needs to match those expected by the value, error and name functions. Error values of different columns (say for classical error propagation) can be addressed using the suffix "sigma", e.g. c(`12C`, `12C sigma`) would pass both the value and error of this variable to the functions.
#' @param value_fun a custom function used to calculate the derived value - needs to match the sets of parameters provided through ...
#' @param error_fun a custom function used to calculate the error (sigma) for the derived value - needs to match the sets of parameters provided through ...
#' @param name_fun a custom function used to construct the variable name for the derived quantity - needs to match the sets of parameters provided through ...
#' @param filter_new an expression to apply as a filter on the new data rows (e.g. plane == "all")
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @return the original data frame with the newly calculated information appended (data_type == "ion_sum")
#' @family calculations
#' @export
calculate <- function(data, data_type, ..., value_fun, 
                      error_fun = function(...) return(NA), 
                      name_fun = default_name,
                      filter_new = NULL,
                      quiet = FALSE) {
  
  # checks
  sapply(c("variable", "value", "data_type"), col_check, data, sys.call())
  
  # default name function (concatenate the deparsed expression)
  default_name <- function(...) {
    rlang::enexprs(...) %>% 
      sapply(rlang::as_label, simplify = TRUE) %>% 
      paste(collapse = " ")
  }
  
  # function to parse parameters passed using c()
  parse_params <- function(param_quo) {
    if (rlang::quo_is_call(param_quo) && rlang::call_name(param_quo) == "c") 
      rlang::call_args(param_quo)
    else
      list(rlang::get_expr(param_quo))
  }
  
  # generate parameter sets
  param_exps <- rlang::enquos(...) 
  parsed_params <- lapply(param_exps, parse_params)
  
  # determine new variable names (calling the name_fun)
  var_new <- sapply(parsed_params, function(ps_exps) {
    rlang::expr(name_fun(!!!ps_exps)) %>% rlang::eval_tidy()
  })
  
  # generate the value and error expressions
  val_fields <-
    lapply(parsed_params, function(ps_exps) {
      rlang::expr(value_fun(!!!ps_exps))
    }) %>% setNames(var_new)
  
  err_fields <-
    lapply(parsed_params, function(ps_exps) {
      rlang::expr(error_fun(!!!ps_exps))
    }) %>% setNames(var_new)
  
  # figure out what are the actual new variables (includes overriding old ones)
  new_data_type <- data_type
  var_old <- data$variable %>% unique() %>% setdiff(var_new)
  var_new_select <- lapply(var_old, function(i) rlang::expr(-!!rlang::sym(i)))
  
  # spread data into wide format (relies on groups getting carried through the spread)
  df <- spread_data(data, values = TRUE, errors = TRUE)
  
  # just in case of grouping, make calculations with do
  new_data <- 
    df %>% 
    do({
      
      df_group <- .
      
      # calculate values and error within in each group
      values <- 
        df_group %>% 
        mutate(!!!val_fields) %>% 
        select(-ends_with("sigma")) %>% 
        select(!!!var_new_select) %>% 
        tidyr::gather("variable", "value", !!!var_new) 
      
      error <- 
        df_group %>% 
        mutate(!!!err_fields) %>% 
        select(-ends_with("sigma")) %>% 
        select(!!!var_new_select) %>% 
        tidyr::gather("variable", "sigma", !!!var_new) 
      
      suppressMessages(left_join(values, error))  %>% 
        mutate(variable = as.character(.data$variable)) %>% # don't like the factor it introduces
        return()
    }) %>% 
    filter(!is.na(value)) %>% # remove calculations that don't exist
    mutate(data_type = new_data_type)
  
  # filter out parts of it
  filter_new_quo <- rlang::enquo(filter_new)
  if (!rlang::quo_is_missing(filter_new_quo) && !rlang::quo_is_null(filter_new_quo)) {
    new_data_add <- filter(new_data, !!filter_new_quo)
  } else {
    new_data_add <- new_data
  }
  
  # info
  if (!quiet) {
    sprintf(
      paste0(
        "INFO: %d '%s' values + errors calculated, %d added (subset: %s)",
        "\n      values added (stored in 'variable' column): %s"),
      new_data %>%  nrow(), new_data_type, nrow(new_data_add),
      if(!rlang::quo_is_missing(filter_new_quo) && !rlang::quo_is_null(filter_new_quo)) 
        rlang::expr_deparse(filter_new_quo) else "all",
      new_data_add %>% 
        group_by(variable) %>% tally() %>%
        mutate(label = paste0("'", variable, "' (", n, "x)")) %>% 
        { .$label } %>% paste(collapse = ", ")
    ) %>% message()
  }
  
  # combine old data with new data
  bind_rows(
    data %>% filter(!variable %in% var_new), # make sure no duplicates
    new_data_add
  )
}


#' Calculate ion sums
#' 
#' This function calculates the ion sums and resulting counting
#' statistics error from multiple raw ion counts. It can be applied to data from both
#' LANS_summary and LANS_maps loading but can be slow if LANS_maps is combined
#' from many analyses. Careful about its error propagation, it assumes it is calculating
#' sums of ions and uses the ion counts themselves for error calculation. This is not 
#' suitable for calculating other types of sums where other types of error propagation
#' may be more appropriate.
#' 
#' @param data a data frame with raw ion counts retrieved from \code{\link{load_LANS_summary}}
#' @param ... the ion sums to calculate, each entry is for one sum of as many ions as desired,
#' e.g. c(`13C`, `12C`), c(`15N12C`, `14C12C`), ...
#' @param name_fun the naming function, receives ... from the top level, default concatenates column names with '+'
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @return the original data frame with the sums information appended (data_type == "ion_sum")
#' @family calculations
#' @export
calculate_sums <- function(data, ..., name_fun = default_name, quiet = FALSE) {
  
  # function to sum up arbitrary number of vectors by entry
  sum_vectors <- 
    function(...) { 
      r <- ..1
      for(i in list(...)[-1]) r <- r+i
      return(r)
    }
  
  # default sums name
  default_name <- function(...) {
    rlang::enexprs(...) %>% 
      sapply(rlang::as_label, simplify = TRUE) %>% 
      paste(collapse = "+")
  }
  
  # calculate sums
  calculate(
    data,
    data_type = "ion_sum",
    ...,
    value_fun = sum_vectors,
    error_fun = function(...) {
      iso.errN(sum_vectors(...))
    },
    name_fun = name_fun,
    quiet = quiet
  )
}

#' Calculate isotope ratios
#' 
#' This function calculates the ratios and resulting counting
#' statistics error from the raw ion counts. It can be applied to data from both
#' LANS_summary and LANS_maps loading but can be slow if LANS_maps is combined
#' from many analyses. It can also be applied to ion_sums generate by calculate_sums
#' to calculate elemental ratios (careful, ionization efficiencies skew their scaling!)
#' 
#' @param data a data frame with raw ion counts retrieved from \code{\link{load_LANS_summary}}
#' @param ... the ratios to calculate, each entry is one ratio with major isotope first, then
#' minor isotope, e.g. c(`13C`, `12C`), c(`15N12C`, `14C12C`), ...
#' @param name_fun the naming function, receives ... from the top level, default concatenates column names with '/'
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @return the original data frame with the ratio information appended (all ratios have data_type == "ratio")
#' @family calculations
#' @export
calculate_ratios <- function(data, ..., name_fun = default_name, quiet = FALSE) {
  
  # default name fun
  default_name <- function(m, M) paste0(deparse(substitute(m)),"/",deparse(substitute(M)))
  
  # calculate ratios
  calculate(
    data,
    data_type = "ratio",
    ...,
    value_fun = function(m, M) iso.R(M, m),
    error_fun = function(m, M) iso.errR(M, m),
    name_fun = name_fun,
    quiet = quiet
  )
}


#' Calculate isotope fractional abundances
#' 
#' This function calculates the isotope abundances (in %!) and resulting counting
#' statistics error from the raw ion counts. It can be applied to data from both
#' LANS_summary and LANS_maps loading but can be slow if LANS_maps is combined
#' from many analyses.
#' 
#' @param data a data frame with raw ion counts retrieved from \code{\link{load_LANS_summary}}
#' @param ... the fractional abundances to calculate, each entry is for one fractional abundance with major isotope first, then
#' minor isotope, e.g. c(`13C`, `12C`), c(`15N12C`, `14C12C`), ...
#' @param name_fun the naming function, receives ... from the top level, default concatenates 'F' + minor ion name
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @return the original data frame with the fractional abundance information appended (all fractional abundances are in % and have data_type == "abundance")
#' @family calculations
#' @export
calculate_abundances <- function(data, ..., name_fun = default_name, quiet = FALSE) {
  
  # default name fun
  default_name = function(m, M) paste(deparse(substitute(m)), "F")
  
  # calculate ratios
  calculate(
    data,
    data_type = "abundance",
    ...,
    value_fun = function(m, M) iso.F(M, m),
    error_fun = function(m, M) iso.errF(M, m),
    name_fun = name_fun,
    quiet = quiet
  )
  
}
