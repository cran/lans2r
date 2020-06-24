context("Data transformations")

test_that("test that transformation safety checks are in place", {
  set.seed(123)
  expect_error(spread_data(tibble()), "column not in dataset: 'variable'")
  expect_error(spread_data(tibble(variable = "A")), "column not in dataset: 'value'")
  expect_error(spread_data(tibble(variable = "A", value = 5)), "column not in dataset: 'sigma'")
  expect_error(spread_data(tibble(variable = "A", value = 5, sigma = 1)), "column not in dataset: 'data_type'")
  expect_error(spread_data(tibble(variable = "A", value = 5, sigma = 1, data_type = "b"),
                           values = FALSE, errors = FALSE), "neither errors nor values")
  
  a <- tibble(variable = "A", ROI = 1:5, value = runif(5), sigma = 0.1, data_type = "temp")
  b <- tibble(variable = "B", ROI = 1:5, value = rnorm(5), sigma = 0.2, data_type = "temp")
  expect_equal(
    spread_data(bind_rows(a,b)) %>% select(ROI, A, B, `A sigma`, `B sigma`) %>% arrange(ROI),
    full_join(
      a %>% rename(A = value, `A sigma` = sigma) %>% select(-data_type, -variable),
      b %>% rename(B = value, `B sigma` = sigma) %>% select(-data_type, -variable),
      by = "ROI"
    ) %>% select(ROI, A, B, `A sigma`, `B sigma`) %>% arrange(ROI))
})