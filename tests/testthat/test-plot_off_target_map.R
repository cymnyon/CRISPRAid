test_that("plot_off_target_map returns a plotly object", {
  off_target_data <- data.frame(position = 1:10, effect_size = runif(10))
  plot <- plot_off_target_map(off_target_data)
  expect_s3_class(plot, "plotly")
})

test_that("plot_off_target_map throws an error with invalid data", {
  expect_error(plot_off_target_map(NULL), "data frame")
  expect_error(plot_off_target_map(data.frame(position = 1:10,
                                              effect_size = letters[1:10])),
               "The column effect_size of off_target_data is not numeric")
})
