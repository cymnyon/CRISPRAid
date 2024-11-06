test_that("visualize_gRNA_efficiency returns a ggplot object", {
  efficiency_data <- data.frame(gRNA = c("gRNA1", "gRNA2"), efficiency = c(80, 90))
  plot <- visualize_gRNA_efficiency(efficiency_data)
  expect_s3_class(plot, "ggplot")
})

test_that("visualize_gRNA_efficiency throws an error with invalid data", {
  expect_error(visualize_gRNA_efficiency(NULL), "data frame")
  expect_error(visualize_gRNA_efficiency(data.frame(gRNA = c("gRNA1"), efficiency = c("high"))),
               "numeric")
})
