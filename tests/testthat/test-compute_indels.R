test_that("compute_indels returns a data frame with indel counts", {
  seq_data <- data.frame(mutation_rate = c(0.05, 0.2, 0.15, 0.01))
  result <- compute_indels(seq_data, indel_thres = 0.1)
  expect_s3_class(result, "data.frame")
  expect_named(result, "indel_count")
  expect_type(result$indel_count, "integer")
})

test_that("compute_indels correctly counts indels above threshold", {
  seq_data <- data.frame(mutation_rate = c(0.05, 0.2, 0.15, 0.01))
  result <- compute_indels(seq_data, indel_thres = 0.1)
  expect_equal(result$indel_count, 2)
})

test_that("compute_indels handles invalid inputs", {
  expect_error(compute_indels(NULL, 0.1), "data frame")
})

test_that("compute_indels calculates indel counts", {
  indel_data <- read.csv(system.file("extdata", "compute_indels.csv", package = "CRISPRAid"))
  result <- compute_indels(indel_data, indel_thres = 0.1)

  expect_true("indel_count" %in% colnames(result))
  expect_true(is.numeric(result$indel_count))
  expect_true(result$indel_count > 0)
})
