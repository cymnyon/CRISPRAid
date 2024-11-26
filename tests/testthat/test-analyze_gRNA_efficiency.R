test_that("analyze_gRNA_efficiency returns a data frame", {
  result <- analyze_gRNA_efficiency("GACGTCTAGT", "GACGTCTAGT")
  expect_s3_class(result, "data.frame")
  expect_named(result, c("gRNA", "target", "gc_content", "similarity_score", "predicted_efficiency"))
})

test_that("analyze_gRNA_efficiency handles invalid inputs", {
  expect_error(analyze_gRNA_efficiency(NULL, "TGCTACGTAGT"))
  expect_error(analyze_gRNA_efficiency("GACGTCTAGT", NULL))
})

test_that("analyze_gRNA_efficiency calculates efficiency scores", {
  gRNA_seq <- "GACGTCTAGT"
  target_seq <- "GACGTCTAGT"

  result <- analyze_gRNA_efficiency(gRNA_seq, target_seq)

  efficiency <- result$predicted_efficiency

  expect_true(is.numeric(efficiency))
  expect_length(efficiency, 1)
  expect_true(efficiency >= 0 && efficiency <= 100)
})
