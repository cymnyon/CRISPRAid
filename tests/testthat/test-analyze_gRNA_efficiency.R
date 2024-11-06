test_that("analyze_gRNA_efficiency returns a data frame", {
  result <- analyze_gRNA_efficiency("GACGTCTAGT", "GACGTCTAGT")
  expect_s3_class(result, "data.frame")
  expect_named(result, c("gRNA", "target", "gc_content", "similarity_score", "predicted_efficiency"))
})

test_that("analyze_gRNA_efficiency handles invalid inputs", {
  expect_error(analyze_gRNA_efficiency(NULL, "TGCTACGTAGT"))
  expect_error(analyze_gRNA_efficiency("GACGTCTAGT", NULL))
})
