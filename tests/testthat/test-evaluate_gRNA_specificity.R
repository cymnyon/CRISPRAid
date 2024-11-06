test_that("evaluate_gRNA_specificity returns a data frame with correct columns", {
  gRNA_seq <- "GACGTCTAGT"
  off_target_seqs <- c("GACGTCTTGT", "TGCATCTAGG", "GACGTGTAGT")

  result <- evaluate_gRNA_specificity(gRNA_seq, off_target_seqs)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("off_target_seq", "specificity_score"))
  expect_type(result$specificity_score, "double")
})

test_that("evaluate_gRNA_specificity calculates specificity correctly", {
  gRNA_seq <- "GACGTCTAGT"
  off_target_seqs <- c("GACGTCTAGT", "GACGTATAGT")

  result <- evaluate_gRNA_specificity(gRNA_seq, off_target_seqs)

  expect_equal(result$specificity_score[1], 1)
})

test_that("evaluate_gRNA_specificity handles invalid inputs", {
  expect_error(evaluate_gRNA_specificity(NULL, c("GACGTCTAGT")))
  expect_error(evaluate_gRNA_specificity("GACGTCTAGT", NULL))
})
