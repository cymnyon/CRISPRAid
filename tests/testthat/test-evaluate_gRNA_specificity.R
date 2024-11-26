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

test_that("evaluate_gRNA_specificity calculates specificity scores", {
  specificity_data <- read.csv(system.file("extdata", "evaluate_gRNA_specificity.csv", package = "CRISPRAid"))
  gRNA_seq <- as.character(specificity_data$gRNA_seq[1])
  off_target_seqs <- as.character(specificity_data$off_target_seqs)

  result <- evaluate_gRNA_specificity(gRNA_seq, off_target_seqs)

  expect_true("specificity_score" %in% colnames(result))
  expect_true(all(!is.na(result$specificity_score)))
  expect_true(max(result$specificity_score) <= 1)
})
