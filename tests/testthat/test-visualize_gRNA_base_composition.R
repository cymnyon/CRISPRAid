test_that("visualize_gRNA_base_composition returns a ggplot object", {
  gRNA_seqs <- c("GACGTCTAGT", "TGCATCTAGG", "GACGTGTAGT")

  plot <- visualize_gRNA_base_composition(gRNA_seqs)

  expect_s3_class(plot, "ggplot")
})

test_that("visualize_gRNA_base_composition handles invalid inputs", {
  expect_error(visualize_gRNA_base_composition(NULL),
               "The input gRNA_seqs is not a character vector")
  expect_error(visualize_gRNA_base_composition(c("GACGTCTAGT", 12345)),
               "The input gRNA_seqs is not a character vector")
})

test_that("visualize_gRNA_base_composition works with gRNA_base_composition.csv", {
  gRNA_data <- read.csv(system.file("extdata", "gRNA_base_composition.csv", package = "CRISPRAid"))
  plot <- visualize_gRNA_base_composition(gRNA_data$gRNA)
  expect_s3_class(plot, "ggplot")
})
