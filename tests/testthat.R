# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(CRISPRAid)

test_check("CRISPRAid")

# tests/testthat/test-analyze_gRNA_efficiency.R

test_that("analyze_gRNA_efficiency returns a data frame", {
  result <- analyze_gRNA_efficiency("GACGTCTAGT", "GACGTCTAGT")
  expect_s3_class(result, "data.frame")
  expect_named(result, c("gRNA", "target", "gc_content", "similarity_score", "predicted_efficiency"))
})

test_that("analyze_gRNA_efficiency handles invalid inputs", {
  expect_error(analyze_gRNA_efficiency(NULL, "TGCTACGTAGT"))
  expect_error(analyze_gRNA_efficiency("GACGTCTAGT", NULL))
})

# tests/testthat/visualize_gRNA_efficiency.R

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

# tests/testthat/test-plot_off_target_map.R

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

# tests/testthat/test-compute_indels.R

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
  expect_error(compute_indels(data.frame(mutation_rate = letters[1:4]), 0.1), "numeric")
})

# tests/testthat/test-evaluate_gRNA_specificity.R

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

# tests/testthat/test-simulate_off_target_effects.R

test_that("simulate_off_target_effects returns a data frame with correct columns", {
  gRNA_seq <- "GACGTCTAGT"
  genome_length <- 1000
  num_sites <- 10

  result <- simulate_off_target_effects(gRNA_seq, genome_length, num_sites)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("position", "effect_size"))

  expect_type(result$position, "integer")
  expect_type(result$effect_size, "double")
})

test_that("simulate_off_target_effects generates correct number of off-target sites", {
  gRNA_seq <- "GACGTCTAGT"
  genome_length <- 1000
  num_sites <- 5

  result <- simulate_off_target_effects(gRNA_seq, genome_length, num_sites)

  expect_equal(nrow(result), num_sites)
})

test_that("simulate_off_target_effects handles invalid inputs", {
  expect_error(simulate_off_target_effects("GACGTCTAGT", -100),
               "The input genome_length is not a positive integer nor in character string type")
  expect_error(simulate_off_target_effects("GACGTCTAGT", 1000, -5),
               "The input num_sites is not a positive integer nor in character string type")
  expect_error(simulate_off_target_effects(c("GACGTCTAGT", "TGCATCTAGG"), 1000, 5),
               "The input gRNA_seq is not a single character string")
})

# tests/testthat/test-visualize_gRNA_base_composition.R

test_that("visualize_gRNA_base_composition returns a ggplot object", {
  gRNA_seqs <- c("GACGTCTAGT", "TGCATCTAGG", "GACGTGTAGT")

  plot <- visualize_gRNA_base_composition(gRNA_seqs)

  expect_s3_class(plot, "ggplot")
})

test_that("visualize_gRNA_base_composition handles invalid inputs", {
  expect_error(visualize_gRNA_base_composition(NULL))
  expect_error(visualize_gRNA_base_composition(c("GACGTCTAGT", 12345)),
               "All elements in gRNA_seqs are not character strings")
})



