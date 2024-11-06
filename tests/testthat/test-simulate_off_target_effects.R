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
