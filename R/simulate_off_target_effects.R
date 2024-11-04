#' Simulate Off-Target Effects
#'
#' This function simulates potential off-target binding sites for a given
#' guide RNA (gRNA) sequence across a genome. It randomly generates binding
#' positions along the genome and assigns an effect size to each site,
#' indicating the likelihood or intensity of off-target binding.
#' This simulation can help researchers visualize potential unintended binding
#' locations and assess off-target risks.
#'
#' @param gRNA_seq A character string representing the guide RNA sequence.
#' @param genome_length An integer representing the length of the genome (in base pairs).
#' @param num_sites The number of potential off-target sites to simulate.
#' @return A data frame with positions and effect sizes for simulated off-target sites.
#' @export
simulate_off_target_effects <- function(gRNA_seq, genome_length, num_sites = 10) {
  # Check if gRNA_seq is in a single character string and
  # genome_length and num_sites are character string type and positive integer
  if (!is.character(gRNA_seq) || !length(gRNA_seq) != 1) {
    stop("gRNA_seq is not a single character string")
  }
  if (!is.numeric(genome_length) || genome_length <= 0) {
    stop("genome_length is not a positive integer nor in character string type")
  }
  if (!is.numeric(num_sites) || num_sites <= 0) {
    stop("num_sites is not a positive integer nor in character string type")
  }

  # Generate random positions for off-target sites within the genome length
  pos <- sample(1:genome_length, num_sites, replace = FALSE)

  # Assign random effect sizes between 0 and 1 to each off-target site
  effect_sizes <- runif(num_sites, min = 0, max = 1)

  # Create a data frame and return it
  result <- data.frame(
    position = pos,
    effect_size = round(effect_sizes, 2)
  )

  return(result)
}
