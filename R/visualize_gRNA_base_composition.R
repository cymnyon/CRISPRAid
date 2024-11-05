#' Visualize gRNA Base Composition
#'
#' This function generates a bar plot displaying the base composition
#' (A, T, G, C) of a set of guide RNA (gRNA) sequences. By summarizing
#' the frequency of each base, this plot provides insights into the nucleotide
#' makeup of the gRNA sequences, which can impact binding stability and efficiency.
#' The output is a ggplot object, making it easy to customize or integrate into
#' reports.
#'
#' @param gRNA_seqs A character vector of gRNA sequences.
#' @return A ggplot object showing base composition.
#' @export
visualize_gRNA_base_composition <- function(gRNA_seqs) {
  # Check if gRNA_seqs is character vector
  if (!is.character(gRNA_seqs)) {
    stop("The input gRNA_seqs is not a character vector")
  }

  # Gather all the sequences into a string
  # then split into individual bases
  bases <- unlist(strsplit(gRNA_seqs, ""))

  # Keep the count for each base
  base_count <- table(bases)
  base_data_frame <- as.data.frame(base_count)

  colnames(base_data_frame) <- c("base", "frequency")

  library(ggplot2)
  # Create a plot and return it
  plot_result <- ggplot(base_data_frame, aes(x = base, y = frequency, fill = base)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    scale_fill_manual(values = c("A" = "#FA8072", "T" = "#87CEEB", "G" = "#90EE90", "C" = "#FFA07A")) +
    labs(title = "gRNA Base Composition",
         x = "Base",
         y = "Frequency")

  return(plot_result)
}
