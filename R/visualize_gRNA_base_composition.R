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
#' @import ggplot2
#' @examples
#' gRNA_seqs <- c("GACGTCTAGT", "TGCATCTAGG", "GACGTGTAGT")
#' visualize_gRNA_base_composition(gRNA_seqs)
visualize_gRNA_base_composition <- function(gRNA_seqs) {
  # Check if gRNA_seqs is character vector
  if (!is.character(gRNA_seqs) || any(!grepl("^[ATGC]*$", gRNA_seqs))) {
    stop("The input gRNA_seqs is not a character vector")
  }

  # Gather all the sequences into a string
  # then split into individual bases
  bases <- unlist(strsplit(gRNA_seqs, ""))

  # Keep the count for each base
  base_count <- table(bases)
  base_df <- as.data.frame(base_count)

  colnames(base_df) <- c("Base", "Count")

  # Create a plot and return it
  plot_result <- ggplot2::ggplot(base_df,
                                 ggplot2::aes(x = Base, y = Count, fill = Base)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values = c("A" = "#87CEEB", "T" = "#FA8072", "G" = "#90EE90", "C" = "#FFA07A")) +
    ggplot2::labs(title = "gRNA Base Composition",
         x = "Base",
         y = "Count")

  return(plot_result)
}
