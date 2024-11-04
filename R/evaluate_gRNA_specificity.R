#' Evaluate gRNA Specificity
#'
#' This function calculates the specificity score of a guide RNA (gRNA) sequence
#' by comparing it with a list of potential off-target sequences. The specificity
#' score represents the similarity between the gRNA and each off-target sequence,
#' helping to identify sequences that may lead to unintended edits.
#' Higher specificity scores indicate a greater likelihood of off-target binding.
#'
#' @param gRNA_seq A character string representing the guide RNA sequence.
#' @param off_target_seqs A character vector of potential off-target sequences.
#' @return A data frame with off-target sequences and their similarity scores.
#' @export
evaluate_gRNA_specificity <- function(gRNA_seq, off_target_seqs) {
  # Check if gRNA_seq is in a single character string and
  # off_target_seqs is in character string type
  if (!is.character(gRNA_seq) || !is.character(off_target_seqs)) {
    stop("The input gRNA_seq and target_seq are not in character string type")
  }
  if (length(gRNA_seq) != 1) {
    stop("gRNA_seq is not a single character string")
  }

  # Calculate the similarity score for each of the given off_target_seqs
  similarity_scores <- sapply(off_target_seqs, function(seq) {
    # Check if each seq has the same length as the gRNA_seq
    if (nchar(seq) != nchar(gRNA_seq)) {
      return(NA)
    }

    # Calculate the proportion of matching bases
    match_bases <- sum(strsplit(gRNA_seq, "")[[1]] == strsplit(seq, "")[[1]])
    similarity_score <- match_bases / nchar(gRNA_seq)
    return(similarity_score)
  })

  # Create a data frame and return it
  result <- data.frame(
    off_target_seq = off_target_seqs,
    specificity_score = round(similarity_scores, 2)
  )

  return(result)
}
