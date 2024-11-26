#' Analyze gRNA Efficiency
#'
#' This function predicts the editing efficiency of a specified guide RNA (gRNA)
#' sequence against a target DNA sequence, providing insight into the likelihood
#' of successful CRISPR editing at the target site. Efficiency is calculated
#' based on sequence properties, such as GC content and sequence similarity,
#' or by using a provided predictive model. The output includes the gRNA sequence,
#' target sequence, and a predicted efficiency score, which can guide researchers
#' in selecting optimal gRNA candidates.
#'
#' @param gRNA_seq A character string representing the guide RNA sequence.
#' @param target_seq A character string representing the target DNA sequence.
#' @param model A model object (optional) for prediction.
#' @return A data frame with columns for gRNA sequence and predicted efficiency.
#' @export
#' @importFrom stats predict
#' @examples
#' gRNA_seq <- "GACGTCTAGT"
#' target_seq <- "GACGTCTAGT"
#' analyze_gRNA_efficiency(gRNA_seq, target_seq)
analyze_gRNA_efficiency <- function(gRNA_seq, target_seq, model = NULL) {
  # Check if gRNA_seq and target_seq are character strings
  if (!is.character(gRNA_seq) || !is.character(target_seq)) {
    stop("The input gRNA_seq and target_seq are not in character string type")
  }

  # Check if gRNA_seq and target_seq have the same length
  if (nchar(gRNA_seq) != nchar(target_seq)) {
    stop("The input gRNA_seq and target_seq are not in the same length")
  }

  # Calculate GC content for the given gRNA_seq
  gc_content <- sum(strsplit(gRNA_seq, "")[[1]] %in% c("G", "C")) / nchar(gRNA_seq)

  # Calculate sequence similarity as the proportion of matching bases
  match_bases <- sum(strsplit(gRNA_seq, "")[[1]] == strsplit(target_seq, "")[[1]])
  similarity_score <- match_bases / nchar(gRNA_seq)

  # Placeholder for a model-based prediction or a simple linear combination if no model
  if (!is.null(model)) {
    # If model is present, include it to predict efficiency
    efficiency <- predict(model, newdata = data.frame(gc_content = gc_content, similarity_score = similarity_score))
  } else {
    # if model is not present, predict efficiency with a weighted formula
    efficiency <- 0.4 * similarity_score + 0.6 * gc_content
  }

  # Create a data frame and return it
  result <- data.frame(
    gRNA = gRNA_seq,
    target = target_seq,
    gc_content = round(gc_content, 2),
    similarity_score = round(similarity_score, 2),
    predicted_efficiency = round(100 * efficiency, 2)
  )

  return(result)
}
