#' Compute Indels
#'
#' This function calculates the frequency of insertions and deletions (indels)
#' in sequencing data based on a specified threshold. Indels are identified
#' when the mutation rate at a given position exceeds the threshold,
#' helping researchers quantify mutation prevalence. The output is a data frame
#' summarizing the total count of indels detected.
#'
#' @param seq_data A data frame of sequencing results.
#' @param indel_thres Numeric threshold for identifying indels.
#' @return A data frame with indel counts.
#' @export
compute_indels <- function(seq_data, indel_thres = 0.1) {
  # Check if seq_data is in data frame format and has the required column
  if (!is.data.frame(seq_data)) {
    stop("The input seq_data is not a data frame")
  }
  if (!"mutation_rate" %in% names(seq_data)) {
    stop("The input seq_data does not have a column 'mutation_rate'")
  }

  #Check if indel_thres is a positive numeric type
  if (!is.numeric(indel_thres) || indel_thres < 0 || length(indel_thres) != 1) {
    stop("The input indel_thres is not a positive numeric type")
  }

  # Compute the number of indels and check that the mutation_rate exceeds indel_thres
  indel_num <- sum(seq_data$mutation_rate > indel_thres, na.rm = TRUE)

  # Create a data frame and return it
  result_df <- data.frame(indel_count = indel_num)

  return(result_df)
}
