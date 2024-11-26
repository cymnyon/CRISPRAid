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
#' @examples
#' seq_data <- data.frame(
#'   mutation_rate = c(0.05, 0.2, 0.15, 0.01, 0.3)
#' )
#' compute_indels(seq_data, indel_thres = 0.1)
compute_indels <- function(seq_data, indel_thres = 0.1) {
  # Check if seq_data is in data frame format and has the required column
  if (!is.data.frame(seq_data)) {
    stop("The input seq_data is not a data frame with a column mutation_rate")
  }
  if (!"mutation_rate" %in% names(seq_data)) {
    stop("The input seq_data does not have a column 'mutation_rate'")
  }
  if (!is.numeric(seq_data$mutation_rate)) {
    stop("The column mutation_rate in seq_data is not numeric")
  }

  #Check if indel_thres is a positive numeric type
  if (!is.numeric(indel_thres) || indel_thres < 0 || length(indel_thres) != 1) {
    stop("The input indel_thres is not a positive numeric type")
  }

  # Compute the number of indels and check that the mutation_rate exceeds indel_thres
  indel_num <- as.integer(sum(seq_data$mutation_rate > indel_thres))

  # Create a data frame and return it
  result_df <- data.frame(indel_count = indel_num)

  return(result_df)
}
