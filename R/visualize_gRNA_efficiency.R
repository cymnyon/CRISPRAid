#' Visualize gRNA Efficiency
#'
#' This function creates a heatmap to visually compare the efficiency
#' of multiple guide RNA (gRNA) sequences. Each gRNA is represented
#' with an efficiency score, and the heatmap provides a quick visual assessment
#' of high and low efficiency sequences, aiding in the selection
#' of optimal gRNAs for experiments. The output is a ggplot object,
#' making it easy to customize or integrate into reports.
#'
#' @param efficiency_data A data frame with columns for gRNA and efficiency scores.
#' @return A ggplot object displaying the heatmap.
#' @export
#' @import ggplot2
#' @examples
#' efficiency_data <- data.frame(
#'   gRNA = c("gRNA1", "gRNA2", "gRNA3"),
#'   efficiency = c(80, 90, 75)
#' )
#' visualize_gRNA_efficiency(efficiency_data)
visualize_gRNA_efficiency <- function(efficiency_data) {
  # Check if efficiency_data is a data frame and has correct columns
  if (!is.data.frame(efficiency_data)) {
    stop("The input efficiency_data is not a data frame")
  }
  if (!all(c("gRNA", "efficiency") %in% names(efficiency_data))) {
    stop("The input efficiency_data does not have columes 'gRNA' and 'efficiency'")
  }

  # Check if the column "efficiency" of efficiency_data is in numericc type
  if (!is.numeric(efficiency_data$efficiency)) {
    stop("The column 'efficiency' of efficiency_data is not in numeric type")
  }

  # Create a heatmap plot and return it
  plot_result <- ggplot2::ggplot(efficiency_data,
                                 ggplot2::aes(x = gRNA, y = 1, fill = efficiency)) +
    ggplot2::geom_tile(color = "#FFFFFF") +
    ggplot2::scale_fill_gradient(low = "#90EE90", high = "#006400", name = "Efficiency") +
    ggplot2::labs(title = "Heatmap of gRNA Efficiency",
         x = "gRNA",
         y = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )

    return(plot_result)
}
