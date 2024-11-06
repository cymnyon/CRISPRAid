#' Plot Off-Target Effects
#'
#' This function creates an interactive plot showing the positions and
#' effect sizes of potential off-target binding sites across a genome.
#' The plot provides an easy-to-navigate visualization, allowing users
#' to explore regions of the genome where off-target effects may occur,
#' with effect sizes representing the likelihood or intensity of off-target
#' binding. This can help researchers identify critical areas for further
#' analysis.
#'
#' @param off_target_data Data frame with information on off-target sites.
#' @return A plotly object for interactive exploration.
#' @export
plot_off_target_map <- function(off_target_data) {
  # Check if off_target_data is a data frame and in a correct format
  if (!is.data.frame(off_target_data)) {
    stop("The input off_target_data is not a data frame")
  }
  if (!all(c("position", "effect_size") %in% names(off_target_data))) {
    stop("The input off_target_data does not have columns 'position' and 'effect_size'")
  }

  # Check if the column effect_size of off_target_data is numeric type
  if (!is.numeric(off_target_data$effect_size)) {
    stop("The column effect_size of off_target_data is not numeric")
  }

  library(plotly)
  # Create a plot of off-target effects and return it
  plot_result <- plot_ly(
    off_target_data,
    x = ~position,
    y = ~effect_size,
    type = 'scatter',
    mode = 'markers',
    marker = list(size = 8, color = ~effect_size, colorscale = 'Viridis', showscale = TRUE),
    text = ~paste("Position: ", position, "<br>Effect Size: ", round(effect_size, 2))
  ) %>%
    layout(
      title = "Map of Off-Target Effects",
      xaxis = list(title = "Genomic Position"),
      yaxis = list(title = "Effect Size"),
      colorbar = list(title = "Effect Size")
    )

  return (plot_result)
}
