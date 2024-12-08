% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_off_target_map.R
\name{plot_off_target_map}
\alias{plot_off_target_map}
\title{Plot Off-Target Effects}
\usage{
plot_off_target_map(off_target_data)
}
\arguments{
\item{off_target_data}{Data frame with information on off-target sites.}
}
\value{
A plotly object for interactive exploration.
}
\description{
This function creates an interactive plot showing the positions and
effect sizes of potential off-target binding sites across a genome.
The plot provides an easy-to-navigate visualization, allowing users
to explore regions of the genome where off-target effects may occur,
with effect sizes representing the likelihood or intensity of off-target
binding. This can help researchers identify critical areas for further
analysis.
}
