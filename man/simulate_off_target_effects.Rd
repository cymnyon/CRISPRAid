% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_off_target_effects.R
\name{simulate_off_target_effects}
\alias{simulate_off_target_effects}
\title{Simulate Off-Target Effects}
\usage{
simulate_off_target_effects(gRNA_seq, genome_length, num_sites = 10)
}
\arguments{
\item{gRNA_seq}{A character string representing the guide RNA sequence.}

\item{genome_length}{An integer representing the length of the genome (in base pairs).}

\item{num_sites}{The number of potential off-target sites to simulate.}
}
\value{
A data frame with positions and effect sizes for simulated off-target sites.
}
\description{
This function simulates potential off-target binding sites for a given
guide RNA (gRNA) sequence across a genome. It randomly generates binding
positions along the genome and assigns an effect size to each site,
indicating the likelihood or intensity of off-target binding.
This simulation can help researchers visualize potential unintended binding
locations and assess off-target risks.
}
