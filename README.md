
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CRISPRAid

<!-- badges: start -->
<!-- badges: end -->

CRISPRAid is an R package designed to facilitate comprehensive analysis
of CRISPR-Cas9 guide RNA (gRNA) data, enabling researchers to evaluate
gRNA efficiency, assess off-target risks, analyze sequence composition,
and visualize results. CRISPRAid includes tools for simulating
off-target effects, computing mutation-induced insertions and deletions
(indels), and generating informative plots to support data
interpretation and decision-making in CRISPR experiments.

The package was developed using R version 4.3.1 and has been optimized
to work efficiently on this platform.

## Installation

To install the latest version of the package:

``` r
require("devtools")
install_github("cymnyon/CRISPRAid", build_vignettes = TRUE)
library("CRISPRAid")
```

To run the Shiny app:

``` r
CRISPRAid::runCRISPRAid()
```

## Overview

To list all functions available in the package:

``` r
ls("package:CRISPRAid")
```

CRISPRAid includes the following functions:

1.  `analyze_gRNA_efficiency` for computing the efficiency of gRNA
    sequences, helping determine their suitability for targeting.

2.  `evaluate_gRNA_specificity` for assessing gRNA specificity against
    target sequences to reduce off-target effects.

3.  `compute_indels` for calculating the frequency of indels in
    sequencing data, aiding in mutation rate analysis.

4.  `simulate_off_target_effects` for simulating potential off-target
    binding across a genome, providing a risk profile for each gRNA.

5.  `visualize_gRNA_efficiency` for generating a heatmap of gRNA
    efficiencies for visual comparison.

6.  `visualize_gRNA_base_composition` for creating a bar plot to display
    nucleotide composition (A, T, G, C) of gRNA sequences.

7.  `plot_off_target_map` for mapping predicted off-target sites across
    a genome, offering insights into binding distribution.

### Workflow Diagram

<figure>
<img src="man/figures/CRISPRAid.png" alt="CRISPRAid Workflow Diagram" />
<figcaption aria-hidden="true">CRISPRAid Workflow Diagram</figcaption>
</figure>

## Contributions

The author of the package is **Soomi Choi**. The author conceptualized
and developed CRISPRAid to facilitate the analysis of CRISPR-Cas9 guide
RNA data. Below is a breakdown of the author’s contributions and
external influences for each function:

**Core Functions**

- `analyze_gRNA_efficiency` was developed by the author to compute the
  efficiency of gRNA sequences. Inspired by off-target modeling
  techniques in CRISPR research.

- `evaluate_gRNA_specificity` was designed by author to assess gRNA
  specificity, reducing off-target effects. This function relies on
  **Biostrings** for sequence handling and mismatch evaluation.

- `compute_indels` was designed by author to calculate indel frequencies
  from sequencing data.

- `simulate_off_target_effects` was written to simulate potential
  off-target sites across a genome using random sampling and probability
  models. Inspired by methods in the **CRISPRseek** package.

**Visualization Functions**

- `visualize_gRNA_base_composition` was written to generate bar plots of
  nucleotide composition for gRNA sequences. Built using **ggplot2** for
  visualization.

- `visualize_gRNA_efficiency`. was developed to create heatmaps for
  visual comparison of gRNA efficiency. Relies on **ggplot2** for clear
  and customizable visual output.

- `plot_off_target_map` was developed to map predicted off-target sites
  across a genome. Built using **plotly** as well for visuals.

Generative AI tool (ChatGPT) was used to assist with:

- Structuring function logic for `analyze_gRNA_efficiency` and
  `evaluate_gRNA_specificity`

- Generating comments and simple examples within functions to clarify
  their purposes

- Debugging errors within development of the functions

## References

- R Core Team. 2023. *R: A language and environment for statistical
  computing.* R Foundation for Statistical Computing, Vienna, Austria.
  URL <https://www.R-project.org/>.

- Wickham, H. 2016. *ggplot2: Elegant Graphics for Data Analysis.*
  Springer-Verlag New York. URL <https://ggplot2.tidyverse.org>.

- Zhu, L.J., Gazin, C., Lawson, N.D., Pagès, H., Lin, S.M., Lapointe,
  D.S., and Green, M.R. 2014. “CRISPRseek: a Bioconductor package to
  identify off-targets of guide RNAs generated by CRISPR-Cas9
  technology.” *BMC Bioinformatics* 15 (1): 142.
  <https://bioconductor.org/packages/CRISPRseek/>.

- Pagès, H., Aboyoun, P., Gentleman, R., and DebRoy, S. 2023.
  *Biostrings: Efficient manipulation of biological strings.* R package
  version 2.69.2, Bioconductor.
  <https://bioconductor.org/packages/Biostrings/>.

- Jinek, M., Chylinski, K., Fonfara, I., Hauer, M., Doudna, J.A., and
  Charpentier, E. 2012. “A programmable dual-RNA–guided DNA endonuclease
  in adaptive bacterial immunity.” *Science* 337 (6096): 816-821.

- Hsu, P.D., Lander, E.S., and Zhang, F. 2014. “DNA targeting
  specificity of RNA-guided Cas9 nucleases.” *Nature Biotechnology* 31
  (9): 827-832.

- Fu, Y., Foden, J.A., Khayter, C., Maeder, M.L., Reyon, D., Joung,
  J.K., and Sander, J.D. 2013. “High-frequency off-target mutagenesis
  induced by CRISPR-Cas nucleases in human cells.” *Nature
  Biotechnology* 31 (9): 822-826.

- Doench, J.G., Fusi, N., Sullender, M., Hegde, M., Vaimberg, E.W.,
  Donovan, K.F., Smith, I., et al. 2016. “Optimized sgRNA design to
  maximize activity and minimize off-target effects of CRISPR-Cas9.”
  *Nature Biotechnology* 34 (2): 184-191.

- Jiang, F., and Doudna, J.A. 2017. “CRISPR–Cas9 Structures and
  Mechanisms.” *Annual Review of Biophysics* 46: 505-529.

- Silva A. (2019). GitHub - anjalisilva/TestingPackage: R Package
  Illustrating Components of an R package for BCB410H - Applied
  Bioinformatics (2019-2023), University of Toronto, Canada. GitHub.
  <https://github.com/anjalisilva/TestingPackage>

- OpenAI. (2024). *ChatGPT (Version 3.5)*. Retrieved from
  <https://chat.openai.com/chat>

## Acknowledgements

This package was developed as part of an assessment for 2024 BCB410H:
Applied Bioinformatics course at the University of Toronto, Toronto,
CANADA. CRISPRAid welcomes issues, enhancement requests, and other
contributions. To subit an issue, use the GitHub issues.
