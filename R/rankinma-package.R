#' rankinma: Rank in Network Meta-Analysis
#'
#' @description
#' \emph{rankinma} is an R package that supports users to easily obtain and
#' visualize various metrics of treatment ranking from network meta-analysis
#' no matter using either frequentist or Bayesian approach. Development of package
#' \emph{rankinma} is based on \bold{R version 4.2.2 (2022-10-31 ucrt)}.
#' Extra imported packages are as follows:
#'
#' \itemize{
#'  \item \href{https://cran.r-project.org/src/contrib/Archive/mvtnorm/mvtnorm_1.1-3.tar.gz}{\emph{mvtnorm} (version 1.1-3)}
#'  \item \href{https://cran.r-project.org/src/contrib/Archive/netmeta/netmeta_2.6-0.tar.gz}{\emph{netmeta} (version 2.6-0)}
#' }
#'
#'
#' @details
#' Current version consists of seven functions, including two functions for
#' data preparation (function \code{\link{GetMetrics}} and \code{\link{SetMetrics}})
#' and five functions for visualization of treatment ranking metrics (i.e.
#' \code{\link{PlotBeads}}, \code{\link{PlotLine}}, \code{\link{PlotBar}},
#' \code{\link{PlotHeat}}, and \code{\link{PlotSpie}}). Probabilities of treatments
#' on each possible rank can be visualized using \code{\link{PlotLine}} and \code{\link{PlotBar}}.
#' Due to concise information, \code{\link{PlotBeads}} is recommended to be used for
#' global metrics of treatment ranking, such as \bold{P-score} and \bold{SUCRA}. The other four
#' visualization functions can also generate graphics of the global metrics.
#'
#' @references
#' 1. Salanti, G., Ades, A. E., & Ioannidis, J. P. (2011). Graphical methods and
#' numerical summaries for presenting results from multiple-treatment meta-analysis:
#' an overview and tutorial. **Journal of clinical epidemiology**, *64(2)*, 163-171.
#'
#' 2. Chaimani, A., Higgins, J. P., Mavridis, D., Spyridonos, P., & Salanti, G.
#' (2013). Graphical tools for network meta-analysis in STATA. **PloS one**, *8(10)*,
#' e76654.
#'
#' 3. Van Valkenhoef, G., Tervonen, T., Zwinkels, T., De Brock, B., & Hillege, H.
#' (2013). ADDIS: a decision support system for evidence-based medicine.
#' **Decision Support Systems**, *55(2)*, 459-475.
#'
#' 4. Rücker, G., & Schwarzer, G. (2015). Ranking treatments in frequentist network
#' meta-analysis works without resampling methods. **BMC medical research methodology**,
#' *15(1)*, 1-9.
#'
#' 5. Daly, C. H., Mbuagbaw, L., Thabane, L., Straus, S. E., & Hamid, J. S. (2020).
#' Spie charts for quantifying treatment effectiveness and safety in multiple outcome
#' network meta-analysis: a proof-of-concept study. **BMC Medical Research Methodology**,
#' *20*, 1-13.
#'
#' 6. Balduzzi, S., Rücker, G., Nikolakopoulou, A., Papakonstantinou, T., Salanti, G.,
#' Efthimiou, O., & Schwarzer, G. (2023). netmeta: An R package for network meta-analysis
#' using frequentist methods. **Journal of Statistical Software**, *106*, 1-40.
#'
#'
#' @name rankinma-package
#'
#' @docType package
#'
#' @keywords package
#'
#'
## usethis namespace: start
#' @importFrom graphics axis
#' @importFrom graphics barplot
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom graphics pie
#' @importFrom graphics points
#' @importFrom graphics rect
#' @importFrom graphics segments
#' @importFrom graphics text
#' @importFrom graphics title
#' @importFrom grDevices col2rgb
#' @importFrom grDevices palette.colors
#' @importFrom grDevices rainbow
#' @importFrom grDevices recordPlot
#' @importFrom grDevices rgb
#' @importFrom stats aggregate
#' @importFrom utils capture.output
## usethis namespace: end
NULL
