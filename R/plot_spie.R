#' @title Illustrate beading plot
#'
#' @author Enoch Kang
#'
#' @description
#' **PlotSpie()** is a function for illustrating spie plot.
#'
#' @param data DATA of metrics for treatment ranking.
#' @param color LIST of colors for outcomes in a network meta-analysis.
#'
#' @return
#' **PlotSpie()** returns a spie plot.
#'
#' @references
#' Daly, C. H., Mbuagbaw, L., Thabane, L., Straus, S. E., & Hamid, J. S. (2020).
#' Spie charts for quantifying treatment effectiveness and safety in multiple
#' outcome network meta-analysis: a proof-of-concept study.
#' **BMC Medical Research Methodology**, *20*, 1-13.
#'
#' @seealso \code{\link{GetMetrics}}, \code{\link{SetMetrics}},
#'          \code{\link{PlotBeads}}, \code{\link{PlotBar}},
#'          \code{\link{PlotLine}}, \code{\link{PlotHeat}}
#'
#' @examples
#' ## Not run:
#' #library(netmeta)
#' #data(Senn2013)
#' #nma <- netmeta(TE, seTE, treat1, treat2,
#' #studlab, data = Senn2013, sm = "SMD")
#'
#' # Get SUCRA
#' #nma.1 <- GetMetrics(nma, outcome = "HbA1c.random", prefer = "small", metrics = "SUCRA",
#' #model = "random", simt = 1000)
#' #nma.2 <- GetMetrics(nma, outcome = "HbA1c.common", prefer = "small", metrics = "SUCRA",
#' #model = "common", simt = 1000)
#'
#' # Combine metrics of multiple outcomes
#' #dataMetrics <- rbind(nma.1, nma.2)
#'
#' # Set data for rankinma
#' #dataRankinma <- SetMetrics(dataMetrics, tx = tx, outcome = outcome,
#' #metrics = SUCRA, metrics.name = "SUCRA")
#'
#' # Illustrate beading plot
#' #PlotSpie(data = dataRankinma)
#' ## End(Not run)
#'
#' @export PlotSpie

PlotSpie <- function(data,
                      color = NULL) {

  dataSpie <- data$data

  lgcInher  <- !inherits(data, "rankinma")
  lgcMtrcs  <- ifelse(data$metrics.name == "Probabilities",
                      TRUE,
                      FALSE)

  lgcColor  <- ifelse(length(which(ls()%in%ls(pattern = "color"))) > 0, FALSE,
                      ifelse(length(color) != data$n.outcome,
                             TRUE, FALSE))

  if (lgcInher) {
    cat(paste(" Inherit -------------------------------------------------- X\n",
              ' REQUIRE: Argument "data" must be an object of class \"rankinma\".'),
        fill = TRUE, sep = "")
  } else {
    cat(paste(" Inherit -------------------------------------------------- V"),
        fill = TRUE, sep = "")
  }

  if (lgcMtrcs) {
    cat(paste(" Metrics -------------------------------------------------- X\n",
              ' REQUIRE: Metrics should not be "Probabilities."'),
        fill = TRUE, sep = "")
  } else {
    cat(paste(" Metrics -------------------------------------------------- V"),
        fill = TRUE, sep = "")
  }

  if (lgcColor) {
    cat(paste(" Color ---------------------------------------------------- X\n",
              ' REQUIRE: Argument "color" must list colors for **EACH OUTCOME**.'),
        fill = TRUE, sep = "")
  } else {
    cat(paste(" Color ---------------------------------------------------- V"),
        fill = TRUE, sep = "")
  }

  if (lgcInher | lgcMtrcs | lgcColor)
    stop("Correct above mentioned problem(s).")

  dataSpie$importance  <- dataSpie$outcomes
  dataSpie$shape       <- 16

  dataSpie$seq.outcome <- max(dataSpie$outcomes) + 1 - dataSpie$outcomes
  dataSpie$seq.tx      <- max(dataSpie$txs) + 1 - dataSpie$txs

  dataSpie$seq.axis.y  <- dataSpie$seq.outcome
  dataSpie             <- dataSpie[order(-dataSpie$seq.axis.y), ]

  txs      <- unique(dataSpie$tx)
  outcomes <- unique(dataSpie$outcome)

  dataSpiePlot <- dataSpie


  colorOutcome <- data.frame(lsOutcome    = unique(dataSpiePlot$outcome),
                             seqOutcome   = c(1:length(unique(dataSpiePlot$outcome))),
                             colorOutcome = rainbow(length(unique(dataSpiePlot$outcome))))

  dataSpiePlot$colorOutcome <- NA

  if (!is.null(color)) {
    if (length(which(ls()%in%ls(pattern = "color"))) > 0) {
      colorOutcome$colorOutcome <- color
      for (color.i in c(1:nrow(dataSpiePlot))) {
        dataSpiePlot[color.i, "colorOutcome"] <- colorOutcome[which(dataSpiePlot[color.i, "outcome"] == colorOutcome$lsOutcome), "colorOutcome"]
      }
    }
  }

  setPar <- par(no.readonly = TRUE)
  on.exit(par(setPar))

  argPlotClm <- ifelse(length(unique(dataSpiePlot$tx)) < 3,
                     length(unique(dataSpiePlot$tx)), 3)
  argPlotRow <- ceiling((length(unique(dataSpiePlot$tx))+1)/3)

  par(mfrow = c(argPlotRow, argPlotClm),
      oma   = c(1, 1, 1, 4),
      mai   = c(0.3, 0.3, 0.3, 0.3))
  for (treat.i in c(1:length(unique(dataSpiePlot$tx)))) {
    TX.Spie <- unique(dataSpiePlot$tx)[treat.i]
    for (rank.i in c(1:length(dataSpiePlot[dataSpiePlot$tx == TX.Spie, "metrics"]))){
      if (rank.i > 1) {
        par(new = TRUE)
      }
      pie(table(dataSpiePlot[dataSpiePlot$tx == TX.Spie, "outcome"]) / sum(table(dataSpiePlot[dataSpiePlot$tx == TX.Spie, "outcome"])),
          labels = if(rank(dataSpiePlot[dataSpiePlot$tx == TX.Spie, "metrics"])[rank.i] == max(rank(dataSpiePlot[dataSpiePlot$tx == TX.Spie, "metrics"]))){
            paste(sprintf('%.2f',round(dataSpiePlot[dataSpiePlot$tx == TX.Spie, "metrics"],2)), sep = "")
          } else {""},
          radius = dataSpiePlot[dataSpiePlot$tx == TX.Spie, "metrics"][rank.i],
          border = c(rgb(0.3, 1, 1, 0)),
          col = c(rep(rgb(0.3, 1, 1, 0), rank.i - 1),
                  c(dataSpiePlot[dataSpiePlot$tx == TX.Spie, "colorOutcome"][rank.i]),
                  rep(rgb(0.3, 1, 1, 0), 4 - rank.i)))
    }
    title(TX.Spie,line = 0)
  }
  mtext(paste("Spie plot of ", data$metrics.name),
        font = 2, side = 1, outer = TRUE, line = -2)

  plot(3, 6, xlim = c(0, 3), ylim = c(0, 6),
       frame = FALSE, col = rgb(1, 1, 1, 1), axes = FALSE)
  title("Outcomes", #adj = 0,
        line = 0)
  segments(0.2,
           5 / max(colorOutcome$seqOutcome) * (max(colorOutcome$seqOutcome) + 1 - colorOutcome$seqOutcome),
           1,
           5 / max(colorOutcome$seqOutcome) * (max(colorOutcome$seqOutcome) + 1 - colorOutcome$seqOutcome),
           col = colorOutcome$colorOutcome,
           lwd = 10)
  text(1.2, 5 / max(colorOutcome$seqOutcome) * (max(colorOutcome$seqOutcome) + 1 - colorOutcome$seqOutcome),
       colorOutcome$lsOutcome, pos = 4)
}
