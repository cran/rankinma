#' @title Illustrate heat plot for treatment ranking
#'
#' @author Enoch Kang
#'
#' @description
#' **PlotHeat()** is a function for illustrating heat plot.
#'
#' @param data DATA of metrics for treatment ranking.
#' @param sorttx LOGIC value for indicating whether sort heat plot by treatments.
#'
#' @return
#' **PlotHeat()** returns a heat plot.
#'
#' @seealso \code{\link{GetMetrics}}, \code{\link{SetMetrics}},
#'          \code{\link{PlotBeads}}, \code{\link{PlotBar}},
#'          \code{\link{PlotLine}}, \code{\link{PlotSpie}}
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
#' # Illustrate heat plot
#' #PlotHeat(data = dataRankinma)
#' ## End(Not run)
#'
#' @export PlotHeat

PlotHeat <- function(data,
                     sorttx = NULL) {

  dataHeat  <- data$data
  lsMetrics <- c("P-best", "SUCRA", "P-score")

  argSorttx <- ifelse(is.null(sorttx), FALSE,
                      ifelse(sorttx == FALSE, FALSE,
                             ifelse(sorttx == TRUE, TRUE,
                                    "ERROR")))

  lgcInher  <- !inherits(data, "rankinma")
  lgcMtrcs  <- ifelse(data$metrics.name == "Probabilities",
                      TRUE,
                      FALSE)

  lgcSorttx   <- ifelse(is.null(sorttx), FALSE,
                        ifelse(argSorttx == TRUE, FALSE,
                               ifelse(argSorttx == FALSE, FALSE,
                                      TRUE)))

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

  if (lgcSorttx) {
    cat(paste(" Sort ----------------------------------------------------- !\n",
              ' INFORM: Argument "sorttx" should be logit value, and *rankinma*
        is producing bar charts with default argument in terms of `sorttx = FALSE`
        now.'),
        fill = TRUE, sep = "")
  } else {
    cat(paste(" Sort ----------------------------------------------------- V"),
        fill = TRUE, sep = "")
  }


  if (lgcInher | lgcMtrcs | lgcSorttx)
    stop("Correct above mentioned problem(s).")

  dataHeat$seq.outcome <- max(dataHeat$outcomes) + 1 - dataHeat$outcomes
  dataHeat$seq.tx      <- max(dataHeat$txs) + 1 - dataHeat$txs

  txs      <- unique(dataHeat$tx)
  outcomes <- unique(dataHeat$outcome)

  dataSmry <- cbind(
    aggregate(dataHeat$txs, by = list(dataHeat$outcome), "length"),
    aggregate(dataHeat$outcomes, by = list(dataHeat$outcome), "unique"),
    aggregate(dataHeat$tx, by = list(dataHeat$outcome), "paste")
  )


  dataSmry$name.tx <- NA
  for (outcome.i in (1:nrow(dataSmry))) {
    dataSmry[outcome.i, "name.tx"] <- capture.output(cat(
      dataHeat[dataHeat$outcome == dataSmry[outcome.i, "Group.1"], "tx"],
      sep = ", "))
  }

  if (length(dataSmry) > 6) {
    dataSmry <- dataSmry[c(1, 2, 4, length(dataSmry))]
  }

  colnames(dataSmry)[c(1:3)] <- c("outcome", "n.tx", "seq.otcm")

  dataSmry$seq.otcm <- max(dataSmry$seq.otcm) + 1 - dataSmry$seq.otcm


  setPar <- par(no.readonly = TRUE)
  on.exit(par(setPar))

  par(mar = c(5, 5, 3, 5), xpd = TRUE)

  plot(c(0, max(dataSmry$n.tx)), c(0, length(outcomes)),
       frame.plot = FALSE, xaxt = "n", yaxt = "n",
       xlab = "", ylab = "", type = "l", lty = 0)

  text(max(dataSmry$n.tx) / 2, 0,
       paste("Summary of ", data$metrics.name, sep = ""),
       cex = 1.2, type = 2)

  text(0, dataSmry$seq.otcm - 0.25,
       dataSmry$outcome, pos = 2)

  for (outcome.i in c(1:length(outcomes))) {
    dataHeatPlot <- dataHeat[dataHeat$outcomes == outcome.i, ]

    if (argSorttx == FALSE) {
      dataHeatPlot <- dataHeatPlot[order(-dataHeatPlot$metrics),]
      dataHeatPlot$seq.metrics <- c(1:nrow(dataHeatPlot))

      dataHeatPlot$xlft <- (max(dataSmry$n.tx) / length(dataHeatPlot$txs)) * dataHeatPlot$seq.metrics - (max(dataSmry$n.tx) / length(dataHeatPlot$txs))
      dataHeatPlot$xrgt <- (max(dataSmry$n.tx) / length(dataHeatPlot$txs)) * dataHeatPlot$seq.metrics
    } else {
      dataHeatPlot$xlft <- dataHeatPlot$txs - 1
      dataHeatPlot$xrgt <- dataHeatPlot$txs
    }

    dataHeatPlot$ybtm <- dataHeatPlot$seq.outcome - 0.5
    dataHeatPlot$ytop <- dataHeatPlot$seq.outcome

    for (tx.i in c(dataHeatPlot$txs)) {
      rect(dataHeatPlot$xlft,
           dataHeatPlot$ybtm,
           dataHeatPlot$xrgt,
           dataHeatPlot$ytop,
           lty = 0,
           col = rgb(1 - dataHeatPlot$metrics,
                     dataHeatPlot$metrics^3,
                     dataHeatPlot$metrics,
                     0.5))
      text(dataHeatPlot$xlft,
           dataHeatPlot$ybtm - 0.2,
           dataHeatPlot$tx,
           pos = 4)

      text(dataHeatPlot$xlft,
           dataHeatPlot$ytop - 0.25,
           round(dataHeatPlot$metrics, 3),
           pos = 4, col = "white")

    }
  }

}
