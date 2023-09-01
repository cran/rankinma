#' @title Illustrate heat plot for treatment ranking
#'
#' @description
#' **PlotHeat()** is a function for illustrating heat plot.
#'
#' @param data    DATA of metrics for treatment ranking.
#' @param sorttx  LOGIC value for indicating whether sort heat plot by treatments.
#' @param rotateX NUMERIC value between 0 and 360 for rotating x axis labels of
#'                heat plot.
#' @param szFntY  NUMERIC value for indicating font size of outcome name(s).
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
                     sorttx  = NULL,
                     rotateX = NULL,
                     szFntY = NULL) {


  # 01. CHECK core arguments -----

  lgcInher  <- !inherits(data, "rankinma")

  if (lgcInher) {
    infoLgcInher <- paste(" Inherit: ERROR\n",
                          ' REQUIRE: Argument "data" must be an object of class \"rankinma\".')
  } else {
    infoLgcInher <- paste(" Inherit: OK")
  }

  infoLgcWarning <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = infoLgcWarning))



  # 02. RETURN results of core argument checking  -----

  if (lgcInher)

    stop(infoLgcInher)



  # 03. DEFINE core data -----

  dataHeat  <- data$data

  lsMetrics <- c("P-best", "SUCRA", "P-score")

  txs      <- unique(dataHeat$tx)
  outcomes <- unique(dataHeat$outcome)

  argSorttx <- ifelse(is.null(sorttx), FALSE,
                      ifelse(sorttx == FALSE, FALSE,
                             ifelse(sorttx == TRUE, TRUE,
                                    "ERROR")))



  # 04. CHECK additive arguments -----

  lgcMtrcs  <- ifelse(data$metrics.name == "Probabilities",
                      TRUE,
                      FALSE)

  lgcSorttx   <- ifelse(is.null(sorttx), FALSE,
                        ifelse(argSorttx == TRUE, FALSE,
                               ifelse(argSorttx == FALSE, FALSE,
                                      TRUE)))

  lgcRotateX <- ifelse(is.null(rotateX),
                       FALSE,
                       ifelse(isFALSE(length(rotateX) == 1),
                              TRUE,
                              ifelse(rotateX < 0 | rotateX > 360,
                                     TRUE, FALSE)))

  lgcszFntY <- ifelse(is.null(szFntY),
                      FALSE,
                      ifelse(isFALSE(length(szFntY) == 1 | length(szFntY) == length(outcomes)),
                             TRUE,
                             ifelse(isFALSE(is.numeric(szFntY)),
                                    TRUE,
                                    ifelse(FALSE %in% (szFntY >= 0),
                                           TRUE,
                                           ifelse(FALSE %in% (szFntY < 6),
                                                  TRUE, FALSE)))))



  # 05 REPORT results from argument checking -----

  if (lgcMtrcs) {
    infoLgcMetrics <- paste(" Metrics: ERROR\n",
              ' REQUIRE: Metrics should not be "Probabilities."')
  } else {
    infoLgcMetrics <- paste(" Metrics: OK")
  }

  if (lgcSorttx) {
    infoLgcScorttx <- paste(" Sort: WARNING!\n",
              ' INFORM: Argument "sorttx" should be logit value, and *rankinma*
        is producing bar charts with default argument in terms of `sorttx = FALSE`
        now.')
  } else {
    infoLgcScorttx <- paste(" Sort: OK")
  }

  if (lgcRotateX) {
    infoLgcRotateX <- paste(" RotateX: WARNING!\n",
                            ' INFORM: Argument "rotateX" should be a numeric value
                            between 0 and 360, and *rankinma* is producing x axis
                            labels of line chart with default argument in terms of
                            `rotateX = 0` now.')
  } else {
    infoLgcRotateX <- paste(" RotateX: OK")
  }


  if (lgcszFntY) {
    infoStopszFntY <- 'Argument "szFntY" must be a numeric value between 0 and 5 for indicating font size of outcome name(s).'
  } else {
    infoStopszFntY <- paste(" Font size of outcome name(s): OK")
  }


  infoStop <- paste(infoLgcInher, "\n",
                    infoLgcMetrics, "\n",
                    infoLgcScorttx, "\n",
                    infoLgcRotateX, "\n",
                    infoStopszFntY, "\n",
                    sep = ""
                    )

  if (lgcInher | lgcMtrcs | lgcSorttx)
    stop(infoStop)



  # 06 PROCESS additive setting -----

  infoRotateX <- ifelse(max(nchar(txs)) > 10, 30, 0)
  argRotateX  <- ifelse(is.null(rotateX),
                        infoRotateX,
                        ifelse(isFALSE(length(rotateX) == 1),
                               infoRotateX,
                               ifelse(rotateX >= 0 & rotateX <= 360,
                                      rotateX, infoRotateX)))
  argPosX     <- ifelse(argRotateX == 0, 4, 2)

  if (is.null(szFntY)) {
    infoszFntY <- ifelse(max(nchar(outcomes)) > 10,
                                   1 / max(nchar(outcomes)) * 10,
                                   1)
  } else {
    infoszFntY <- szFntY
  }


  dataHeat$seq.outcome <- max(dataHeat$outcomes) + 1 - dataHeat$outcomes
  dataHeat$seq.tx      <- max(dataHeat$txs) + 1 - dataHeat$txs

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
  dataSmry$szFntY   <- infoszFntY



  # 07 PLOT line chart -----

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
       dataSmry$outcome,
       cex = dataSmry$szFntY,
       pos = 2)

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

    infoSpeceX <- max(dataHeatPlot$xlft) - max(dataHeatPlot$xlft[-which(dataHeatPlot$xlft == max(dataHeatPlot$xlft))])

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

    }

    if (argRotateX == 0) {
      text(dataHeatPlot$xlft + infoSpeceX / 2,
           dataHeatPlot$ybtm - 0.05,
           dataHeatPlot$tx,
           cex = ifelse(length(txs) > 10,
                        1 / length(txs) * 10,
                        1),
           srt = argRotateX)
    } else {
      text(dataHeatPlot$xlft + infoSpeceX / 2,
           dataHeatPlot$ybtm - 0.05,
           dataHeatPlot$tx,
           cex = ifelse(length(txs) > 10,
                        1 / length(txs) * 10,
                        1),
           pos = argPosX, srt = argRotateX)
    }

    text(dataHeatPlot$xlft + infoSpeceX / 2,
         dataHeatPlot$ytop - 0.25,
         round(dataHeatPlot$metrics, 3),
         cex = ifelse(length(txs) > 10,
                      1 / length(txs) * 10,
                      1),
         #pos = 4,
         col = "white")

  }

}
