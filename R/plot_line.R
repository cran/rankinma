#' @title Illustrate line chart of treatment ranking metrics
#'
#' @author Enoch Kang
#'
#' @description
#' **PlotLine()** is a function for illustrating line chart in both simple
#' and composite styles.
#'
#' @param data DATA of metrics for treatment ranking.
#' @param accum LOGIC value for indicating whether use accumulative probabilities.
#'        This parameter is only for probabilities but not global metrics of
#'        treatment ranking.
#' @param compo LOGIC value for indicating whether use composite line chart.
#'        This parameter is only for probabilities but not global metrics of
#'        treatment ranking.
#' @param merge LOGIC value for indicating whether merge line charts together.
#' @param color LIST of colors for treatments in a network meta-analysis,
#'        or CHARACTER of a color for the line on not composite line chart.
#'
#' @return
#' **PlotLine()** returns a line chart.
#'
#' @references
#' Chaimani, A., Higgins, J. P., Mavridis, D., Spyridonos, P., &
#' Salanti, G. (2013). Graphical tools for network meta-analysis in STATA.
#' **PloS one**, *8(10)*, e76654.
#'
#' @seealso \code{\link{GetMetrics}}, \code{\link{SetMetrics}},
#'          \code{\link{PlotBeads}}, \code{\link{PlotBar}},
#'          \code{\link{PlotHeat}}, \code{\link{PlotSpie}}
#'
#' @examples
#' ## Not run:
#' #library(netmeta)
#' #data(Senn2013)
#' #nma <- netmeta(TE, seTE, treat1, treat2,
#' #studlab, data = Senn2013, sm = "SMD")
#'
#' # Get SUCRA
#' #dataMetrics <- GetMetrics(nma, outcome = "HbA1c", prefer = "small", metrics = "SUCRA",
#' #model = "random", simt = 1000)
#'
#' # Set data for rankinma
#' #dataRankinma <- SetMetrics(dataMetrics, tx = tx, outcome = outcome,
#' #metrics = SUCRA, metrics.name = "SUCRA")
#'
#' # Illustrate bar plot
#' #PlotLine(dataRankinma)
#' ## End(Not run)
#'
#' @export PlotLine

PlotLine <- function(data,
                     accum = NULL,
                     compo = NULL,
                     merge = NULL,
                     color = NULL) {

  dataLine <- data$data

  lsMetrics <- c("Probabilities", "P-best", "SUCRA", "P-score")

  argAccum  <- ifelse(is.null(accum), FALSE,
                      ifelse(accum == FALSE, FALSE,
                             ifelse(accum == TRUE, TRUE,
                                    "ERROR")))

  argCompo  <- ifelse(is.null(compo), FALSE,
                      ifelse(compo == FALSE, FALSE,
                             ifelse(compo == TRUE, TRUE,
                                    "ERROR")))

  argMerge  <- ifelse(is.null(merge), FALSE,
                      ifelse(merge == FALSE, FALSE,
                             ifelse(merge == TRUE, TRUE,
                                    "ERROR")))

  if (data$metrics.name == "Probabilities") {
    if (argCompo == TRUE) {
      if (isFALSE(is.null(color))) {
        if (length(color) == data$n.tx) {
          argColor  <- color
        } else {
          argColor  <- NULL
        }
      } else {
        argColor  <- NULL
      }
    } else if (length(color) == 1) {
      argColor  <- color
    } else {
      argColor  <- NULL
    }
  } else if (isFALSE(is.null(color))) {
    if (length(color) == 1) {
      argColor  <- color
    } else {
      argColor  <- NULL
    }
  } else {
    argColor  <- NULL
  }


  lgcInher  <- !inherits(data, "rankinma")

  lgcAccum  <- ifelse(argAccum == TRUE,
                      ifelse(data$metrics.name != "Probabilities",
                             TRUE, FALSE),
                      FALSE)

  lgcCompo  <- ifelse(argCompo == TRUE,
                      ifelse(data$metrics.name != "Probabilities",
                             TRUE, FALSE),
                      FALSE)

  lgcMerge  <- ifelse(is.null(merge), FALSE,
                      ifelse(argMerge == TRUE, FALSE,
                             ifelse(argMerge == FALSE, FALSE,
                                    TRUE)))

  lgcColor  <- ifelse(is.null(color), FALSE,
                      ifelse(data$metrics.name == "Probabilities",
                             ifelse(argCompo == TRUE,
                                    ifelse(length(color) != data$n.tx, TRUE, FALSE),
                                    ifelse(length(color) != 1, TRUE, FALSE)),
                             ifelse(length(color) != 1, TRUE, FALSE)))

  # 02 REPORT results from argument checking -----

    if (lgcInher) {
      infoLgcInher <- paste(" Inherit: ERROR\n",
              ' REQUIRE: Argument "data" must be an object of class \"rankinma\".')
  } else {
    infoLgcInher <- paste(" Inherit: OK")
  }

  if (lgcCompo) {
    infoLgcCompo <- paste(" Composite: WARNING!\n",
              ' INFORM: Composite line chart is available for metrics
         "Probabilities" only, and *rankinma* is producing line charts
         with default argument in terms of `compo = FALSE` now.')
  } else {
    infoLgcCompo <- paste(" Composite: OK")
  }

  if (lgcMerge) {
    infoLgcMerge <- paste(" Merge: WARNING!\n",
              ' INFORM: Argument "merge" should be logit value, and *rankinma*
        is producing line charts with default argument in terms of `merge = FALSE`
        now.')
  } else {
    infoLgcMerge <- paste(" Merge: OK")
  }

  if (data$metrics.name == "Probabilities") {
    if (isFALSE(is.null(color))) {
      if (argCompo == TRUE) {
        if (length(color) != data$n.tx) {
          infoLgcColor <- paste(" Color: ERROR\n",
                    ' REQUIRE: Argument of "color" must list colors for
              **EACH TREATMENT** when using "Probabilities" as metrics.')
        } else {
          infoLgcColor <- paste(" Color: OK")
        }
      } else {
        infoLgcColor <- paste(" Color: OK")
      }
    } else {
      infoLgcColor <- paste(" Color: OK")
    }
  } else if (isFALSE(is.null(color))) {
    if (length(color) != 1) {
      infoLgcColor <- paste(" Color: ERROR\n",
                ' REQUIRE: Argument of "color" must be only single color\n
                when using global metrics (e.g. SUCRA and P-score).')
    } else {
      infoLgcColor <- paste(" Color: OK")
    }
  } else {
    infoLgcColor <- paste(" Color: OK")
  }


  infoStop <- paste(infoLgcInher, "\n",
                    infoLgcCompo, "\n",
                    infoLgcMerge, "\n",
                    infoLgcColor, "\n",
                    sep = ""
                    )

  if (lgcInher | lgcColor)
    stop(infoStop)

  txs      <- unique(dataLine$tx)
  outcomes <- unique(dataLine$outcome)

  setPar <- par(no.readonly = TRUE)
  on.exit(par(setPar))

  if (data$metrics.name == "Probabilities") {
    # 3.1. Ranking with probabilities -----

    argPlotRow <- ceiling((data$n.tx)/3)
    argPlotClm <- ifelse((data$n.tx) < 3,
                         (data$n.tx), 3)

    #cat(paste("This function is developing..."),
    #    fill = TRUE, sep = "")
    #cat(paste("Merged line chart will be available in the future."),
    #    fill = TRUE, sep = "")

    if (argAccum == TRUE) {
      # 3.1.1. Cumulative probabilities -----

      infoColumnCum <- which(substr(colnames(dataLine),
                                    nchar(colnames(dataLine)) - 2,
                                    nchar(colnames(dataLine))) == "Cum")

      dataLineCum <- dataLine[, c(1:6, infoColumnCum)]
      colnames(dataLineCum)[c(7:length(dataLineCum))] <- dataLineCum$tx

      if (argCompo == TRUE) {
        # 3.1.1.1. Composite line chart (cumulative probabilities) -----

        par(mar = c(5, 5, 5, 7), xpd = TRUE)
        for (outcome.i in c(1:length(outcomes))) {
          dataLinePlot <- dataLineCum[dataLineCum$outcomes == outcome.i, ]
          plot(x = dataLinePlot[, c("txs")],
               y = dataLinePlot[, c(7)],
               type = "line",
               cex.names = 0.8,
               cex.axis = 0.8,
               xlim = c(1, max(dataLinePlot$txs)),
               ylim = c(0, 1),
               xaxt = "n",
               yaxt = "n",
               xlab = "Rank",
               ylab = "Cumulative probability",
               col = if (is.null(argColor)) {
                 c(dataLinePlot$colorTx[1])
               } else {
                 argColor[1]
               },
               legend = FALSE, frame.plot = FALSE)
          axis(side = 2, cex.axis = 0.8, at = seq(0, 1, by = 0.1), las = 1)
          title(paste("Composite line chart of cumulative probability on ",
                      data$ls.outcome[outcome.i], sep = ""),
                cex.main = 1, font = 1, line = 0.5)

          for (tx.i in c(8:(6 + data$n.tx))) {
            lines(dataLinePlot[, c(5)],
                  dataLinePlot[, c(tx.i)],
                  col = if (is.null(argColor)) {
                    c(dataLinePlot$colorTx[tx.i - 6])
                  } else {
                    argColor[tx.i - 6]
                  }
            )
          }

          axis(side = 1, cex.axis = 0.8,
               at = c(1:max(dataLinePlot$txs)),
               labels = dataLinePlot$txs)

          segments(rep(data$n.tx * 1.01, nrow(dataLinePlot)),
                   (0.95) / nrow(dataLinePlot) * dataLinePlot$txs,
                   rep(data$n.tx * 1.05, nrow(dataLinePlot)),
                   (0.95) / nrow(dataLinePlot) * dataLinePlot$txs,
                   lwd = 2,
                   col = if (is.null(argColor)) {
                     c(dataLinePlot$colorTx)
                   } else {
                     argColor}
          )

          text(rep(data$n.tx * 1.06, nrow(dataLinePlot)),
               (0.95) / nrow(dataLinePlot) * dataLinePlot$txs,
               dataLinePlot$tx,
               pos = 4,
               cex = 0.8)

          plotTemp <-  grDevices::recordPlot()
        }


      } else if (argMerge == TRUE) {
          # 3.1.1.2. Merged line charts (cumulative probabilities) -----

          par(mfrow = c(argPlotRow, argPlotClm),
              oma = c(4,4,4,0.5),
              mai = c(0.3,0.3,0.3,0.3))

          for (tx.i in c(7:(7 + length(txs) - 1))) {

            dataLinePlot <- dataLineCum

            plot(x = dataLinePlot[, c("txs")],
                 y = dataLinePlot[, tx.i],
                 type = "line",
                 cex.names = 0.8,
                 cex.axis = 0.8,
                 xlim = c(1, max(dataLinePlot$txs)),
                 ylim = c(0, 1),
                 xaxt = "n", yaxt = "n",
                 xlab = "Rank", ylab = "Probabilities",
                 legend = FALSE,
                 frame.plot = FALSE,
                 #col = ifelse(is.null(argColor), "dodgerblue3", argColor)
                 col = if (length(color) == data$n.tx) {
                   dataLinePlot$colorTx
                 } else {
                   ifelse(is.null(argColor), "dodgerblue3", argColor[1])
                 }
            )
            axis(side = 1, cex.axis = 0.8,
                 at = c(1:max(dataLinePlot$txs)),
                 labels = dataLinePlot$txs)
            axis(side = 2, cex.axis = 0.8,
                 at = seq(0, 1, by = 0.1), las = 1)
            title(paste(colnames(dataLinePlot)[tx.i], sep = ""),
                  cex.main = 1, font = 1, line = 0.5)
          }

          mtext(paste("Line chart of " ,
                      ifelse(data$metrics.name %in% lsMetrics,
                             data$metrics.name, "???"),
                      " on\n",
                      data$ls.outcome,
                      sep = ""),
                side = 3, font = 2, line = -0.5, outer = TRUE)
          mtext(ifelse(data$metrics.name %in% lsMetrics,data$metrics.name, "???"),
                side = 2, font = 1, line = 1, outer = TRUE)
          mtext("Rank",
                side = 1, font = 1, line = 1, outer = TRUE)
          par(mfrow = c(1,1))
        } else {
        # 3.1.1.3. separated line charts (cumulative probabilities) -----
        par(mar = c(5, 5, 5, 7), xpd = TRUE)
        for (outcome.i in c(1:length(outcomes))) {
          for (tx.i in c(7:(6 + data$n.tx))) {
            dataLinePlot <- dataLineCum[dataLineCum$outcomes == outcome.i, c(1:6, tx.i)]

            plot(dataLinePlot[, 7],
                 type = "line",
                 cex.names = 0.8,
                 cex.axis = 0.8,
                 xlim = c(1, max(dataLinePlot$txs)),
                 ylim = c(0, 1),
                 xaxt = "n", yaxt = "n",
                 xlab = "Rank", ylab = "Cumulative probability",
                 legend = FALSE,
                 frame.plot = FALSE,
                 col = ifelse(is.null(argColor), "dodgerblue3", argColor)
            )
            axis(side = 1, cex.axis = 0.8,
                 at = c(1:max(dataLinePlot$txs)),
                 labels = dataLinePlot$txs)
            axis(side = 2, cex.axis = 0.8, at = seq(0, 1, by = 0.1), las = 1)
            title(paste("Line chart for cumulative probability of\n",
                        data$ls.tx[tx.i - 6], " on ",
                        data$ls.outcome[outcome.i],
                        sep = ""),
                  cex.main = 0.8, font = 1, line = 0.5)

            plotTemp <-  grDevices::recordPlot()
          }

        }
      }

      outLineCum <- dataLineCum
    } else {
      #dataLineProb <- dataLine[, -c((7 + data$n.tx):length(dataLine))]
      dataLineProb <- dataLine[, c(1:(6 + data$n.tx))]
      colnames(dataLineProb)[c(7:length(dataLineProb))] <- dataLineProb$tx

      if (argCompo == TRUE) {
        # 3.1.2. Probabilities of ranks -----
        # 3.1.2.1. Composite line chart (probabilities) -----

      par(mar = c(5, 5, 5, 7), xpd = TRUE)
      for (outcome.i in c(1:length(outcomes))) {
        dataLinePlot <- dataLineProb[dataLineProb$outcomes == outcome.i, ]
        plot(x = dataLinePlot[, "rank"],
             y = dataLinePlot[, c(7)],
             type = "line",
             cex.names = 0.8,
             cex.axis = 0.8,
             xlim = c(1, max(dataLinePlot$txs)),
             ylim = c(0, 1),
             xaxt = "n",
             yaxt = "n",
             xlab = "Rank",
             ylab = "Probability",
             col = if (is.null(argColor)) {
               c(dataLinePlot$colorTx[1])
             } else {
               argColor[1]
             },
             legend = FALSE, frame.plot = FALSE)
        axis(side = 2, cex.axis = 0.8, at = seq(0, 1, by = 0.1), las = 1)
        title(paste("Composite line chart of rank probability on ",
                    data$ls.outcome[outcome.i], sep = ""),
              cex.main = 1, font = 1, line = 0.5)

        for (tx.i in c(8:(data$n.tx + 6))) {
          lines(dataLinePlot[, c(5)],
                dataLinePlot[, c(tx.i)],
                col = if (is.null(argColor)) {
                  c(dataLinePlot$colorTx[tx.i - 6])
                } else {
                  argColor[tx.i - 6]
                }
          )
        }

        axis(side = 1, cex.axis = 0.8,
             at = c(1:max(dataLinePlot$txs)),
             labels = dataLinePlot$txs)

        segments(rep(data$n.tx * 1.01, nrow(dataLinePlot)),
                 (0.95) / nrow(dataLinePlot) * dataLinePlot$txs,
                 rep(data$n.tx * 1.05, nrow(dataLinePlot)),
                 (0.95) / nrow(dataLinePlot) * dataLinePlot$txs,
                 lwd = 2,
                 col = if (is.null(argColor)) {
                   c(dataLinePlot$colorTx)
                 } else {
                   argColor}
        )

        text(rep(data$n.tx * 1.06, nrow(dataLinePlot)),
             (0.95) / nrow(dataLinePlot) * dataLinePlot$txs,
             dataLinePlot$tx,
             pos = 4,
             cex = 0.8)

        plotTemp <-  grDevices::recordPlot()
      }
      par(mar = c(5, 5, 5, 5), xpd = TRUE)

    } else if (argMerge == TRUE) {
      # 3.1.2.2. Merged line charts (probabilities) -----
      par(mfrow = c(argPlotRow, argPlotClm),
          oma = c(4,4,4,0.5),
          mai = c(0.3,0.3,0.3,0.3))

      for (tx.i in c(7:(7 + length(txs) - 1))) {

        dataLinePlot <- dataLineProb

        plot(x = dataLinePlot[, c("txs")],
             y = dataLinePlot[, tx.i],
             type = "line",
             cex.names = 0.8,
             cex.axis = 0.8,
             xlim = c(1, max(dataLinePlot$txs)),
             ylim = c(0, 1),
             xaxt = "n", yaxt = "n",
             xlab = "Rank", ylab = "Probabilities",
             legend = FALSE,
             frame.plot = FALSE,
             #col = ifelse(is.null(argColor), "dodgerblue3", argColor)
             col = if (length(color) == data$n.tx) {
               dataLinePlot$colorTx
             } else {
               ifelse(is.null(argColor), "dodgerblue3", argColor[1])
             }
             )

        axis(side = 1, cex.axis = 0.8,
             at = c(1:max(dataLinePlot$txs)),
             labels = dataLinePlot$txs)
        axis(side = 2, cex.axis = 0.8,
             at = seq(0, 1, by = 0.1), las = 1)
        title(paste(colnames(dataLinePlot)[tx.i], sep = ""),
              cex.main = 1, font = 1, line = 0.5)
      }

      mtext(paste("Line chart of " ,
                  ifelse(data$metrics.name %in% lsMetrics,
                         data$metrics.name, "???"),
                  " on\n",
                  data$ls.outcome,
                  sep = ""),
            side = 3, font = 2, line = -0.5, outer = TRUE)
      mtext(ifelse(data$metrics.name %in% lsMetrics,data$metrics.name, "???"),
            side = 2, font = 1, line = 1, outer = TRUE)
      mtext("Rank",
            side = 1, font = 1, line = 1, outer = TRUE)
      par(mfrow=c(1,1))

    } else {
      # 3.1.2.3. Separated line charts (probabilities) -----

      par(mar = c(5, 5, 5, 5), xpd = TRUE)

      for (outcome.i in c(1:length(outcomes))) {
        for (tx.i in c(7:(6 + data$n.tx))) {
          dataLinePlot <- dataLineProb[dataLineProb$outcomes == outcome.i, c(1:6, tx.i)]

          plot(dataLinePlot[, 7],
               type = "line",
               cex.names = 0.8,
               cex.axis = 0.8,
               xlim = c(1, max(dataLinePlot$txs)),
               ylim = c(0, 1),
               xaxt = "n", yaxt = "n",
               xlab = "Rank", ylab = "Probabilities",
               legend = FALSE,
               frame.plot = FALSE,
               col = ifelse(is.null(argColor), "dodgerblue3", argColor)
          )
          axis(side = 1, cex.axis = 0.8,
               at = c(1:max(dataLinePlot$txs)),
               labels = dataLinePlot$txs)
          axis(side = 2, cex.axis = 0.8, at = seq(0, 1, by = 0.1), las = 1)
          title(paste("Line chart for rank probability of\n",
                      data$ls.tx[tx.i - 6], " on ",
                      data$ls.outcome[outcome.i],
                      sep = ""),
                cex.main = 0.8, font = 1, line = 0.5)

          plotTemp <-  grDevices::recordPlot()
        }
      }
    }
  }

  } else {
    # 3.2. Global metrics -----
    argPlotRow <- ceiling((data$n.outcome)/3)
    argPlotClm <- ifelse((data$n.outcome) < 3,
                         (data$n.outcome), 3)

    if (argMerge == TRUE) {
      # 3.2.1. Merged line chart (global metrics) -----

      par(mfrow = c(argPlotRow, argPlotClm),
          oma = c(4,4,4,0.5),
          mai = c(0.3,0.3,0.3,0.3))

      for (outcome.i in c(1:length(outcomes))) {

        dataLinePlot <- dataLine[dataLine$outcomes == outcome.i, ]

        dataLinePlot <- dataLinePlot[order(-dataLinePlot$txs),]
        dataLinePlot$seq.tx <- c(1:nrow(dataLinePlot))

        plot(x = dataLinePlot[, c("seq.tx")],
             y = dataLinePlot[, c("metrics")],
             type = "line",
             cex.names = 0.8,
             cex.axis = 0.8,
             xlim = c(1, max(dataLinePlot$seq.tx)),
             ylim = c(0, 1),
             xaxt = "n", yaxt = "n",
             xlab = "Treatment", ylab = data$metrics.name,
             legend = FALSE,
             frame.plot = FALSE,
             #col = ifelse(is.null(argColor), "dodgerblue3", argColor)
             col = if (length(color) == data$n.tx) {
               dataLinePlot$colorTx
               } else {
                 ifelse(is.null(argColor), "dodgerblue3", argColor[1])
               }
             )

        axis(side = 1, cex.axis = 0.8,
             at = c(1:max(dataLinePlot$seq.tx)),
             labels = dataLinePlot$tx)
        axis(side = 2, cex.axis = 0.8,
             at = seq(0, 1, by = 0.1), las = 1)
        title(paste(data$ls.outcome[outcome.i], sep = ""),
              cex.main = 1, font = 1, line = 0.5)
      }

      mtext(paste("Line chart of " ,
                  ifelse(data$metrics.name %in% lsMetrics,
                         data$metrics.name, "???"),
                  sep = ""),
            side = 3, font = 2, line = -0.5, outer = TRUE)
      mtext(ifelse(data$metrics.name %in% lsMetrics,data$metrics.name, "???"),
            side = 2, font = 1, line = 1, outer = TRUE)
      mtext("Treatment",
            side = 1, font = 1, line = 1, outer = TRUE)
      par(mfrow=c(1,1))

    } else {
      # 3.2.2. Separated line charts (global metrics) -----

      par(mar = c(5, 5, 5, 7), xpd = TRUE)

      for (outcome.i in c(1:length(outcomes))) {
        dataLinePlot <- dataLine[dataLine$outcomes == outcome.i, ]

        dataLinePlot <- dataLinePlot[order(-dataLinePlot$txs),]
        dataLinePlot$seq.tx <- c(1:nrow(dataLinePlot))

        plot(dataLinePlot[, "metrics"],
             type = "line",
             cex.names = 0.8,
             cex.axis = 0.8,
             xlim = c(1, max(dataLinePlot$seq.tx)),
             ylim = c(0, 1),
             xaxt = "n", xlab = "Treatment",
             yaxt = "n", ylab = data$metrics.name,
             col = ifelse(is.null(argColor), "dodgerblue3", argColor[1]),
             legend = FALSE, frame.plot = FALSE)
        axis(side = 2, cex.axis = 0.8, at = seq(0, 1, by = 0.1), las = 1)
        axis(side = 1, cex.axis = 0.8,
             at = dataLinePlot[, "seq.tx"],
             labels = dataLinePlot[, "tx"])
        title(paste("Line chart of ", data$metrics.name, " on ",
                    outcomes[outcome.i], sep = ""),
              cex.main = 1, font = 1, line = 0.5)

        plotTemp <- grDevices::recordPlot()
        plotLine <- plotTemp
      }
    }
  }

}

