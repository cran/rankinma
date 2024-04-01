#' @title Illustrate bar chart of treatment ranking metrics
#'
#' @description
#' **PlotBar()** is a function for illustrating bar chart in both separated and
#' accumulative styles.
#'
#' @param data    DATA of metrics for treatment ranking.
#' @param accum   LOGIC value for indicating whether use accumulative probabilities.
#'                This parameter is only for probabilities but not global metrics
#'                of treatment ranking.
#' @param merge   LOGIC value for indicating whether merge bar charts together.
#' @param color   LIST of colors for treatments in a network meta-analysis,
#'                or CHARACTER of a color for the bar on not accumulated bar chart.
#' @param rotateX NUMERIC value between 0 and 360 for rotating x axis labels of
#'                bars.
#'
#' @return
#' **PlotBar()** returns a bar chart.
#'
#' @references
#' Van Valkenhoef, G., Tervonen, T., Zwinkels, T., De Brock, B., &
#' Hillege, H. (2013). ADDIS: a decision support system for evidence-based
#' medicine. **Decision Support Systems**, *55(2)*, 459-475.
#'
#' @seealso \code{\link{GetMetrics}}, \code{\link{SetMetrics}},
#'          \code{\link{PlotBeads}}, \code{\link{PlotLine}},
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
#' #PlotBar(dataRankinma)
#' ## End(Not run)
#'
#' @export PlotBar


PlotBar <- function(data,
                    accum   = NULL,
                    merge   = NULL,
                    color   = NULL,
                    rotateX = NULL
                    ) {


  # 01. CHECK core arguments -----

  lgcInher <- !inherits(data, "rankinma")

  if (lgcInher) {
    infoLgcInher <- paste(" Inherit: ERROR\n",
                          ' REQUIRE: Argument "data" must be an object of class \"rankinma\".')
  } else {
    infoLgcInher <- paste(" Inherit: OK")
  }


  # 02. RETURN results of core argument checking  -----

  if (lgcInher)

    stop(infoLgcInher)



  # 03. DEFINE core data -----
  dataBar   <- data$data

  lsMetrics <- c("Probabilities", "P-best", "SUCRA", "P-score")

  txs       <- unique(dataBar$tx)
  outcomes  <- unique(dataBar$outcome)

  argAccum  <- ifelse(is.null(accum), FALSE,
                      ifelse(accum == FALSE, FALSE,
                             ifelse(accum == TRUE, TRUE,
                                    "ERROR")))

  argMerge  <- ifelse(is.null(merge), FALSE,
                      ifelse(merge == FALSE, FALSE,
                             ifelse(merge == TRUE, TRUE,
                                    "ERROR")))

  if (data$metrics.name == "Probabilities") {
    if (argAccum == TRUE) {
      if (isFALSE(is.null(color))) {
        if (length(color) == data$n.tx) {
          argColor <- color
        } else {
          argColor <- NULL
        }
      } else {
        argColor <- NULL
      }
    } else if (length(color) == 1) {
      argColor <- color
    } else {
      argColor <- NULL
    }
  } else if (isFALSE(is.null(color))) {
    if (length(color) == 1) {
      argColor <- color
    } else {
      argColor <- NULL
    }
  } else {
    argColor <- NULL
  }



  # 04. CHECK additive arguments -----

  lgcAccum <- ifelse(argAccum == TRUE,
                     ifelse(data$metrics.name != "Probabilities",
                            TRUE, FALSE),
                     FALSE)

  lgcMerge <- ifelse(is.null(merge), FALSE,
                     ifelse(argMerge == TRUE, FALSE,
                            ifelse(argMerge == FALSE, FALSE,
                                   TRUE)))

  lgcColor  <- ifelse(is.null(color), FALSE,
                      ifelse(argAccum == TRUE,
                             ifelse(length(color) != data$n.tx, TRUE, FALSE),
                             ifelse(length(color) != 1,
                                    ifelse(length(color) != data$n.tx,
                                           TRUE, FALSE),
                                    FALSE)))

  lgcRotateX <- ifelse(is.null(rotateX),
                         FALSE,
                         ifelse(isFALSE(length(rotateX) == 1),
                                TRUE,
                                ifelse(rotateX < 0 | rotateX > 360,
                                       TRUE, FALSE)))



  # 05 REPORT results from argument checking -----

  if (lgcAccum) {
    infoLgcAccum <- paste(" Accumulative: WARNING!\n",
              ' INFORM: Accumulative line chart is available for metrics
        "Probabilities" only, and *rankinma* is producing bar charts
        with default argument in terms of `accum = FALSE` now.')
  } else {
    infoLgcAccum <- paste(" Accumulative: OK")
  }

  if (lgcMerge) {
    infoLgcMerge <- paste(" Merge: WARNING!\n",
              ' INFORM: Argument "merge" should be logit value, and *rankinma*
        is producing bar charts with default argument in terms of `merge = FALSE`
        now.')
  } else {
    infoLgcMerge <- paste(" Merge: OK")
  }

  if (data$metrics.name == "Probabilities") {
    if (isFALSE(is.null(color))) {
      if (argAccum == TRUE) {
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
    if (length(color) != 1 & length(color) != data$n.tx) {
      infoLgcColor <- paste(" Color: ERROR\n",
                ' REQUIRE: Argument of "color" must be only single color or\n
                length of color list equals to numbers of treatments when using
                global metrics (e.g. SUCRA and P-score).')
    } else {
      infoLgcColor <- paste(" Color: OK")
    }
  } else {
    infoLgcColor <- paste(" Color: OK")
  }

  if (lgcRotateX) {
    infoLgcRotateX <- paste(" RotateX: WARNING!\n",
                            ' INFORM: Argument "rotateX" should be a numeric value
                            between 0 and 360, and *rankinma* is producing x axis
                            labels of bars with default argument in terms of
                            `rotateX = 0` now.')
  } else {
    infoLgcRotateX <- paste(" RotateX: OK")
  }


  infoStop <- paste(infoLgcInher, "\n",
                    infoLgcAccum, "\n",
                    infoLgcMerge, "\n",
                    infoLgcColor, "\n",
                    infoLgcRotateX, "\n",
                    sep = ""
                    )

  if (lgcInher | lgcColor)
    stop(infoStop)



  # 06 PREPARE data -----

  infoRotateX <- ifelse(max(nchar(txs)) > 10, 30, 0)
  argRotateX  <- ifelse(is.null(rotateX),
                        infoRotateX,
                        ifelse(isFALSE(length(rotateX) == 1),
                               infoRotateX,
                               ifelse(rotateX >= 0 & rotateX <= 360,
                                      rotateX, infoRotateX)))
  argPosX     <- ifelse(argRotateX == 0, 4, 2)


  ## 06.1 SET colors by user-defined colors -----

  colorTx <- data$color.txs

  if (!is.null(color)) {
    if (length(which(ls()%in%ls(pattern = "color"))) > 0) {
      colorTx$colorTx <- rgb(col2rgb(color)[1, ]/255,
                             col2rgb(color)[2, ]/255,
                             col2rgb(color)[3, ]/255,
                             data$trans)
      for (color.i in c(1:nrow(dataBar))) {
        dataBar[color.i, "colorTx"] <- colorTx[which(dataBar[color.i, "tx"] == colorTx$lsTx), "colorTx"]
      }
    }
  }

  ## 06.2 SET parameters for bar chart -----

  argPlotRow <- ceiling(data$n.outcome/3)
  #argPlotClm <- ifelse(data$n.outcome < 3,
  #                     data$n.outcome, 3)

  argPlotClm <- ifelse(data$n.outcome < 3,
                       data$n.outcome,
                       ifelse(data$n.outcome == 4, 2, 3)
                       )

  setPar <- par(no.readonly = TRUE)
  on.exit(par(setPar))



  # 07 PLOT bar chart -----

  if (data$metrics.name == "Probabilities") {

  ## 07.1 PLOT bar chart for probabilities -----

    par(mar = c(5, 5, 5, 7), xpd = TRUE)
    for (outcome.i in c(1:length(outcomes))) {

      dataBarPlot <- dataBar[dataBar$outcomes == outcome.i, ]

      plotB <- barplot(as.matrix(dataBarPlot[, c(7:(length(unique(dataBarPlot$tx))+6))]),
                       #cex.names = 0.8,
                       #cex.axis = 0.8,
                       #xlim = c(0, nrow(dataBarPlot)),
                       ylim = c(-0.2, 1.2),
                       #names.arg = dataBarPlot$tx,
                       yaxt = "n", xaxt = "n",
                       ylab = "Probability", xlab = "Treatment",
                       col = if (is.null(argColor)) {
                         c(dataBarPlot$colorTx)
                         } else {
                           argColor
                         },
                       width = (1/length(unique(dataBarPlot$tx)))^(1/3),
                       #ifelse(argAccum == TRUE,
                       #       0.8,
                       #       (1/length(unique(dataBarPlot$tx)))^(1/3)
                       #       ),
                       beside = ifelse(argAccum == TRUE, FALSE, TRUE),
                       border = FALSE, legend = FALSE,
                       frame = FALSE)

      infoSpace <- (max(plotB) - max(plotB[plotB != max(plotB)])) / 2

      if (argRotateX == 0) {
        text(plotB, #c(1:nrow(dataBarPlot)),
             rep(-0.1, nrow(dataBarPlot)), #par("usr")[3],
             dataBarPlot$tx,
             cex = ifelse(max(nchar(dataBarPlot$tx)) > 10,
                          1 / max(nchar(dataBarPlot$tx)) * 10,
                          1),
             col = "black",  xpd = TRUE,
             srt = argRotateX)
      } else {
        text(plotB, #c(1:nrow(dataBarPlot)),
             rep(-0.1, nrow(dataBarPlot)), #par("usr")[3],
             dataBarPlot$tx,
             cex = ifelse(max(nchar(dataBarPlot$tx)) > 10,
                          1 / max(nchar(dataBarPlot$tx)) * 10,
                          1),
             col = "black", xpd = TRUE,
             pos = argPosX,
             srt = argRotateX)
      }

      axis(side = 2, cex.axis = 0.8,
           at = seq(0, 1, by = 0.2),
           las = 1)

      title(paste("Accumulative bar chart of rank probability on \n",
                  data$ls.outcome[outcome.i], sep = ""),
            cex.main = 1, font = 1, line = 0.5)

      segments(rep(max(plotB) + infoSpace + 0.2, nrow(dataBarPlot)),
               (0.95) / nrow(dataBarPlot) * (max(dataBarPlot$txs) + 1 - dataBarPlot$txs),
               rep(max(plotB) + infoSpace + 0.3, nrow(dataBarPlot)),
               (0.95) / nrow(dataBarPlot) * (max(dataBarPlot$txs) + 1 - dataBarPlot$txs),
               lwd = 10,
               col = if (is.null(argColor)) {
                 c(dataBarPlot$colorTx)
               } else {
                 argColor
                 }
               )

      text(rep(max(plotB) + infoSpace + 0.4, nrow(dataBarPlot)),
           (0.95) / nrow(dataBarPlot) * (max(dataBarPlot$txs) + 1 - dataBarPlot$txs),
           paste("Rank ", dataBarPlot$rank, sep = ""),
           pos = 4,
           cex = 0.8)

      plotTemp <- grDevices::recordPlot()
      plotBar  <- plotTemp
    }
  } else {

    ## 07.2 PLOT bar chart for global metrics -----

    par(mar = c(5, 5, 5, 5), xpd = TRUE)

    for (outcome.i in c(1:length(outcomes))) {

      dataBarPlot             <- dataBar[dataBar$outcomes == outcome.i, ]

      dataBarPlot             <- dataBarPlot[order(-dataBarPlot$txs),]
      dataBarPlot$seq.tx      <- c(1:nrow(dataBarPlot))
      dataBarPlot             <- dataBarPlot[order(dataBarPlot$metrics, decreasing = TRUE),]
      dataBarPlot$seq.metrics <- c(1:nrow(dataBarPlot))

      plotB <- barplot(dataBarPlot[, "metrics"],
                       #cex.names = 0.8,
                       #cex.axis = 0.8,
                       #xlim = c(0, nrow(dataBarPlot)),
                       ylim = c(-0.2, 1.2),
                       #names.arg = dataBarPlot$tx,
                       yaxt = "n", xaxt = "n",
                       ylab = data$metrics.name, xlab = "Treatment",
                       col = ifelse(is.null(argColor), "dodgerblue3", argColor[1]),
                       width = (1/length(unique(dataBarPlot$tx)))^(1/3), #0.8,
                       beside = FALSE,
                       border = FALSE, legend = FALSE,
                       frame = FALSE)

      #infoBarWidth <- (abs(max(plotB)) - abs(max(plotB))) / nrow(plotB)

      #plot(c(-0.3, -0.3),
      #     c((max(plotB) + infoBarWidth * 0.45) * 1.3, 1.3),
      #     yaxt = "n", xaxt = "n",
      #     pch = "",
      #     col = rgb(1, 1, 1, 1),
      #     frame = FALSE)
      #rect(plotB - (infoBarWidth * 0.45),
      #     rep(0, nrow(plotB)),
      #     plotB + (infoBarWidth * 0.45),
      #     dataBarPlot[, "metrics"],
      #     lty = 0,
      #     col = ifelse(is.null(argColor), "dodgerblue3", argColor[1]))

      if (argRotateX == 0) {
        text(plotB, #c(1:nrow(dataBarPlot)),
             rep(-0.1, nrow(dataBarPlot)), #par("usr")[3],
             dataBarPlot$tx,
             cex = ifelse(max(nchar(dataBarPlot$tx)) > 10,
                          1 / max(nchar(dataBarPlot$tx)) * 10,
                          1),
             col = "black", xpd = TRUE,
             srt = argRotateX)
      } else {
        text(plotB, #c(1:nrow(dataBarPlot)),
             rep(-0.1, nrow(dataBarPlot)), #par("usr")[3],
             dataBarPlot$tx,
             cex = ifelse(max(nchar(dataBarPlot$tx)) > 10,
                          1 / max(nchar(dataBarPlot$tx)) * 10,
                          1),
             col = "black", xpd = TRUE,
             pos = argPosX,
             srt = argRotateX)
      }

      axis(side = 2, cex.axis = 0.8,
           at = seq(0, 1, by = 0.2),
           las = 1)

      title(paste("Bar chart of ", data$metrics.name, " on \n",
                  data$ls.outcome[outcome.i], sep = ""),
            cex.main = 1, font = 1, line = 0.5)
    }
  }

  if (argMerge == TRUE) {
    par(mfrow = c(argPlotRow, argPlotClm),
        oma = c(4, 4, 4, 0.5),
        mai = c(0.3, 0.3, 0.3, 0.3))

    for (outcome.i in c(1:length(outcomes))) {

      dataBarPlot             <- dataBar[dataBar$outcomes == outcome.i, ]

      dataBarPlot             <- dataBarPlot[order(-dataBarPlot$txs), ]
      dataBarPlot$seq.tx      <- c(1:nrow(dataBarPlot))
      dataBarPlot             <- dataBarPlot[order(dataBarPlot$metrics, decreasing = TRUE), ]
      dataBarPlot$seq.metrics <- c(1:nrow(dataBarPlot))

      plotB <- barplot(dataBarPlot[, "metrics"],
                       #cex.names = 0.8,
                       #cex.axis = 0.8,
                       #xlim = c(0, nrow(dataBarPlot)),
                       ylim = c(-0.2, 1.2),
                       #names.arg = dataBarPlot$tx, las = 1,
                       yaxt = "n", xaxt = "n",
                       ylab = data$metrics.name, xlab = "", #"Treatment",
                       col = if (length(color) == data$n.tx) {
                         dataBarPlot$colorTx
                         } else {
                           ifelse(is.null(argColor), "dodgerblue3", argColor[1])
                         },
                       width = (1/length(unique(dataBarPlot$tx)))^(1/3), #0.8,
                       beside = FALSE,
                       border = FALSE, legend = FALSE,
                       frame = FALSE)

      if (argRotateX == 0) {
        text(plotB, #c(1:nrow(dataBarPlot)),
             rep(-0.1, nrow(dataBarPlot)), #par("usr")[3],
             dataBarPlot$tx,
             cex = ifelse(max(nchar(dataBarPlot$tx)) > 10,
                          1 / max(nchar(dataBarPlot$tx)) * 10,
                          1),
             col = "black", xpd = TRUE,
             srt = argRotateX)
      } else {
        text(plotB, #c(1:nrow(dataBarPlot)),
             rep(-0.1, nrow(dataBarPlot)), #par("usr")[3],
             dataBarPlot$tx,
             cex = ifelse(max(nchar(dataBarPlot$tx)) > 10,
                          1 / max(nchar(dataBarPlot$tx)) * 10,
                          1),
             col = "black", xpd = TRUE,
             pos = argPosX,
             srt = argRotateX)
      }

      axis(side = 2, cex.axis = 0.8,
           at = seq(0, 1, by = 0.2),
           las = 1)

      title(paste(data$ls.outcome[outcome.i], sep = ""),
            cex.main = 1, font = 1, line = 0.5)
    }

    mtext(paste("Bar chart of " ,
                ifelse(data$metrics.name %in% lsMetrics,
                       data$metrics.name, "???"),
                sep = ""),
          side = 3, font = 2, line = -0.5, outer = TRUE)
    mtext(ifelse(data$metrics.name %in% lsMetrics,data$metrics.name, "???"),
          side = 2, font = 1, line = 1, #adj = ifelse(argPlotRow == 1, 0.65, 0.5),
          outer = TRUE)
    mtext("Treatment",
          side = 1, font = 1, line = 1,
          outer = TRUE)
    par(mfrow = c(1, 1))
  }

}

