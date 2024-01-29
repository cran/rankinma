#' @title Illustrate beading plot
#'
#' @author Chiehfeng Chen & Enoch Kang
#'
#' @description
#' **PlotBeads()** is a function for illustrating beading plot.
#'
#' @param data      DATA of metrics for treatment ranking.
#' @param scaleX    STRING for indicating scale on the x axis.
#' @param txtValue  STRING for indicating labels of metrics or effects on each point.
#' @param color     LIST of colors for treatments in a network meta-analysis.
#' @param szPnt     NUMERIC value for indicating point size of ranking metrics.
#' @param szFntTtl  NUMERIC value for indicating font size of main title.
#' @param szFntTtlX NUMERIC value for indicating font size of title on X-axis.
#' @param szFntX    NUMERIC value for indicating font size of numeric scale on X-axis.
#' @param szFntY    NUMERIC value for indicating font size of outcome name(s).
#' @param szFntTxt  NUMERIC value for indicating font size of value of each point.
#' @param szFntLgnd NUMERIC value for indicating legend font size.
#' @param rotateTxt NUMERIC value between 0 and 360 for rotating labels of text values
#'                  of each point.
#'
#' @return
#' **PlotBeads()** returns a beading plot.
#'
#' @references
#' Chen, C., Chuang, Y.C., Chan, E., Chen, J.H., Hou, W.H., & Kang, E. (2023).
#' Beading plot: A novel graphics for ranking interventions in network evidence.
#' PREPRINT (Version 1) available at Research Square.
#'
#' @seealso \code{\link{GetMetrics}}, \code{\link{SetMetrics}}
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
#' #PlotBeads(data = dataRankinma)
#' ## End(Not run)
#'
#' @export PlotBeads

PlotBeads <- function(data,
                      scaleX    = "Rank",
                      txtValue  = "Effects",
                      color     = NULL,
                      szPnt     = NULL,
                      szFntTtl  = NULL,
                      szFntTtlX = NULL,
                      szFntX    = NULL,
                      szFntY    = NULL,
                      szFntTxt  = NULL,
                      szFntLgnd = NULL,
                      rotateTxt = 60) {


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

  dataBeads <- data$data

  txs       <- unique(dataBeads$tx)
  outcomes  <- unique(dataBeads$outcome)
  lsScaleX  <- c("Rank", "Numeric")
  lsTxtVal  <- c("Effects", "Metrics", "None")



  # 04. CHECK additive arguments -----

  lgcMtrcs  <- ifelse(data$metrics.name == "Probabilities",
                      TRUE,
                      FALSE)

  lgcScaleX <- ifelse(scaleX %in% lsScaleX,
                      FALSE,
                      TRUE)

  lgcTxtVal <- ifelse(txtValue %in% lsTxtVal,
                      FALSE,
                      TRUE)

  lgcColor  <- ifelse(length(which(ls()%in%ls(pattern = "color"))) > 0, FALSE,
                      ifelse(length(color) != data$n.tx,
                             TRUE, FALSE))

  lgcSzPnt  <- ifelse(is.null(szPnt),
                      FALSE,
                      ifelse(isFALSE(length(szPnt) == 1),
                             TRUE,
                             ifelse(isFALSE(is.numeric(szPnt)),
                                    TRUE,
                                    ifelse(FALSE %in% (szPnt >= 0),
                                           TRUE,
                                           ifelse(FALSE %in% (szPnt < 6),
                                                  TRUE, FALSE))))
                      )

  lgcSzFntTtl <- ifelse(is.null(szFntTtl),
                      FALSE,
                      ifelse(isFALSE(length(szFntTtl) == 1),
                             TRUE,
                             ifelse(isFALSE(is.numeric(szFntTtl)),
                                    TRUE,
                                    ifelse(FALSE %in% (szFntTtl >= 0),
                                           TRUE,
                                           ifelse(FALSE %in% (szFntTtl < 6),
                                                  TRUE, FALSE))))
                      )

  lgcSzFntTtlX <- ifelse(is.null(szFntTtlX),
                      FALSE,
                      ifelse(isFALSE(length(szFntTtlX) == 1),
                             TRUE,
                             ifelse(isFALSE(is.numeric(szFntTtlX)),
                                    TRUE,
                                    ifelse(FALSE %in% (szFntTtlX >= 0),
                                           TRUE,
                                           ifelse(FALSE %in% (szFntTtlX < 6),
                                                  TRUE, FALSE))))
                      )

  lgcSzFntX <- ifelse(is.null(szFntX),
                      FALSE,
                      ifelse(isFALSE(length(szFntX) == 1),
                             TRUE,
                             ifelse(isFALSE(is.numeric(szFntX)),
                                    TRUE,
                                    ifelse(FALSE %in% (szFntX >= 0),
                                           TRUE,
                                           ifelse(FALSE %in% (szFntX < 6),
                                                  TRUE, FALSE))))
                      )

  lgcSzFntY <- ifelse(is.null(szFntY),
                      FALSE,
                      ifelse(isFALSE(length(szFntY) == 1 | length(szFntY) == length(outcomes)),
                             TRUE,
                             ifelse(isFALSE(is.numeric(szFntY)),
                                    TRUE,
                                    ifelse(FALSE %in% (szFntY >= 0),
                                           TRUE,
                                           ifelse(FALSE %in% (szFntY < 6),
                                                  TRUE, FALSE))))
                      )

  lgcSzFntTxt <- ifelse(is.null(szFntTxt),
                      FALSE,
                      ifelse(isFALSE(length(szFntTxt) == 1 | length(szFntTxt) == length(outcomes)),
                             TRUE,
                             ifelse(isFALSE(is.numeric(szFntTxt)),
                                    TRUE,
                                    ifelse(FALSE %in% (szFntTxt >= 0),
                                           TRUE,
                                           ifelse(FALSE %in% (szFntTxt < 6),
                                                  TRUE, FALSE))))
                      )

  lgcSzFntLgnd <- ifelse(is.null(szFntLgnd),
                         FALSE,
                         ifelse(isFALSE(length(szFntLgnd) == 1),
                                TRUE,
                                ifelse(isFALSE(is.numeric(szFntLgnd)),
                                       TRUE,
                                       ifelse(FALSE %in% (szFntLgnd >= 0),
                                              TRUE,
                                              ifelse(FALSE %in% (szFntLgnd < 6),
                                                     TRUE, FALSE))))
                         )

  lgcRotateTxt <- ifelse(is.null(rotateTxt),
                       FALSE,
                       ifelse(isFALSE(length(rotateTxt) == 1),
                              TRUE,
                              ifelse(rotateTxt < 0 | rotateTxt > 360,
                                     TRUE, FALSE)))



  # 05 REPORT results from argument checking -----

  if (lgcMtrcs) {
    infoLgcMetrics <- paste(" Metrics: ERROR\n",
                            ' REQUIRE: Metrics should not be "Probabilities."')
  } else {
    infoLgcMetrics <- paste(" Metrics: OK")
  }

  if (lgcScaleX) {
    infoLgcScaleX  <- paste(" Scale on x-axis: ERROR\n",
                            ' REQUIRE: Argument "scaleX" should be "Rank" or "Numeric."')
  } else {
    infoLgcScaleX  <- paste(" Scale on x-axis: OK")
  }

  if (lgcTxtVal) {
    infoLgcTxtVal  <- paste(" Label of values: ERROR\n",
                            ' REQUIRE: Argument "txtValue" should be "Effects" or "Metrics"')
  } else {
    infoLgcTxtVal  <- paste(" Label of values: OK")
  }

  if (lgcColor) {
    infoLgcColor <- paste(" Color: ERROR\n",
              ' REQUIRE: Argument "color" must list colors for **EACH TREATMENT**.')
  } else {
    infoLgcColor <- paste(" Color: OK")
  }

  if (lgcSzPnt) {
    infoStopSzPnt <- 'Argument "szPnt" must be a numeric value between 0 and 5 for indicating point size of ranking metrics.'
  } else {
    infoStopSzPnt <- paste(" Bead size: OK")
  }

  if (lgcSzFntTtl) {
    infoStopSzFntTtl <- 'Argument "szFntTtl" must be a numeric value between 0 and 5 for indicating font size of main title of the beading plot.'
  } else {
    infoStopSzFntTtl <- paste(" Font size of main title: OK")
  }

  if (lgcSzFntTtlX) {
    infoStopSzFntTtlX <- 'Argument "szFntTtlX" must be a numeric value between 0 and 5 for indicating font size of title on X-axis.'
  } else {
    infoStopSzFntTtlX <- paste(" Font size of title on X-axis: OK")
  }

  if (lgcSzFntX) {
    infoStopSzFntX <- 'Argument "szFntX" must be a numeric value between 0 and 5 for indicating font size of numeric scale on X-axis.'
  } else {
    infoStopSzFntX <- paste(" Font size of numeric scale on X-axis: OK")
  }

  if (lgcSzFntY) {
    infoStopSzFntY <- 'Argument "szFntY" must be a numeric value between 0 and 5 for indicating font size of outcome name(s).'
  } else {
    infoStopSzFntY <- paste(" Font size of outcome name(s): OK")
  }

  if (lgcSzFntTxt) {
    infoStopSzFntTxt <- 'Argument "szFntTxt" must be a numeric value between 0 and 5 for indicating font size of value of each point.'
  } else {
    infoStopSzFntTxt <- paste(" Font size of point value(s): OK")
  }

  if (lgcSzFntLgnd) {
    infoStopSzFntLgnd <- 'Argument "szFntLgnd" must be a numeric value between 0 and 5 for indicating legend font.'
  } else {
    infoStopSzFntLgnd <- paste(" Legend font size: OK")
  }

  if (lgcRotateTxt) {
    infoLgcRotateTxt <- paste(" Rotate labels of values: WARNING!\n",
                            ' INFORM: Argument "rotateTxt" should be a numeric value
                            between 0 and 360, and *rankinma* is producing label of
                            value for each point with default argument in terms of
                            `rotateTxt = 60`.')
  } else {
    infoLgcRotateTxt <- paste(" Rotate labels of values: OK")
  }

  infoStop <- paste(infoLgcInher,      "\n",
                    infoLgcMetrics,    "\n",
                    infoLgcScaleX,     "\n",
                    infoLgcTxtVal,     "\n",
                    infoLgcColor,      "\n",
                    infoStopSzPnt,     "\n",
                    infoStopSzFntTtl,  "\n",
                    infoStopSzFntTtlX, "\n",
                    infoStopSzFntX,    "\n",
                    infoStopSzFntY,    "\n",
                    infoStopSzFntTxt,  "\n",
                    infoStopSzFntLgnd, "\n",
                    infoLgcRotateTxt,  "\n",
                    sep = "")

  if (lgcInher | lgcMtrcs | lgcScaleX | lgcTxtVal | lgcColor)
    stop(infoStop)



  # 06 PROCESS additive setting -----

  infoScaleX <- scaleX
  infoTxtVal <- txtValue

  if (infoTxtVal == "Effects") {
    dataBeads$txtVal <- paste(dataBeads$SM, ": ",
                              round(dataBeads$ES, 2),
                              sep = "")
  } else if (infoTxtVal == "Metrics") {
    dataBeads$txtVal <- paste(data$metrics.name, ": ",
                              round(dataBeads$metrics, 2),
                              sep = "")
  } else {
    dataBeads$txtVal <- NA
  }

  dataBeads$importance  <- dataBeads$outcomes
  dataBeads$shape       <- 16

  dataBeads$seq.outcome <- max(dataBeads$outcomes) + 1 - dataBeads$outcomes
  dataBeads$seq.tx      <- max(dataBeads$txs) + 1 - dataBeads$txs

  dataBeads$seq.axis.y  <- dataBeads$seq.outcome
  dataBeads             <- dataBeads[order(-dataBeads$seq.axis.y), ]

  dataBeadsPlot <- dataBeads

  colorTx <- data$color.txs
  data$n.char.outcome <- nchar(data$n.outcome)

  infoMaxChar         <- max(data$n.char.outcome)
  infoExcessSpaceLeft <- -(infoMaxChar/5)

  if (!is.null(color)) {
    if (length(which(ls() %in% ls(pattern = "color"))) > 0) {
      colorTx$colorTx <- rgb(col2rgb(color)[1, ]/255,
                             col2rgb(color)[2, ]/255,
                             col2rgb(color)[3, ]/255,
                             data$trans)
      for (color.i in c(1:nrow(dataBeadsPlot))) {
        dataBeadsPlot[color.i, "colorTx"] <- colorTx[which(dataBeadsPlot[color.i, "tx"] == colorTx$lsTx), "colorTx"]
      }
    }
  }

  if (is.null(szPnt)) {
    infoSzPnt           <- 2
    dataBeadsPlot$szPnt <- infoSzPnt
  } else {
    infoSzPnt           <- szPnt
    dataBeadsPlot$szPnt <- szPnt
  }

  if (is.null(szFntTtl)) {
    infoSzFntTtl           <- 1.2
    dataBeadsPlot$szFntTtl <- infoSzFntTtl
  } else {
    infoSzFntTtl           <- szFntTtl
    dataBeadsPlot$szFntTtl <- szFntTtl
  }

  if (is.null(szFntTtlX)) {
    infoSzFntTtlX           <- 1
    dataBeadsPlot$szFntTtlX <- infoSzFntTtlX
  } else {
    infoSzFntTtlX           <- szFntTtlX
    dataBeadsPlot$szFntTtlX <- szFntTtlX
  }

  if (is.null(szFntX)) {
    infoSzFntX           <- 0.8
    dataBeadsPlot$szFntX <- infoSzFntTtlX
  } else {
    infoSzFntX           <- szFntX
    dataBeadsPlot$szFntX <- szFntX
  }

  if (is.null(szFntY)) {
    infoSzFntY           <- ifelse(max(nchar(dataBeadsPlot$outcome)) > 10,
                                   1 / max(nchar(dataBeadsPlot$outcome)) * 10,
                                   1)
    dataBeadsPlot$szFntY <- infoSzFntY
  } else {
    infoSzFntY           <- szFntY
    dataBeadsPlot$szFntY <- szFntY
  }

  if (is.null(szFntTxt)) {
    infoSzFntTxt           <- 0.8
    dataBeadsPlot$szFntTxt <- infoSzFntTxt
  } else {
    infoSzFntTxt           <- szFntTxt
    dataBeadsPlot$szFntTxt <- szFntTxt
  }

  if (is.null(szFntLgnd)) {
    infoSzFntLgnd           <- ifelse(max(nchar(dataBeadsPlot$tx)) > 10,
                                      1 / max(nchar(dataBeadsPlot$tx)) * 10,
                                      1)
    dataBeadsPlot$szFntLgnd <- infoSzFntLgnd
  } else {
    infoSzFntLgnd           <- szFntLgnd
    dataBeadsPlot$szFntLgnd <- szFntLgnd
  }

  infoRotateTxt <- rotateTxt


  # 07 PLOT heat plot -----

  setPar <- par(no.readonly = TRUE)
  on.exit(par(setPar))

  par(mar = c(5, 5, 3, 5), xpd = TRUE)

  ## 07.1 Main part -----

  plot(dataBeadsPlot$metrics,
       dataBeadsPlot$seq.axis.y - 0.5, frame.plot = FALSE,
       #xlim = c(-0.3, 1.3),
       xlim = c(infoExcessSpaceLeft, 1.3),
       ylim = c(0, ceiling(max(dataBeadsPlot$importance, na.rm = TRUE))),
       xlab = "", xaxt = "n", yaxt = "n",ylab="",
       pch = 0,
       cex = 0)

  axis(side = 3, at = c(0.5),
       line = -2,
       tick = FALSE,
       labels = paste("Beading plot of ", data$metrics.name,
                      sep = ""),
       font = 2,
       cex.axis = infoSzFntTtl)

  #text(rep(-0.3, data$n.outcome),
  text(rep(-0.05, data$n.outcome),
       unique(dataBeadsPlot$seq.outcome) - 0.5,
       c(unique(dataBeadsPlot$outcome)),
       cex = dataBeadsPlot$szFntY,
       pos = 2)

  segments(0, c(dataBeadsPlot$outcomes) - 0.5,
           1, c(dataBeadsPlot$outcomes) - 0.5,
           col = "gray",
           lty = c(rep(1, data$n.outcome)))


  if (infoScaleX == "Rank") {
    axis(side = 1, at = c(0, 1),
         labels = c("Last", "Best"),
         cex.axis = infoSzFntX)

    axis(side = 1, at = c(0.5), line = 2, tick = FALSE,
         labels = paste("Rank order based on ",
                        data$metrics.name,
                        sep = ""),
         font = 1,
         cex.axis = infoSzFntTtlX)

    points(dataBeadsPlot$place,
           dataBeadsPlot$seq.axis.y - 0.5,
           pch = dataBeadsPlot$shape,
           col = c(dataBeadsPlot$colorTx),
           bg = c(dataBeadsPlot$colorTx),
           cex = infoSzPnt)
  } else {
    axis(side = 1, at = c(0, 0.2, 0.4, 0.6, 0.8, 1),
         cex.axis = infoSzFntX)

    axis(side = 1, at = c(0.5), line = 2, tick = FALSE,
         labels = paste("Worse <---   ",
                        data$metrics.name,
                        "   ---> Better",
                        sep = ""),
         font = 1,
         cex.axis = infoSzFntTtlX)

    points(dataBeadsPlot$metrics,
           dataBeadsPlot$seq.axis.y - 0.5,
           pch = dataBeadsPlot$shape,
           col = c(dataBeadsPlot$colorTx),
           bg = c(dataBeadsPlot$colorTx),
           cex = infoSzPnt)
  }


  ## 07.2 Text values on each point -----

  if (infoScaleX == "Rank") {
    text(dataBeadsPlot$place,
         dataBeadsPlot$seq.axis.y - 0.6,
         dataBeadsPlot$txtVal,
         col = "gray25",
         cex = infoSzFntTxt,
         pos = 2,
         srt = infoRotateTxt)
  } else {
    text(dataBeadsPlot$metrics,
         dataBeadsPlot$seq.axis.y - 0.6,
         dataBeadsPlot$txtVal,
         col = "gray25",
         cex = infoSzFntTxt,
         pos = 2,
         srt = infoRotateTxt)
  }




  ## 07.3 Legend -----

  dataLgnd <- dataBeadsPlot[order(dataBeadsPlot$tx), ]
  vctTx    <- unique(dataLgnd$tx)
  vctColor <- unique(dataLgnd$colorTx)

  points(c(rep(1.1, data$n.tx)),
         (data$n.outcome - 0.5) / data$n.tx * c(data$n.tx:1),
         pch = dataBeadsPlot$shape,
         col = vctColor,
         bg = vctColor,
         cex = infoSzPnt)

  text(c(rep(1.15, data$n.tx)),
       (data$n.outcome - 0.5) / data$n.tx * c(data$n.tx:1),
       vctTx,
       cex = infoSzFntLgnd,
       pos = 4)

}
