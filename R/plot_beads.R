#' @title Illustrate beading plot
#'
#' @author Chiehfeng Chen & Enoch Kang
#'
#' @description
#' **PlotBeads()** is a function for illustrating beading plot.
#'
#' @param data DATA of metrics for treatment ranking.
#' @param color LIST of colors for treatments in a network meta-analysis.
#'
#' @return
#' **PlotBeads()** returns a beading plot.
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
                      color = NULL) {

  dataBeads <- data$data

  lgcInher  <- !inherits(data, "rankinma")
  lgcMtrcs  <- ifelse(data$metrics.name == "Probabilities",
                      TRUE,
                      FALSE)

  lgcColor  <- ifelse(length(which(ls()%in%ls(pattern = "color"))) > 0, FALSE,
                      ifelse(length(color) != data$n.tx,
                             TRUE, FALSE))

  if (lgcInher) {
    infoLgcInher <- paste(" Inherit: ERROR\n",
              ' REQUIRE: Argument "data" must be an object of class \"rankinma\".')
  } else {
    infoLgcInher <- paste(" Inherit: OK")
  }

  if (lgcMtrcs) {
    infoLgcMetrics <- paste(" Metrics: ERROR\n",
              ' REQUIRE: Metrics should not be "Probabilities."')
  } else {
    infoLgcMetrics <- paste(" Metrics: OK")
  }

  if (lgcColor) {
    infoLgcColor <- paste(" Color: ERROR\n",
              ' REQUIRE: Argument "color" must list colors for **EACH TREATMENT**.')
  } else {
    infoLgcColor <- paste(" Color: OK")
  }

  infoStop <- paste(infoLgcInher, "\n",
                    infoLgcMetrics, "\n",
                    infoLgcColor, "\n",
                    sep = ""
                    )

  if (lgcInher | lgcMtrcs | lgcColor)
    stop(infoStop)

  dataBeads$importance  <- dataBeads$outcomes
  dataBeads$shape       <- 16

  dataBeads$seq.outcome <- max(dataBeads$outcomes) + 1 - dataBeads$outcomes
  dataBeads$seq.tx      <- max(dataBeads$txs) + 1 - dataBeads$txs

  dataBeads$seq.axis.y  <- dataBeads$seq.outcome
  dataBeads             <- dataBeads[order(-dataBeads$seq.axis.y), ]

  txs      <- unique(dataBeads$tx)
  outcomes <- unique(dataBeads$outcome)

  dataBeadsPlot <- dataBeads

  colorTx <- data$color.txs

  if (!is.null(color)) {
    if (length(which(ls()%in%ls(pattern = "color"))) > 0) {
      colorTx$colorTx <- rgb(col2rgb(color)[1, ]/255,
                             col2rgb(color)[2, ]/255,
                             col2rgb(color)[3, ]/255,
                             data$trans)
      for (color.i in c(1:nrow(dataBeadsPlot))) {
        dataBeadsPlot[color.i, "colorTx"] <- colorTx[which(dataBeadsPlot[color.i, "tx"] == colorTx$lsTx), "colorTx"]
      }
    }
  }

  setPar <- par(no.readonly = TRUE)
  on.exit(par(setPar))

  par(mar = c(5, 5, 3, 5), xpd = TRUE)

  plot(dataBeadsPlot$metrics,
       dataBeadsPlot$seq.axis.y - 0.5, frame.plot = FALSE,
       xlim = c(-0.3, 1.3),
       ylim = c(0, ceiling(max(dataBeadsPlot$importance, na.rm = TRUE))),
       xlab = "", xaxt = "n", yaxt = "n",ylab="",
       pch = 0, cex = 0)
  axis(side = 1, at = c(0, 0.2, 0.4, 0.6, 0.8,1), cex.axis = 0.8)
  axis(side = 3, at = c(0.5), line = -2, tick = FALSE,
       labels = paste("Beading plot of ", data$metrics.name,
                      sep = ""),
       font = 2, cex.axis = 1.2)
  axis(side = 1, at = c(0.5), line = 2, tick = FALSE,
       labels = paste("Worse <---   ", data$metrics.name,
                      "   ---> Better", sep = ""),
       font = 1, cex.axis = 1)
  text(rep(-0.3, data$n.outcome),
       unique(dataBeadsPlot$seq.outcome) - 0.5,
       c(unique(dataBeadsPlot$outcome)), pos = 4, cex = 1)
  segments(0, c(dataBeadsPlot$outcomes) - 0.5,
           1, c(dataBeadsPlot$outcomes) - 0.5, col = "gray",
           lty = c(rep(1, data$n.outcome)))

  points(dataBeadsPlot$metrics,
         dataBeadsPlot$seq.axis.y - 0.5,
         pch = dataBeadsPlot$shape,
         col = c(dataBeadsPlot$colorTx),
         bg = c(dataBeadsPlot$colorTx), cex = 2)
  points(c(rep(1.1, data$n.tx)),
         (data$n.outcome - 0.5) / data$n.tx * c(1:data$n.tx),
         pch = dataBeadsPlot$shape,
         col = c(unique(dataBeadsPlot$colorTx)),
         bg = c(unique(dataBeadsPlot$colorTx)), cex = 2)

  text(c(rep(1.15, data$n.tx)),
       (data$n.outcome - 0.5) / data$n.tx * c(1:data$n.tx),
       unique(dataBeadsPlot$tx), pos = 4)
}
