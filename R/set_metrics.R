#' @title Setup data of treatment ranking metrics for rankinma
#'
#' @description
#' **SetMetrics()** is a function for checking and preparing data set of metrics
#' for further ploting in *rankinma*.
#'
#' @param data DATAFRAME of treatment, metrics, and name of outcomes.
#' @param outcome VARIABLE string data for of outcome(s).
#' @param tx VARIABLE with string data for treatments.
#' @param metrics VARIABLE with numeric data for global metrics, but it should
#'        be "NULL" when using "Probabilities" as metrics.
#' @param metrics.name STRING for metrics of treatment ranking in terms of
#'        "SUCRA" and "P-score" for the value of surface under the cumulative
#'        ranking curve or P-score.
#' @param trans NUMERIC for indicating transparency of colors of treatments.
#'
#' @return
#' **SetMetrics()** returns a confirmed data.frame of treatment, metrics of
#' treatment ranking, and outcome name.
#' \item{metrics.name}{A string shows type of metrics of treatment ranking.}
#' \item{ls.outcome}{Strings list outcomes.}
#' \item{ls.tx}{Strings list treatments.}
#' \item{n.outcome}{An integer shows numbers of outcomes.}
#' \item{n.tx}{An integer shows numbers of treatments.}
#' \item{data}{A data frame consists of seven columns of core information among
#'       all outcomes.}
#' \item{data.sets}{A list shows data frame of core information by each outcome.}
#' \item{ptrn.tx}{A data frame shows treatments on each outcome.}
#' \item{ptrn.outcome}{A data frame shows outcomes by treatments.}
#' \item{color.txs}{A data frame shows color of each treatment.}
#' \item{trans}{A numeric value shows transparency for colors of each treatment.}
#'
#' @seealso \code{\link{GetMetrics}}
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
#' ## End(Not run)
#'
#' @export SetMetrics

SetMetrics <- function(data,
                       outcome = NULL,
                       tx = NULL,
                       metrics = NULL,
                       metrics.name = NULL,
                       trans = 0.8) {

  data <- data.frame(data)

  # 01 CHECK arguments -----

  lsMetrics <- c("Probabilities", "P-best", "SUCRA", "P-score")

  namTx      <- deparse(substitute(tx))
  namMetrics <- deparse(substitute(metrics))
  namOutcome <- deparse(substitute(outcome))

  lgcTx      <- !(namTx %in% colnames(data))
  lgcOutcome <- !(namOutcome %in% colnames(data))
  lgcMetrics <- ifelse(metrics.name == "Probabilities",
                       FALSE,
                       !(namMetrics %in% colnames(data)))
  lgcDupl    <- ifelse(sum(table(data$tx, data$outcome)) == sum(table(data$tx, data$outcome) == 1),
                       FALSE, TRUE)
  lgcTrans   <- ifelse(is.numeric(trans),
                       ifelse(trans <= 1 & trans >= 0,
                              FALSE, TRUE), TRUE)

  infoLgcWarning <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = infoLgcWarning))

  # 02 REPORT results from argument checking -----

  if (!(namTx %in% colnames(data))) {
    infoLgcTx <- paste(" Treatment: ERROR\n",
              ' REQUIRE: Argument "tx" must be specified.\n')
  } else {
    colnames(data)[which(colnames(data) == namTx)] <- "tx"
    infoLgcTx <- paste(" Treatment: OK")
  }

  if (!(namOutcome %in% colnames(data))) {
    infoLgcOutcome <- paste(" Outcome: ERROR\n",
              ' REQUIRE: Argument "outcome" must be specified.')
  } else {
    colnames(data)[which(colnames(data) == namOutcome)] <- "outcome"
  }

  if (isFALSE(sum(table(data$tx, data$outcome)) == sum(table(data$tx, data$outcome) == 1))) {
    infoLgcOutcome <- paste(" Outcome: ERROR\n",
              ' REQUIRE: Duplicates of treatment in a same outcome.')
  } else {
    infoLgcOutcome <- paste(" Outcome: OK")
  }

  if (is.null(metrics.name)) {
    infoLgcName <- paste(" Metrics name: ERROR\n",
              ' REQUIRE: Argument "metrics.name" must be "Probabilities,"
              "SUCRA," or "P-score."')
  } else if (!(metrics.name %in% lsMetrics)) {
    infoLgcName <- paste(" Metrics name: ERROR\n",
              ' REQUIRE: Argument "metrics.name" must be "Probabilities,"
              "SUCRA," or "P-score."')
  } else {
    infoLgcName <- paste(" Metrics name: OK")
  }

  if (metrics.name != "Probabilities") {
    if (!(namMetrics %in% colnames(data))) {
      infoLgcMetrics <- paste(" Metrics: ERROR\n",
                ' REQUIRE: Argument "metrics" must be specified for SUCRA or P-score.')
    } else {
      colnames(data)[which(colnames(data) == namMetrics)] <- "metrics"
      if (!is.numeric(data$metrics)) {
        infoLgcMetrics <- paste(" Metrics: ERRROR\n",
                  ' REQUIRE: Argument "metrics" must be numeric data for SUCRA
                  or P-score.')
      } else {
        infoLgcMetrics <- paste(" Metrics: OK")
      }
    }
  } else if (isFALSE(is.null(namMetrics))) {
    infoLgcMetrics <- paste(" Metrics: WARNING!\n",
              ' INFORM: Argument "metrics" is not mandatory for "Probabilities,
              and *rankinma* ignores your argument "', namMetrics,  '".',
              sep = "")
  }

  if (lgcTrans) {
    infoLgcTrans <- paste(" Transparency: ERROR\n",
              ' REQUIRE: argument "trans" must be between 0 and 1.')
  } else {
    infoLgcTrans <- paste(" Transparency: OK")
  }

  if (metrics.name == "Probabilities") {
    if (lgcTx |
        lgcOutcome |
        !(metrics.name %in% lsMetrics) |
        lgcMetrics |
        lgcDupl |
        lgcTrans) {
      lgcOverall <- TRUE
    } else {
      lgcOverall <- FALSE
    }
  } else if (lgcTx |
             lgcOutcome |
             !(metrics.name %in% lsMetrics) |
             lgcMetrics |
             !is.numeric(data$metrics) |
             lgcDupl |
             lgcTrans) {
    lgcOverall <- TRUE
  } else {
    lgcOverall <- FALSE
  }

  infoStop <- paste(infoLgcTx, "\n",
                    infoLgcOutcome, "\n",
                    infoLgcName, "\n",
                    infoLgcMetrics, "\n",
                    infoLgcTrans, "\n",
                    sep = ""
                    )

  if (lgcOverall)
    stop(infoStop)


  # 03 PREPARE data -----

  txs          <- unique(data$tx)
  outcomes     <- unique(data$outcome)
  infoOutcomes <- length(outcomes)

  if (metrics.name != "Probabilities") {
    colnames(data)[length(data) - 1] <- "ES"
    colnames(data)[length(data) - 2] <- "SM"
  }

  for (i in c(1:nrow(data))) {
    data[i, "txs"] <- which(as.character(txs) == as.character(data[i, "tx"]))
  }
  for (i in seq(1, nrow(data))) {
    data[i, "outcomes"] <- which(as.character(outcomes) == as.character(data[i, "outcome"]))
  }

  data$importance <- max(data$outcomes) + 1 - data$outcomes


  ## 03.1 SET colors for treatments -----

  colorTx <- data.frame(lsTx   = unique(data$tx),
                        seqTx  = c(1:length(unique(data$tx))),
                        colorTx = rainbow(length(unique(data$tx)))
  )

  colorTrans      <- rgb(1, 1, 1, trans)
  colorTrans      <- substring(colorTrans, 8, 9)
  colorTx$colorTx <- paste(colorTx$colorTx, colorTrans, sep = "")

  data$colorTx <- NA

  for (color.i in c(1:nrow(data))) {
    data[color.i, "colorTx"] <- colorTx[which(data[color.i, "tx"] == colorTx$lsTx), "colorTx"]
  }


  ## 03.2 SET metrics -----

  if (metrics.name == "Probabilities") {
    dataSet <- as.data.frame(cbind(
      data[, c("outcome", "outcomes", "tx", "txs", "rank", "colorTx")],
      data[, c(3:(nrow(data) * 2 + 2))]))
  } else {

    dataSet <- data[, c("outcome", "outcomes", "importance", "tx", "txs", "colorTx", "metrics", "SM", "ES")]

    for (outcome.i in c(1:max(dataSet$outcomes))) {
      dataTempA <- dataSet[dataSet$outcomes == outcome.i, ]
      dataTempA <- dataTempA[order(dataTempA$metrics, decreasing = TRUE), ]

      dataTempA$rank  <- 0

      for (tx.i in c(1:nrow(dataTempA))) {
        if (tx.i == 1) {
          dataTempA[tx.i, "rank"]  <- 1
        } else {
          dataTempA[tx.i, "rank"] <- ifelse(dataTempA[tx.i, "metrics"] == dataTempA[tx.i - 1, "metrics"],
                                            dataTempA[tx.i - 1, "rank"],
                                            tx.i)
        }
      }

      dataTempA$place <- (max(dataTempA$rank) - dataTempA$rank) / (max(dataTempA$rank) - 1)

      if (length(which(as.data.frame(table(dataTempA$metrics))$Freq > 1)) > 0) {
        for (tx.i in c(1:(nrow(dataTempA) - 1))) {
          if (dataTempA$importance[tx.i] %in% dataTempA$importance[c((tx.i + 1):nrow(dataTempA))]) {
            importanceAdj <- sum(dataTempA$metrics[c((tx.i + 1):nrow(dataTempA))] == dataTempA$metrics[tx.i]) / 20
            dataTempA$importance[tx.i] <- importanceAdj + dataTempA$importance[tx.i]
          }
        }
      }

      if (outcome.i == 1) {
        dataTempB <- dataTempA
      } else {
        dataTempB <- rbind(dataTempA, dataTempB)
      }
    }
    dataSet <- dataTempB
  }

  dataSet <- dataSet[order(dataSet$outcomes), ]


  if (infoOutcomes > 1) {

    ## 03.3 BUILD a table of treatments patterns (txs on outcomes)  -----
    if (metrics.name != "Probabilities") {
      mtxTxOutcome <- table(dataSet$outcome, dataSet$tx)

      tblTxOutcome <- data.frame(mtxTxOutcome[, 1])

      for (tx.i in c(1:nrow(tblTxOutcome))) {
        tblTxOutcome[tx.i, c(2:length(colnames(mtxTxOutcome)))] <- mtxTxOutcome[tx.i, c(2:length(colnames(mtxTxOutcome)))]
      }

      colnames(tblTxOutcome)[c(1:length(colnames(mtxTxOutcome)))] <- colnames(mtxTxOutcome)

      tblTxOutcome$nTx <- rowSums(tblTxOutcome)

      tblTxOutcome$patternTx <- NA

      for (tx.i in c(1:(length(tblTxOutcome)-2))) {
        if (tx.i == 1) {
          tblTxOutcome[, "patternTx"] <- tblTxOutcome[, tx.i]
        } else {
          tblTxOutcome[, "patternTx"] <- paste(tblTxOutcome[, "patternTx"],
                                               tblTxOutcome[, tx.i],
                                               sep = "")
        }
      }

      tblTxOutcome$Patterns <- NA

      for (outcome.i in c(1:nrow(tblTxOutcome))) {
        pattern <- which(names(table(tblTxOutcome$patternTx)) == tblTxOutcome[outcome.i, "patternTx"])
        tblTxOutcome[outcome.i, "Patterns"] <- pattern
      }

      tblTxOutcome$pattern <- NA

      for (outcome.i in c(1:nrow(tblTxOutcome))) {
        txOutcome <- which(tblTxOutcome[outcome.i, c(1:(length(tblTxOutcome) - 3))] == 1)

        for (tx.i in txOutcome) {
          if (tx.i == txOutcome[1]) {
            tblTxOutcome[outcome.i, "pattern"] <- colnames(tblTxOutcome)[tx.i]
          } else {
            tblTxOutcome[outcome.i, "pattern"] <- paste(tblTxOutcome[outcome.i, "pattern"],
                                                        colnames(tblTxOutcome)[tx.i],
                                                        sep = ", ")
          }
        }
      }

      tblTxOutcome <- tblTxOutcome[, -c(1:(length(mtxTxOutcome) / nrow(mtxTxOutcome)), which(colnames(tblTxOutcome) == "patternTx"))]
    }


    ## 03.4 BUILD a table of outcome patterns (outcomes on txs)  -----
    if (metrics.name != "Probabilities") {
      mtxOutcomeTx <- table(dataSet$tx, dataSet$outcome)

      tblOutcomeTx <- data.frame(mtxOutcomeTx[, 1])

      for (tx.i in c(1:nrow(tblOutcomeTx))) {
        tblOutcomeTx[tx.i, c(2:length(colnames(mtxOutcomeTx)))] <- mtxOutcomeTx[tx.i, c(2:length(colnames(mtxOutcomeTx)))]
      }

      colnames(tblOutcomeTx)[c(1:length(colnames(mtxOutcomeTx)))] <- colnames(mtxOutcomeTx)

      tblOutcomeTx$nOutcome <- rowSums(tblOutcomeTx)

      tblOutcomeTx$patternOutcome <- NA

      for (outcome.i in c(1:(length(tblOutcomeTx)-2))) {
        if (outcome.i == 1) {
          tblOutcomeTx[, "patternOutcome"] <- tblOutcomeTx[, outcome.i]
        } else {
          tblOutcomeTx[, "patternOutcome"] <- paste(tblOutcomeTx[, "patternOutcome"],
                                                    tblOutcomeTx[, outcome.i],
                                                    sep = "")
        }
      }

      tblOutcomeTx$Patterns <- NA

      for (outcome.i in c(1:nrow(tblOutcomeTx))) {
        pattern <- which(names(table(tblOutcomeTx$patternOutcome)) == tblOutcomeTx[outcome.i, "patternOutcome"])
        #tblOutcomeTx[outcome.i, "nPattern"] <- table(tblOutcomeTx$patternOutcome)[[pattern]]
        tblOutcomeTx[outcome.i, "Patterns"] <- pattern
      }


      tblOutcomeTx$pattern <- NA

      for (tx.i in c(1:nrow(tblOutcomeTx))) {
        outcomeTx <- which(tblOutcomeTx[tx.i, c(1:(length(tblOutcomeTx) - 3))] == 1)

        for (outcome.i in outcomeTx) {
          if (outcome.i == outcomeTx[1]) {
            tblOutcomeTx[tx.i, "pattern"] <- colnames(tblOutcomeTx)[outcome.i]
          } else {
            tblOutcomeTx[tx.i, "pattern"] <- paste(tblOutcomeTx[tx.i, "pattern"],
                                                   colnames(tblOutcomeTx)[outcome.i],
                                                   sep = ", ")
          }
        }
      }

      tblOutcomeTx <- tblOutcomeTx[, -c(1:(length(mtxOutcomeTx) / nrow(mtxOutcomeTx)), which(colnames(tblOutcomeTx) == "patternOutcome"))]
    }

  }


  # 04 BUILD an object of *rankinma* class  -----

  dataList        <- list(metrics.name = metrics.name,
                          ls.outcome   = unique(data$outcome),
                          ls.tx        = unique(data$tx),
                          n.outcome    = length(unique(data$outcome)),
                          n.tx         = length(unique(data$tx)))
  class(dataList)       <- "rankinma"
  dataList$data         <- dataSet
  dataList$data.sets    <- split(dataSet, dataSet$outcome)
  if (infoOutcomes > 1) {
    if (metrics.name != "Probabilities") {
      dataList$ptrn.tx      <- tblTxOutcome
      dataList$ptrn.outcome <- tblOutcomeTx
    }
  }
  dataList$color.txs    <- colorTx
  dataList$trans        <- trans
  dataRankinma          <- dataList


  # 05 REPORT returns of function `SetMetrics()` -----

  cat(paste("\n"), fill = TRUE, sep = "")
  cat(paste("Summary of metrics:\n",
            "Metrics: ", dataList$metrics.name, "\n",
            "Outcomes:   ", dataList$n.outcome, "\n",
            "Treatments: ", dataList$n.tx, "\n"),
      fill = TRUE, sep = "")

  cat(paste("List of outcomes:"),
      fill = TRUE, sep = "")
  cat(paste(" ", c(1:dataList$n.outcome), dataList$ls.outcome, sep = " "),
      fill = TRUE, sep = "\n")

  cat(paste("List of treatments:"),
      fill = TRUE, sep = "")
  cat(paste(" ", c(1:dataList$n.tx), dataList$ls.tx, sep = " "),
      fill = TRUE, sep = "\n")

  dataRankinma <- dataList

}

#' @title Display color for each treatment
#'
#' @author Enoch Kang
#'
#' @description
#' ShowColor is a function for showing colors of every treatment on plot of
#' treatment rank metrics in *rankinma*.
#'
#' @param data DATA of *rankinma* class.
#'
#' @return
#' **ShowColor()** show a plot of color for each treatment.
#'
#' @noRd
ShowColor <- function(data) {

  dataColor  <- data$color.txs

  if (!inherits(data, "rankinma"))
    stop('Argument "data" must be an object of class \"rankinma\".')

  plot(c(0, 1.5),
       c(0, 1),
       frame.plot = FALSE,
       xlab = "", ylab="", xaxt = "n", yaxt = "n",
       col = "white")
  segments(rep(0.5, max(dataColor$seqTx)),
           (0.95) / max(dataColor$seqTx) * unique(dataColor$seqTx),
           rep(0.7, max(dataColor$seqTx)),
           (0.95) / max(dataColor$seqTx) * unique(dataColor$seqTx),
           lwd = 6,
           col = dataColor$colorTx)
  text(rep(0.8, max(dataColor$seqTx)),
       (0.95) / max(dataColor$seqTx) * unique(dataColor$seqTx),
       dataColor$lsTx, pos = 4)
}
