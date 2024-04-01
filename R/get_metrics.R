#' @title Get treatment ranking metrics from network meta-analysis output
#'
#' @description
#' **GetMetrics()** is a function for gathering metrics of treatment ranking
#' from *netmeta* output.
#'
#' @import netmeta
#' @import mvtnorm
#'
#' @param data    DATA of *netmeta* output.
#' @param outcome STRING for name of outcome.
#' @param prefer  STRING for indicating which direction is beneficial treatment
#'                effect in terms of "small" and "large" values in statistic test.
#' @param metrics STRING for metrics of treatment ranking in terms of "SUCRA",
#'                "P-score", and "P-best" for the value of surface under the
#'                cumulative ranking curve, P-score, and probability of achieving
#'                the best treatment.
#' @param model   STRING for analysis model in terms of "random" and "common" for
#'                random-effects model and common-effect model.
#' @param simt    INTEGER for times of simulations to estimate surface under the
#'                cumulative ranking curve (SUCRA).
#' @param rob     STRING for column name of risk of bias.
#'
#' @return
#' **GetMetrics()** returns a data.frame with three columns, including
#' treatment, metrics of treatment ranking, and outcome name.
#'
#' @references
#' Rücker, G., & Schwarzer, G. (2015). Ranking treatments in frequentist
#' network meta-analysis works without resampling methods.
#' **BMC medical research methodology**, *15(1)*, 1-9.
#'
#' Salanti, G., Ades, A. E., & Ioannidis, J. P. (2011). Graphical methods and
#' numerical summaries for presenting results from multiple-treatment
#' meta-analysis: an overview and tutorial. **Journal of clinical epidemiology**,
#' *64(2)*, 163-171.
#'
#' @seealso \code{\link{SetMetrics}}
#'
#' @examples
#' ## Not run:
#' #library(netmeta)
#' #data(Senn2013)
#' #nma <- netmeta(TE, seTE, treat1, treat2,
#' #studlab, data = Senn2013, sm = "SMD")
#'
#' # Get SUCRA
#' #dataMetrics <- GetMetrics(nma, outcome = "HbA1c", prefer = "small",
#' #metrics = "SUCRA", model = "random", simt = 1000)
#'
#' # Get P-score
#' #dataMetrics <- GetMetrics(nma, outcome = "HbA1c", prefer = "small",
#' #metrics = "P-score", model = "random", simt = 1000)
#' ## End(Not run)
#'
#' @export GetMetrics

GetMetrics <- function(data,
                       outcome = NULL,
                       prefer  = NULL,
                       metrics = NULL,
                       model   = "random",
                       simt    = 1000,
                       rob     = NULL) {

  # 01 CHECK arguments -----

  lsMetrics <- c("Probabilities", "P-best", "SUCRA", "P-score", "ES", "ALL")

  argOutcome <- ifelse(!is.null(outcome), outcome, "outcome")
  argPrefer  <- ifelse(prefer == "small", "good",
                       ifelse(prefer == "large", "bad", "unspecified"))
  argMetrics <- metrics
  argModel   <- ifelse(model == "common", "common",
                       ifelse(model == "fixed", "common",
                              ifelse(model == "random", "random",
                                     "unspecified")))

  lgcInher   <- !inherits(data, "netmeta")
  lgcPrefer  <- isTRUE(argPrefer == "unspecified")
  lgcMetrics <- !(argMetrics %in% lsMetrics)
  lgcModel   <- isTRUE(argModel == "unspecified")

  infoLgcWarning <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = infoLgcWarning))

  # 02 REPORT results from argument checking -----

  if (lgcInher) {
    infoLgcInher <- paste(" Inherit: ERROR\n",
                          ' REQUIRE: Argument "data" must be an object of class \"netmeta\".')
  } else {
    infoLgcInher <- paste(" Inherit: OK")
  }

  if (argOutcome == "outcome") {
    infoLgcOutcome <- paste(" Outcome: WARNING!\n",
                            ' SUGGEST: It is better to have a \"specific name\" in the argument "outcome."')
  } else {
    infoLgcOutcome <- paste(" Outcome: OK")
  }

  if (lgcPrefer) {
    infoLgcPrefer <- paste(" Prefer: ERROR\n",
                           ' REQUIRE: Argument "prefer" must be \"small\" or  \"large\".')
  } else {
    infoLgcPrefer <- paste(" Prefer: OK")
  }

  if (lgcMetrics) {
    infoLgcMetrics <- paste(" Metrics: ERROR\n",
                            ' REQUIRE: Argument "metrics" must be \"Probabilities\", \"SUCRA\", or  \"P-score\".')
  } else {
    infoLgcMetrics <- paste(" Metrics: OK")
  }

  if (lgcModel) {
    infoLgcModel <- paste(" Model: ERROR\n",
                          ' REQUIRE: Argument "model" must be \"random\" or  \"common\".')
  } else {
    infoLgcModel <- paste(" Model: OK")
  }

  infoStop <- paste(infoLgcInher, "\n",
                    infoLgcOutcome, "\n",
                    infoLgcPrefer, "\n",
                    infoLgcMetrics, "\n",
                    infoLgcModel, "\n",
                    sep = ""
  )

  if (lgcInher |
      lgcPrefer |
      lgcMetrics |
      lgcModel)
    stop(infoStop)


  # 03 GET information from object of *netmeta* class -----

  infoSeedOld <- set.seed(NULL)
  on.exit(set.seed(infoSeedOld))

  ## 03.1 GET treatment ranking probabilities and global metrics from object of *netmeta* class -----

  if (argModel == "random") {
    set.seed(101020)
    outRankogram <- netmeta::rankogram(data, nsim = simt,
                                       random = TRUE,
                                       small.values = argPrefer)

    vctPbest     <- outRankogram$ranking.matrix.random[, 1]

    set.seed(101020)
    vctPscore    <- netmeta::netrank(data, method = "P-score",
                                     random = TRUE,
                                     small.values = argPrefer)$ranking.random

    set.seed(101020)
    vctSUCRA     <- netmeta::netrank(data, method = "SUCRA",
                                     random = TRUE,
                                     small.values = argPrefer)$ranking.random

    if (data$sm == "MD" | data$sm == "SMD") {
      vctES <- data$TE.random[, which(colnames(data$TE.random) == data$reference.group)]
    } else {
      vctES <- exp(data$TE.random[, which(colnames(data$TE.random) == data$reference.group)])
    }

  } else {
    set.seed(101020)
    outRankogram <- netmeta::rankogram(data, nsim = simt,
                                       random = FALSE,
                                       small.values = argPrefer)

    vctPbest     <- outRankogram$ranking.matrix.common[, 1]

    set.seed(101020)
    vctPscore    <- netmeta::netrank(data, method = "P-score",
                                     common = TRUE,
                                     small.values = argPrefer)$ranking.common

    set.seed(101020)
    vctSUCRA     <- netmeta::netrank(data, method = "SUCRA",
                                     common = TRUE,
                                     small.values = argPrefer)$ranking.common

    if (data$sm == "MD" | data$sm == "SMD") {
      vctES <- data$TE.common[, which(colnames(data$TE.common) == data$reference.group)]
    } else {
      vctES <- exp(data$TE.common[, which(colnames(data$TE.common) == data$reference.group)])
    }

  }


  ## 03.2 GET studies contribution and risk of bias from object of *netmeta* class -----

  if (is.null(substitute(rob))) {
    namRoB <- "NULL"

  } else {

    namRoB <- deparse(substitute(rob))

    lsRoBL <- c(1, "L", "low", "Low", "LOW", "low risk", "Low risk", "LOW RISK")
    lsRoBM <- c(2, "M", "moderate", "Moderate", "MODERATE", "some concern", "Some concern", "SOME CONCERN", "some concerns", "Some concerns", "SOME CONCERNs", "unclear", "Unclear", "UNCLEAR")
    lsRoBH <- c(3, "H", "high", "High", "HIGH", "high risk", "High risk", "HIGH RISK")
    vctTx  <- unique(c(data$treat1, data$treat2))

    lsContributionStudy <- netmeta::netimpact(data,
                                              verbose = TRUE)
    if (argModel == "random") {
      tblContributionStudy <- as.data.frame(lsContributionStudy$impact.random)
    } else {
      tblContributionStudy <- as.data.frame(lsContributionStudy$impact.fixed)
    }

    infoComparisons <- ncol(tblContributionStudy)
    vctStudy        <- rownames(tblContributionStudy)
    tblRoBGlobal    <- data.frame(study = vctStudy,
                                  rob   = NA)

    for (i.study in vctStudy) {
      tblRoBGlobal[tblRoBGlobal$study == i.study, "rob"] <- data$data[which(data$data$studlab == i.study), namRoB][1]
    }

    tblRoBGlobal$rob <- ifelse(tblRoBGlobal$rob %in% lsRoBL,
                               1,
                               ifelse(tblRoBGlobal$rob %in% lsRoBM,
                                      2, 3)
    )

    for (i.tx in vctTx) {
      tblContributionStudy$tx <- NA
      infoContributionTotal   <- sum(tblContributionStudy[, grep(i.tx, colnames(tblContributionStudy))])

      for (i.study in vctStudy) {
        if (i.study == vctStudy[1]) {
          vctRoBGlobal <- sum(tblContributionStudy[i.study, grep(i.tx,
                                                                 colnames(tblContributionStudy))])
        } else {
          tempRoBContribution <- sum(tblContributionStudy[i.study, grep(i.tx,
                                                                        colnames(tblContributionStudy))])
          vctRoBGlobal  <- c(vctRoBGlobal, tempRoBContribution)
        }
      }

      tblRoBGlobal$tx <- vctRoBGlobal / infoContributionTotal
      colnames(tblRoBGlobal)[which(colnames(tblRoBGlobal) == "tx")] <- i.tx
    }

    tblTxContributionStudy <- tblRoBGlobal

    for (i.study in vctStudy) {
      tblRoBGlobal[which(tblRoBGlobal$study == i.study), vctTx] <- tblRoBGlobal[which(tblRoBGlobal$study == i.study), "rob"] * tblRoBGlobal[which(tblRoBGlobal$study == i.study), vctTx]
    }

    tblRoBTxGlobal <- as.data.frame(colSums(tblRoBGlobal[, c(3:ncol(tblRoBGlobal))]))
    colnames(tblRoBTxGlobal) <- c("robMean")
    tblRoBTxGlobal$tx <- rownames(tblRoBTxGlobal)
    tblRoBTxGlobal    <- tblRoBTxGlobal[, c("tx", "robMean")]

    tblRoBTxGlobal$robMajor <- NA
    tblRoBTxGlobal$robWorst <- NA

    for (i.tx in tblRoBTxGlobal$tx) {
      tblRoBTx <- tblRoBGlobal[, c("study", "rob", i.tx)]
      infoRoBH <- max(tblRoBTx$rob)
      infoRoBL <- min(tblRoBTx$rob)

      if(1 %in% tblRoBTx$rob) {
        infoRoBL <- ifelse(sum(tblRoBTx[which(tblRoBTx$rob == 1), i.tx]) > 0,
                           1, 0)
        lgcRoBL  <- ifelse(infoRoBL > 0, TRUE, FALSE)
      } else {
        infoRoBL <- 0
        lgcRoBL  <- FALSE
      }

      if(2 %in% tblRoBTx$rob) {
        infoRoBM <- ifelse(sum(tblRoBTx[which(tblRoBTx$rob == 2), i.tx]) > 0,
                           1, 0)
        lgcRoBM  <- ifelse(infoRoBM > 0, TRUE, FALSE)
      } else {
        infoRoBM <- 0
        lgcRoBM  <- FALSE
      }

      if(3 %in% tblRoBTx$rob) {
        infoRoBH <- ifelse(sum(tblRoBTx[which(tblRoBTx$rob == 3), i.tx]) > 0,
                           1, 0)
        lgcRoBH  <- ifelse(infoRoBH > 0, TRUE, FALSE)
      } else {
        infoRoBH <- 0
        lgcRoBH  <- FALSE
      }

      tblRoBTxGlobal[which(tblRoBTxGlobal$tx == i.tx), "robMajor"] <- ifelse(infoRoBH == max(c(infoRoBL, infoRoBM, infoRoBH)),
                                                                             3,
                                                                             ifelse(infoRoBM == max(c(infoRoBL, infoRoBM, infoRoBH)),
                                                                                    2, 1))

      tblRoBTxGlobal[which(tblRoBTxGlobal$tx == i.tx), "robWorst"] <- ifelse(lgcRoBH,
                                                                             3,
                                                                             ifelse(lgcRoBM,
                                                                                    2,
                                                                                    1)
      )
    }
  }


  # 04 BUILD an object as output of function `GetMetrics()` -----
  ## 04.1 BUILD an object based on treatment ranking probabilities as output of function `GetMetrics()` -----

  if (argMetrics == "Probabilities") {
    dataGetProb  <- as.data.frame(t(outRankogram$ranking.matrix.random))

    colnames(dataGetProb) <- paste(colnames(dataGetProb),
                                   rep("Prob",
                                       length(dataGetProb)),
                                   sep = "")

    dataGetProbCum <- as.data.frame(t(outRankogram$cumrank.matrix.random))
    colnames(dataGetProbCum) <- paste(colnames(dataGetProbCum),
                                      rep("Cum",
                                          length(dataGetProbCum)),
                                      sep = "")

    #dataGetProbCum$rank <- seq(1:length(dataGetProbCum))
    dataGet <- cbind(dataGetProb, dataGetProbCum)

    dataGet$tx   <- colnames(dataGet)[c(1:nrow(dataGet))]
    dataGet$tx   <- substr(dataGet$tx,
                           1,
                           nchar(dataGet$tx) - 4)
    dataGet$rank <- as.numeric(rownames(dataGet))
    dataGet      <- dataGet[, c(length(dataGet) - 1,
                                length(dataGet),
                                2:length(dataGet) - 2)]

    if (namRoB == "NULL") {

      dataGet$robMean  <- NA
      dataGet$robMajor <- NA
      dataGet$robWorst <- NA

    } else {
      for (i.tx in dataGet$tx) {
        dataGet[which(dataGet$tx == i.tx), "robMean"]  <- tblRoBTxGlobal[which(tblRoBTxGlobal$tx == i.tx), "robMean"]
        dataGet[which(dataGet$tx == i.tx), "robMajor"] <- tblRoBTxGlobal[which(tblRoBTxGlobal$tx == i.tx), "robMajor"]
        dataGet[which(dataGet$tx == i.tx), "robWorst"] <- tblRoBTxGlobal[which(tblRoBTxGlobal$tx == i.tx), "robWorst"]
      }
    }
  }


  ## 04.2 BUILD an object based on P-best as output of function `GetMetrics()` -----

  if (argMetrics == "P-best") {
    dataGet <- as.data.frame(vctPbest)

    dataGet$SM <- data$sm
    dataGet$ES <- vctES
    dataGet$tx <- rownames(dataGet)
    dataGet    <- dataGet[, c(length(dataGet), 1:(length(dataGet) - 1))]

    if (namRoB == "NULL") {

      dataGet$robMean  <- NA
      dataGet$robMajor <- NA
      dataGet$robWorst <- NA

    } else {
      for (i.tx in dataGet$tx) {
        dataGet[which(dataGet$tx == i.tx), "robMean"]  <- tblRoBTxGlobal[which(tblRoBTxGlobal$tx == i.tx), "robMean"]
        dataGet[which(dataGet$tx == i.tx), "robMajor"] <- tblRoBTxGlobal[which(tblRoBTxGlobal$tx == i.tx), "robMajor"]
        dataGet[which(dataGet$tx == i.tx), "robWorst"] <- tblRoBTxGlobal[which(tblRoBTxGlobal$tx == i.tx), "robWorst"]
      }
    }

    colnames(dataGet) <- c("tx", argMetrics, "measure", "effect", "robMean", "robMajor", "robWorst")

  }


  ## 04.3 BUILD an object based on SUCRA as output of function `GetMetrics()` -----

  if (argMetrics == "SUCRA") {
    dataGet <- as.data.frame(vctSUCRA)

    dataGet$SM <- data$sm
    dataGet$ES <- vctES
    dataGet$tx <- rownames(dataGet)
    dataGet    <- dataGet[, c(length(dataGet), 1:(length(dataGet) - 1))]

    if (namRoB == "NULL") {

      dataGet$robMean  <- NA
      dataGet$robMajor <- NA
      dataGet$robWorst <- NA

    } else {
      for (i.tx in dataGet$tx) {
        dataGet[which(dataGet$tx == i.tx), "robMean"]  <- tblRoBTxGlobal[which(tblRoBTxGlobal$tx == i.tx), "robMean"]
        dataGet[which(dataGet$tx == i.tx), "robMajor"] <- tblRoBTxGlobal[which(tblRoBTxGlobal$tx == i.tx), "robMajor"]
        dataGet[which(dataGet$tx == i.tx), "robWorst"] <- tblRoBTxGlobal[which(tblRoBTxGlobal$tx == i.tx), "robWorst"]
      }
    }

    colnames(dataGet) <- c("tx", argMetrics, "measure", "effect", "robMean", "robMajor", "robWorst")

  }


  ## 04.4 BUILD an object based on P-score as output of function `GetMetrics()` -----

  if (argMetrics == "P-score") {
    dataGet <- as.data.frame(vctPscore)

    dataGet$SM <- data$sm
    dataGet$ES <- vctES
    dataGet$tx <- rownames(dataGet)
    dataGet    <- dataGet[, c(length(dataGet), 1:(length(dataGet) - 1))]

    if (namRoB == "NULL") {

      dataGet$robMean  <- NA
      dataGet$robMajor <- NA
      dataGet$robWorst <- NA

    } else {
      for (i.tx in dataGet$tx) {
        dataGet[which(dataGet$tx == i.tx), "robMean"]  <- tblRoBTxGlobal[which(tblRoBTxGlobal$tx == i.tx), "robMean"]
        dataGet[which(dataGet$tx == i.tx), "robMajor"] <- tblRoBTxGlobal[which(tblRoBTxGlobal$tx == i.tx), "robMajor"]
        dataGet[which(dataGet$tx == i.tx), "robWorst"] <- tblRoBTxGlobal[which(tblRoBTxGlobal$tx == i.tx), "robWorst"]
      }
    }

    colnames(dataGet) <- c("tx", argMetrics, "measure", "effect", "robMean", "robMajor", "robWorst")

  }


  ## 04.5 BUILD an object based on all global metrics as output of function `GetMetrics()` -----

  if (argMetrics == "ALL") {
    dataGet <- as.data.frame(
      cbind(vctPbest, vctSUCRA, vctPscore)
      )

    dataGet$SM <- data$sm
    dataGet$ES <- vctES
    dataGet$tx <- rownames(dataGet)
    dataGet    <- dataGet[, c(length(dataGet), 1:(length(dataGet) - 1))]

    if (namRoB == "NULL") {

      dataGet$robMean  <- NA
      dataGet$robMajor <- NA
      dataGet$robWorst <- NA

    } else {
      for (i.tx in dataGet$tx) {
        dataGet[which(dataGet$tx == i.tx), "robMean"]  <- tblRoBTxGlobal[which(tblRoBTxGlobal$tx == i.tx), "robMean"]
        dataGet[which(dataGet$tx == i.tx), "robMajor"] <- tblRoBTxGlobal[which(tblRoBTxGlobal$tx == i.tx), "robMajor"]
        dataGet[which(dataGet$tx == i.tx), "robWorst"] <- tblRoBTxGlobal[which(tblRoBTxGlobal$tx == i.tx), "robWorst"]
      }
    }

    colnames(dataGet) <- c("tx", "P-best", "SUCRA", "P-score", "measure", "effect", "robMean", "robMajor", "robWorst")

  }


  # 04.6 ADDING additional information on the object of output of function `GetMetrics()` -----

  rownames(dataGet) <- c(1:nrow(dataGet))

  if (argOutcome == "outcome") {
    dataGet$outcome <- "outcome"
  } else {
    dataGet$outcome <- outcome
  }


  # 05 REPORT returns of function `GetMetrics()` -----

  cat(paste("Summary of metrics:\n",
            "Metrics:    ", argMetrics, "\n",
            "Outcomes:   ", length(unique(dataGet$outcome)), "\n",
            "Treatments: ", length(unique(dataGet$tx)), "\n"),
      fill = TRUE, sep = "")
  cat(paste("List of treatments:"),
      fill = TRUE, sep = "")
  cat(paste(" ",
            c(1:length(unique(dataGet$tx))),
            unique(dataGet$tx),
            sep = " "),
      fill = TRUE, sep = "\n")

  dataGet <- data.frame(dataGet)

}
