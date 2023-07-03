#' @title Get treatment ranking metrics from network meta-analysis output
#'
#' @author Enoch Kang
#'
#' @description
#' **GetMetrics()** is a function for gathering metrics of treatment ranking
#' from *netmeta* output.
#'
#' @import netmeta
#' @import mvtnorm
#'
#' @param data DATA of *netmeta* output.
#' @param outcome STRING for name of outcome.
#' @param prefer STRING for indicating which direction is beneficial treatment
#'        effect in terms of "small" and "large" values in statistic test.
#' @param metrics STRING for metrics of treatment ranking in terms of "SUCRA"
#'        and "P-score" for the value of surface under the cumulative ranking
#'        curve or P-score.
#' @param model STRING for analysis model in terms of "random" and "common" for
#'        random-effects model and common-effect model.
#' @param simt INTEGER for times of simulations to estimate surface under the
#'        cumulative ranking curve (SUCRA).
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
                       simt    = 1000) {

  # 01 CHECK arguments -----

  lsMetrics <- c("Probabilities", "P-best", "SUCRA", "P-score", "ALL")

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


  # 03 GET treatment ranking metrics from object of *netmeta* class -----
  ## 03.1 GET probabilities from object of *netmeta* class -----

  if (argMetrics == "Probabilities") {
    if (argModel == "random") {
      outRankogram <- netmeta::rankogram(data, nsim = simt,
                                           random = TRUE,
                                           small.values = argPrefer)
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

    } else if (argModel == "common") {
      outRankogram <- netmeta::rankogram(data, nsim = simt,
                                           random = FALSE,
                                           small.values = argPrefer)
      dataGetProb  <- as.data.frame(t(outRankogram$ranking.matrix.common))
      colnames(dataGetProb) <- paste(colnames(dataGetProb),
                                     rep("Prob",
                                         length(dataGetProb)),
                                     sep = "")

      dataGetProbCum <- as.data.frame(t(outRankogram$cumrank.matrix.common))
      colnames(dataGetProbCum) <- paste(colnames(dataGetProbCum),
                                        rep("Cum",
                                            length(dataGetProbCum)),
                                        sep = "")

      #dataGetProbCum$rank <- seq(1:length(dataGetProbCum))
      dataGet <- cbind(dataGetProb, dataGetProbCum)
    }
  }


  ## 03.2 GET P-best from object of *netmeta* class -----

  if (argMetrics == "P-best") {
    if (argModel == "random") {
      dataGet <- as.data.frame(
        netmeta::rankogram(data, nsim = simt, random = TRUE,
                           small.values = argPrefer)$ranking.matrix.random[, 1])
    } else if (argModel == "common"){
      dataGet <- as.data.frame(
        netmeta::rankogram(data,nsim = simt, random = FALSE,
                           small.values = argPrefer)$ranking.matrix.common[, 1])
    }
  }


  ## 03.3 GET SUCRA from object of *netmeta* class -----

  if (argMetrics == "SUCRA") {
    if (argModel == "random") {
      dataGet <- as.data.frame(
        netmeta::rankogram(data, nsim = simt, random = TRUE,
                           small.values = argPrefer)$ranking.random)
    } else if (argModel == "common") {
      dataGet <- as.data.frame(
        netmeta::rankogram(data, nsim = simt, random = FALSE,
                           small.values = argPrefer)$ranking.common)
    }
  }


  ## 03.4 GET P-score from object of *netmeta* class -----

  if (argMetrics == "P-score") {
    if (argModel == "random") {
      dataGet <- as.data.frame(
        netmeta::netrank(data, method = "P-score", random = TRUE,
                         small.values = argPrefer)$ranking.random)
    } else if (argModel == "common") {
      dataGet <- as.data.frame(
        netmeta::netrank(data, method = "P-score", random = FALSE,
                         small.values = argPrefer)$ranking.common)
    }
  }


  ## 03.5 GET all global metrics from object of *netmeta* class -----

  if (argMetrics == "ALL") {
    if (argModel == "random") {
      dataGet <- as.data.frame(
        cbind(
          netmeta::rankogram(data, nsim = simt, random = TRUE,
                             small.values = argPrefer)$ranking.matrix.random[, 1],
          netmeta::rankogram(data,nsim = simt,random = TRUE,
                             small.values = argPrefer)$ranking.random,
          netmeta::netrank(data, method = "P-score", random = TRUE,
                           small.values = argPrefer)$ranking.random
        )
      )
    } else if (argModel == "common") {
      dataGet <- as.data.frame(
        cbind(
          netmeta::rankogram(data, nsim = simt, random = FALSE,
                             small.values = argPrefer)$ranking.matrix.common[, 1],
          netmeta::rankogram(data, nsim = simt, random = FALSE,
                             small.values = argPrefer)$ranking.common,
          netmeta::netrank(data, method = "P-score", random = FALSE,
                           small.values = argPrefer)$ranking.common
        )
      )
    }
  }


  # 04 BUILD an object as output of function `GetMetrics()` -----

  if (argMetrics == "Probabilities") {
    dataGet$tx   <- colnames(dataGet)[c(1:nrow(dataGet))]
    dataGet$tx   <- substr(dataGet$tx,
                           1,
                           nchar(dataGet$tx) - 4)
    dataGet$rank <- as.numeric(rownames(dataGet))
    dataGet      <- dataGet[, c(length(dataGet)-1,
                                length(dataGet),
                                2:length(dataGet)-2)]
  } else if (argMetrics == "ALL") {
    colnames(dataGet) <- c("P-best", "SUCRA", "P-score")
    dataGet$tx        <- rownames(dataGet)
    dataGet           <- dataGet[, c(4, 1, 2, 3)]
  } else {
    colnames(dataGet) <- argMetrics
    dataGet$tx        <- rownames(dataGet)
    dataGet           <- dataGet[, c(2, 1)]
  }

  if (argOutcome == "outcome") {
    dataGet$outcome <- "outcome"
  } else {
    dataGet$outcome <- outcome
  }

  rownames(dataGet) <- c(1:nrow(dataGet))


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
