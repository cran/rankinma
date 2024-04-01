## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
library(rankinma)

## ----echo = FALSE, out.width = "10%"------------------------------------------
knitr::include_graphics("rankinma_logo.png")

## ----eval = FALSE-------------------------------------------------------------
#  library(rankinma)

## ----setup, echo = FALSE, warning = FALSE, message = FALSE--------------------
library(rankinma)
library(netmeta)

## ----eval = FALSE-------------------------------------------------------------
#  library(rankinma)
#  library(netmeta)
#  data(Senn2013)
#  nmaOutput <- netmeta(TE,
#                       seTE,
#                       treat1,
#                       treat2,
#                       studlab,
#                       data = Senn2013,
#                       sm = "SMD")

## ----eval = FALSE-------------------------------------------------------------
#  dataMetrics <- GetMetrics(nmaOutput,
#                            outcome = "HbA1c.random",
#                            prefer = "small",
#                            metrics = "Probabilities",
#                            model = "random",
#                            simt = 1000)

## ----eval = FALSE-------------------------------------------------------------
#  dataRankinma <- SetMetrics(dataMetrics,
#                             tx = tx,
#                             outcome = outcome,
#                             metrics.name = "Probabilities")

## ----eval = FALSE-------------------------------------------------------------
#  PlotLine(data = dataRankinma,
#           compo = TRUE)

## ----eval = TRUE, echo = FALSE, warning = FALSE, results = FALSE, message = FALSE, results = "hide", fig.cap = "**Figure 1**. Composite line chart for probabilities of treatments on each rank.", fig.height = 5, fig.width = 7, fig.align = "center", out.width = "90%"----
data(Senn2013)
nmaOutput <- netmeta(TE, 
                     seTE, 
                     treat1, 
                     treat2, 
                     studlab, 
                     data = Senn2013, 
                     sm = "SMD")
dataMetrics <- GetMetrics(nmaOutput, 
                          outcome = "HbA1c.random", 
                          prefer = "small", 
                          metrics = "Probabilities", 
                          model = "random", 
                          simt = 1000)
dataRankinma <- SetMetrics(dataMetrics, 
                           tx = tx, 
                           outcome = outcome, 
                           metrics.name = "Probabilities")
PlotLine(data = dataRankinma, 
         compo = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  PlotBar(data = dataRankinma,
#          accum = TRUE)

## ----eval = TRUE, echo = FALSE, warning = FALSE, results = FALSE, message = FALSE, results = "hide", fig.cap = "**Figure 2**. Accumulative bar chart for probabilities of treatments on each rank.", fig.height = 5, fig.width = 7, fig.align = "center", out.width = "90%"----
data(Senn2013)
nmaOutput <- netmeta(TE, 
                     seTE, 
                     treat1, 
                     treat2, 
                     studlab, 
                     data = Senn2013, 
                     sm = "SMD")
dataMetrics <- GetMetrics(nmaOutput, 
                          outcome = "HbA1c.random", 
                          prefer = "small", 
                          metrics = "Probabilities", 
                          model = "random", 
                          simt = 1000)
dataRankinma <- SetMetrics(dataMetrics, 
                           tx = tx, 
                           outcome = outcome, 
                           metrics.name = "Probabilities")
PlotBar(data = dataRankinma, 
        accum = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  library(rankinma)
#  library(netmeta)
#  data(Senn2013)
#  nmaOutput <- netmeta(TE,
#                       seTE,
#                       treat1,
#                       treat2,
#                       studlab,
#                       data = Senn2013,
#                       sm = "SMD")

## ----eval = FALSE-------------------------------------------------------------
#  nmaRandom <- GetMetrics(nmaOutput,
#                          outcome = "HbA1c.random",
#                          prefer = "small",
#                          metrics = "SUCRA",
#                          model = "random",
#                          simt = 1000)
#  nmaCommon <- GetMetrics(nmaOutput,
#                          outcome = "HbA1c.common",
#                          prefer = "small",
#                          metrics = "SUCRA",
#                          model = "common",
#                          simt = 1000)

## ----eval = FALSE-------------------------------------------------------------
#  nmaRandom <- GetMetrics(nmaOutput,
#                          outcome = "HbA1c.random",
#                          prefer = "small",
#                          metrics = "P-score",
#                          model = "random",
#                          simt = 1000)
#  nmaCommon <- GetMetrics(nmaOutput,
#                          outcome = "HbA1c.common",
#                          prefer = "small",
#                          metrics = "P-score",
#                          model = "common",
#                          simt = 1000)

## ----eval = FALSE-------------------------------------------------------------
#  nmaRandom <- GetMetrics(nmaOutput,
#                          outcome = "HbA1c.random",
#                          prefer = "small",
#                          metrics = "P-best",
#                          model = "random",
#                          simt = 1000)
#  nmaCommon <- GetMetrics(nmaOutput,
#                          outcome = "HbA1c.common",
#                          prefer = "small",
#                          metrics = "P-best",
#                          model = "common",
#                          simt = 1000)

## ----eval = FALSE-------------------------------------------------------------
#  dataMetrics <- rbind(nmaRandom, nmaCommon)

## ----eval = FALSE-------------------------------------------------------------
#  dataRankinma <- (dataMetrics,
#                   tx = tx,
#                   outcome = outcome,
#                   metrics = SUCRA,
#                   metrics.name = "SUCRA")

## ----eval = FALSE-------------------------------------------------------------
#  dataRankinma <- (dataMetrics,
#                   tx = tx,
#                   outcome = outcome,
#                   metrics = P.score,
#                   metrics.name = "P-score")

## ----eval = FALSE-------------------------------------------------------------
#  dataRankinma <- (dataMetrics,
#                   tx = tx,
#                   outcome = outcome,
#                   metrics = P.best,
#                   metrics.name = "P-best")

## ----eval = FALSE-------------------------------------------------------------
#  PlotBeads(data = dataRankinma)

## ----eval = TRUE, echo = FALSE, message = FALSE, results = "hide", fig.cap = "**Figure 3A**. Beading plot for SUCRA on two outcomes", fig.height = 6, fig.width = 8, fig.align = "center", out.width = "90%"----
data(Senn2013)
nmaOutput <- netmeta(TE, 
                     seTE, 
                     treat1, 
                     treat2, 
                     studlab, 
                     data = Senn2013, 
                     sm = "SMD")
nmaRandom <- GetMetrics(nmaOutput, 
                        outcome = "HbA1c.random", 
                        prefer = "small", 
                        metrics = "SUCRA", 
                        model = "random", 
                        simt = 1000)
nmaCommon <- GetMetrics(nmaOutput, 
                        outcome = "HbA1c.common", 
                        prefer = "small", 
                        metrics = "SUCRA", 
                        model = "common", 
                        simt = 1000)
dataMetrics <- rbind(nmaRandom, nmaCommon)
dataRankinma <- SetMetrics(dataMetrics, 
                           tx = tx, 
                           outcome = outcome, 
                           metrics = SUCRA, 
                           metrics.name = "SUCRA")
PlotBeads(data = dataRankinma)

## ----eval = FALSE-------------------------------------------------------------
#  PlotBeads(data = dataRankinma,
#            scaleX = "Rank",
#            txtValue = "Effects")

## ----eval = TRUE, echo = FALSE, message = FALSE, results = "hide", fig.cap = "**Figure 3A**. Beading plot for SUCRA on two outcomes", fig.height = 6, fig.width = 8, fig.align = "center", out.width = "90%"----
data(Senn2013)
nmaOutput <- netmeta(TE, 
                     seTE, 
                     treat1, 
                     treat2, 
                     studlab, 
                     data = Senn2013, 
                     sm = "SMD")
nmaRandom <- GetMetrics(nmaOutput, 
                        outcome = "HbA1c.random", 
                        prefer = "small", 
                        metrics = "P-score", 
                        model = "random", 
                        simt = 1000)
nmaCommon <- GetMetrics(nmaOutput, 
                        outcome = "HbA1c.common", 
                        prefer = "small", 
                        metrics = "P-score", 
                        model = "common", 
                        simt = 1000)
dataMetrics <- rbind(nmaRandom, nmaCommon)
dataRankinma <- SetMetrics(dataMetrics, 
                           tx = tx, 
                           outcome = outcome, 
                           metrics = P.score, 
                           metrics.name = "P-score")
PlotBeads(data = dataRankinma, 
          scaleX = "Rank",
          txtValue = "Effects")

## ----eval = FALSE-------------------------------------------------------------
#  PlotBeads(data = dataRankinma,
#            lgcBlind = TRUE)

## ----eval = TRUE, echo = FALSE, message = FALSE, results = "hide", fig.cap = "**Figure 3C**. Colorblind friendly beading plot for P-score on two outcomes", fig.height = 6, fig.width = 8, fig.align = "center", out.width = "90%"----
data(Senn2013)
nmaOutput <- netmeta(TE, 
                     seTE, 
                     treat1, 
                     treat2, 
                     studlab, 
                     data = Senn2013, 
                     sm = "SMD")
nmaRandom <- GetMetrics(nmaOutput, 
                        outcome = "HbA1c.random", 
                        prefer = "small", 
                        metrics = "P-best", 
                        model = "random", 
                        simt = 1000)
nmaCommon <- GetMetrics(nmaOutput, 
                        outcome = "HbA1c.common", 
                        prefer = "small", 
                        metrics = "P-best", 
                        model = "common", 
                        simt = 1000)
dataMetrics <- rbind(nmaRandom, nmaCommon)
dataRankinma <- SetMetrics(dataMetrics, 
                           tx = tx, 
                           outcome = outcome, 
                           metrics = P.best, 
                           metrics.name = "P-best")
PlotBeads(data = dataRankinma,
          lgcBlind = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  PlotSpie(data = dataRankinma)

## ----eval = TRUE, echo = FALSE, message = FALSE, results = "hide", fig.cap = "**Figure 3B**. Spie plot for P-score on two outcomes", fig.height = 6, fig.width = 8, fig.align = "center", out.width = "90%"----
data(Senn2013)
nmaOutput <- netmeta(TE, 
                     seTE, 
                     treat1, 
                     treat2, 
                     studlab, 
                     data = Senn2013, 
                     sm = "SMD")
nmaRandom <- GetMetrics(nmaOutput, 
                        outcome = "HbA1c.random", 
                        prefer = "small", 
                        metrics = "P-score", 
                        model = "random", 
                        simt = 1000)
nmaCommon <- GetMetrics(nmaOutput, 
                        outcome = "HbA1c.common", 
                        prefer = "small", 
                        metrics = "P-score", 
                        model = "common", 
                        simt = 1000)
dataMetrics <- rbind(nmaRandom, nmaCommon)
dataRankinma <- SetMetrics(dataMetrics, 
                           tx = tx, 
                           outcome = outcome, 
                           metrics = P.score, 
                           metrics.name = "P-score")
PlotSpie(data = dataRankinma)

