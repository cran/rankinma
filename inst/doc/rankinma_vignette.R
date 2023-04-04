## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
library(rankinma)

## ----echo = FALSE, out.width = "10%"------------------------------------------
knitr::include_graphics("rankinma_logo.png")

## ----setup--------------------------------------------------------------------
library(rankinma)

## ---- eval = FALSE------------------------------------------------------------
#  data <- data.frame(tx = c("A", "B", "C", "A", "B", "C"),
#                     outcome = c("mortality", "mortality", "mortality", "recurrent", "recurrent", "recurrent"),
#                     SUCRA = c(0.8, 0.7, 0.5, 0.9, 0.5, 0.8))

## ---- eval = FALSE------------------------------------------------------------
#  dataRankinma <- SetMetrics(data,
#                             tx = tx,
#                             outcome = outcome,
#                             metrics = SUCRA,
#                             metrics.name = "SUCRA")

## ---- eval = FALSE------------------------------------------------------------
#  PlotBar(data = dataRankinma)

## ---- echo = FALSE, warning = FALSE, results = FALSE, message = FALSE, error = FALSE, fig.keep = "all", fig.cap = c("", "Figure 1. examples of bar chart for SUCRA on two outcomes."), fig.height = 5, fig.width = 5, fig.align = "center", out.width = "50%"----
library("rankinma")
data <- data.frame(tx = c("A", "B", "C", "A", "B", "C"),
                   outcome = c("mortality", "mortality", "mortality", "recurrent", "recurrent", "recurrent"), 
                   SUCRA = c(0.8, 0.7, 0.5, 0.9, 0.5, 0.8))
dataRankinma <- SetMetrics(data, 
                           tx = tx, 
                           outcome = outcome, 
                           metrics = SUCRA, 
                           metrics.name = "SUCRA")
PlotBar(data = dataRankinma)

## ---- eval = FALSE------------------------------------------------------------
#  library(netmeta)
#  data(Senn2013)
#  nmaOutput <- netmeta(TE,
#                       seTE,
#                       treat1,
#                       treat2,
#                       studlab,
#                       data = Senn2013,
#                       sm = "SMD")

## ---- eval = FALSE------------------------------------------------------------
#  dataMetrics <- GetMetrics(nmaOutput,
#                            outcome = "HbA1c.random",
#                            prefer = "small",
#                            metrics = "Probabilities",
#                            model = "random",
#                            simt = 1000)

## ---- eval = FALSE------------------------------------------------------------
#  dataRankinma <- SetMetrics(dataMetrics,
#                             tx = tx,
#                             outcome = outcome,
#                             metrics.name = "Probabilities")

## ---- eval = FALSE------------------------------------------------------------
#  PlotLine(data = dataRankinma,
#           compo = TRUE)

## ---- eval = TRUE, echo = FALSE, warning = FALSE, results = FALSE, fig.cap = "Figure 2A. an exmaple of composite line chart for probabilities of treatments on each rank.", fig.height = 5, fig.width = 7, fig.align = "center", out.width = "90%"----
library(netmeta)
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

## ---- eval = TRUE, echo = FALSE, warning = FALSE, results = FALSE, fig.cap = "**Figure 2B**. an example of accumulative bar chart for probabilities of treatments on each rank.", fig.height = 5, fig.width = 7, fig.align = "center", out.width = "90%"----
library(netmeta)
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

## ---- eval = FALSE------------------------------------------------------------
#  library(netmeta)
#  data(Senn2013)
#  nmaOutput <- netmeta(TE,
#                       seTE,
#                       treat1,
#                       treat2,
#                       studlab,
#                       data = Senn2013,
#                       sm = "SMD")

## ---- eval = FALSE------------------------------------------------------------
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

## ---- eval = FALSE------------------------------------------------------------
#  dataMetrics <- rbind(nmaRandom, nmaCommon)

## ---- eval = FALSE------------------------------------------------------------
#  dataRankinma <- (dataMetrics,
#                   tx = tx,
#                   outcome = outcome,
#                   metrics = P.score,
#                   metrics.name = "P-score")

## ---- eval = FALSE------------------------------------------------------------
#  PlotBeads(data = dataRankinma)

## ---- eval = TRUE, echo = FALSE,  fig.cap = "Figure 3. an example of beading plot for P-score on two outcomes", fig.height = 4, fig.width = 8, fig.align = "center", out.width = "80%"----
library(netmeta)
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
PlotBeads(data = dataRankinma)

