# Change of package `rankinma`


## rankinma 0.1.8
### Date: July 03, 2023

**Update to CRAN**  
\

**Modify function:** remove routinely reported processing information from all functions in the *rankinma* package. The information only appear when users use function(s) inappropriately.
\

**Modify function:** improve function `PlotLine()` in order to produce cumulative line charts, and merged line chart.
\

**Modify function:** improve function `PlotLine()` in order to produce cumulative line charts, and merged line chart.
\

### Date: June 23, 2023

**Modify function:** improve data setting in function `SetMetrics()` for scatter plot.
\

**Modify function:** improve data setting in both `GetMetrics()` and `SetMetrics()` functions for cumulative probability of rank.
\


## rankinma 0.1.7
### Date: April 23, 2023

**Update to CRAN**  
\

### Date: April 22, 2023

**Modify function:** add transparency in both functions of `SetMetrics()` and `PlotBeads()`.
\

**Modify function:** modify function `ShowColor()` based on the updated function `SetMetrics()` on April 20, 2023.
\

**Modify function:** improve function `PlotBar()`, `PlotLine()`, and `PlotSpie()` to show user-defined colors appropriately.
\

**Modify function:** improve function `PlotHeat()` to show heat plot with order of metrics or treatment appropriately.
\


## rankinma 0.1.6
### Date: April 20, 2023

**Add function:** `PlotSpie()` is added to the *rankinma* in order to illustrate Spie plot of global metrics of treatment ranking (i.e. SUCRA and P-score) using object of class *rankinma* set by function `SetMetrics()`.
\

**Modify function:** improve data setting and color in both functions of `SetMetrics()` and `PlotBeads()`.
\

**Update to CRAN**  
\


## rankinma 0.1.5
### Date: April 4, 2023

**Modify function:** keep user's options of par (*'par(no.readonly)'*) after illustrating plots with *rankinma* by using function `on.exit()`.
\

**Re-submit to CRAN**  
\

### Date: April 1, 2023

**Add function:** `PlotHeat()` is added to the *rankinma* in order to illustrate heat plot of global metrics of treatment ranking (i.e. SUCRA and P-score) using object of class *rankinma* set by function `SetMetrics()`.
\


## rankinma 0.1.4
### Date: March 27, 2023

**Modify function:** remove global assignment (*'<<-'*) from package.
\

**Submit to CRAN**  
\

### Date: February 19, 2023

**Push to github**  
\

### Date: February 10, 2023

**Modify function:** all functions were modified due to warnings regarding *... invisible binding for '<<-' assignment ...* .
\


## rankinma 0.1.3
### Date: February 06, 2023

**Add function:** `PlotLine()` is added to the *rankinma* in order to illustrate line chart of global metrics of treatment ranking (i.e. SUCRA and P-score) using object of class *rankinma* set by function `SetMetrics()`.
\


## rankinma 0.1.2
### Date: January 31, 2023

**Add function:** `PlotBar()` is added to the *rankinma* in order to illustrate bar chart of global metrics of treatment ranking (i.e. SUCRA and P-score) using object of class *rankinma* set by function `SetMetrics()`.
\


## rankinma 0.1.1 
### Date: January 30, 2023

**Add function:** `GetMetrics()` is added to the *rankinma* in order to get metrics of treatment ranking from object of class *netmeta*.
\


## rankinma 0.1.0
### Date: January 29, 2023

Build rankinma with first two functions including `SetMetrics()` and `PlotBeads()`.

> 1. `SetMetrics()` is to set imported data in *rankinma* format.
>
> 2. `PlotBeads()` is to illustrate beading plot using object of class *rankinma* set by function `SetMetrics()`.
>
>

\


### Writing style of *rankinma* (January 29, 2023)

This package is written according to Google's R style. For readers, details of naming rules are listed as follows:

> 1. **.R file** is named using lower case with underscore "_" between words (*e.g. get_metrics.R*). 
>
> 2. **function** is named using verb or verb with noun, and the first character of each word is written in capital letter (e.g. `GetMetrics()`).
>
> 3. **object** is named using noun with the first word in lower case, but the first character of rest words is written using capital letter (e.g. `argOutcome`).
>
> 4. **variable** is named using noun written in lower case. Words of variable name are separated by "." if a variable name consists of more than two words (e.g. `data$metrics.name`).
>
>

\


### Version numbering rule of *rankinma* (October 30, 2023)

version number consists of three integers with a period between them (eg. version 1.0.0).

> 1. Updating the first integer refers to a modification with new methodological impact. 
>
> 2. Changing the second integer refers to an update with a new function without new methodological impact. 
>
> 3. Updating the third integer refers to a modification in a function.
>
>
