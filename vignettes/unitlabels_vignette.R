## ---- results='asis'-----------------------------------------------------
# load the package
library(unitlabels)

## ---- results='asis'-----------------------------------------------------
# return Rmarkdown label for temperature in degrees Celcius
ul("T.degC", "md")

## ------------------------------------------------------------------------
plot(1:10,1:10, ylab = ul("T.degC", "R"), xlab = ul("Tsw.degC", "R"))

## ---- results='asis'-----------------------------------------------------
system.file("extdata", package = "unitlabels")

## ---- results='asis'-----------------------------------------------------
require(unitlabels)
def <- system.file("extdata", package = "unitlabels")
def <- paste0(def,"/definitions.csv")
def <- read.csv2(def, as.is = T)
knitr::kable(def[,c("standard.name", "sh.symbol", "symbol.md")])
knitr::kable(def[,c("standard.name", "sh.units", "units.md")])

