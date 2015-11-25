#' @title Insert parameter and unit labels in different markup languges
#'
#' @description Given a user defined 'short-hand' code for a parameter, unit of parameter-unit combination this functions returns a label in the selected markup language.
#'
#' @details User defined 'short-hand' code for a parameters and units and their conversions into different markup languages are stored in a data file \code{data/definitions.csv}. Edit this file to add or adjust definitions. Short hand code can take any form, but may not contain R's reserved words (\url{http://cran.r-project.org/doc/manuals/r-release/R-lang.html#Reserved-words}). Parameter-unit combinations should be delimitated by a '.'
#'
#' @param sh User defined 'short-hand' code for a parameter, unit or 'parameter.unit' combination
#' @param output The desired markup languages to be returned
#' @param definitions A file path to a csv file containing definitions and markup conversions
#' @return A character string in the desired markup representing the requested label
#'
#' \code{uls} returns the quantity symbol
#'
#' @rdname insert_param_unit_labs
#' @examples
#' uls(sh="T", output="R")
#' uls(sh="T", output="md")
#' ulu("degC", output="R")
#' ulu("degC", output="md")
#' ul("T.degC", output="R")
#' ul("T.degC", output="md")
#' @export
uls <- function(sh, output="md", definitions=NULL){
  if(is.null(definitions)){definitions <- system.file("extdata", package = "unitlabels")
  definitions <- paste0(definitions,"/definitions.csv")}
  definitions <- read.csv(definitions, as.is = T)
  sh <- strsplit(sh,".", fixed=T)[[1]][1]
  if(output=="R"){
    out <-definitions[match(sh,definitions$sh.symbol),paste0("symbol.",output)]
    out <- parse(text=out)
  }
  if(output=="md"){
    out <-definitions[match(sh,definitions$sh.symbol),paste0("symbol.",output)]
    #knitr::opts_chunk$set("results", "asis") <- "asis"
  }
  return(out)
}

#' @return \code{ulu} returns the quantity units in the specified markup
#' @rdname insert_param_unit_labs
#' @export
ulu <- function(sh, output="md", definitions=NULL){
  if(is.null(definitions)){definitions <- system.file("extdata", package = "unitlabels")
  definitions <- paste0(definitions,"/definitions.csv")}
  definitions <- read.csv(definitions, as.is = T)
  sh <- strsplit(sh,".", fixed=T)[[1]][2]
  if(output=="R"){
    out <-definitions[match(sh,definitions$sh.units),paste0("units.",output)]
    out <- parse(text=out)
  }
  if(output=="md"){
    out <-definitions[match(sh,definitions$sh.units),paste0("units.",output)]
    #knitr::opts_chunk$set("results", "asis") <- "asis"
  }
  return(out)
}

#' @return \code{ul} returns the combined quantity-units in the specified markup
#' @rdname insert_param_unit_labs
#' @export

ul <- function(sh, output = "md", definitions = NULL) {
  if (is.null(definitions)) {
    definitions <- system.file("extdata", package = "unitlabels")
    definitions <- paste0(definitions,"/definitions.csv")
  }
  definitions <- read.csv(definitions, as.is = T)
  sh <- strsplit(sh,"[.]")

  get.lab <- function(sh,output) {
    sy <-
      definitions[match(sh[1],definitions$sh.symbol),paste0("symbol.",output)]
    if(length(sh)==1){
      out <- sy
      if (output == "R") {
        out <- parse(text = sy)
      }

    }else{
      un <-
        definitions[match(sh[2],definitions$sh.units),paste0("units.",output)]

      if (output == "R") {
        out <- parse(text = paste0(sy,"~(", un, ")"))
      }
      if (output == "md") {
        out <- paste0(sy," (", un, ")")
      }
    }
    return(out)
  }
  out <- unlist(lapply(sh, get.lab, output = output))
  if (output == "R") {
    out <- parse(text = out)
  }
  return(out)
}
