#' @title Insert parameter and unit labels in different markup languges
#' @description Given a user defined 'short-hand' code for a parameter-unit this functions returns a label in the selected markup language.
#' @details User defined 'short-hand' code for a parameter-unit combinations and their conversions into different markup languages are stored in a data file \code{data/definitions.csv}. Edit this file to add or adjust definitions. Short hand code can take any form, but may not contain R's reserved words (\url{http://cran.r-project.org/doc/manuals/r-release/R-lang.html#Reserved-words})  
#' @param sh User defined 'short-hand' code for a parameter, unit or 'parameter.unit' combination
#' @param output The desired markup languages to be returned
#' @return A character string in the desired markup representing the requested label. \code{uls} returns the quantity symbol, \code{ulu} returns the quantity units, \code{ul} returns the combined quantity symbol and unit, \code{ulln} returns the quantity long name and \code{ulsn} returns the quantity standard name   
#' @examples 
#' uls(sh="T", output="R")
#' uls(sh="T", output="md")
#' ulu("degC", output="R")
#' ulu("degC", output="md")
#' ul("T.degC", output="R")
#' ul("T.degC", output="md")
#' @export      
uls <- function(sh, output="R"){
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

ulu <- function(sh, output="R"){
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

ul <- function(sh, output="R"){
  sh <- unlist(strsplit(sh,"[.]"))
  if(output=="R"){
    sy <-definitions[match(sh[1],definitions$sh.symbol),paste0("symbol.",output)]
    un <-definitions[match(sh[2],definitions$sh.units),paste0("units.",output)]
    out <- parse(text=paste0(sy,"~(", un, ")"))
  }
  if(output=="md"){
    sy <-definitions[match(sh[1],definitions$sh.symbol),paste0("symbol.",output)]
    un <-definitions[match(sh[2],definitions$sh.units),paste0("units.",output)]
    out <- paste0(sy,"~(", un, ")")
    #knitr::opts_chunk$set("results", "asis") <- "asis"
  }
  return(out)
}
