#' @title Object of class \linkS4class{DD}
#'
#' @description \code{ExampleDD} is an object of class \linkS4class{DD}.
#'
#' The slot \code{VarNameCorresp} is of class \linkS4class{VarNameCorresp} with component 
#' \code{MicroData}. The corresponding slot \code{MicroData} is of class \linkS4class{DDdt} as the 
#' rest of them, although they are empty.
#'
#' The columns of the slot \code{MicroData} are:
#' \itemize{
#'   \item \code{Variable}: statistical variable name.
#'   \item \code{Sort}: semantic sort of the variable (IDQual, NonIDQual, IDDD).
#'   \item \code{Class}: class of the variable (\code{numeric}, \code{character},...).
#'   \item \code{Qual1} to \code{Qual3}: 1st to 3rd variable qualifier.
#'   \item \code{ValueRegExp}: regexp for the variable values.
#' }
#' 
#' @docType data
#'
#' @name ExampleDD
#'
#' @usage data(ExampleDD)
#'
#' @format Object of class \linkS4class{DD}. Its slot \code{VarNameCorresp} has a component of class 
#' \linkS4class{VNCdt} with 16 rows and 9 columns. Its slot \code{MicroData} is a data.table of 
#' class \linkS4class{DDdt} with 14 rows and 7 columns.
NULL
