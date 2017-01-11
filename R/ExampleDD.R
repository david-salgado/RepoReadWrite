#' @title Object of class \linkS4class{DD}
#'
#' @description \code{ExampleDD} is an object of class \linkS4class{DD}.
#'
#' The slot \code{VarNameCorresp} is of class \linkS4class{VarNameCorresp} with components
#' \code{MicroData} and \code{Aggregates}. The corresponding slots \code{MicroData} and 
#' \code{Aggregates} are of class \linkS4class{DDdt} as the rest of them, although they are empty 
#' (thus they do not appear on screen according to the method show).
#'
#' The columns of slots \code{MicroData} and \code{Aggregates} are:
#' \itemize{
#'   \item \code{Variable}: statistical variable name.
#'   \item \code{Sort}: semantic sort of the variable (IDQual, NonIDQual, IDDD).
#'   \item \code{Class}: class of the variable (\code{numeric}, \code{character},...).
#'   \item \code{Length}: highest length for each variable.
#'   \item \code{Qual1} to \code{Qualn}: 1st to n variable qualifier (n = 3 in \code{MicroData},
#'                                                                    n = 2 in \code{Aggregates}).
#'   \item \code{ValueRegExp}: regexp for the variable values (not yet active).
#' }
#' 
#' 
#' @docType data
#'
#' @name ExampleDD
#'
#' @usage data(ExampleDD)
#'
#' @format Object of class \linkS4class{DD}. Its slot \code{VarNameCorresp} has two components of
#' class \linkS4class{VNCdt}: \code{MicroData}, with 15 rows and 7 columns; and \code{Aggregates},
#' with 3 rows and 6 columns. Its slot \code{MicroData} is a data.table of class \linkS4class{DDdt}
#' with 13 rows and 7 columns and its slot \code{Aggregates} is a data.table of class
#' \linkS4class{DDdt} with 2 rows and 6 columns.
NULL
