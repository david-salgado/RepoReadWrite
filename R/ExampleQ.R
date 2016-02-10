#' @title Object of class \linkS4class{StQ}
#'
#' @description \code{ExampleQ} is an object of class \linkS4class{StQ} whose
#' slot \code{Data} is a \link{data.table} with key-value pair structure with
#' anonymised data of an undisclosed time period.
#'
#' The columns of slot \code{Data} of this object is:
#' \itemize{
#'   \item NOrden: First qualifier (statistical unit qualifier).
#'   \item CCAA: Second qualifier (variable name qualifier).
#'   \item EsRemuner: Third qualifier (variable name qualifier).
#'   \item TipoRem: Fourth qualifier (variable name qualifier).
#'   \item IDDD: Variable names.
#'   \item Value: Variable values (default class: \code{character}).
#' }
#'
#'
#' @docType data
#'
#' @name ExampleQ
#'
#' @usage data(ExampleQ)
#'
#' @format Object of class \code{StQ}. Its slot \code{Data} has 1710942 rows and
#'  6 columns.
