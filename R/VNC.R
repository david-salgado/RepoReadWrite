#' @title Object of class \linkS4class{VarNameCorresp}
#'
#' @description \code{VNC} is an object of class \linkS4class{VarNameCorresp}
#' whose slot \code{VarNameCorresp} is a \linkS4class{list} of one element that
#' is a \link{data.table} with the content of the variable names correspondence
#' Excel sheet.
#'
#' The columns of this \link{data.table} are:
#'\itemize{
#'   \item IDQual: Statistical unit qualifiers.
#'   \item NonIDQual: Variable name qualifiers.
#'   \item IDDD: Variable names.
#'   \item NOrden: First statistical unit qualifier.
#'   \item CCAA: First variable name qualifier (values 01-17).
#'   \item EsRemuner: Second variable name qualifier ('Remunerado': 1 or 'No 
#'   remunerado': 0).
#'   \item TipoRem: Third variable name qualifier ('Fijo' : 1, 'Eventual': 2, 
#'   'ETT': 3, 'Otros': 4).
#'   \item Unit1: Variable names used by the unit1.
#'   \item Unit2: Variable names used by the unit2.
#' }
#'
#'
#' @docType data
#'
#' @name VNC
#'
#' @usage data(VNC)
#'
#' @format Object of class \code{VarNameCorresp}. Its slot \code{VarNameCorresp}
#' has one \link{data.table} with 67 rows and 9 columns.
