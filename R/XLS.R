#' \title \link{data.frame} with the content of the variable names 
#' correspondence Excel sheet
#'
#' \code{XLS} is a \link{data.frame} with the content of the corresponding Excel
#' sheet containing the correspondence between the variable names used by 
#' different production units of the Service Sector Activity Indicator surveys.
#' 
#' The columns of this \link{data.frame} are:
#'\itemize{
#'   \item CalificadoresID: Statistical unit qualifiers.
#'   \item CalificadoresNoID: Variable name qualifiers.
#'   \item Variables: Variable names.
#'   \item NOrden: First statistical unit qualifier.
#'   \item CCAA: First variable name qualifier (values 01-17).
#'   \item EsRemuner: Second variable name qualifier ('Remunerado': 1 or 'No 
#'   remunerado': 0).
#'   \item TipoRem: Third variable name qualifier ('Fijo' : 1, 'Eventual': 2, 
#'   'ETT': 3, 'Otros': 4).
#'   \item SP: Variable names used by the conducting unit (\emph{unidad 
#'   promotora}).
#'   \item SGMRD: Variable names used by the data collection unit.
#' }
#'
#' @docType data
#' @name XLS
#' @usage data(XLS)
#' @format \link{data.frame} with 68 rows and 9 columns.
NULL
