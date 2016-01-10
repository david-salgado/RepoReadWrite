#' Objeto de clase StQ.
#'
#' \code{ExampleQ} es un objeto de clase \linkS4class{StQ} cuyo slot
#' \code{Data} es una \code{\link{data.table}} con estructura par clave-valor
#' con los datos de la encuesta de IASS correspondientes a un periodo no 
#' especificado (el mismo periodo utilizado para el objeto \code{ExampleDM}).
#'
#' Las columnas del slot Data del objeto son las siguientes:
#' \itemize{
#'   \item NOrden. Primer calificador (identificador de unidad).
#'   \item CCAA. Segundo calificador (de variable).
#'   \item EsRemuner. Tercer calificador (de variable).
#'   \item TipoRem. Cuarto calificador (de variable).
#'   \item IDDD. Nombres de las variable.
#'   \item Valor. Valores de las variable (por defecto, de clase \code{character}).
#' }
#' 
#' 
#' @docType data
#' 
#' @name ExampleQ
#' 
#' @usage data(ExampleQ)
#' 
#' @format Objeto de clase \code{StQ}. Su slot \code{Data} tiene 1710942 filas
#'  y 6 columnas.
NULL