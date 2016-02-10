#' Data.table con el contenido del fichero DD (excepto la columna descripcion)
#'
#' \code{RepoDD} es un \code{\link{data.table}} con el contenido del fichero \code{DD}
#' de definición de datos ubicado en el repositorio de la encuesta de IASS.
#' 
#' Las columnas del \code{data.table} \code{RepoDD} son las siguientes:
#' \itemize{
#'   \item NOMID. Nombres de los calificadores identificadores de unidad.
#'   \item NOMCALIFICADOR. Nombres de los calificadores de nombre de variables.
#'   \item NOMIDDD. Nombres de las variables en el repositorio.
#'   \item TIPO. \code{STRING} para variables de tipo \code{character} o 
#'   \code{NUMBER} para variables de tipo \code{double} o \code{integer}.
#'   \item FORM. Formato SAS de la variable.
#'   \item CALIFj. Nombre del calificador \code{j} para cada variable (los calificadores
#'   de identificación no están incluidos por defecto para las variables del 
#'   cuestionario; son excepciones los índices ponderados, impuestos, ...)
#'   \item TIPOCALIFj. Tipo del calificador \code{j} (toma el valor \code{1} para
#'   calificadores identificadores de unidad y el valor \code{2} para calificadores
#'   de variables).
#' }
#' 
#' 
#'
#' @docType data
#' @name RepoDD
#' @usage data(RepoDD)
#' @format \link{data.table} con 54 filas y 22 columnas.
NULL