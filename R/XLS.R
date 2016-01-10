#' Hoja de Excel en formato data.frame
#'
#' \code{XLS} es un \code{\link{data.frame}} con la equivalencia entre los nombres
#' de las variables de la encuesta en la SGMRD y los nombres de las mismas 
#' en el servicio promotor.
#' 
#' Las columnas del \code{data.frame} son las siguientes:
#'\itemize{
#'   \item CalificadoresID. Calificadores identificativos de unidad.
#'   \item CalificadoresNoID. Calificadores de variable.
#'   \item Variables. Nombres de las variables en el repositorio.
#'   \item NOrden. Calificador de unidad. Aparece por completitud.
#'   \item CCAA. Comunidad autonoma (valores 01-17).
#'   \item EsRemuner. Remunerado (1) or no remunerado (0).
#'   \item TipoRem. Tipo de remuneracion (1 Fijo, 2 Eventual, 3 ETT, 4 Otros).
#'   \item SP. Nombres de las variables en el servicio promotor de la encuesta.
#'   \item SGMRD. Nombres de las variables en la SGRMD.
#' }
#'
#' @docType data
#' @name XLS
#' @usage data(XLS)
#' @format \code{data.frame} con 68 filas y 9 columnas.
NULL