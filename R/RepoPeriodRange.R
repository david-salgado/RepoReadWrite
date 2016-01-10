#' Obtención de los períodos de un tipo dado de ficheros en un directorio
#'
#' \code{RepoPeriodRange} obtiene los periodos que existen de un tipo concreto de
#' ficheros en un determinado directorio.
#' 
#' Esta función obtiene para el tipo de ficheros indicado en el input \code{Type},
#' los distintos periodos que existen en el directorio especificado en el parámetro
#' de entrada \code{Path}. 
#' 
#' Por defecto devuelve todos los periodos del fichero que haya ubicados en el 
#' directorio. El parámetro \code{Last} nos permite solicitar los periodos hasta
#' uno concreto de ellos.
#' 
#' @param Path \code{Vector} de tipo \code{character} de longitud uno, con la 
#' ruta del directorio en el que queremos buscar los ficheros del tipo \code{Type}.
#' 
#' @param Type \code{Vector} de tipo \code{character} de longitud uno con el 
#' tipo del fichero del que queremos saber los periodos en el directorio indicado
#' en \code{Path}. Puede tomar los valores \code{FF}, \code{FG}, \code{FD}, 
#' \code{FL} o \code{FT}.
#' 
#' @param Last \code{Vector} de tipo \code{character} de longitud uno con el último
#' periodo que queremos obtener del tipo de fichero especificado.
#'  
#' @return \code{Vector} de tipo \code{character} con los distintos periodos que
#' existen en el directorio de entrada del tipo de fichero especificado.
#'
#' @examples
#' \dontrun{
#' RepoPeriodRange('R:/E30183', 'FF', 'MM102014')
#' }
#' 
#' @export
RepoPeriodRange <- function(Path, Type, Last = out[length(out)]){

    Files <- list.files(Path)
    Files <- Files[grep(Type, Files)]
    Files.desc <- strsplit(Files, ".", fixed = TRUE)
    out <- lapply(Files.desc, '[', i = 3)
    ord <- lapply(out, function(x){
        aux <- substr(x, 3, nchar(x))
        out <- paste0(substr(aux, 3, 6), '-', substr(aux, 1, 2), '-01')
        out <- as.Date(out)
        return(out)
        })
    out <- unique(unlist(out)[order(unlist(ord))])
    Last.index <- which(out == Last)
    out <- out[1:Last.index]
    
    return(out)
}