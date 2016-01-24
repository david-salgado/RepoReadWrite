#' Número de la última versión de un fichero en un directorio
#'
#' \code{RepoTopn} obtiene el número de la última versión que existe de un fichero
#' en un determinado directorio.
#' 
#' Esta función busca en el directorio especificado en el parámetro de entrada
#' \code{Path}, la última versión que existe del fichero indicado en \code{FileNameString}
#' y devuelve el número correspondiente a dicha versión.
#' 
#' Esta función está pensada para ficheros que tiene anexada la versión a la
#' que corresponden sus datos al final de su nombre (versión definitiva o parcial).
#'
#' @param Path \code{Vector} de tipo \code{character} de longitud uno con la 
#' ruta del directorio en el que queremos buscar el fichero.
#' 
#' @param FileNameString \code{Vector} de tipo \code{character} de longitud uno con
#' el nombre del fichero para el que queremos determinar el número de la última  
#' versión disponible en el directorio indicado en \code{Path}.
#'  
#' @return \code{Vector} de tipo \code{integer} de longitud uno con el número de
#' la última versión del fichero especificado que existe en el directorio de
#' entrada.
#'
#' @examples
#' \dontrun{
#' RepoTopn('R:/E30183', 'FF_V1.MM122014')
#' }
#' 
#' @export
RepoTopn <- function(Path, FileNameString){
  
  Files <- list.files(Path)
  Files <- Files[grep(FileNameString, Files)]
  if (length(Files) == 0) stop('[RepoTopn RepoReadWrite] No existen ficheros con este nombre.')
  
  nVer <- unlist(lapply(as.list(Files), function(x){substr(x, nchar(x), nchar(x))}))
  out <- max(nVer)
  
return(out)
  
}
