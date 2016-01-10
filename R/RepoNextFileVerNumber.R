#' Creación del nombre de una nueva versión de un fichero
#'
#' \code{RepoNextFileVerNumber} devuelve el nombre que debe tener la siguiente
#' versión de un fichero.
#' 
#' Esta función toma los ficheros ubicados en el directorio indicado en el 
#' parámetro \code{Path} correspondientes al tipo especificado en el input \code{FileType}
#' y para los periodos en \code{Periods} y genera para cada uno de ellos los 
#' nombres de las siguientes versiones de cada uno de ellos que corresponde 
#' generar.
#'
#' @param Periods \code{Vector} de tipo \code{character} con los periodos para
#' los que queremos generar los nombres de la última versión de los ficheros.
#' 
#' @param Path \code{Vector} de tipo \code{character} con la ruta donde queremos
#' buscar los ficheros.
#' 
#' @param FileType \code{Vector} de tipo \code{character} con el tipo de fichero 
#' que queremos buscar.
#'  
#' @return Devuelve un \code{Vector} de tipo \code{character} de longitud igual 
#' al número de periodos especificados en \code{Periods} con los nombres completos
#' de los ficheros de tipo \code{FileType} que corresponde a las siguiente versión
#' que corresponde escribir para cada uno de periodos.
#'
#' @examples
#' \dontrun{
#' RepoTopn(Path, Type)
#' }
#' 
#' @export
RepoNextFileVerNumber <- function(Periods, Path, FileType){
  
  Files <- list.files(Path)
  Files <- Files[grep(FileType, Files)]
  if (length(Files) == 0) return(NULL)
  Periods <- unlist(lapply(Periods, getRepo))
  SelFiles <- c()
  for (Per in Periods){
    aux <- strsplit(Files[grep(Per, Files)], Per)
    aux <- lapply(aux, function(x){
      x[1] <- paste0(x[1], Per)
      return(x)
    })
    
    if (length(aux) == 0) {
      
      out <- NULL
      return(out)
      
    }
    
    
    if (length(aux) == 1) {
      
      aux <- aux[[1]]
      
    } else {
      
      aux <- Reduce(rbind, aux)
      
    }
    
    SelFiles <- rbind(SelFiles, aux)
  }
  
  OrderedPeriods <- unique(SelFiles[, 1])
  rownames(SelFiles) <- NULL
  SelFiles <- as.data.frame(SelFiles)
  SelFiles <- split(SelFiles, SelFiles[, 1])[OrderedPeriods]
  
  NextVer <- lapply(SelFiles, function(df){
    
    nVer <- unlist(strsplit(as.character(df[nrow(df), 2]), '_'))
    nVer[2] <- as.integer(nVer[2]) + 1
    nVer <- paste0(nVer[1], '_', nVer[2])
    return(nVer)
  })
  
  out <- unlist(lapply(as.list(names(NextVer)), function(Name){paste0(Name, NextVer[[Name]])}))
  return(out)  
  
}