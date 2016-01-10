#' Lectura de un fichero del repositorio
#' 
#' \code{ReadRepoFile} devuelve un \code{\link{data.table}} con el 
#' contenido del fichero del repositorio especificado.
#' 
#' Esta función permite la lectura del ficheros ASCII ubicados en el repositorio 
#' de la encuesta.
#' 
#' @param FileName \code{\link{vector}} de tipo \code{character} de longitud 1 
#' con el nombre del fichero que se desea leer. El nombre debe incluir la ruta
#' completa de ubicación del fichero (directorio en el que se encuentra el
#' fichero y su nombre). 
#' 
#' @return \code{data.table} con los datos del fichero leído.
#' 
#' @examples
#' # Se asume que el fichero con estructura par clave-valor, 
#' # ASCII \code{E30183.FF_V1.MM032014.D_1}, se encuentra en el escritorio del 
#' # administrador:
#' RepoName <- 'C:/Users/Administrador/Desktop/E30183.FF_V1.MM032014.D_1'
#' Example.kv <- ReadRepoFile(RepoName)
#' str(Example.kv)
#' 
#' @seealso \link{ReadSASFile}, \link{WriteRepoFile}
#'
#' @import data.table
#'
#' @export
    ReadRepoFile <- function(FileName) {
    
    trim <- function(x){gsub(pattern = " ", replacement = "", x = x, 
                             useBytes = T, fixed = T)}
    
    # Se lee todo el fichero en un vector carácter con cada línea en una componente
    s <- file.info(FileName)$size 
    buf <- readChar(con = FileName, nchars = s, useBytes = TRUE)
    FileVector <- strsplit(x = buf, split = "\r\n", fixed = T, useBytes = T)[[1]]
    
    FirstLine <- FileVector[[1]]
    
    FileDT <- data.table(FileVector = FileVector[-1])

    # Se determinan los nombres y longitudes de las variables
    Param <- as.list(unlist(strsplit(x = FirstLine, split = ",")))
    Param <- as.vector(lapply(Param, "[[" , 1L))
    Param <- lapply(lapply(Param, strsplit, split = '='), '[[', 1L)
    Names <- unlist(lapply(Param, '[', 1)[-c(1, length(Param))])
    Lengths <- lapply(Param, '[', 2)[-1]
    Lengths <- lapply(Lengths, function(x){
        if (substr(x, 1, 1) == '$') {
            return(substr(x = x, start = 2, stop = nchar(x)))
        } else {
            return(x) 
        }
    })
    Lengths <- lapply(Lengths, function(x){
        if (substr(x = x, start = nchar(x), stop = nchar(x)) == '.') {
            return(as.integer(substr(x = x, start = 1, stop = nchar(x) - 1)))
        } else {
            return(as.integer(x)) 
        }
    })
    Max <- Lengths[[length(Lengths)]]
    Lengths <- unlist(Lengths[-length(Lengths)])
    # Se determinan las posiciones inicial y final de cada variable en cada línea
    Pos1 <- c(1L)
    for (i in seq(along = Lengths)){Pos1 <- c(Pos1, Lengths[i] + Pos1[length(Pos1)])}
    Pos2 <- Pos1[-1] - 1L
    Pos2[length(Pos2)] <- Pos1[length(Pos1)] - 1L
    Pos1 <- Pos1[-length(Pos1)]
    # Se construye una columna por cada variable
    for (indexVar in seq(along = Names)){
        FileDT[, Names[indexVar]:= trim(substr(x = FileVector, 
                                               start = Pos1[indexVar], 
                                               stop = Pos2[indexVar])), with = F]
    }
    FileDT[, FileVector := NULL]

    DupRows <- duplicated(FileDT)
    if (sum(DupRows) > 0) {

        cat('Las siguientes filas estaban duplicadas y han sido eliminadas.\n')
        print(FileDT[DupRows])
        FileDT <- FileDT[!DupRows]
    }
    
    if ('DESC' %in% names(FileDT)) {
        
        FileDT <- FileDT[, DESC := NULL]
        cat('[ReadRepoFile] El campo DESC se ha eliminado.\n')   
    }
    
    if ('IDDD' %in% names(FileDT) && 'Valor' %in% names(FileDT)) setcolorder(FileDT, c(setdiff(names(FileDT), c('IDDD', 'Valor')), c('IDDD', 'Valor')))
    
    return(FileDT)
}
