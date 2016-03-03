#' @title Write a file with a key-value
#' 
#' @description \code{ReadRepoFile} returns a \linkS4class{data.table} with the 
#' content of the file corresponding to the input name.
#' 
#' @param FileName Character vector of length 1 with the name of the file to 
#' read. The file will be read from the working directory (see 
#' \link[base]{getwd}) unless the full path is specified.
#' 
#' @return \linkS4class{data.table} with all data from the read file.
#' 
#' @examples
#' # We assume that the key-value ASCII file \code{E30183.FF_V1.MM032014.D_1} is 
#' in the administrator desktop (change accordingly otherwise): 
#' RepoName <- 'C:/Users/Administrador/Desktop/E30183.FF_V1.MM032014.D_1'
#' Example.kv <- ReadRepoFile(RepoName)
#' str(Example.kv)
#' 
#' @seealso \code{\link{ReadSASFile}}, \code{\link{WriteRepoFile}}
#'
#' @import data.table
#' 
#' @export
    ReadRepoFile <- function(FileName) {
    
    ## Se lee todo el fichero en un vector carácter con cada línea en una componente
    #s <- file.info(FileName)$size 
    #buf <- readChar(con = FileName, nchars = s, useBytes = TRUE)
    #FileVector <- strsplit(x = buf, split = "\r\n", fixed = T, useBytes = T)[[1]]

    #FirstLine <- FileVector[[1]]
    #FirstLine <- gsub('Valor', 'Value', FirstLine)

    #FileDT <- data.table(FileVector = FileVector[-1])

    File <- fread(FileName, sep = '|', sep2 = ' ',
                  header = FALSE, 
                  skip = 0L, 
                  strip.white = FALSE,
                  stringsAsFactors = FALSE)
    FirstLine <- File[1]
    FirstLine <- gsub('Valor', 'Value', FirstLine)
    FileDT <- File[-1]
    setnames(FileDT, 'FileVector')
    
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
    for (i in seq(along = Lengths)){Pos1 <- c(Pos1, 
                                              Lengths[i] + Pos1[length(Pos1)])}
    Pos2 <- Pos1[-1] - 1L
    Pos2[length(Pos2)] <- Pos1[length(Pos1)] - 1L
    Pos1 <- Pos1[-length(Pos1)]

    # Se construye una columna por cada variable
    for (indexVar in seq(along = Names)){
        FileDT[, Names[indexVar]:= gdata::trim(substr(x = FileVector, 
                                                      start = Pos1[indexVar], 
                                                      stop = Pos2[indexVar])), 
               with = F]
    }
    FileDT[, FileVector := NULL]

    DupRows <- duplicated(FileDT)
    if (sum(DupRows) > 0) {

        cat('[RepoReadWrite::ReadRepoFile] The following rows are duplicated and have been removed:\n\n')
        print(FileDT[DupRows])
        FileDT <- FileDT[!DupRows]
    }
    
    if ('DESC' %in% names(FileDT)) {
        
        FileDT <- FileDT[, DESC := NULL]
        cat('[RepoReadWrite::ReadRepoFile] The column DESC has been removed.\n\n')   
    }
    
    if ('IDDD' %in% names(FileDT) && 'Value' %in% names(FileDT)) {
        
        setcolorder(FileDT, c(setdiff(names(FileDT), c('IDDD', 'Value')), 
                              c('IDDD', 'Value')))
    
    }
    return(FileDT)
}
