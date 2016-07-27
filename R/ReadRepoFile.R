#' @title Write a file with a key-value
#' 
#' @description \code{ReadRepoFile} returns a \linkS4class{data.table} with the content of the file 
#' corresponding to the input name.
#' 
#' @param FileName Character vector of length 1 with the name of the file to read. The file will be 
#' read from the working directory (see \link[base]{getwd}) unless the full path is specified.
#' 
#' @param language character vector of length 1 with the language of the file to read. The values 
#' allowed are: 'SP' (Spanish) and 'EN' (English), being the default value is 'SP'.
#' 
#' @return \linkS4class{data.table} with all data from the read file.
#' 
#' @examples
#' \dontrun{
#' #We assume that the key-value ASCII file \code{E30183.FF_V1.MM032014.D_1} is in the administrator 
#' desktop (change accordingly otherwise): 
#' RepoName <- 'C:/Users/Administrador/Desktop/E30183.FF_V1.MM032014.D_1'
#' Example.kv <- ReadRepoFile(RepoName)
#' str(Example.kv)
#' }
#' 
#' @seealso \code{\link{ReadSASFile}}, \code{\link{WriteRepoFile}}
#'
#' @import data.table
#' 
#' @export
ReadRepoFile <- function(FileName, DD) {
    
    File <- fread(FileName, sep = '@', header = FALSE, skip = 0L, nrows = -1, na.strings = ' ',
                  strip.white = TRUE,
                  stringsAsFactors = FALSE, colClasses = 'character')
    NCol <- dim(File)[2]
    if (NCol > 5) {
        
        for (col in 6:NCol){
         
            File[, V5 := paste0(V5, get(paste0('V', col)), collapse = '@')]
        }
    }
    File[, V2 := NULL]
    File[, V4 := NULL]
    setnames(File, c('IDDDKey', 'QualKey', 'Value'))
    
    Value <- File[['Value']]
    File[, Value := NULL]
    key <- new(Class = 'rawKey', File)
    rawDatadt <- BuildrawDatadt(key, Value)
    rawStQ <- new(Class = 'rawStQ', Data = rawDatadt, DD = DD)
return(rawStQ)
    StQ <- rawStQToStQ(rawStQ, DD)
    return(StQ)
}
