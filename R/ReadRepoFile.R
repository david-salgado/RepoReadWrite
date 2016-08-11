#' @title Write a file with a key-value
#' 
#' @description \code{ReadRepoFile} returns a \linkS4class{data.table} with the content of the file 
#' corresponding to the input name.
#' 
#' @param FileName Character vector of length 1 with the name of the file to read. The file will be 
#' read from the working directory (see \link[base]{getwd}) unless the full path is specified.
#' 
#' @param DD Object of class \linkS4class{DD} with the definition and characteristics of the data 
#' contained in the file to read.
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
#' @seealso \code{\link{ReadSASFile}}, \code{\link{ReadXLSFile}}, \code{\link{WriteRepoFile}}
#'
#' @import data.table
#' 
#' @export
ReadRepoFile <- function(FileName, DD) {
    
    File <- fread(FileName, sep = '\n', header = FALSE, skip = 0L, nrows = -1, na.strings = ' ',
                  strip.white = TRUE,
                  stringsAsFactors = FALSE, colClasses = 'character')
    
    File$IDDDKey <- gsub("(\\w+)@@([A-Za-z0-9\\. ]+)@@(.+)", "\\1", File$V1)
    File$QualKey <- gsub("(\\w+)@@([A-Za-z0-9\\. ]+)@@(.+)", "\\2", File$V1)
    File$Value <- gsub("(\\w+)@@([A-Za-z0-9\\. ]+)@@(.+)", "\\3", File$V1)
    File[, V1 := NULL]
    rawDatadt <- new(Class = 'rawDatadt', File)
    rawStQ <- new(Class = 'rawStQ', Data = rawDatadt, DD = DD)
    StQ <- rawStQToStQ(rawStQ)
    return(StQ)
}
