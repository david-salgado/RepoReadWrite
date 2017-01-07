#' @title Read a file with a key-value pair structure
#' 
#' @description \code{ReadRepoFile} returns an \linkS4class{StQ} (default) or a \linkS4class{rawStQ}
#'  object with the content of the file corresponding to the input name.
#' 
#' @param FileName Character vector of length 1 with the name of the file to read. The file will be 
#' read from the working directory (see \link[base]{getwd}) unless the full path is specified.
#' 
#' @param DD Object of class \linkS4class{DD} with the definition and characteristics of the data 
#' contained in the file to read.
#' 
#' @param out Character vector of length 1 indicating whether to output an \linkS4class{StQ} object
#' (\code{out} = 'StQ'; default) or a \linkS4class{rawStQ} object (\code{out} = 'rawStQ').
#' 
#' @param perl Logical vector of length 1 indicating whether Perl is installed in the system or not.
#' 
#' @param sep Logical vector of length 1 containing the combination of characters used as separator 
#' in the input file (default value @@).
#' 
#' @return Return an object of class \linkS4class{StQ} or class \linkS4class{rawStQ} with all data 
#' from the input file.
#' 
#' @examples
#' \dontrun{
#' #We assume that the key-value ASCII file \code{E30183.FF_V1.MM032014.D_1} is in the administrator 
#' desktop (change accordingly otherwise): 
#' RepoName <- 'C:/Users/Administrador/Desktop/E30183.FF_V1.MM032014.D_1'
#' Example.StQ <- ReadRepoFile(RepoName)
#' str(Example.StQ)
#' }
#' 
#' @seealso \code{\link{ReadSASFile}}, \code{\link{ReadXLSFile}}, \code{\link{WriteRepoFile}}
#'
#' @import data.table
#' 
#' @importFrom stringi stri_replace_all_regex
#' 
#' @export
ReadRepoFile <- function(FileName, DD, out = 'StQ', perl = FALSE, sep = '@@') {
    
    if (out != 'StQ' & out != 'rawStQ') stop('[RepoReadWrite::ReadRepoFile] The input parameter out must be "StQ" or "rawStQ".\n')
    
    if (!perl %in% c(TRUE, FALSE)) stop('[RepoReadWrite::ReadRepoFile] The input parameter perl must be TRUE or FALSE.\n')
    
    if (length(sep) != 1) stop('[RepoReadWrite::ReadRepoFile] The input parameter sep must a character vector of length 1.\n')
    
    File <- fread(FileName, sep = '\n', header = FALSE, skip = 0L, nrows = -1, na.strings = ' ',
                  strip.white = TRUE,
                  stringsAsFactors = FALSE, colClasses = 'character')
    
    cat('\n Parsing IDDD identifiers...')
    regexp <- paste0("(\\w+)", sep, "([A-Za-z0-9_\\. ]+)", sep, "(.*)")
    if (perl) {
        
        File[, IDDDKey := gsub(regexp, "\\1", File$V1, perl = TRUE)]
        
    } else {
        
        File[, IDDDKey := stri_replace_all_regex(V1, regexp, "$1")]
        
    }
    cat(' ok.\n')
    
    cat('\n Parsing qualifiers...')
    if (perl) {
        
        File[, QualKey := gsub(regexp, "\\2", File$V1, perl = TRUE)]
        
    } else {
        
    File[, QualKey := stri_replace_all_regex(V1, regexp, "$2")]
        
    }
    cat(' ok.\n')
    
    cat('\n Parsing values...')
    if (perl) {
    
        File[, Value := gsub(regexp, "\\3", File$V1, perl = TRUE)]
        
    } else {
    
        File[, Value := stri_replace_all_regex(V1, regexp, "$3")]
        
    }
    cat(' ok.\n')
    
    File[, V1 := NULL]
    cat('\n Building rawStQ object...')
    rawDatadt <- new(Class = 'rawDatadt', File)
    output <- new(Class = 'rawStQ', Data = rawDatadt, DD = DD)
    cat(' ok.\n')
    if (out == 'StQ'){
        
        cat('\n Transforming rawStQ object into an StQ object...')
        output <- rawStQToStQ(output)
        cat(' ok.\n')
    }
    return(output)
}
