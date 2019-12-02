#' @title Read a file with a key-value pair structure.
#' 
#' @description \code{ReadRepoFile} returns an \code{\link[StQ]{StQ}} (default) or a 
#' \code{\link[StQ]{rawStQ}} object with the content of the file corresponding to the 
#' input name.
#' 
#' @param FileName Character vector of length 1 with the name of the file to read. The file will be 
#' read from the working directory (see \link[base]{getwd}) unless the full path is specified.
#' 
#' @param DD Object of class \code{\link[StQ]{DD}} with the definition and characteristics of the data 
#' contained in the file to read.
#' 
#' @param out Character vector of length 1 indicating whether to output an \code{\link[StQ]{StQ}} 
#' object (\code{out} = 'StQ'; default) or a \code{\link[StQ]{rawStQ}} object 
#' (\code{out} = 'rawStQ').
#' 
#' @param perl Logical vector of length 1 indicating whether Perl is installed in the system or not.
#' 
#' @param sep Character vector of length 1 containing the combination of characters used as separator 
#' in the input file (default value @@).
#' 
#' @param encoding Character vector of length 1 with default value is "unknown". Other possible 
#' options are "UTF-8" and "Latin-1". 
#' Note: it is not used to re-encode the input, rather enables handling of encoded strings in their 
#' native encoding.
#' 
#' @param verbose Logical vector of length 1 indicating whether report timings are shown or not.
#' 
#' @return Return an object of class \code{\link[StQ]{StQ}} or class \code{\link[StQ]{rawStQ}} with all data 
#' from the input file.
#' 
#' @examples
#' \dontrun{
#' #We assume that the key-value ASCII file \code{E30183.FF_V1.MM032014.D_1} is in the administrator 
#' desktop (change accordingly otherwise): 
#' ExcelName <- 'T:/E30163/E30163.NombresVariables_V1.xlsx'
#' DD <- RepoXLSToDD(ExcelName)
#' RepoName <- 'T:/E30163/E30163.FF_V1.MM032014.D_1'
#' Example.StQ <- ReadRepoFile(RepoName, DD, perl = TRUE)
#' str(Example.StQ)
#' }
#' 
#' @seealso \code{\link{WriteRepoFile}}
#'
#' @import data.table
#' 
#' @importFrom stringi stri_replace_all_regex
#' 
#' @export
ReadRepoFile <- function(FileName, DD, out = 'StQ', perl = FALSE, sep = '@@', encoding = 'unknown', verbose = TRUE) {
    
    IDDDKey <- V1 <- QualKey <- Value <- NULL
    
    if (out != 'StQ' & out != 'rawStQ') stop('[RepoReadWrite::ReadRepoFile] The input parameter out must be "StQ" or "rawStQ".\n')
    
    if (!perl %in% c(TRUE, FALSE)) stop('[RepoReadWrite::ReadRepoFile] The input parameter perl must be TRUE or FALSE.\n')
    
    if (length(sep) != 1) stop('[RepoReadWrite::ReadRepoFile] The input parameter sep must a character vector of length 1.\n')
    
    File <- fread(FileName, sep = '\n', header = FALSE, skip = 0L, nrows = -1, na.strings = '',
                  strip.white = TRUE,
                  stringsAsFactors = FALSE, verbose = FALSE, colClasses = 'character', encoding = encoding)
    
    if (verbose) cat('\n Parsing IDDD identifiers...')
    regexp <- paste0("(\\w+)", sep, "([A-Za-z0-9_\\:\\-\\. /\\+]+)", sep, "(.*)")
    
    if (perl) {
        
        File[, IDDDKey := gsub(regexp, "\\1", File$V1, perl = TRUE)]
        
    } else {
        
        File[, IDDDKey := stri_replace_all_regex(V1, regexp, "$1")]
        
    }
    if (verbose) cat(' ok.\n')
    
    if (verbose) cat('\n Parsing qualifiers...')
    if (perl) {
        
        File[, QualKey := gsub(regexp, "\\2", File$V1, perl = TRUE)]
        
    } else {
        
    File[, QualKey := stri_replace_all_regex(V1, regexp, "$2")]
        
    }
    if (verbose) cat(' ok.\n')
    
    if (verbose) cat('\n Parsing values...')
    if (perl) {
    
        File[, Value := gsub(regexp, "\\3", File$V1, perl = TRUE)]
        
    } else {
    
        File[, Value := stri_replace_all_regex(V1, regexp, "$3")]
        
    }
    if (verbose) cat(' ok.\n')
    
    File[, V1 := NULL]
    if (verbose) cat('\n Building rawStQ object...')
    rawDatadt <- File
    output <- rawStQ(rawData = rawDatadt, DD = DD)

    if (verbose) cat(' ok.\n')
    if (out == 'StQ'){
        
        if (verbose) cat('\n Transforming rawStQ object into an StQ object...')
        output <- rawStQToStQ(output)
        if (verbose) cat(' ok.\n')
    }
    return(output)
}
