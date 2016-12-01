#' @title Produce an object \linkS4class{DD} from an xlsx file
#' 
#' @description This function builds an object \linkS4class{DD} using the contents of an xlsx file.
#' \code{RepoXLSToDD} transforms the content of an xlsx file into a DD file.  
#' 
#' @param ExcelName Character vector of length 1 with the name of the file to read. The file will be 
#' read from the working directory (see \link[base]{getwd}) unless the full path is specified.
#' 
#' @return This function returns an object of class \linkS4class{DD}.
#' 
#' @examples
#' # We assume that the xlsx file ExampleXLS.NombresVariables.xlsx with the appropriate structure is
#' # in the working directory (change accordingly otherwise):
#' \dontrun{
#' DD <- RepoXLSToDD('ExampleXLS')
#' show(DD)
#' }
#' 
#' @import data.table xlsx XML
#'       
#' @export
RepoXLSToDD <- function(ExcelName){
    
        VNC <- RepoXLSToVNC(ExcelName)
        RepoXLSToRepoDD(ExcelName)
        ExcelNameParsed <- strsplit(ExcelName, split = '.', fixed = TRUE)[[1]]
        ExcelNameParsed[length(ExcelNameParsed)] <- NULL
        Version <- strsplit(ExcelNameParsed, split = '_')[[1]][2]
        ExcelNameParsed[2] <- paste0('DD_', Version)
        RepoDDName <- paste0(ExcelNameParsed, collapse = '.')
        DD <- RepoDDToDD(RepoDDName, VNC)
        return(DD)
}

