#' @title Produce an object \linkS4class{DD} from an Excel file
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
#' 
#' \dontrun{
#' ExcelName <- 'T:/E30163/E30163.NombresVariables_V1.xlsx'
#' DD <- RepoXLSToDD(ExcelName)
#' show(DD)
#' }
#' 
#' @import data.table xlsx XML
#' 
#' @include RepoXLSToVNC.R RepoFileVersion.R RepoDDToDD.R
#'                        
#' @export
RepoXLSToDD <- function(ExcelName){
        
        ParsedExcelName <- strsplit(ExcelName, '/', fixed = TRUE)[[1]]
        ParsedExcelName <- lapply(ParsedExcelName, strsplit, split = '\\', fixed = TRUE)
        ParsedExcelName <- unlist(ParsedExcelName)
        FileName <- ParsedExcelName[length(ParsedExcelName)]
        RepoPath <- paste0(ParsedExcelName[-length(ParsedExcelName)], collapse = '/')
        DDname <- gsub('NombresVariables', 'DD', FileName)
        DDname <- strsplit(DDname, '.', fixed = TRUE)[[1]]
        DDname <- paste0(DDname[-length(DDname)], collapse = '.')
        DDname <- paste0(c(RepoPath, DDname), collapse = '/')
        VNC <- RepoXLSToVNC(ExcelName)
        if (file.exists(DDname)) {
            
            DD <- RepoDDToDD(DDname, VNC)
        
        } else {
            
            RepoXLSToRepoDD(ExcelName)
            DD <- RepoDDToDD(DDname, VNC)
            
        }
        return(DD)
}

