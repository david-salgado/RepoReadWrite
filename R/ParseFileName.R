#' @title Produce a vector of file names parsed out of a pattern and a sequence of time period names
#' 
#' @description This function parses each component of a vector of file names with a pattern filling
#'  out the corresponding codes with the input time periods.
#' 
#' @param FileNames Character vector with the names of the files to parse.
#' 
#' @param TimePeriods Character vector with the time periods.
#' 
#' @return Return a vector similar to the input vector \code{FileNames} with the period codes parsed
#'  according to the input \code{TimePeriods}.
#' 
#' @examples
#' \dontrun{
#' ParseFileName(c('N:/UDMTD/UDTMDCOM/DepSel.Repositorio/E30183/DataSAS/Ruta_dat[aa]/dat[mm][aa].sas7bdat'), c('MM122015', 'MM012016', 'MM022016'))
#' }
#' 
#' @import data.table xlsx XML
#'       
#' @export
ParseFileName <- function(FileNames, TimePeriods){
    
    PeriodType <- unique(substr(TimePeriods, 1, 1))
    if (length(PeriodType) != 1) stop('[RepoReadWrite::ParseFileName] Only one time period type allowed at a time.')
    
    if (PeriodType == 'M') {
        
        output <- unlist(lapply(TimePeriods, function(TimePeriod){
            
            month <- unlist(substr(TimePeriod, 3, 4))
            ParsedFileNames <- unlist(lapply(FileNames, function(FileName){
                
                outLocal <- gsub('[mm]', month, FileName, fixed = TRUE)
                return(outLocal)
            }))
            year4 <- unlist(substr(TimePeriod, 5, 8))
            ParsedFileNames <- unlist(lapply(ParsedFileNames, function(FileName){
                
                outLocal <- gsub('[aaaa]', year4, FileName, fixed = TRUE)
                return(outLocal)
            }))
            year2 <- unlist(substr(TimePeriod, 7, 8))
            ParsedFileNames <- unlist(lapply(ParsedFileNames, function(FileName){
                
                outLocal <- gsub('[aa]', year2, FileName, fixed = TRUE)
                return(outLocal)
            }))
        }))
    }
    return(output)
    
}

