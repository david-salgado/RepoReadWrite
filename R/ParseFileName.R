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
#' ParseFileName(c('N:/UDMTD/UDTMDCOM/DepSel.Repositorio/E30183/DataSAS/Ruta_dat/dat[mm][aa].sas7bdat'), c('MM122015', 'MM012016', 'MM022016'))
#' }
#' 
#' @import data.table xlsx XML
#'       
#' @export
ParseFileName <- function(FileNames, TimePeriods){
    
    PeriodType <- unique(substr(TimePeriods, 1, 1))
    if (length(PeriodType) != 1) stop('[RepoReadWrite::ParseFileName] Only one time period type allowed at a time.')
    
    if (PeriodType == 'M') {
        
        months <- unlist(lapply(TimePeriods, substr, 3, 4))
        output <- unique(unlist(lapply(months, function(month){
            
            outLocal <- gsub('[mm]', month, FileNames, fixed = TRUE)
            return(outLocal)
        })))
        years4 <- unlist(lapply(TimePeriods, substr, 5, 8))
        output <- unique(unlist(lapply(years4, function(year4){
            
            outLocal <- gsub('[aaaa]', year4, output, fixed = TRUE)
            return(outLocal)
        })))
        years2 <- unlist(lapply(TimePeriods, substr, 7, 8))
        output <- unique(unlist(lapply(years2, function(year2){
            
            outLocal <- gsub('[aa]', year2, output, fixed = TRUE)
            return(outLocal)
        })))
        
return(output)
    }
    
    if (length(FileNames) == 1){
        
        FileNames.sub <- gsub('\\\\', '/', FileNames)
        ParsedFileNames <- as.list(strsplit(FileNames.sub, split ='/')[[1]])
        index.parsed <- grep('[', ParsedFileNames, fixed = TRUE)
        if (length(index.parsed) == 0) return(FileNames)
        for (i in index.parsed) {
            
            aux <- ParsedFileNames[i]
            
        }
        return(index.parsed)
        
    } else {
        
        return('ok')
        
    }
    
}

