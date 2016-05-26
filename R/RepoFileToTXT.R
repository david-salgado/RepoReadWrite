#' @title Write a key-value pair txt file.
#' 
#' @description \code{RepoFileToTXT} writes the key-value objects found in the input parameter 
#'  StQList in fixed-width column txt files. 
#' 
#'  This method reads the files found with the input parameter StQList and writes their content as a
#'  fixed-width column txt file with key-value pair structure with the same name as files read, but
#'  extension .txt.
#' 
#' @param RepoPath Character vector of length 1 with the path of the repository from which files are
#'  to be read.
#'  
#' @param StQList Object of class \linkS4class{StQList}.
#' 
#' @param SurveyCode Character vector of lenght 1 with the code of the survey 
#'  
#' @param FileType Character vector of length 1 with the type of the file to be written (FI, FF, FG 
#'  or FD).
#' 
#' @param OutPath Character vector of length 1 with the path where files will be written. It
#'  must not be another txt file of the same periods which are being considered.
#'  
#' @examples
#' \dontrun{
#'  StQList <- RepoFileToStQList('Z:/', 'XXXXXX', 'C:/', 'E30183', 'MM112014', 'MM122014', 'FF')
#'  RepoFileToTXT('Z:/', StQList, 'FF', 'C:/')
#' }
#'       
#' @export
    RepoFileToTXT <- function(RepoPath, StQList, SurveyCode, FileType, OutPath){
        
        if (!FileType %in% c('FI', 'FF', 'FD', 'FG')){
            
            stop('[RepoFiletoTXT] Only FI, FF, FG or FD files are allowed.')
        }
        
        Data <- getData(StQList)
        Periods <- names(Data)
        Ficheros <- vector('character', length(Periods))
        for (Period.index in seq(along = Periods)){
            
            RepoVars <- getIDDD(StQList[[Period.index]])
            dcastedStQ <- dcast_StQ(StQList[[Period.index]], VarNames = RepoVars, 'MicroData')
            IDDDNames <- names(dcastedStQ)
            UnitNames <- IDDDToUnitNames(getVNC(StQList[[Period.index]]), IDDDNames)
            UnitNames <- UnitNames[[2]]
            setnames(dcastedStQ, IDDDNames, UnitNames)
            LastFileVersion <- RepoTopn(RepoPath, paste0(FileType, '_V1.', Periods[Period.index]))
            NewVer <- as.numeric(LastFileVersion) + 1
            if (FileType == 'FF'){
                
                NewFile <- paste0(OutPath, SurveyCode, '.', FileType, '_matricial.', Periods[Period.index], '.D_', NewVer)
            }else{
                
                NewFile <- paste0(OutPath, SurveyCode, '.', FileType, '_matricial.', Periods[Period.index], '.P_', NewVer)
            }
            Ficheros[Period.index] <- NewFile
            write.table(dcastedStQ, NewFile, sep='~', row.names = FALSE, quote = FALSE, na = '', dec = '.')
        }
}

