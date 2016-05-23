#' @title Produce an object of class \linkS4class{rawStQList} from files with the structure
#' key-value pair.
#' 
#' @description \code{RepoFileTorawStQList} returns an object of class \linkS4class{rawStQList} from
#' files with the structure key-value pair.
#' 
#' @param SurveyCode Character vector of length 1 with the code of each survey.
#' 
#' @param IniPeriod Character vector of length 1 with the initial time period to be read (in the 
#' repository notation).
#' 
#' @param FinPeriod Character vector of length 1 with the final time period to be read (in the 
#' repository notation).
#' 
#' @param FileType Character vector of length 1 with the type of the file to be read (FF, FG or FD).
#' 
#' @param RepoPath Character vector of length 1 with the path of the repository from which files are
#' to be read.
#' 
#' @param Rot Logical vector of length 1 indicating whether rotated sample files are to be included 
#' (default value FALSE).
#' 
#' @param includeFI Logical vector of length 1 indicating whether ID variables will be included in
#' the  slot of class \linkS4class{VarNameCorresp} of the \linkS4class{DD} object.
#' 
#' @return Object of class \linkS4class{rawStQList}.
#' 
#' @examples
#' \dontrun{
#'  RepoFileTorawStQList('E30183', 'N:/UDMTD/UDTMDCOM/DepSel.Repositorio/DemIRIAaSP_IASS/E30183/', 
#'                     'FF', 'MM022016', 'MM022016')
#' }
#'
#' @include RepoXLSToVNC.R ReadRepoFile.R RepoDDToDD.R ReadRepoFile.R
#' 
#' @import data.table 
#' 
#' @export
RepoFileTorawStQList <- function(SurveyCode, RepoPath, FileType, IniPeriod, FinPeriod, Rot = FALSE, 
                              includeFI = TRUE){
    
    if (!FileType %in% c('FF', 'FD', 'FG')){
        
        stop('[StQ::RepoFileToStQList] Only FI, FF, FG or FD files are allowed.')
    }
    
    IniRepoTime <- newRepoTime(IniPeriod)
    FinRepoTime <- newRepoTime(FinPeriod)
    Months <- Seq(IniRepoTime, FinRepoTime, Rot = Rot)
    MonthsNamesM <- getRepo(Months)
    
    ## VNC Construction
    ExcelName <- paste0(RepoPath, SurveyCode, '.NombresVariables', '.xlsx')
    wb <- loadWorkbook(ExcelName)
    SheetNames <- names(getSheets(wb))
    
    VNC <- list()
    for (sheet in SheetNames[SheetNames != 'ID']) {
        
        VNC[[sheet]] <- RepoXLSToVNC(ExcelName, sheet)
        
    }
    
    if (includeFI) VNC[['ID']] <- RepoXLSToVNC(ExcelName, 'ID')
    
    VNC <- Reduce(`+`, VNC, VNC[[1L]])       
  
    ## DD Construction
    
    DDName <- paste0(RepoPath, SurveyCode, '.DD_V', RepoTopn(RepoPath, 'DD'))
    RepoDD <- ReadRepoFile(DDName)
    DD <- RepoDDToDD(RepoDD, VNC)
   
    #####                 Creación de lista de objetos StQ             #####
    rawQList <- list()
    
    for (Month.index in seq(along = MonthsNamesM)) {
        
        NamePrefix <- paste0(SurveyCode, '.', FileType, '_V1.')
        
        if (FileType == 'FF') {
            
            Name <- paste0(RepoPath, NamePrefix, MonthsNamesM[[Month.index]], '.D_', 
                           RepoTopn(RepoPath, paste0(NamePrefix, MonthsNamesM[[Month.index]])))
            
        } else {
            
            Name <- paste0(RepoPath, NamePrefix, MonthsNamesM[[Month.index]], '.P_', 
                           RepoTopn(RepoPath, paste0(NamePrefix, MonthsNamesM[[Month.index]])))
        }
        
        Repo <- ReadRepoFile(Name)
        key <- new(Class='rawKey', Repo[['Key']])
        Repo[['Key']] <- key
        
        rawQList[[Month.index]] <- new(Class = 'rawStQ', Data = new(Class = 'rawDatadt', Repo), DD = DD)
    }
    
    names(rawQList) <- MonthsNamesM
    rawStQList <- BuildrawStQList(rawQList)
    
    return(rawStQList)
}
