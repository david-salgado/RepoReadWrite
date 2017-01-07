#' @title Generates StQ object of aggregate/index weights
#' 
#' @description \code{GenerateWeights} returns a \linkS4class{StQ} with data from the SAS files of 
#' weighting corresponding to the specified SAS files.
#' 
#' This function reads the SAS files of weightings corresponding to the Excel sheets specified as 
#' input parameter \code{SheetNamesPond} and returns its content in a \linkS4class{StQ} object. 
#' 
#' This correspondence is specified through the respective component from \code{VarNameCorresp} 
#' slot of the \code{DD} input object, which is a \link{data.table} with the content of a sheet of 
#' the Excel file, and \code{DD}, which is an object of class \linkS4class{DD} containing the DD  
#' file with the definition and properties of every variable. 
#' 
#' The object \code{DD} is naturally obtained from the original \code{DD} file as output of 
#' functions \code{\link{RepoDDToDD}} or \code{\link{xmlToDD}}. 
#'  
#' @param SASFilesNames A named character vector with the corresponding SAS files of weightings. The 
#' name of each component must be the name of the corresponding Excel sheet of weightings.
#' 
#' @param DD Object of class \linkS4class{DD} with the content of the file \code{DD} of definitions 
#' and properties of every variable.
#' 
#' @param SurveyName Character vector of length 1 with the name of the survey.
#' 
#' @return \linkS4class{StQ} object with the content of the SAS files of weights corresponding to 
#' the SheetNamesPond.
#' 
#' @examples
#' \dontrun{
#' # Param SASWeighFiles must be as following:
#' SASWeightFiles <- c('C:/Users/Administrador/Desktop/pondcarama14.sas7bdat')
#' names(SASWeightFiles) <- 'PondCARama'
#' SASWeightFiles
#' 
#' # We assume data created previosly:
#' Weightss <- GenerateWeights(SASFilesNames, DD, SurveyName = 'IASS')
#' }
#' 
#' @seealso  \link{ReadSASWeigh}
#' 
#' @import data.table StQ
#' 
#' @include ReadSASWeights.R
#' 
#' @export
GenerateWeights <- function(SASFilesNames, DD, SurveyName){
    
    PondVar <- paste0(SurveyName, 'Pond')
    SheetNamesWeigh <- names(SASFilesNames)
    auxdt <- new(Class = 'Datadt')
    IDQual <- c()
    NonIDQual <- c()
    
    for (Pond in SheetNamesWeigh) {
        
        SASWeighFile <- SASFilesNames[[Pond]]
        auxPond <- ReadSASWeigh(SASWeighFile, DD, VNCName = Pond)
        
        VNC <- getVNC(DD)
        auxVar <- getNonIDQual(VNC[[Pond]])
        NonIDQual <- unique(c(NonIDQual, auxVar))
        IDQual <- unique(c(IDQual, getIDQual(VNC[[Pond]])))
        
        namesVarPonder <- names(auxPond)[!names(auxPond) %in% getData(DD)[['Variable']]]
        noVarPonder <- setdiff(names(auxPond), namesVarPonder)
        
        auxlist <- list()
        
        for (Var in seq(along = namesVarPonder)) {
            
            aux <- auxPond[, c(noVarPonder, namesVarPonder[Var]), with = F]
            for (newVar in seq(along = auxVar)) {
                aux[,auxVar[newVar] := strsplit(namesVarPonder[Var], '_')[[1]][newVar + 1]]
            }
            
            setnames(aux, old = namesVarPonder[Var], new = 'Value')
            auxlist[[Var]] <- aux
        }
        
        auxPond <- rbindlist(auxlist)
        auxPond[, IDDD := PondVar]
        setcolorder(auxPond, 
                    c(getIDQual(VNC[[Pond]]), getNonIDQual(VNC[[Pond]]), c('IDDD', 'Value')))
        
        auxdt <- auxdt + new(Class = 'Datadt', auxPond)
        
    }
    
    setcolorder(auxdt, c(IDQual, setdiff(NonIDQual, 'VarPonder'), c('VarPonder', 'IDDD', 'Value')))
    setorderv(auxdt, c(IDQual, 'VarPonder'))
    Pond <- new(Class = 'StQ', Data = auxdt, DD = DD)
    
    return(Pond)

}

