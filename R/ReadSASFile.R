#' @title Read a SAS file renaming variables as specified in input parameters
#' 
#' @description \code{ReadSASFile} returns a \linkS4class{data.table} with all 
#' data from the input SAS file.
#' 
#' This function reads the SAS file specified as input parameter 
#' \code{SASFileName} and returns its content in a \linkS4class{data.table} with
#' statistical units in rows and variables in columns. The names of the 
#' variables are assigned according to the Excel file with variable names 
#' correspondence for each statistical operation.
#' 
#' This correspondence is specified with the input parameters \code{Exceldf}, 
#' which is a \link{data.frame} with the content of a sheet of the Excel file, 
#' and \code{DD}, which is a \link{data.frame} with the content of the DD file 
#' containing the definition and properties of every variable. 
#' 
#' The \link{data.frame} \code{DD} is naturally obtained from the original 
#' \code{DD} file as output of functions \code{\link{RepoDDToDD}} or
#' \code{\link{xmlToDD}}. 
#'  
#' @param SASFileName Character vector of length 1 with name of the file. The 
#' file will be read from the working directory (see \link[base]{getwd}) unless 
#' the full path is specified.
#' 
#' @param Exceldf \code{\link{data.frame}} con el contenido del fichero Excel 
#' de equivalencias entre los nombres de las variables de la encuesta asignados 
#' por las distintas unidades del INE implicadas.
#' 
#' @param DD Object of class \linkS4class{DD} with the content of the file 
#' \code{DD} of definitions and properties of every variable.
#' 
#' @return \linkS4class{data.table} with the contents of the \code{DD} file, 
#' with statistical units in rows and variables as columns.
#' 
#' @examples
#' # We assume that the SAS file \code{MM032014.sas7bdat} is in the 
#' administrator desktop:
#' SASName <- 'C:/Users/Administrador/Desktop/MM032014.sas7bdat'
#' data(XLS)
#' data(RepoDD)
#' DD <- RepoDDToDD(RepoDD)
#' Example.DM <- ReadSASFile(SASName, XLS, DD)
#' 
#' @seealso  \link{RepoDDToDD}, \link{ReadRepoFile}, \link{WriteRepoFile}
#' 
#' @importFrom haven read_sas
#' 
#' @import data.table
#' 
#' @export
ReadSASFile <- function(SASFileName, Exceldf, DD){
    
    out.SP <- haven::read_sas(SASFileName)    
    
    ColClasses <- unlist(lapply(out.SP, class))
    
    out.SP <- as.data.table(out.SP)
    
      
    Exceldf <- as.data.table(Exceldf)
    
    CalID <- Exceldf$CalificadoresID
    CalID <- CalID[!is.na(CalID)]
    CalID <- CalID[CalID != '']
    
    CalNoID <- Exceldf$CalificadoresNoID
    CalNoID <- CalNoID[!is.na(CalNoID)]
    CalNoID <- CalNoID[CalNoID != '']
    
    Cals <- union(CalID, CalNoID)
    VarSP <- Exceldf$SP
    VarSP <- VarSP[!is.na(VarSP)]
    MissVar <- setdiff(VarSP, names(out.SP))
    if (length(MissVar) > 0) cat(paste0('[RepoReadWrite::ReadSASFile] The following variables of the Excel sheet are not present in the SAS file:\n\n', 
                                        paste0(MissVar, collapse = ' '), '\n\n'))
    VarSP <- intersect(VarSP, names(out.SP))
    out.SP <- out.SP[, VarSP, with = F]
    
    Exceldf <- Exceldf[is.na(Variables) & !is.na(CalificadoresID), 
                       Variables:= CalificadoresID]
    Exceldf <- Exceldf[is.na(Variables) & !is.na(CalificadoresNoID), 
                       Variables:= CalificadoresNoID]
    
    pasteNA <- function(x, y){
        out <- ifelse(is.na(y) | y == '', paste0(x, ''), paste(x, y, sep ="_"))
        return(out)
    }
    
    Exceldf <- Exceldf[, NewVar := Variables]
    Exceldf <- Exceldf[CalificadoresID != '' & Variables == '' & SP != '', 
                       NewVar := CalificadoresID]
    Exceldf <- Exceldf[CalificadoresNoID != '' & Variables == '' & SP != '', 
                       NewVar := CalificadoresNoID]
    for (Cal in Cals){
        Exceldf <- copy(Exceldf)[, NewVar:= pasteNA(NewVar, get(Cal))]
    }

    EquivalName <- names(out.SP)
    names(EquivalName) <- Exceldf$NewVar[Exceldf$SP %in% EquivalName]
    setnames(out.SP, EquivalName, names(EquivalName))
        
    DD <- getData(DD)
    
    ExtractNames <- function(NamesVector){
      NamesVector <- as.list(NamesVector)
      NamesVector <- unlist(lapply(NamesVector, 
                                   function(name){strsplit(name, '_')[[1]][1]}))
      return(NamesVector)
    }

    DDVarNames <- unlist(lapply(as.list(names(out.SP)), ExtractNames))
    names(DDVarNames) <- names(out.SP)

    for (Var in names(out.SP)){

      out.SP[, Var := as(get(Var), 
                         DD[Variable == DDVarNames[Var], Class]), with = F]
      
    }
    
    return(out.SP)

}
