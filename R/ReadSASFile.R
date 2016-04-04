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
#' @param DD Object of class \linkS4class{DD} with the content of the file 
#' \code{DD} of definitions and properties of every variable.
#' 
#' @param DDslot Character vector of length 1 with the name of DD slot in which
#' data to be read are defined. Its default value is \code{MicroData}.
#' 
#' @param VNCName Character vector of length 1 with the name of the element of
#' the list of slot of VarNameCorresp slot in which data to be read are defined.
#' 
#' @return \linkS4class{data.table} with the contents of the \code{DD} file, 
#' with statistical units in rows and variables as columns.
#' 
#' @examples
#' # We assume that the SAS file \code{MM032014.sas7bdat} is in the 
#' administrator desktop:
#' SASName <- 'C:/Users/Administrador/Desktop/MM032014.sas7bdat'
#' # We assume data created previosly:
#' data(XLS)
#' data(RepoDD)
#' data(VNC)
#' DD <- RepoDDToDD(RepoDD, VNC)
#' Example.DM <- ReadSASFile(SASName, DD, VNCName = 'MicroData')
#' 
#' @seealso  \link{RepoDDToDD}, \link{ReadRepoFile}, \link{WriteRepoFile}
#' 
#' @importFrom haven read_sas
#' 
#' @importFrom gdata trim
#' 
#' @import data.table StQ
#' 
#' @export
ReadSASFile <- function(SASFileName, DD, DDslot = 'MicroData', VNCName = DDslot){
   
    # Comprobamoos que el slot del DD que se especifica realmente es uno de los slots del objeto DD
    if (DDslot != 'MicroData' & DDslot != 'Aggregates' & DDslot != 'AggWeights'
        & DDslot != 'Other'){
      stop(paste0('[Validity ReadSASFile]"', DDslot, '" is not a slot of the DD input object.'))
    }
  
    out.SP <- haven::read_sas(SASFileName)
    ColClasses <- unlist(lapply(out.SP, class))
    
    out.SP <- as.data.table(out.SP)
    for (col in names(out.SP)){
        
        out.SP[, col := gdata::trim(get(col)), with = FALSE]
        
    }
      
    Exceldf <- getVNC(DD)@VarNameCorresp[[VNCName]]
    CalID <- Exceldf$IDQual
    CalID <- CalID[!is.na(CalID)]
    CalID <- CalID[CalID != '']
    CalID <- intersect(names(Exceldf), CalID)
    
    CalNoID <- Exceldf$NonIDQual
    CalNoID <- CalNoID[!is.na(CalNoID)]
    CalNoID <- CalNoID[CalNoID != '']
    CalNoID <- intersect(names(Exceldf), CalNoID)
    
    
    Cals <- union(CalID, CalNoID)
    VarSP <- Exceldf$Unit1
    VarSP <- VarSP[!is.na(VarSP) & VarSP !=""]
    MissVar <- setdiff(VarSP, names(out.SP))

    if (length(MissVar) > 0) cat(paste0('[RepoReadWrite::ReadSASFile] The following variables of the Excel sheet are not present in the SAS file:\n\n', 
                                        paste0(MissVar, collapse = ' '), '\n\n'))
    VarSP <- intersect(VarSP, names(out.SP))
    if (length(VarSP) == 0) stop('[RepoReadWrite::ReadSASFile] Variables specified in the VNC slot of the input DD object are not present in the SAS file.')
    out.SP <- out.SP[, VarSP, with = F]

    Exceldf <- Exceldf[is.na(IDDD) & !is.na(IDQual), IDDD:= IDQual]
    Exceldf <- Exceldf[is.na(IDDD) & !is.na(NonIDQual), IDDD:= NonIDQual]
    
    pasteNA <- function(x, y){
        out <- ifelse(is.na(y) | y == '', paste0(x, ''), paste(x, y, sep ="_"))
        return(out)
    }
    
    Exceldf <- copy(Exceldf)[, NewVar := IDDD]
    Exceldf <- Exceldf[IDQual != '' & 
                       IDDD == '' & 
                       Unit1 != '', NewVar := IDQual]
    Exceldf <- Exceldf[NonIDQual != '' & IDDD == '' & Unit1 != '',
                       NewVar := NonIDQual]
    for (Cal in Cals){
        Exceldf <- copy(Exceldf)[, NewVar:= pasteNA(NewVar, get(Cal))]
    }
    
    EquivalName <- names(out.SP)
    names(EquivalName) <- Exceldf$NewVar[Exceldf$Unit1 %in% EquivalName]
    setnames(out.SP, EquivalName, names(EquivalName))
    
    if (DDslot == 'MicroData'){
      DD <- getData(DD)
    }else if (DDslot == 'Aggregates'){
      DD <- getAggr(DD)
    }else if (DDslot == 'AggWeights'){
      DD <- getAggWeights(DD)
    }else{
      DD <- getOtherDD(DD)
    }  
    
    if (nrow(DD) == 0){
      stop(paste0('[Validity ReadSASFile]Data to be read are not defined in the slot "', DDslot, '" of the DD input object.'))
    }
   
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
