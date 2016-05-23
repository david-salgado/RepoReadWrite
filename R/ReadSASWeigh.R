#' @title Read a SAS file renaming variables as specified in input parameters
#' 
#' @description \code{ReadSASWeigh} returns a \linkS4class{data.table} with all data from the input 
#' SAS file of weightings.
#' 
#' This function reads the SAS file of weightings specified as input parameter, \code{SASFileName}, 
#' and returns its content in a \linkS4class{data.table} with statistical units in rows and variables 
#' in columns. The names of the variables are assigned according to the Excel file with variable 
#' names correspondence for each statistical operation.
#' 
#' This correspondence is stablished with the component \code{VNCName} specified as input param, of 
#' the \code{VarNameCorresp} slot of the \code{DD} input object. That component is a 
#' \link{data.table} with the content of a sheet of the Excel file
#' 
#' The object \code{DD} is naturally obtained from the original \code{DD} file as output of 
#' functions \code{\link{RepoDDToDD}} or \code{\link{xmlToDD}}. 
#'  
#' @param SASFileName Character vector of length 1 with name of the file. The file will be read from
#' the working directory (see \link[base]{getwd}) unless the full path is specified.
#' 
#' @param DD Object of class \linkS4class{DD} with the content of the file \code{DD} of definitions 
#' and properties of every variable.
#' 
#' @param DDslot Character vector of length 1 with the name of DD slot in which data to be read are 
#' defined. Its default value is \code{MicroData}.
#' 
#' @param VNCName Character vector of length 1 with the name of the element of the list of slot of 
#' VarNameCorresp slot in which data to be read are defined. Its default value is the value of the 
#' \code{DDslot} param.
#' 
#' @return \linkS4class{data.table} with the contents of the \code{DD} file, with statistical units 
#' in rows and variables as columns.
#' 
#' @examples
#' \dontrun{
#' # We assume data created previosly:
#' Weigh <- ReadSASWeigh(SASName, DD, VNCName = 'PondCARama')
#' }
#' 
#' @seealso  \link{RepoDDToDD}, \link{ReadRepoFile}, \link{WriteRepoFile}
#' 
#' @importFrom haven read_sas
#' 
#' @importFrom gdata trim
#' 
#' @import data.table 
#' 
#' @export
ReadSASWeigh <- function(SASFileName, DD, DDslot = 'MicroData', VNCName = DDslot){
    
    # Comprobamos que el slot del DD que se especifica realmente es uno de los slots del objeto DD
    if (!(DDslot %in% c('ID', 'MicroData', 'ParaData', 'Aggregates', 'AggWeights', 'Other'))) {
        
        stop(paste0('[Validity ReadSASWeigh]"', DDslot, '" is not a slot of the DD input object.'))
    
    }
    
    # Comprobamos que el valor especificado para VNCName es realmente un componente del slot 
    # VarNameCorresp del objeto DD del input.
    if (!(VNCName %in% names(getVNC(DD)))) {
        
        stop(paste0('[Validity ReadSASWeigh]"', VNCName, '" is not a component in the VarNameCorresp slot of the DD input object.'))
    
    }
    
    
    out.SP <- haven::read_sas(SASFileName)
    ColClasses <- unlist(lapply(out.SP, class))
    out.SP <- as.data.table(out.SP)
    for (col in names(out.SP)) {
        
        out.SP[, col := gdata::trim(get(col)), with = FALSE]
        
    }
    
    VNCdt <- getVNC(DD)[[VNCName]]
    CalID <- getIDQual(VNCdt)
    CalID <- intersect(names(VNCdt), CalID)
    
    for (cal in CalID) {
        
        VNCdt[get(cal) == '.', cal := '', with = F]
        
    }
    
    CalNoID <- getNonIDQual(VNCdt)
    CalNoID <- intersect(names(VNCdt), CalNoID)
    
    
    Cals <- union(CalID, CalNoID)
    VarSP <- VNCdt$Unit1
    VarSP <- VarSP[!is.na(VarSP) & VarSP != ""]
    MissVar <- setdiff(VarSP, names(out.SP))
    
    if (length(MissVar) > 0) cat(paste0('[RepoReadWrite::ReadSASWeigh] The following variables of the Excel sheet are not present in the SAS file:\n\n', 
                                        paste0(MissVar, collapse = ' '), '\n\n'))
    
    VarSP <- intersect(VarSP, names(out.SP))
    if (length(VarSP) == 0) stop('[RepoReadWrite::ReadSASWeigh] Variables specified in the VNC slot of the input DD object are not present in the SAS file.')
    out.SP <- out.SP[, VarSP, with = F]
    
    VNCdt <- VNCdt[is.na(IDDD) & !is.na(IDQual), IDDD := IDQual]
    VNCdt <- VNCdt[is.na(IDDD) & !is.na(NonIDQual), IDDD := NonIDQual]
    
    pasteNA <- function(x, y) {
        out <- ifelse(is.na(y) | y == '', paste0(x, ''), paste(x, y, sep = "_"))
        return(out)
    }
    
    VNCdt <- copy(VNCdt)[, NewVar := IDDD]
    VNCdt <- VNCdt[IDQual != '' & IDDD == '' & Unit1 != '', NewVar := IDQual]
    VNCdt <- VNCdt[NonIDQual != '' & IDDD == '' & Unit1 != '', NewVar := NonIDQual]
    for (Cal in Cals) {
        VNCdt <- copy(VNCdt)[, NewVar := pasteNA(NewVar, get(Cal))]
    }
    
    EquivalName <- names(out.SP)
    names(EquivalName) <- unlist(lapply(EquivalName, function(x) {
                                            VNCdt[VNCdt[['Unit1']] == x, NewVar]}
                                        )
                                 )
    setnames(out.SP, EquivalName, names(EquivalName))
    
    if (DDslot == 'ID') {DDdt <- getID(DD)
    
    }else if (DDslot == 'MicroData') {DDdt <- getData(DD)
    
    }else if (DDslot == 'ParaData') {DDdt <- getParaData(DD)
    
    }else if (DDslot == 'Aggregates') {DDdt <- getAggr(DD)
    
    }else if (DDslot == 'AggWeights') {DDdt <- getAggWeights(DD)
    
    }else {DDdt <- getOtherDD(DD)
    
    }  
    
    if (nrow(DDdt) == 0) {
        stop(paste0('[Validity ReadSASWeigh]Data to be read are not defined in the slot "', DDslot, '" of the DD input object.'))
    }
    

    
    DDdtVarNames <- unlist(lapply(as.list(names(out.SP)), ExtractNames))
    names(DDdtVarNames) <- names(out.SP)
    
    for (Var in names(out.SP)) {
        
        out.SP[, Var := as(get(Var), DDdt[Variable == DDdtVarNames[Var], Class]), with = F]
        
    }
    return(out.SP)
    
}
