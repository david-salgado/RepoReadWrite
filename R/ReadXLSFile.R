#' @title Read a XLS file renaming variables as specified in input parameters
#' 
#' @description \code{ReadXLSFile} returns a \linkS4class{data.table} with all data from the input 
#' XLS file.
#' 
#' This function reads the XLS file specified as input parameter \code{XLSFileName} and returns its 
#' content in a \linkS4class{data.table} with statistical units in rows and variables in columns. 
#' The names of the variables are assigned according to the Excel file with variable names 
#' correspondence for each statistical operation.
#' 
#' This correspondence is specified through the input param \code{DD}, which is a \linkS4class{DD} 
#' object with the content of the DD file containing the definition and properties of every 
#' variable. 
#' 
#' The object \code{DD} is naturally obtained from the original \code{DD} file as output of 
#' functions \code{\link{RepoDDToDD}}}. 
#'  
#' @param XLSFileName Character vector of length 1 with name of the file. The file will be read from 
#' the working directory (see \link[base]{getwd}) unless the full path is specified.
#' 
#' @param DD Object of class \linkS4class{DD} with the content of the file \code{DD} of definitions 
#' and properties of every variable.
#' 
#' @return \linkS4class{data.table} with the contents of the \code{DD} file, with statistical units 
#' in rows and variables as columns.
#' 
#' @examples
#' \dontrun{
#' # We assume that the SAS file \code{mm201604.xlsx} is in the 
#' #administrator desktop:
#' XLSFileName <- 'C:/Users/Administrador/Desktop/mm201604.xlsx'
#' # We assume data created previosly:
#' data(ExampleDD)
#' Example.DM <- ReadXLSFile(XLSFileName, ExampleDD)
#' }
#' 
#' @seealso  \link{RepoDDToDD}, \link{ReadRepoFile}, \link{WriteRepoFile}
#' 
#' @importFrom xlsx read.xlsx2
#' 
#' @importFrom gdata trim
#' 
#' @import data.table
#' 
#' @export
ReadXLSFile <- function(XLSFileName, DD){
    
    VNC <- Reduce('+', getVNC(DD))
    
    wb <- loadWorkbook(XLSFileName)
    SheetName <- names(getSheets(wb))
    out.SP <- read.xlsx2(XLSFileName, sheetName = SheetName, stringsAsFactors = FALSE)
    ColClasses <- unlist(lapply(out.SP, class))
    setDT(out.SP)
    
    for (col in names(out.SP)) {
        
        out.SP[, col := gdata::trim(get(col)), with = FALSE]
        
    }
    
    
    CalID <- getIDQual(VNC)
    CalID <- intersect(names(VNC), CalID)
    
    for (cal in CalID) {
        
        VNC[get(cal) == '.', cal := '', with = F]
        
    }
    
    
    CalNoID <- getNonIDQual(VNC)
    CalNoID <- intersect(names(VNC), CalNoID)
    
    
    Cals <- union(CalID, CalNoID)
    VarSP <- VNC$Unit1
    VarSP <- VarSP[!is.na(VarSP) & VarSP != ""]
    MissVar <- setdiff(names(out.SP), VarSP)
    
    if (length(MissVar) > 0) stop(paste0('[RepoReadWrite::ReadXLSFile] The following variables in the XLS file are not present in the Excel sheet:\n\n', 
                                         paste0(MissVar, collapse = ' '), '\n\n'))
    #VarSP <- intersect(VarSP, names(out.SP))
    #out.SP <- out.SP[, VarSP, with = F]
    
    VNC <- VNC[IDDD == "" & IDQual != "", IDDD := IDQual]
    VNC <- VNC[IDDD == "" & NonIDQual != "", IDDD := NonIDQual]
    
    pasteNA <- function(x, y){
        out <- ifelse(is.na(y) | y == '', paste0(x, ''), paste(x, y, sep = "_"))
        return(out)
    }
    
    VNC <- copy(VNC)[, NewVar := IDDD]
    VNC <- VNC[IDQual != '' & IDDD == '' & Unit1 != '', NewVar := IDQual]
    VNC <- VNC[NonIDQual != '' & IDDD == '' & Unit1 != '', NewVar := NonIDQual]
    
    for (Cal in Cals) {
        VNC <- copy(VNC)[, NewVar := pasteNA(NewVar, get(Cal))]
    }
    
    EquivalName <- names(out.SP)
    names(EquivalName) <- unlist(lapply(EquivalName, function(x) {VNC[VNC[['Unit1']] == x, NewVar]}))
    setnames(out.SP, EquivalName, names(EquivalName))
    
    
    slots <- setdiff(slotNames(DD), 'VarNameCorresp')
    DDdt <- new(Class = 'DDdt')
    for (slot in slots) {DDdt <- DDdt + slot(DD, slot) }
    
    DDVarNames <- unlist(lapply(as.list(names(out.SP)), ExtractNames))
    NoDDdt <- setdiff(DDVarNames, DDdt[['Variable']])
    if (length(NoDDdt) > 0) {stop('[RepoReadWrite::ReadXLSFile] The following variables in the XLS file are not present in the DD file:\n\n', 
                                  paste0(NoDDdt, collapse = ' '), '\n\n')
        
    } else {
        
        names(DDVarNames) <- names(out.SP)
        
    }
    
    for (Var in names(out.SP)) {
        
        out.SP[, Var := as(get(Var), DDdt[Variable == DDVarNames[[Var]], Class]), with = F]
    }
    return(out.SP)
    
}
