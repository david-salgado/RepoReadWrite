#' @title Read an XLS file renaming variables as specified in input parameters
#' 
#' @description \code{ReadXLSFile} returns a \linkS4class{data.table} with all data from the input 
#' XLS file.
#' 
#' @details This function reads the XLS file specified as input parameter \code{XLSFileName} and 
#' returns its content in a \linkS4class{data.table} with statistical units in rows and variables in
#'  columns. The names of the variables are assigned according to the Excel file with variable names 
#' correspondence for each statistical operation.
#' 
#' This correspondence is specified through the input param \code{DD}, which is a 
#' \code{DD} object with the content of the DD file containing the definition and 
#' properties of every variable. 
#' 
#' The object \code{DD} is naturally obtained from the original \code{DD} file as output of 
#' functions \code{\link{RepoDDToDD}}. 
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
#' # We assume that the SAS file \code{mm201604.xlsx} is in the administrator desktop:
#' XLSFileName <- 'C:/Users/Administrador/Desktop/mm201604.xlsx'
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
    
    wb <- loadWorkbook(XLSFileName)
    SheetName <- names(getSheets(wb))
    out.SP <- read.xlsx2(XLSFileName, sheetName = SheetName, stringsAsFactors = FALSE)
    ColClasses <- unlist(lapply(out.SP, class))
    setDT(out.SP)
    
    for (col in names(out.SP)) {
        
        out.SP[, col := gdata::trim(get(col)), with = FALSE]
        
    }
  
    VNC <- getVNC(DD)
    CalID <- getIDQual(VNC)
    CalNoID <- getNonIDQual(VNC)
    Cals <- unique(union(CalID, CalNoID))

    VNC <- lapply(VNC, function(VNCdt){
        
        for (cal in intersect(names(VNCdt), Cals)) {
            
            VNCdt[get(cal) == '.', cal := '', with = F]
            
        }    
        return(VNCdt)
    })

    VNC <- new(Class = 'VarNameCorresp', VNC)
    
    IDDDNames <- UnitToIDDDNames(VNC, names(out.SP))
    
    setnames(out.SP, IDDDNames[['UnitName']], IDDDNames[['IDDDName']])
    
    return(out.SP) 
    
}
