#' @title Read a SAS file renaming variables as specified in input parameters
#' 
#' @description \code{ReadSASFile} returns a \linkS4class{data.table} with all data from the input 
#' SAS file.
#' 
#' This function reads the SAS file specified as input parameter \code{SASFileName} and returns its 
#' content in a \linkS4class{data.table} with statistical units in rows and variables in columns. 
#' The names of the variables are assigned according to the Excel file with variable names 
#' correspondence for each statistical operation.
#' 
#' This correspondence is specified through the input param \code{DD}, which is a \linkS4class{DD} 
#' object with the content of the DD file containing the definition and properties of every 
#' variable. 
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
#' @return \linkS4class{data.table} with the contents of the \code{DD} file, with statistical units 
#' in rows and variables as columns.
#' 
#' @examples
#' \dontrun{
#' # We assume that the SAS file \code{MM032014.sas7bdat} is in the 
#' #administrator desktop:
#' SASName <- 'C:/Users/Administrator/Desktop/MM032014.sas7bdat'
#' # We assume data created previosly:
#' data(ExampleDD)
#' Example.DM <- ReadSASFile(SASName, ExampleDD)
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
ReadSASFile <- function(SASFileName, DD){
    
    out.SP <- haven::read_sas(SASFileName)
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
