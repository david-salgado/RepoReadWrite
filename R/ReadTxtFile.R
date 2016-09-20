#' @title Read a txt file renaming variables as specified in input parameters
#' 
#' @description \code{ReadTxtFile} returns a \linkS4class{data.table} with all data from the input 
#' txt file.
#' 
#' This function reads the txt file specified as input parameter \code{TxtFileName} and returns its 
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
#' @param TxtFileName Character vector of length 1 with name of the file. The file will be read from 
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
#' # We assume that the txt file \code{tabla_b.txt} is in the 
#' #administrator desktop:
#' TxtName <- 'C:/Users/Administrator/Desktop/tabla_b.txt'
#' # We assume data created previosly:
#' data(ExampleDD)
#' Example.DM <- ReadTxtFile(TxtName, ExampleDD)
#' }
#' 
#' @seealso  \link{RepoDDToDD}, \link{ReadRepoFile}, \link{WriteRepoFile}
#' 
#' @importFrom gdata trim
#' 
#' @import data.table
#' 
#' @export
ReadTxtFile <- function(TxtFileName, DD){
  
  out.SP <- read.csv(TxtFileName, sep = '~')
  setDT(out.SP)
  
  for (col in names(out.SP)) {
    
    out.SP[, col := gdata::trim(get(col)), with = FALSE]
    
  }
  
  return(out.SP) 
}
