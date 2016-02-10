#' @title Produce an object of class \linkS4class{VarNameCorresp} from a
#' xls file.
#' 
#' @description This function is a constructor for the class
#' \linkS4class{VarNameCorresp} using the contents of a xls file.
#' 
#' \code{RepoXLSToVNC} transforms the content of a xls file into an object of 
#' class \linkS4class{VarNameCorresp}. 
#' 
#' This function internally builds a \linkS4class{list} whose components are
#'  \linkS4class{data.table} with a row per each variable and with the columns: 
#'  \code{IDQual}, \code{NonIDQual}, \code{IDDD} and \code{Unit1}.
#' These internal \linkS4class{data.table} are then used to initialize a
#'  \linkS4class{VarNameCorresp} object.
#' 
#' The column \code{IDQual} contains the names of unit qualifiers.
#' 
#' The column \code{NonIDQual} contains the names of variable names qualifiers.
#' 
#' The column \code{IDDD} contains the names of variables.
#' 
#' The column \code{Unit1} specifies the variable names used by production Unit1 
#' 
#' 
#' @param ExcelName Character vector of length 1 with the name of the file to 
#'  read.The file will be read from the working directory (see
#'  \link[base]{getwd}) unless the full path is specified.
#' 
#' @param SheetNames Character vector with the names of the sheets in the file
#'  to read.
#' 
#' @return Object of class \linkS4class{VarNameCorresp}.
#' 
#' @examples
#' # We assume that the xlsx file \code{NombresVariables.E30053.xlsx} whit the
#' # appropriate structure
#' library(data.table)   
#' ExcelName <- 'R:/MacrosSAS.Doc/NombresVariables.E30053.xlsx'
#' SheetNames <- c('Datos', paste0(c('poicn0.', 'poicn1.', 'poicn2m.','ponca.',
#'                  'pond_divi.', 'ponderas.'),substring(MesInicial,5,8)))
#'  
#' VNC <- RepoXLSToVNC(ExcelName, SheetNames)
#' 
#' @import data.table xlsx
#'       
#' @export
    RepoXLSToVNC <- function(ExcelName, SheetNames){
        
        ExcelSheet <- list()
        for (sName in SheetNames) {
            
            ExcelSheet[[sName]] <- as.data.table(read.xlsx2(ExcelName, sheetName = sName, stringsAsFactors = F))
        }
        
        lapply(ExcelSheet, function(SheetName){
            
            ColNames <- names(SheetName)
            setnames(SheetName, ColNames[1:3], c('IDQual', 'NonIDQual', 'IDDD'))
            
            IDQual <- SheetName[['IDQual']]
            numIDQual <- length(IDQual[IDQual != ""])
            NonIDQual <- SheetName[['NonIDQual']]
            numNonIDQual <- length(NonIDQual[NonIDQual!=""])
            
            Units <- ColNames[(4+numIDQual+numNonIDQual):length(ColNames)]
            UnitsNames <- paste0('Unit', seq(along = Units))
            setnames(SheetName, Units, UnitsNames)
        })
        
        VNC <- new(Class = 'VarNameCorresp', VarNameCorresp = ExcelSheet)
        
        return(VNC)
    }
