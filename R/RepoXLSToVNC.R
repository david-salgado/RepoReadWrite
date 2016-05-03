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
#' # We assume that the xlsx file \code{NombresVariables.E30053.xlsx} with the
#' # appropriate structure
#' library(data.table)   
#' ExcelName <- 'S:/E30183/E30183.NombresVariables.xlsx'
#' SheetNames <- c('MicroData')  
#' VNC <- RepoXLSToVNC(ExcelName, SheetNames)
#' 
#' @import data.table xlsx StQ
#'       
#' @export
RepoXLSToVNC <- function(ExcelName, SheetNames){
    
    if (missing(SheetNames)) {
        
        wb <- loadWorkbook(ExcelName)
        SheetNames <- names(getSheets(wb))
        
    }
    ExcelSheet <- list()
    for (sName in SheetNames) {
        
        ExcelSheet[[sName]] <- read.xlsx(ExcelName, sheetName = sName, 
                                         stringsAsFactors = F,
                                         encoding = 'UTF-8', na = '')
        OrigOrder <- dimnames(ExcelSheet[[sName]])[1][[1]]
        #ExcelSheet[[sName]][ExcelSheet[[sName]] == '.'] <- ''
        ExcelSheet[[sName]] <- as.data.table(ExcelSheet[[sName]])
        ExcelSheet[[sName]][, OrigOrder := as.integer(OrigOrder)]
        ExcelSheet[[sName]] <- ExcelSheet[[sName]][order(rank(OrigOrder)),]
        ExcelSheet[[sName]][, OrigOrder := NULL]
    }

    ExcelSheet <- lapply(ExcelSheet, function(SheetDT){
            
        ColNames <- names(SheetDT)
        setnames(SheetDT, ColNames[1:3], c('IDQual', 'NonIDQual', 'IDDD'))
            
        IDQual <- SheetDT[['IDQual']]
        numIDQual <- sum(!is.na(IDQual))
        NonIDQual <- SheetDT[['NonIDQual']]
        numNonIDQual <- sum(!is.na(NonIDQual))
        Units <- ColNames[(4 + numIDQual + numNonIDQual):length(ColNames)]
        UnitsNames <- paste0('Unit', seq(along = Units))
        setnames(SheetDT, Units, UnitsNames)
        for (col in names(SheetDT)){
            
            SheetDT <- SheetDT[, col := as.character(get(col)), with = F]
            SheetDT <- SheetDT[is.na(get(col)), col := '', with = F]
            
        }
        return(SheetDT)
    })

    VNCdts <- lapply(ExcelSheet, new, Class = 'VNCdt')
    
    VNC <- new(Class = 'VarNameCorresp', VNCdts)
        
    return(VNC)
}
