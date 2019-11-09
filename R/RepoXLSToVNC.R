#' @title Produce an object of class \code{\link[StQ]{VNC}} from an xls file
#' 
#' @description This function is a constructor for the class \code{\link[StQ]{VNC}} using the 
#' contents of a xls file.
#' 
#' \code{RepoXLSToVNC} transforms the content of a xls file into an object of class 
#' \code{\link[StQ]{VNC}}. 
#' 
#' This function internally builds a \link{list} whose components are 
#' \linkS4class{data.table} with a row per each variable and with the columns: \code{IDQual}, 
#' \code{NonIDQual}, \code{IDDD} and \code{Unit1}. 
#' These internal \linkS4class{data.table} are then used to initialize a 
#' \code{\link[StQ]{VNC}} object.
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
#' @param ExcelName Character vector of length 1 with the name of the file to read.The file will be 
#' read from the working directory (see \link[base]{getwd}) unless the full path is specified.
#' 
#' @param SheetNames Character vector with the names of the sheets in the file to read.
#' 
#' @return Object of class \code{\link[StQ]{VNC}}.
#' 
#' @examples
#' \dontrun{
#' # We assume that the xlsx file \code{ExampleXLS.NombresVariables.xlsx} with the appropriate 
#' # structure is in the administrator desktop (change accordingly otherwise):
#' ExcelName <- 'T:/E30163/E30163.NombresVariables_V1.xlsx'
#' VNC <- RepoXLSToVNC(ExcelName)
#' show(VNC)
#' }
#' 
#' @import data.table openxlsx
#' 
#' @export
RepoXLSToVNC <- function(ExcelName, SheetNames){
    
    #if (!requireNamespace('openxlsx', quietly = TRUE)) stop('[RepoReadWrite::RepoXLSToVNC] Package openxlsx must be installed in the system.\n')
    
    if (missing(SheetNames)) {
        
        # wb <- loadWorkbook(ExcelName)
        # SheetNames <- setdiff(names(getSheets(wb)), 'VarSpec')
        SheetNames <- setdiff(openxlsx::getSheetNames(ExcelName), 'VarSpec')
        
    }
    ExcelSheet <- list()
    for (sName in SheetNames) {

        # ExcelSheet[[sName]] <- try(read.xlsx2(ExcelName, sheetName = sName, stringsAsFactors = FALSE))
        ExcelSheet[[sName]] <- try(openxlsx::read.xlsx(ExcelName, sheet = sName))
        if (inherits(ExcelSheet[[sName]], 'try-error')) {
            
            warning('\n[RepoReadWrite::RepoXLSToVNC] The package openxlsx will be loaded.\n')
            ExcelSheet[[sName]] <- openxlsx::read.xlsx(ExcelName, sheet = sName)
        }
        ExcelSheet[[sName]] <- as.data.table(ExcelSheet[[sName]])
        for (col in names(ExcelSheet[[sName]])) {
            
            ExcelSheet[[sName]] <- ExcelSheet[[sName]][, (col) := as.character(get(col))]
            ExcelSheet[[sName]] <- ExcelSheet[[sName]][is.na(get(col)), (col) := '']
        }
        OrigOrder <- dimnames(ExcelSheet[[sName]])[1][[1]]
        # ExcelSheet[[sName]] <- as.data.table(ExcelSheet[[sName]])
        ExcelSheet[[sName]][, OrigOrder := as.integer(OrigOrder)]
        ExcelSheet[[sName]] <- ExcelSheet[[sName]][order(rank(OrigOrder)),]
        ExcelSheet[[sName]][, OrigOrder := NULL]
        names.Data <- strsplit(names(ExcelSheet[[sName]]), '[_][Vv]')
        names.Data <- unlist(lapply(names.Data, function(x) {x[1]}))
        setnames(ExcelSheet[[sName]], names(ExcelSheet[[sName]]), names.Data)
    }

    ExcelSheet <- lapply(ExcelSheet, function(SheetDT){
            
        ColNames <- names(SheetDT)
        setnames(SheetDT, ColNames[1:3], c('IDQual', 'NonIDQual', 'IDDD'))
            
        IDQual <- SheetDT[['IDQual']]
        numIDQual <- sum(IDQual != '')
        NonIDQual <- SheetDT[['NonIDQual']]
        numNonIDQual <- sum(NonIDQual != '')
        SheetDT <- SheetDT[, names(SheetDT)[1:(5 + numIDQual + numNonIDQual)], with = F]
        # for (col in names(SheetDT)){
        # 
        #     SheetDT <- SheetDT[, (col) := as.character(get(col))]
        #     SheetDT <- SheetDT[is.na(get(col)), (col) := '']
        # 
        # }
        return(SheetDT)
    })

    VNC <- StQ::BuildVNC(ExcelSheet)
    
    return(VNC)
    
}

