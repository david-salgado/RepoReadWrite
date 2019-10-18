#' @title Check compatibility of two xlsx files to create DDs
#' 
#' @description This function checks the validity of the xlsx file which will be used to create the
#'  data dictionary with XML format.
#' 
#' @param oldVersion Character vector of length 1 with the name of the file with the old version of 
#' the data dictionary.
#' 
#' @param newVersion Character vector of length 1 with the name of the file with the new version of 
#' the data dictionary.
#' 
#' @param verbose \code{TRUE} or \code{FALSE} (default) to request verbose mode.
#' 
#' @details The input xlsx file is a naive user interface to generate a data dictionary in XML 
#' format with the specification of each single key-value pair in the dictionary. The function 
#' performs a sequence of checks over each sheet of the input xlsx file. These checks are necessary 
#' (though not sufficient) for the construction of the XML file with the function 
#' \code{\link{RepoXLSToDD}}.
#' 
#' @return \code{TRUE} or \code{FALSE}.
#' 
#' @examples
#' \dontrun{
#' ExcelName <- 'T:/E30163/E30163.NombresVariables_V1.xlsx'
#' is.validXLSX(ExcelName)
#' }
#' 
#' @include is.validXLSX.R
#' 
#' @import data.table
#' 
#' @importFrom StQ ExtractNames
#' 
#' @importFrom openxlsx getSheetNames
#'       
#' @export
is.compatibleXLSX <- function(oldVersion, newVersion, verbose = FALSE){
    
    
    if (!is.validXLSX(oldVersion)) {
        
        stop(paste0('[RepoReadWrite::is.compatibleXLSX] Excel file ', oldVersion, ' is not valid.\n'))
    }
    
    if (!is.validXLSX(newVersion)) {
        
        stop(paste0('[RepoReadWrite::is.compatibleXLSX] Excel file ', newVersion, ' is not valid.\n'))
    }
    
    # Global Variables
    varRootSheetNames <- c('ID', 'MicroData', 'ParaData', 'Aggregates', 'AggWeights', 'Other')
    
    SheetNames_old <- openxlsx::getSheetNames(oldVersion)
    varSheetNames_old <- c()
    for (sh in varRootSheetNames){
        
        tempSheetName <- SheetNames_old[grep(sh, SheetNames_old)]
        varSheetNames_old <- c(varSheetNames_old, tempSheetName)
    }
    
    SheetNames_new <- openxlsx::getSheetNames(oldVersion)
    varSheetNames_new <- c()
    for (sh in varRootSheetNames){
        
        tempSheetName <- SheetNames_new[grep(sh, SheetNames_new)]
        varSheetNames_new <- c(varSheetNames_new, tempSheetName)
    }
    
    if (!identical(varSheetNames_old, varSheetNames_new)) {
        
        stop(paste0('[RepoReadWrite::is.compatibleXLSX] Both files must have the same sheet for data (ID, MicroData, ParaData, etc.).\n'))
    }
    
    varSheetNames <- varSheetNames_new

    ExcelSheets_old.list <- list()
    for (sName in varSheetNames) {
        
        ExcelSheets_old.list[[sName]] <- openxlsx::read.xlsx(oldVersion, sheet = sName)
        ExcelSheets_old.list[[sName]] <- as.data.table(ExcelSheets_old.list[[sName]])
        for (col in names(ExcelSheets_old.list[[sName]])) {
            
            ExcelSheets_old.list[[sName]] <- ExcelSheets_old.list[[sName]][, (col) := as.character(get(col))]
            ExcelSheets_old.list[[sName]] <- ExcelSheets_old.list[[sName]][is.na(get(col)), (col) := '']
        }
        OrigOrder <- dimnames(ExcelSheets_old.list[[sName]])[1][[1]]
        ExcelSheets_old.list[[sName]][, OrigOrder := as.integer(OrigOrder)]
        ExcelSheets_old.list[[sName]] <- ExcelSheets_old.list[[sName]][order(rank(OrigOrder)),]
        ExcelSheets_old.list[[sName]][, OrigOrder := NULL]
        ExcelSheets_old.list[[sName]] <- ExcelSheets_old.list[[sName]][!apply(is.na(ExcelSheets_old.list[[sName]]) | ExcelSheets_old.list[[sName]] == "", 1, all),]
        
    }
    
    ExcelSheets_new.list <- list()
    for (sName in varSheetNames) {
        
        ExcelSheets_new.list[[sName]] <- openxlsx::read.xlsx(newVersion, sheet = sName)
        ExcelSheets_new.list[[sName]] <- as.data.table(ExcelSheets_new.list[[sName]])
        for (col in names(ExcelSheets_new.list[[sName]])) {
            
            ExcelSheets_new.list[[sName]] <- ExcelSheets_new.list[[sName]][, (col) := as.character(get(col))]
            ExcelSheets_new.list[[sName]] <- ExcelSheets_new.list[[sName]][is.na(get(col)), (col) := '']
        }
        OrigOrder <- dimnames(ExcelSheets_new.list[[sName]])[1][[1]]
        ExcelSheets_new.list[[sName]][, OrigOrder := as.integer(OrigOrder)]
        ExcelSheets_new.list[[sName]] <- ExcelSheets_new.list[[sName]][order(rank(OrigOrder)),]
        ExcelSheets_new.list[[sName]][, OrigOrder := NULL]
        ExcelSheets_new.list[[sName]] <- ExcelSheets_new.list[[sName]][!apply(is.na(ExcelSheets_new.list[[sName]]) | ExcelSheets_new.list[[sName]] == "", 1, all),]
        
    }
    
    quals_old <- c()
    idQuals_old <- c()
    nonIDQuals_old <- c()
    quals_new <- c()
    idQuals_new <- c()
    nonIDQuals_new <- c()
    IDDD_old <- c()
    IDDD_new <- c()
    for (sName in varSheetNames) {
        
        IDQual_old <- ExcelSheets_old.list[[sName]][['IDQual']]
        IDQual_old <- IDQual_old[!is.na(IDQual_old) & IDQual_old != '']
        idQuals_old <- c(idQuals_old, IDQual_old)
        
        IDQual_new <- ExcelSheets_new.list[[sName]][['IDQual']]
        IDQual_new <- IDQual_new[!is.na(IDQual_new) & IDQual_new != '']
        idQuals_new <- c(idQuals_new, IDQual_new)
        
        
        NonIDQual_old <- ExcelSheets_old.list[[sName]][['NonIDQual']]
        NonIDQual_old <- NonIDQual_old[!is.na(NonIDQual_old) & NonIDQual_old != '']
        nonIDQuals_old <- c(nonIDQuals_old, NonIDQual_old)
        
        NonIDQual_new <- ExcelSheets_new.list[[sName]][['NonIDQual']]
        NonIDQual_new <- NonIDQual_new[!is.na(NonIDQual_new) & NonIDQual_new != '']
        nonIDQuals_new <- c(nonIDQuals_new, NonIDQual_new)
        
        IDDD_old <- ExcelSheets_old.list[[sName]][['IDDD']]
        IDDD_old <- NonIDQual_old[!is.na(NonIDQual_old) & NonIDQual_old != '']
        IDDD_old <- c(nonIDQuals_old, NonIDQual_old)
        
        IDDD_new <- ExcelSheets_new.list[[sName]][['IDDD']]
        IDDD_new <- NonIDQual_new[!is.na(NonIDQual_new) & NonIDQual_new != '']
        IDDD_new <- c(nonIDQuals_new, NonIDQual_new)
        
        quals_old <- c(quals_old, IDQual_old, NonIDQual_old)
        quals_new <- c(quals_new, IDQual_new, NonIDQual_new)
    }
    quals_old <- unique(quals_old)
    quals_new <- unique(quals_new)
    idQuals_old <- unique(idQuals_old)
    idQuals_new <- unique(idQuals_new)
    nonIDQuals_old <- unique(nonIDQuals_old)
    nonIDQuals_new <- unique(nonIDQuals_new)
    IDDD_old <- unique(IDDD_old)
    IDDD_new <- unique(IDDD_new)
    
    
    qualPerIDDD_old.list <- lapply(ExcelSheets_old.list, function(DT){
        
        colNames <- names(DT)
        qualCols.idx <- which(ExtractNames(colNames) %in% quals_old)
        qualCols <- colNames[qualCols.idx]
        colNames <- c('IDDD', qualCols)
        DT <- DT[, ..colNames][IDDD != '']
        tempDTlist <- split(DT, DT[['IDDD']])
        tempDTlist <- lapply(tempDTlist, function(IDDD.dt){
            
            indic <- colSums(as.matrix(IDDD.dt[, ..qualCols]) != '') > 0
            quals <- ExtractNames(names(indic)[indic])
        })
        return(tempDTlist)
    })
    
    qualPerIDDD_new.list <- lapply(ExcelSheets_new.list, function(DT){
        
        colNames <- names(DT)
        qualCols.idx <- which(ExtractNames(colNames) %in% quals_new)
        qualCols <- colNames[qualCols.idx]
        colNames <- c('IDDD', qualCols)
        DT <- DT[, ..colNames][IDDD != '']
        tempDTlist <- split(DT, DT[['IDDD']])
        tempDTlist <- lapply(tempDTlist, function(IDDD.dt){
            
            indic <- colSums(as.matrix(IDDD.dt[, ..qualCols]) != '') > 0
            quals <- ExtractNames(names(indic)[indic])
        })
        return(tempDTlist)
    })
    if (verbose) cat('[RepoReadWrite::is.compatibleXLSX] Checking length compatibility in sheet ...\n')
    length_compatible <- lapply(varSheetNames, function(sName){
        
        lst_old <- qualPerIDDD_old.list[[sName]]
        lst_new <- qualPerIDDD_new.list[[sName]]
        commonIDDDs <- intersect(names(lst_old), names(lst_new))
        symDif_incomp <- sapply(commonIDDDs, function(iddd){
            
            ( length(setdiff(lst_old[[iddd]], lst_new[[iddd]])) > 0 )
#            ( length(setdiff(lst_old[[iddd]], lst_new[[iddd]])) > 0 ) &  
#            ( length(setdiff(lst_new[[iddd]], lst_old[[iddd]])) > 0 )
        })
        names(symDif_incomp) <- commonIDDDs
        return(!symDif_incomp)
    })
    names(length_compatible) <- varSheetNames
    
    for (sName in varSheetNames){
        
        if (verbose) cat(paste0('   ', sName, '...'))
        
        compatValue <- length_compatible[[sName]]
        IDDDs <- names(compatValue)
        incompatIDDD <- IDDDs[!compatValue]
        if (length(incompatIDDD) > 0) {
            
            stop(paste0('[RepoReadWrite::is.compatibleXLSX] The following IDDDs are incompatible: ',
                        paste0(incompatIDDD, collapse = ', ')))
        }
        if (verbose) cat(' ok.\n')
    }
    
    if (verbose) cat(paste0('\n                       BOTH EXCEL FILES ARE COMPATIBLE!\n\n'))
    return(TRUE)
    
}

