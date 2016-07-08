#' @title Validate xls file to create DD
#' 
#' @description This function validate the xls file it will be used to create the XLM file DD.
#' 
#' \code{ValidateXLS} checks the xls file to be consistent to create the XML file DD. 
#' 
#' @param ExcelName Character vector of length 1 with the name of the file to read.The file will be 
#' read from the working directory (see \link[base]{getwd}) unless the full path is specified.
#' 
#' @return NULL
#' 
#' @examples
#' \dontrun{
#' ExcelName <- 'N:/UDMTD/UDTMDCOM/DepSel.Repositorio/E30163/E30163.NombresVariables.xlsx'
#' ExcelName <- 'N:/UDMTD/UDMTD04/Repositorio/E30163.NombresVariables.xlsx'
#' }
#' 
#' @import data.table xlsx
#'       
#' @export
ValidateXLS <- function(ExcelName){
    
    wb <- loadWorkbook(ExcelName)
    SheetNames <- names(getSheets(wb))
    
    NonValSheet <- setdiff(SheetNames, c('VarSpec', 'ID', 'MicroData', 'ParaData'))
    
    if (length(NonValSheet) > 0) {
        stop('[ValidateXLS] Excel file only must contain sheets named "VarSpec", "ID", "MicroData", "ParaData".')
    }
    
    ValSheet <- setdiff(c('VarSpec', 'ID', 'MicroData', 'ParaData'), SheetNames)
    
    if (length(ValSheet) > 0) {
        stop('[ValidateXLS] The following sheets must be in the Excel file:', ValSheet)
    }
    
 
    ExcelSheet <- list()
    for (sName in SheetNames) {
        
        ExcelSheet[[sName]] <- read.xlsx(ExcelName, sheetName = sName, 
                                         stringsAsFactors = F,
                                         encoding = 'UTF-8')
        OrigOrder <- dimnames(ExcelSheet[[sName]])[1][[1]]
        #ExcelSheet[[sName]][ExcelSheet[[sName]] == '.'] <- ''
        ExcelSheet[[sName]] <- as.data.table(ExcelSheet[[sName]])
        ExcelSheet[[sName]][, OrigOrder := as.integer(OrigOrder)]
        ExcelSheet[[sName]] <- ExcelSheet[[sName]][order(rank(OrigOrder)),]
        ExcelSheet[[sName]][, OrigOrder := NULL]
        
    }
    
    Name <- ExcelSheet[['VarSpec']][['Name']]
    DupName <-  Name[duplicated(Name)]
    if (length(DupName) > 0) {
        stop(paste0('[ValidateXLS] The following variables in sheet "VarSpec" are duplicated: ', DupName))
    }
    
    TypeVal <- ExcelSheet[['VarSpec']][['Type']]
    names(TypeVal) <- Name
    if (!all(TypeVal %in% c('STRING', 'NUMBER'))) {
        stop('[ValidateXLS] Column "Type" of sheet "VarSpec" must have the value "STRING" or "NUMBER".')
    }
    
    LengthVal <- ExcelSheet[['VarSpec']][['Length']]
    names(LengthVal) <- Name
    if (any(is.na(LengthVal) | LengthVal <= 0)) {
        stop('[ValidateXLS] Column "Length" of sheet "VarSpec" must be a positive integer.')
    }
    

    SheetNames <- setdiff(SheetNames, 'VarSpec')
    IDQualTot <- c()
    NonIDQualTot <- c()
    IDDDTot <- c()
    for (sName in SheetNames) {
        
        IDQual <- ExcelSheet[[sName]][['IDQual']]
        IDQual <- IDQual[!is.na(IDQual)]
        NonIDQual <- ExcelSheet[[sName]][['NonIDQual']]
        NonIDQual <- NonIDQual[!is.na(NonIDQual)]
        IDDD <- ExcelSheet[[sName]][['IDDD']]
        IDDD <- IDDD[!is.na(IDDD)]
        
        IDQualTot <- unique(c(IDQualTot, IDQual))
        NonIDQualTot <- unique(c(NonIDQualTot, NonIDQual))
        IDDDTot <- unique(c(IDDDTot, IDDD))
        
    #Validations
        
        #Duplicados
        DupIDQual <-  IDQual[duplicated(IDQual)]
        DupNonIDQual <-  NonIDQual[duplicated(NonIDQual)]
        
        if (length(DupIDQual) > 0) {
            stop(paste0('[ValidateXLS] There are duplicated IDQual variables in sheet "', sName, '": ', DupIDQual)) 
        }
        
        if (length(DupNonIDQual) > 0) {
            stop(paste0('[ValidateXLS] There are duplicated NonIDQual variables in sheet "', sName, '": ', DupNonIDQual)) 
        }
        
        #Columnas de calificadores
        colNames <- names(ExcelSheet[[sName]])
        
        difcolIDQual <- setdiff(IDQual, colNames)
        if (length(difcolIDQual) > 0) {
            stop(paste0('[ValidateXLS] There must be a column in sheet "', sName, '" with the following IDQual variables": ', difcolIDQual)) 
        }
        
        difcolNonIDQual <- setdiff(NonIDQual, colNames)
        if (length(difcolNonIDQual) > 0) {
            stop(paste0('[ValidateXLS] There must be a column in sheet "', sName, '" with the following NonIDQual variables": ', difcolNonIDQual)) 
        }
        
        #Coherencia con VarSpec
        difIDQual <- setdiff(IDQual, Name)
        if (length(difIDQual) > 0) {
            stop(paste0('[ValidateXLS] The following variables in sheet "', sName, '" are not in "VarSpec": ', difIDQual)) 
        }
        
        
        difNonIDQual <- setdiff(NonIDQual, Name)
        if (length(difIDQual) > 0) {
            stop(paste0('[ValidateXLS] The following variables in sheet "', sName, '" are not in "VarSpec": ', difNonIDQual)) 
        }
        

        difIDDD <- setdiff(IDDD, Name)
        if (length(difIDQual) > 0) {
            stop(paste0('[ValidateXLS] The following variables in sheet "', sName, '" are not in "VarSpec": ', difIDDD)) 
        }
        
        
        #Longitudes
        AuxExcel <- ExcelSheet[[sName]][!is.na(IDDD),NonIDQual, with = F]
        AuxExcel[is.na(AuxExcel)] <- ''
        aux <- apply(AuxExcel, 1 , FUN = nchar)
        aux <- apply(aux, 1, FUN = max)
        for (Col in NonIDQual) {
            if (aux[Col] > LengthVal[Col]) {
                stop('[ValidateXLS] There are inconsistent values lengths in column "', Col, '" from sheet "', sName,'".')
            }
        }
    }
    
    
    difName <- setdiff(Name, c(IDQualTot, NonIDQualTot, IDDDTot))
    if (length(difName) > 0) {
            stop('[ValidateXLS] The following variables in Excel sheet "VarSpec" are neither in "ID", "MicroData" nor "ParaData": ', 
                                toString(difName))
    }
    return(cat('Fichero Validado.'))
}

