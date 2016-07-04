#' @title Produce a file DD from an xlsx file
#' 
#' @description This function builds and writes a DD file  using the contents of an xlsx file.
#' 
#' \code{RepoXLSToRepoDD} transforms the content of an xlsx file into a DD file.  
#' 
#' @param SurveyCode Character vector of length 1 with the code of the survey. The xlsx file will be 
#' read from the working directory (see \link[base]{getwd}) unless the full path is specified.
#' 
#' @param Version Character vector of length 1 with the version number of the file DD to produce.
#' 
#' @return NULL.
#' 
#' @examples
#' \dontrun{
#' # We assume that the xlsx file \code{E30163.NombresVariables.xlsx} with the appropriate structure
#' # is in the administrator desktop (change accordingly otherwise):
#' RepoXLSToRepoDD(SurveyCode, Version)
#' }
#' 
#' @import data.table xlsx XML
#'       
#' @export
RepoXLSToRepoDD <- function(SurveyCode, Version){
    
    ExcelName <- paste0(SurveyCode, '.NombresVariables.xlsx')
    
    VarSpec <- read.xlsx2(ExcelName, sheetName = 'VarSpec', stringsAsFactors = FALSE)
    return(VarSpec)
    
    VarSpec <- as.data.table(VarSpec)
    wb <- loadWorkbook(ExcelName)
    SheetNames <- names(getSheets(wb))
    SheetNames <- SheetNames[SheetNames != 'VarSpec']
    Data.list <- list()
    for (sheet in SheetNames){
        
        Data.list[[sheet]] <- as.data.table(read.xlsx2(ExcelName, sheetName = sheet, stringsAsFactors = FALSE))
    }
    Data <- rbindlist(Data.list, fill = TRUE)
    Data <- Data[(IDQual != '' & !is.na(IDQual)) | (NonIDQual != '' & !is.na(NonIDQual)) | (IDDD != '' & !is.na(IDDD))]
    colData <- names(Data)
    for (col in colData){
        
        Data[is.na(get(col)), col := '', with = F]
        
    }
    Data[, Name := ifelse(IDQual != '', IDQual, ifelse(NonIDQual != '', NonIDQual, IDDD))]
    IDQual<- Data[['IDQual']]
    IDQual <- unique(IDQual[IDQual != ''])
    NonIDQual <- Data[['NonIDQual']]
    NonIDQual <- unique(NonIDQual[NonIDQual != ''])
    Data[, QualType := ifelse(Name %in% IDQual, 'I', ifelse(Name %in% NonIDQual, 'Q', 'V'))]
    
    Qual <- c(IDQual, NonIDQual)
    names(Qual) <- seq(along = Qual)
    Order <- data.table(Name = Qual, Order = names(Qual))
    Data <- merge(Data, Order, by = 'Name', all = TRUE)
    Data[is.na(Order), Order := '']
    Data[, c("IDQual", "NonIDQual", "IDDD", names(Data)[grep('Unit', names(Data))]) := NULL, with = FALSE]
    colData <- setdiff(names(Data), c('Name', 'Order', 'QualType'))
    for (col in colData){
        
        Data[, col := ifelse(get(col) == '', 0, 1), with = FALSE]
        
    }
    
    Data[,(colData):=lapply(.SD, sum),.SDcols=colData, by = 'Name']
    setkeyv(Data, 'Name')
    Data <- Data[!duplicated(Data)]
    for (col in colData){
        
        Data[, col := ifelse(get(col) == 0, 0, 1), with = F]
        
    }
    
    Data <- merge(Data, VarSpec, by = 'Name', all = TRUE)
    
    
    DD <- newXMLNode(name = 'DD', attrs = c(SurveyCode = SurveyCode, version = Version))
    
    newXMLNode(name = 'sqlConnection', attrs = c(id = 'idsqlConnection'),
               .children = c(newXMLNode(name = 'jdbcConnection')))
    
    identifiers.list <- list()
    for(VarName in Data[['Name']]){
        
        identifiers.list[[VarName]] <- newXMLNode('identifier', attrs = c(identifierType = Data[Name == VarName, QualType]))
        newXMLNode(name = 'name', VarName, parent = identifiers.list[[VarName]])
        newXMLNode(name = 'description', Data[Name == VarName, Variable.Description], parent = identifiers.list[[VarName]])
        newXMLNode(name = 'metadataId', 'URL a base de datos de metadatos', parent = identifiers.list[[VarName]])
        newXMLNode(name = 'iriaQuestionsId', 'ID de la pregunta en IRIA', parent = identifiers.list[[VarName]])
        newXMLNode(name = 'order', Data[Name == VarName, Order], parent = identifiers.list[[VarName]])
        newXMLNode(name = 'varType', Data[Name == VarName, Type], parent = identifiers.list[[VarName]])
        newXMLNode(name = 'Length', Data[Name == VarName, Length], parent = identifiers.list[[VarName]])
        newXMLNode(name = 'values', parent = identifiers.list[[VarName]],
                   .children = c(newXMLNode('description', Data[Name == VarName, Value.Description]),
                                 newXMLNode('value', Data[Name == VarName, Value.RegExp])))
    }
    
    addChildren(DD, identifiers.list)
    
    saveXML(DD, paste0(SurveyCode, '.DD_V', Version))
    
    
}

