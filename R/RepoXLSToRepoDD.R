#' @title Produce a file DD from an xlsx file
#' 
#' @description This function builds and writes a DD file  using the contents of an xlsx file.
#' \code{RepoXLSToRepoDD} transforms the content of an xlsx file into a DD file.  
#' 
#' @param ExcelName Character vector of length 1 with the name of the file to read.The file will be 
#' read from the working directory (see \link[base]{getwd}) unless the full path is specified.
#' 
#' @return Return invisible NULL.
#' 
#' @examples
#' # We assume that the xlsx file ExampleXLS.NombresVariables.xlsx with the appropriate structure is
#' # in the working directory (change accordingly otherwise):
#' 
#' \dontrun{
#' ExcelName <- 'T:/E30163/E30163.NombresVariables_V1.xlsx'
#' RepoXLSToRepoDD(ExcelName)
#' }
#' 
#' @import data.table XML
#' 
#'       
#' @export
RepoXLSToRepoDD <- function(ExcelName){
    
    #if (!requireNamespace('openxlsx', quietly = TRUE)) stop('[RepoReadWrite::RepoXLSToRepoDD] Package openxlsx must be installed in the system.\n')
        
    # Read the contents of the xlsx file 
    StrSplExcelName <- gsub('\\\\', '/', ExcelName)
    StrSplExcelName <- strsplit(StrSplExcelName, split = '/', fixed = TRUE)[[1]]
    StrSplExcelName <- StrSplExcelName[[length(StrSplExcelName)]]
    StrSplExcelName <- strsplit(StrSplExcelName, split = '.', fixed = TRUE)[[1]]
    SurveyCode <- StrSplExcelName[1]
    Version <- strsplit(StrSplExcelName[2], split = '[_][Vv]')[[1]][2]
    
    # VarSpec <- xlsx::read.xlsx2(ExcelName, sheetName = 'VarSpec', stringsAsFactors = FALSE)
    VarSpec <- as.data.table(openxlsx::read.xlsx(ExcelName, sheet = 'VarSpec'))
    for (col in names(VarSpec)) {
        
        VarSpec <- VarSpec[, (col) := as.character(get(col))]
        VarSpec <- VarSpec[is.na(get(col)), (col) := '']
    }
    
    # VarSpec <- as.data.table(VarSpec)
    # wb <- xlsx::loadWorkbook(ExcelName)
    # SheetNames <- names(xlsx::getSheets(wb))
    # SheetNames <- SheetNames[SheetNames != 'VarSpec']
    SheetNames <- setdiff(openxlsx::getSheetNames(ExcelName), 'VarSpec')
    Data.list <- list()
    for (sheet in SheetNames){
        
        # Data.list[[sheet]] <- as.data.table(read.xlsx2(ExcelName, sheetName = sheet, stringsAsFactors = FALSE))
        Data.list[[sheet]] <- openxlsx::read.xlsx(ExcelName, sheet = sheet)
        Data.list[[sheet]] <- as.data.table(Data.list[[sheet]])
        for (col in names(Data.list[[sheet]])) {
            
            Data.list[[sheet]] <- Data.list[[sheet]][, (col) := as.character(get(col))]
            Data.list[[sheet]] <- Data.list[[sheet]][is.na(get(col)), (col) := '']
        }
        names.Data <- strsplit(names(Data.list[[sheet]]), '[_][Vv]')
        names.Data <- unlist(lapply(names.Data, function(x) {x[1]}))
        setnames(Data.list[[sheet]], names(Data.list[[sheet]]), names.Data)
        if ('function.' %in% names(Data.list[[sheet]])) setnames(Data.list[[sheet]], 'function.', 'function')
    }
    
    Data.list.tot <- rbindlist(Data.list, fill = TRUE)
    
    
    # Check integrity of the contents of the xlsx file
    
    # Assign order to each qualifier
    Order.list <- lapply(names(Data.list), function(sheetName){
        
        sheet <- Data.list[[sheetName]]
        IDQual<- sheet[['IDQual']]
        IDQual <- unique(IDQual[IDQual != ''])
        NonIDQual <- sheet[['NonIDQual']]
        NonIDQual <- unique(NonIDQual[NonIDQual != ''])
        Qual <- c(IDQual, NonIDQual)
        Order <- data.table(Name = Qual, Order = seq(along = Qual))
        setnames(Order, 'Order', sheetName)
        return(Order)
    })
    
    Order <- Reduce(function(x, y){
        
        out <- merge(x, y, by = 'Name', all = TRUE)
        return(out)
        
    }, Order.list)
       
    Data.list <- lapply(names(Data.list), function(sheetName){
        
        Data <- Data.list[[sheetName]]
        Data <- Data[(IDQual != '' & !is.na(IDQual)) | (NonIDQual != '' & !is.na(NonIDQual)) | (IDDD != '' & !is.na(IDDD))]
        Data[, Name := ifelse(IDQual != '', IDQual, ifelse(NonIDQual != '', NonIDQual, IDDD))]
        IDQual <- Data[['IDQual']]
        IDQual <- unique(IDQual[IDQual != ''])
        NonIDQual <- Data[['NonIDQual']]
        NonIDQual <- unique(NonIDQual[NonIDQual != ''])
        Data[, QualType := ifelse(Name %in% IDQual, 'I', ifelse(Name %in% NonIDQual, 'Q', 'V'))]
        Data <- merge(Data, VarSpec, by = 'Name', all.x = TRUE)
        setcolorder(Data, 
                    c(setdiff(names(Data), 
                              c('InFiles', 'table_column', 'filter', 'function', 'ValueRegExp', 
                                'ValueDescription')), 'InFiles', 'table_column', 'filter', 'function', 
                                'ValueRegExp', 'ValueDescription'))
        return(Data)
    })
    names(Data.list) <- setdiff(names(Order), 'Name')
    Data.DT <- rbindlist(Data.list, fill = TRUE)
    Data.DT.QualType <- split(Data.DT, Data.DT[['QualType']])
    
    # Construct the DD file with the agreed schema 
    DD <- newXMLNode(name = 'DD', attrs = c(SurveyCode = SurveyCode, version = Version))
   
    # Node identifiers
    identifiers <- newXMLNode(name = 'identifiers', parent = DD)
    
    # We build the children nodes of identifiers as a list
    identifiers.list.QualType <- lapply(names(Data.DT.QualType), function(QualType){
        
        identifiers.list <- list()
        
        # For qualifiers
        if (QualType %in% c('Q', 'I')){
            
            Data <- Data.DT.QualType[[QualType]]
            Data <- Data[, c('Name', 'QualType', 'Type', 'Length', 'InFiles', 'MetadataCode', 'ValueDescription', 'ValueRegExp'), with = F]
            setkeyv(Data, 'Name')
            Data <- Data[!duplicated(Data, by = key(Data))]
            for (VarName in Data[['Name']]){
                identifiers.list[[VarName]] <- newXMLNode('identifier', 
                                                          attrs = c(identifierType = Data[Name == VarName, 
                                                                                          QualType]))
                # Node name
                newXMLNode(name = 'name', VarName, parent = identifiers.list[[VarName]])
                
                # Node description
                newXMLNode(name = 'description', parent = identifiers.list[[VarName]],
                           .children = c(newXMLNode('MetadataCode', Data[Name == VarName, MetadataCode])))
                
                # Node VarType
                newXMLNode(name = 'varType', Data[Name == VarName, Type], 
                           parent = identifiers.list[[VarName]])
                
                # Node Length
                newXMLNode(name = 'Length', Data[Name == VarName, Length], 
                           parent = identifiers.list[[VarName]])
                
                # Node InFiles
                newXMLNode(name = 'InFiles', Data[Name == VarName, InFiles], 
                           parent = identifiers.list[[VarName]])
                
                # Node UnitNames
                UnitNames <- newXMLNode(name = 'UnitNames', parent = identifiers.list[[VarName]])
                IDQualValue <- Data.DT.QualType[[QualType]][Name == VarName, 'UnitName', with = FALSE]
                IDQualValue <- IDQualValue[!duplicated(IDQualValue, by = key(IDQualValue))]
                IDQuals.list <- lapply(IDQualValue, function(IDQual){
                    
                    out <- newXMLNode(name = 'UnitName', IDQual)
                    
                })
                addChildren(UnitNames, IDQuals.list)
                
                # Node values
                newXMLNode(name = 'values', parent = identifiers.list[[VarName]],
                           .children = c(newXMLNode('description', Data[Name == VarName, ValueDescription]),
                                         newXMLNode('value', Data[Name == VarName, ValueRegExp])))
            }
        }
        
        # For variables
        if (QualType == 'V'){
            
            Data <- Data.DT.QualType[[QualType]]
            colData <- setdiff(names(Data), 
                               c('Name', 'VarDescription', 'QualType', 'Type', 'Length',
                                 'InFiles', 'MetadataCode', 'table_column', 'filter', 
                                 'function', 'ValueRegExp', 'ValueDescription'))
            for (col in colData){
                
                Data[, (col) := ifelse(get(col) == '' | is.na(get(col)), 0, 1)]
                
            }
            
            Data[,(colData) := lapply(.SD, sum), .SDcols = colData, by = 'Name']
            setkeyv(Data, 'Name')
            Data <- Data[!duplicated(Data, by = key(Data))]
            for (col in colData){
                
                Data[, (col) := ifelse(get(col) == 0, 0, 1)]
                
            }
            
            for (VarName in Data[['Name']]){
                identifiers.list[[VarName]] <- newXMLNode('identifier', 
                                                          attrs = c(identifierType = Data[Name == VarName, 
                                                                                          QualType]))
                # Node name
                newXMLNode(name = 'name', VarName, parent = identifiers.list[[VarName]])
                
                # Node description
                newXMLNode(name = 'description', parent = identifiers.list[[VarName]],
                           .children = c(newXMLNode('MetadataCode', Data[Name == VarName, MetadataCode])))
                
                # Node varType
                newXMLNode(name = 'varType', Data[Name == VarName, Type], 
                           parent = identifiers.list[[VarName]])
                
                # Node Length
                newXMLNode(name = 'Length', Data[Name == VarName, Length], 
                           parent = identifiers.list[[VarName]])
                # Node InFiles
                newXMLNode(name = 'InFiles', Data[Name == VarName, InFiles], 
                           parent = identifiers.list[[VarName]])
                
                # Node quals
                quals <- newXMLNode(name = 'quals', parent = identifiers.list[[VarName]])
                colDataQuals <- setdiff(names(Data),
                                        c('Name', 'QualType', 'Type', 'Length', 'MetadataCode',
                                          'table_column', 'filter', 'function', 'ValueRegExp', 'UnitName', 'InFiles',
                                          'ValueDescription', 'VarDescription', 'IDQual', 'NonIDQual', 'IDDD'))
                QualsVec <- as.logical(Data[Name == VarName, colDataQuals, with = FALSE])
                QualsVec2 <- colDataQuals[QualsVec]
                OrderQuals <- Order[Name %in% QualsVec2,]
                namesOrderQuals <- copy(names(OrderQuals))
                for (col in namesOrderQuals){
                    
                    if (any(is.na(OrderQuals[[col]]))) OrderQuals[, (col) := NULL]
                }

                OrderQuals <- OrderQuals[, 1:2 , with = FALSE]
                setkeyv(OrderQuals, names(OrderQuals)[2])
                OrderQuals[, Order := seq(along = QualsVec2)]
                OrderQuals[, c('Name', 'Order'), with = FALSE]
                quals.list <- lapply(OrderQuals[['Name']], function(qual){
                    
                    out <- newXMLNode(name = 'qual', qual, attrs = c(QualOrder = OrderQuals[Name == qual, Order]))
                    
                })
                addChildren(quals, quals.list)
                
                # Node UnitNames
                UnitNames <- newXMLNode(name = 'UnitNames', parent = identifiers.list[[VarName]])
                IDDDValue <- Data.DT[IDDD == VarName, 
                                     c(QualsVec2, 'UnitName', 'InFiles'), 
                                     with = FALSE]
                
                
                attrs.list.aux <- vector('list', dim(IDDDValue)[1])    
                
                for (i in seq(1, dim(IDDDValue)[1])){
                    
                    attrs <- c()
                    
                    for (name in setdiff(names(IDDDValue), 'UnitName')){
                        
                        attrs <- c(attrs, IDDDValue[i, ][[name]])
                    }       
                    attrs.list.aux[[i]] <- attrs
                    names(attrs.list.aux[[i]]) <- c(QualsVec2, 'InFiles')
                }
                names(attrs.list.aux) <- IDDDValue[['UnitName']]
                
                for(name in names(attrs.list.aux)){
                    
                    out1 <- newXMLNode('name', name)
                    
                    out2 <- newXMLNode('questionIria',
                                       .children = c(newXMLNode('table_column', Data.list.tot[UnitName == name, table_column]),
                                                     newXMLNode('filter', Data.list.tot[UnitName == name, filter]),
                                                     newXMLNode('function', Data.list.tot[UnitName == name][['function']])))
                    
                    newXMLNode(name = 'UnitName',
                               attrs = c(attrs.list.aux[[name]]),
                               parent = UnitNames,
                               .children = c(out1, out2))
                    
                }
                
                
                # Node values
                newXMLNode(name = 'values', parent = identifiers.list[[VarName]],
                           .children = c(newXMLNode('description', Data[Name == VarName, ValueDescription]),
                                         newXMLNode('value', Data[Name == VarName, ValueRegExp])))
            }
        }
        
        return(identifiers.list)
        
    })
    identifiers.list <- Reduce(c, identifiers.list.QualType)
    
    addChildren(identifiers, identifiers.list)
    
    # Save the DD object in a xml file  (DD file)
    ParsedExcelName <- strsplit(ExcelName, '/', fixed = TRUE)[[1]]
    ParsedExcelName <- lapply(ParsedExcelName, strsplit, split = '\\', fixed = TRUE)
    ParsedExcelName <- unlist(ParsedExcelName)
    FileName <- ParsedExcelName[length(ParsedExcelName)]
    RepoPath <- paste0(ParsedExcelName[-length(ParsedExcelName)], collapse = '/')
    DDname <- gsub('NombresVariables', 'DD', FileName)
    DDname <- strsplit(DDname, '.', fixed = TRUE)[[1]]
    DDname <- paste0(DDname[-length(DDname)], collapse = '.')
    outDDname <- paste0(c(RepoPath, DDname), collapse = '/')
    saveXML(doc = xmlDoc(DD), file = outDDname)
    cat(paste0('The DD file (xml file) ', DDname, ' has been generated and written in ', RepoPath, '/\n'))
    
    return(invisible(NULL))
    
}
