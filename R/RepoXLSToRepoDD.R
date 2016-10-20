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
#' \dontrun{
#' RepoDD <- RepoXLSToRepoDD('ExampleXLS', '1')
#' show(RepoDD)
#' }
#' 
#' @import data.table xlsx XML
#'       
#' @export
RepoXLSToRepoDD <- function(ExcelName){
    
    # Read the contents of the xlsx file 
    StrSplExcelName <- gsub('\\\\', '/', ExcelName)
    StrSplExcelName <- strsplit(StrSplExcelName, split = '/', fixed = TRUE)[[1]]
    StrSplExcelName <- StrSplExcelName[[length(StrSplExcelName)]]
    StrSplExcelName <- strsplit(StrSplExcelName, split = '.', fixed = TRUE)[[1]]
    SurveyCode <- StrSplExcelName[1]
    Version <- strsplit(StrSplExcelName[2], split = '[_][Vv]')[[1]][2]
    
    VarSpec <- read.xlsx2(ExcelName, sheetName = 'VarSpec', stringsAsFactors = FALSE)
    
    VarSpec <- as.data.table(VarSpec)
    wb <- loadWorkbook(ExcelName)
    SheetNames <- names(getSheets(wb))
    SheetNames <- SheetNames[SheetNames != 'VarSpec']
    Data.list <- list()
    for (sheet in SheetNames){
        
        Data.list[[sheet]] <- as.data.table(read.xlsx2(ExcelName, sheetName = sheet, stringsAsFactors = FALSE))
        if ('function.' %in% names(Data.list[[sheet]])) setnames(Data.list[[sheet]], 'function.', 'function')
    }
    
    Data.list.tot <- rbindlist(Data.list, fill = TRUE)
    names.Data <- strsplit(names(Data.list.tot), '[_][Vv]')
    names.Data <- unlist(lapply(names.Data, function(x) {x[1]}))
    setnames(Data.list.tot, names(Data.list.tot), names.Data)
    
    # Check integrity of the contents of the xlsx file
    
    # Assign order to each qualifier
    Order.list <- lapply(Data.list, function(sheet){
        
        IDQual<- sheet[['IDQual']]
        IDQual <- unique(IDQual[IDQual != ''])
        NonIDQual <- sheet[['NonIDQual']]
        NonIDQual <- unique(NonIDQual[NonIDQual != ''])
        Qual <- c(IDQual, NonIDQual)
        Order <- data.table(Name = Qual, Order = seq(along = Qual))
        return(Order)
    })
    
    Order <- Reduce(function(x, y){
        
        out <- merge(x, y, by = 'Name', all = TRUE)
        out[, Order := pmax(Order.x, Order.y, na.rm = TRUE)]
        out[, Order.x := NULL]
        out[, Order.y := NULL]
        setkeyv(out, 'Order')
        return(out)
        
    }, Order.list, Order.list[[1]])
        
    Order[, NewOrder := as.character(seq(along = Order))]
    Order[, Order := NULL]
    setnames(Order, 'NewOrder', 'Order')
    
    # Consolidate a unique data.table with all variables and their corresponding qualifiers
    # From this data.table the qualifier of each variable is identified
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

    Data <- merge(Data, Order, by = 'Name', all = TRUE)
   
    Data[is.na(Order), Order := '']
    Data[, c("IDQual", "NonIDQual", "IDDD", 'UnitName') := NULL, with = FALSE]
    colData <- setdiff(names(Data), c('Name', 'Order', 'QualType'))
    for (col in colData){
        
        Data[, col := ifelse(get(col) == '', 0, 1), with = FALSE]
        
    }
    
    Data[,(colData):=lapply(.SD, sum), .SDcols=colData, by = 'Name']
    setkeyv(Data, 'Name')
    Data <- Data[!duplicated(Data)]
    for (col in colData){
        
        Data[, col := ifelse(get(col) == 0, 0, 1), with = F]
        
    }
    
    Data <- merge(Data, VarSpec, by = 'Name', all = TRUE)
    setcolorder(Data, c(setdiff(names(Data), c('EnFicheros', 'table_column', 'filter', 'function', 'ValueRegExp', 'ValueDescription')), 'EnFicheros', 'table_column', 'filter', 'function', 'ValueRegExp', 'ValueDescription'))
 
    # Construct the DD file with the agreed schema 
    DD <- newXMLNode(name = 'DD', attrs = c(SurveyCode = SurveyCode, version = Version))

    # Node identifiers
    identifiers <- newXMLNode(name = 'identifiers', parent = DD)
    identifiers.list <- list()

    for(VarName in Data[['Name']]){

        identifiers.list[[VarName]] <- newXMLNode('identifier', 
                                                  attrs = c(identifierType = Data[Name == VarName, 
                                                                                  QualType]))
        newXMLNode(name = 'name', VarName, parent = identifiers.list[[VarName]])
        newXMLNode(name = 'description', parent = identifiers.list[[VarName]],
                   .children = c(newXMLNode('MetadataCode', Data[Name == VarName, MetadataCode])))
        newXMLNode(name = 'varType', Data[Name == VarName, Type], 
                   parent = identifiers.list[[VarName]])
        newXMLNode(name = 'Length', Data[Name == VarName, Length], 
                   parent = identifiers.list[[VarName]])
        UnitNames <- newXMLNode(name = 'UnitNames', parent = identifiers.list[[VarName]])

        if (Data[Name == VarName, QualType] == 'I'){
            
            IDQualValue <- Data.list.tot[IDQual == VarName, 'UnitName', with = FALSE]
            IDQualValue <- IDQualValue[!duplicated(IDQualValue)]
            
            IDQuals.list <- lapply(IDQualValue, function(IDQual){
                
                out <- newXMLNode(name = 'UnitName', IDQual)
                
            })
            
            addChildren(UnitNames, IDQuals.list)
        }

        if (Data[Name == VarName, QualType] == 'V'){
        
            quals <- newXMLNode(name = 'quals', parent = identifiers.list[[VarName]])
            colData <- setdiff(names(Data), 
                               c('Name', 'QualType', 'Order', 'Type', 'Length', 'MetadataCode', 'table_column', 'filter', 'function', 'ValueRegExp', 'ValueDescription', 'VarDescription'))

            QualsVec <- as.logical(Data[Name == VarName, colData, with = FALSE])
            QualsVec2 <- colData[QualsVec]

            OrderQuals <- Order[Name %in% QualsVec2,]
            orderqual <- intersect(QualsVec2, OrderQuals[['Name']])
            setkeyv(OrderQuals, 'Name')
            OrderQuals <- OrderQuals[orderqual]
            OrderQuals[, Order := seq(along = Order)]
            setkeyv(OrderQuals, 'Order')
            quals.list <- lapply(OrderQuals[['Name']], function(qual){
                
                out <- newXMLNode(name = 'qual', qual, attrs = c(QualOrder = OrderQuals[Name == qual, Order]))
                
            })
            addChildren(quals, quals.list)
            
            
            Data.list.UnitNames <- lapply(Data.list, function(DT){
                
                Names <- DT[['UnitName']]
                Names <- Names[Names != '']
                return(Names)
            })
            
            
            IDDDValue <- Data.list.tot[IDDD == VarName, c(QualsVec2, 'UnitName', 'EnFicheros'), with = FALSE]
            
            
            attrs.list.aux <- vector('list', dim(IDDDValue)[1])    

            for (i in seq(1, dim(IDDDValue)[1])){
                
                attrs <- c()
                
                for (name in setdiff(names(IDDDValue), 'UnitName')){
                    
                    attrs <- c(attrs, IDDDValue[i, ][[name]])
                }       
                attrs.list.aux[[i]] <- attrs
                names(attrs.list.aux[[i]]) <- QualsVec2
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
            
        }
        

        
        newXMLNode(name = 'values', parent = identifiers.list[[VarName]],
                   .children = c(newXMLNode('description', Data[Name == VarName, ValueDescription]),
                                 newXMLNode('value', Data[Name == VarName, ValueRegExp])))
    }
    addChildren(identifiers, identifiers.list)
    
   
    # Save the DD object in a xml file  (DD file)
    outName <- paste0(SurveyCode, '.DD_V', Version)
    saveXML(doc = xmlDoc(DD), file = outName)
    cat(paste0('The DD file (xml file) ', outName, ' has been generated and written in ', getwd(), '\n'))
    return(invisible(NULL))
    
}

