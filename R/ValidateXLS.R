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
#' ExcelName <- 'T:/E30163/E30163.NombresVariables_V1.xlsx'
#' ValidateXLS(ExcelName)
#' }
#' 
#' @import data.table openxlsx gdata
#' 
#' @importFrom StQ ExtractNames
#'       
#' @export
ValidateXLS <- function(ExcelName){
    
    if (!requireNamespace('openxlsx', quietly = TRUE)) stop('[RepoReadWrite::ValidateXLS] Package openxlsx must be installed in the system.\n')
    
    # wb <- loadWorkbook(ExcelName)
    # SheetNames <- names(getSheets(wb))
    SheetNames <- openxlsx::getSheetNames(ExcelName)
    
    
    cat('\n[RepoReadWrite::ValidateXLS] Minimal compulsory sheet names: VarSpec, ID, MicroData...')
    CompulsorySheets <- setdiff(c('VarSpec', 'ID', 'MicroData'), SheetNames)
    if (length(CompulsorySheets) > 0) {
        
        stop(paste0('[RepoReadWrite::ValidateXLS] The following sheets must be in the Excel file:', CompulsorySheets, '.\n'))
    }
    cat(' ok.\n')
    
    cat('\n[RepoReadWrite::ValidateXLS] Optional (possibly compound) sheet names: ParaData, Aggregates, AggWeights, Other...')
    OptionalSheets <- setdiff(SheetNames, c('VarSpec', 'ID', 'MicroData'))
    OptionalSheets <- unlist(lapply(OptionalSheets, function(Sheet){ strsplit(Sheet, '_')[[1]][[1]] }))
    OptionalValSheet <- setdiff(OptionalSheets, c('ParaData', 'Aggregates', 'AggWeights', 'Other'))
    if (length(OptionalValSheet) > 0) {
        
        stop(paste0('[RepoReadWrite::ValidateXLS] The following prefixes in the Excel sheet names are not valid:', OptionalValSheet, '.\n'))
    }
    cat(' ok.\n')
    
    ExcelSheets.list <- list()
    for (sName in SheetNames) {
        
        # ExcelSheets.list[[sName]] <- read.xlsx2(ExcelName, 
        #                                         sheetName = sName, 
        #                                         colClasses = 'character',
        #                                         stringsAsFactors = F)
        ExcelSheets.list[[sName]] <- openxlsx::read.xlsx(ExcelName, sheet = sName)
        ExcelSheets.list[[sName]] <- as.data.table(ExcelSheets.list[[sName]])
        for (col in names(ExcelSheets.list[[sName]])) {
            
            ExcelSheets.list[[sName]] <- ExcelSheets.list[[sName]][, (col) := as.character(get(col))]
            ExcelSheets.list[[sName]] <- ExcelSheets.list[[sName]][is.na(get(col)), (col) := '']
        }
        OrigOrder <- dimnames(ExcelSheets.list[[sName]])[1][[1]]
        # ExcelSheets.list[[sName]] <- as.data.table(ExcelSheets.list[[sName]])
        ExcelSheets.list[[sName]][, OrigOrder := as.integer(OrigOrder)]
        ExcelSheets.list[[sName]] <- ExcelSheets.list[[sName]][order(rank(OrigOrder)),]
        ExcelSheets.list[[sName]][, OrigOrder := NULL]
        if (dim(ExcelSheets.list[[sName]])[1] == 0) stop(paste0('[RepoReadWrite::ValidateXLS] The following sheet is empty: ', sName, '.\n'))
        ExcelSheets.list[[sName]] <- ExcelSheets.list[[sName]][!apply(is.na(ExcelSheets.list[[sName]]) | ExcelSheets.list[[sName]] == "", 1, all),]
        
    }
    
    cat('\n[RepoReadWrite::ValidateXLS] No duplicates in the sheet VarSpec...')
    Name <- ExcelSheets.list[['VarSpec']][['Name']]
    DupName <-  Name[duplicated(Name, by = key(Name))]
    if (length(DupName) > 0) {
        
        stop(paste0('[RepoReadWrite::validateXLS] The following variables in sheet "VarSpec" are duplicated: ', DupName, '.\n'))
    }
    cat(' ok.\n')
    
    cat('\n[RepoReadWrite::ValidateXLS] Validating column "Type" in the sheet VarSpec...')
    TypeVal <- ExcelSheets.list[['VarSpec']][['Type']]
    names(TypeVal) <- Name
    if (!all(TypeVal %in% c('STRING', 'NUMBER'))) {
        
        stop('[RepoReadWrite::validateXLS] Column "Type" of sheet "VarSpec" must have the value "STRING" or "NUMBER".\n')
    }
    cat(' ok.\n')
    
    cat('\n[RepoReadWrite::ValidateXLS] Validating format of column "Length" in the sheet VarSpec...')
    LengthVal <- ExcelSheets.list[['VarSpec']][['Length']]
    names(LengthVal) <- Name
    if (any(is.na(LengthVal) | LengthVal <= 0)) {
        
        stop('[RepoReadWrite::validateXLS] Column "Length" of sheet "VarSpec" must be a positive integer.\n')
    }
    cat(' ok.\n')
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for name consistency in VarSpec...')
    IDQualTot <- c()
    NonIDQualTot <- c()
    IDDDTot <- c()
    for (sName in setdiff(SheetNames, 'VarSpec')) {
        
        IDQual <- ExcelSheets.list[[sName]][['IDQual']]
        IDQual <- IDQual[!is.na(IDQual) & IDQual != '']
        NonIDQual <- ExcelSheets.list[[sName]][['NonIDQual']]
        NonIDQual <- NonIDQual[!is.na(NonIDQual) & NonIDQual != '']
        IDDD <- ExcelSheets.list[[sName]][['IDDD']]
        IDDD <- IDDD[!is.na(IDDD) & IDDD != '']
        
        IDQualTot <- unique(c(IDQualTot, IDQual))
        NonIDQualTot <- unique(c(NonIDQualTot, NonIDQual))
        IDDDTot <- unique(c(IDDDTot, IDDD))
    }
    difName <- setdiff(Name, c(IDQualTot, NonIDQualTot, IDDDTot))
    if (length(difName) > 0) {
        stop('[RepoReadWrite::validateXLS] The following variables in Excel sheet "VarSpec" are not in any other valid sheet: ', 
             toString(difName))
    }
    cat(' ok.\n')
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for correct syntax of column "IDDD" in each sheet...')
    specialchar <- c('_', 'á', 'é', 'í', 'ó', 'ú', 'ñ', 'ü', 'ç')
    for (sName in setdiff(SheetNames, 'VarSpec')) {
        
        IDDD <- ExcelSheets.list[[sName]][['IDDD']]
        IDDDwUnderscore <- unique(unlist(lapply(specialchar, function(char){
            
            return(IDDD[grepl(char, IDDD)])
        })))
        if (length(IDDDwUnderscore) != 0) {
            
            stop(paste0('[RepoReadWrite::validateXLS] The following variable identifiers (IDDD) in sheet ', sName, ' are not valid: ', paste0(IDDDwUnderscore, collapse = ', ')))
        }
    }
    cat(' ok.\n')
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for duplicated qualifiers in sheet...\n')
    for (sName in setdiff(SheetNames, 'VarSpec')) {
        
        cat(paste0('  ', sName, '...'))
        
        IDQual <- ExcelSheets.list[[sName]][['IDQual']]
        IDQual <- IDQual[!is.na(IDQual) & IDQual != '']
        
        NonIDQual <- ExcelSheets.list[[sName]][['NonIDQual']]
        NonIDQual <- NonIDQual[!is.na(NonIDQual) & NonIDQual != '']
        
        IDDD <- ExcelSheets.list[[sName]][['IDDD']]
        IDDD <- IDDD[!is.na(IDDD) & IDDD != '']
        
        DupIDQual <-  IDQual[duplicated(IDQual, by = key(IDQual))]
        if (length(DupIDQual) > 0) {
            stop(paste0('[RepoReadWrite::validateXLS] There are duplicated unit qualifiers (IDQual) in sheet "', sName, '": ', DupIDQual, '.\n')) 
        }
        
        DupNonIDQual <-  NonIDQual[duplicated(NonIDQual, by = key(NonIDQual))]
        if (length(DupNonIDQual) > 0) {
            stop(paste0('[RepoReadWrite::validateXLS] There are duplicated non-unit qualifiers (NonIDQual) in sheet "', sName, '": ', DupNonIDQual, '.\n')) 
        }
        cat(' ok.\n')
    }
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for qualifier columns in sheet according to columns IDQual and NonIDQual ...\n')
    for (sName in setdiff(SheetNames, 'VarSpec')) {
        
        cat(paste0('  ', sName, '...'))
        
        colNames <- names(ExcelSheets.list[[sName]])
        IDQual <- ExcelSheets.list[[sName]][['IDQual']]
        IDQual <- IDQual[!is.na(IDQual) & IDQual != '']
        
        NonIDQual <- ExcelSheets.list[[sName]][['NonIDQual']]
        NonIDQual <- NonIDQual[!is.na(NonIDQual) & NonIDQual != '']
        
        difcolIDQual <- setdiff(IDQual, ExtractNames(colNames))
        if (length(difcolIDQual) > 0) {
            stop(paste0('[RepoReadWrite::validateXLS] There must be a column in sheet "', sName, '" with the following IDQual variables": ', difcolIDQual, '.\n')) 
        }
        
        difcolNonIDQual <- setdiff(NonIDQual, ExtractNames(colNames))
        if (length(difcolNonIDQual) > 0) {
            stop(paste0('[RepoReadWrite::validateXLS] There must be a column in sheet "', sName, '" with the following NonIDQual variables": ', difcolNonIDQual, '.\n')) 
        }
        cat(' ok.\n')
    }
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for other columns in the sheet ...\n')
    
    for (sName in setdiff(SheetNames, 'VarSpec')) {
        
        cat(paste0('  ', sName, '...'))
        
        colNames <- names(ExcelSheets.list[[sName]])
        otherCol <- c("UnitName", "InFiles", "VarDescription", "table_column", "filter", "function")
        
        difcol <- setdiff(otherCol, colNames)
        if (length(difcol) > 0) {
            stop(paste0('[RepoReadWrite::validateXLS] The following columns are missing in sheet ', sName, ': ', paste0(difcol, collapse = ', '), '.\n')) 
        }
        
        IDQual <- ExcelSheets.list[[sName]][['IDQual']]
        IDQual <- IDQual[!is.na(IDQual) & IDQual != '']
        
        NonIDQual <- ExcelSheets.list[[sName]][['NonIDQual']]
        NonIDQual <- NonIDQual[!is.na(NonIDQual) & NonIDQual != '']
        
        otherColExtractedNotIRIA <- c("UnitName", "InFiles", "VarDescription")
        
        IRIAcols <- c("table_column", "filter", "function")
        
        correctCol <- c('IDQual', 'NonIDQual', 'IDDD', IDQual, NonIDQual, otherColExtractedNotIRIA, IRIAcols)
        colNamesNotIRIA <- setdiff(colNames, IRIAcols)
        wrongCol <- setdiff(c(ExtractNames(colNamesNotIRIA), IRIAcols), correctCol)
        wrongCol <- gsub('^X[0-9]+', 'With_no_name', wrongCol)
        if (length(wrongCol) > 0) {
            
            stop(paste0('[RepoReadWrite::validateXLS] The following columns do not appear as valid qualifiers in the sheet ', sName, ': ', paste0(wrongCol, collapse = ', '), '.\n')) 
        }
        cat(' ok.\n')
    }
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for consistency in the order of other columns in all sheet...\n')
    for (sName in setdiff(SheetNames, 'VarSpec')) {
        
        cat(paste0('  ', sName, '...'))
        
        colNames <- names(ExcelSheets.list[[sName]])
        
        otherCol <- c("UnitName", "InFiles", "VarDescription", "table_column", "filter", "function")
        
        IDQual <- ExcelSheets.list[[sName]][['IDQual']]
        IDQual <- IDQual[!is.na(IDQual) & IDQual != '']
        
        NonIDQual <- ExcelSheets.list[[sName]][['NonIDQual']]
        NonIDQual <- NonIDQual[!is.na(NonIDQual) & NonIDQual != '']
        
        otherColExtractedNotIRIA <- c("UnitName", "InFiles", "VarDescription")
        
        IRIAcols <- c("table_column", "filter", "function")
        
        colNamesNotIRIA <- setdiff(colNames, IRIAcols)
        otherColSheet <- setdiff(c(ExtractNames(colNamesNotIRIA), IRIAcols), c('IDQual', 'NonIDQual', 'IDDD', IDQual, NonIDQual))
        if (!identical(otherColSheet, otherCol)) {
            
            stop(paste0('[RepoReadWrite::validateXLS] The order of qualifiers in sheet ', sName, ' is not ', paste0(otherCol , collapse = ', '), '.\n')) 
        }
        cat(' ok.\n')
    }
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for name consistency between VarSpec and sheet...\n')
    for (sName in setdiff(SheetNames, 'VarSpec')) {
        
        cat(paste0('  ', sName, '...'))
        
        IDQual <- ExcelSheets.list[[sName]][['IDQual']]
        IDQual <- IDQual[!is.na(IDQual) & IDQual != '']
        
        NonIDQual <- ExcelSheets.list[[sName]][['NonIDQual']]
        NonIDQual <- NonIDQual[!is.na(NonIDQual) & NonIDQual != '']
        
        IDDD <- ExcelSheets.list[[sName]][['IDDD']]
        IDDD <- IDDD[!is.na(IDDD) & IDDD != '']
        
        difIDQual <- setdiff(IDQual, Name)
        if (length(difIDQual) > 0) {
            stop(paste0('[RepoReadWrite::validateXLS] The following unit qualifiers (IDQUal) in sheet "', sName, '" are not in the sheet VarSpec: ', 
                        paste0(difIDQual, collapse = ', '), 
                        '.\n')) 
        }
        
        
        difNonIDQual <- setdiff(NonIDQual, Name)
        if (length(difNonIDQual) > 0) {
            stop(paste0('[RepoReadWrite::validateXLS] The following non-unit qualifiers (NonIDQual) in sheet "', sName, '" are not in the sheet VarSpec: ', 
                        paste0(difNonIDQual, collapse = ', '),
                        '.\n'))
        }
        
        
        difIDDD <- setdiff(IDDD, Name)
        if (length(difIDDD) > 0) {
            stop(paste0('[RepoReadWrite::validateXLS] The following variables (IDDD) in sheet "', sName, '" are not in the sheet VarSpec: ', 
                        paste0(difIDDD, collapse = ', '),
                        '.\n')) 
        }
        cat(' ok.\n')
    }    
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for qualifier length consistency between VarSpec and the rest of sheet...')
    QualLength <- data.table(Qual = character(0), Length = character(0))
    for (sName in setdiff(SheetNames, 'VarSpec')) {
        
        colNames <- names(ExcelSheets.list[[sName]])
        NonIDQual <- ExcelSheets.list[[sName]][['NonIDQual']]
        NonIDQual <- NonIDQual[!is.na(NonIDQual) & NonIDQual != '']
        
        localNonIDQual <- c()
        for (aux in NonIDQual) {
            
            localNonIDQual <- c(localNonIDQual, colNames[grep(aux, colNames)])
            
        }
        AuxExcel <- ExcelSheets.list[[sName]][IDDD != '', localNonIDQual, with = F]
        if (dim(AuxExcel)[1] == 0) next
        AuxExcel[is.na(AuxExcel)] <- ''
        
        if (dim(AuxExcel)[1] == 1 ){
            
            auxLength <- vapply(AuxExcel, 1 , FUN = nchar)
            auxLength <- vapply(auxLength, 1, FUN = max)  
            
        } else {
            
            auxLength <- apply(AuxExcel, 1 , FUN = nchar)
            if (is.null(dim(auxLength))) auxLength <- matrix(auxLength, nrow = 1, dimnames = list(names(AuxExcel), NULL))
            auxLength <- apply(auxLength, 1, FUN = max)
            
            
            
        }
        auxDT <- data.table(Qual = names(auxLength), Length = auxLength)
        QualLength <- rbindlist(list(QualLength, auxDT))
    }
    QualLength <- split(QualLength, QualLength[['Qual']])
    QualLength <- lapply(QualLength, function(DT){max(DT[['Length']])})
    QualLength <- data.table(Qual = ExtractNames(names(QualLength)), Length = unlist(as.integer(QualLength)))
    VarSpecQualLength <- ExcelSheets.list[['VarSpec']][, c('Name', 'Length'), with = F]
    VarSpecQualLength[, Length := as.integer(Length)]
    QualLength <- merge(QualLength, VarSpecQualLength, by.x = 'Qual', by.y = 'Name')
    QualLength[, Correct := (Length.y >= Length.x)][]
    InconsistLength <- QualLength[['Qual']][!QualLength[['Correct']]]
    if (length(InconsistLength) > 0) {
        
        stop(paste0('[RepoReadWrite::validateXLS] The following qualifiers do not have consistent lengths between the sheet VarSpec and the rest of sheets:', 
                    paste0(InconsistLength, collapse = ', '), 
                    '.\n'))
        
    }
    cat(' ok.\n')
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for consistency in the number of qualifiers for each variable in all sheet...')
    IDDD.list <- lapply(setdiff(SheetNames, 'VarSpec'), function(sName){
        
                    IDDD <- ExcelSheets.list[[sName]][['IDDD']]
                    IDDD <- IDDD[!is.na(IDDD) & IDDD != '']
                    IDDD <- unique(IDDD)
                })
    names(IDDD.list) <- setdiff(SheetNames, 'VarSpec')
    
    IDDDComm <- unlist(lapply(1:(length(IDDD.list) - 1), function(i) {
        
        intersect(IDDD.list[[i]], IDDD.list[[i + 1]])
    }))
    
    Quals.list <- lapply(names(IDDD.list), function(sName){
            
            DT <- ExcelSheets.list[[sName]]
            Quals <- setdiff(names(DT), c('IDQual', 'NonIDQual', 'IDDD', 'UnitName', 'InFiles', 'VarDescription', 'table_column', 'filter', 'function'))    
            return(Quals)
    })
    names(Quals.list) <- names(IDDD.list)
    
    IDDDComm.list <- lapply(IDDDComm, function(iddd){
        
        Quals.iddd <- lapply(names(Quals.list), function(sName){
            
            DT <- ExcelSheets.list[[sName]][IDDD == iddd]
            Quals <- Quals.list[[sName]]
            Quals.length <- unlist(lapply(Quals, function(qual){
                long <- DT[[qual]]
                long <- length(long[long != ''])
            }))
            Quals <- Quals[Quals.length > 0]
            return(Quals)
        })
        names(Quals.iddd) <- names(IDDD.list)
        return(Quals.iddd)
    })
    names(IDDDComm.list) <- IDDDComm
    
    IDDD.Quals <- lapply(IDDDComm, function(iddd){
        
        unique(ExtractNames(unlist(IDDDComm.list[[iddd]])))
    })
    names(IDDD.Quals) <- IDDDComm
    
    IDDD.sheets <- lapply(IDDDComm, function(iddd){
        
        sheets <- c()
        for (sheet in names(IDDDComm.list[[iddd]])){
            if (length(IDDDComm.list[[iddd]][[sheet]]) > 0) sheets <- c(sheets, sheet)
        }
        return(sheets)
    })
    names(IDDD.sheets) <- IDDDComm
    
    for (iddd in IDDDComm){
        
        iddd.quals <- IDDD.Quals[[iddd]]
        iddd.sheets <- IDDD.sheets[[iddd]]
        for (sheet in iddd.sheets){
            if (!all(iddd.quals %chin% ExtractNames(Quals.list[[sheet]]))){
                
                stop(paste0('[[RepoReadWrite::ValidateXLS] Variable (IDDD) ', iddd, ' has not all its non-unit qualifiers (NonIDQual) in sheet ', sheet))
            }
        }
    }
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for consistency in the order of qualifiers in all sheet...')
    Qual.list <- list()
    
    for (sName in setdiff(SheetNames, 'VarSpec')) {
        
        IDQual <- ExcelSheets.list[[sName]][['IDQual']]
        IDQual <- IDQual[!is.na(IDQual) & IDQual != '']
        
        NonIDQual <- ExcelSheets.list[[sName]][['NonIDQual']]
        NonIDQual <- NonIDQual[!is.na(NonIDQual) & NonIDQual != '']
        
        Qual.list[[sName]] <- c(IDQual, NonIDQual)
    }
    RangeIndices <- setdiff(seq(along = names(Qual.list)), length(Qual.list))
    for (indexsName in RangeIndices) {
        
        CurrentQuals <- Qual.list[[indexsName]]
        for (indexOthersName in (indexsName + 1):(length(Qual.list))) {
            OtherQuals <- Qual.list[[indexOthersName]]
            localCurrentQual <- CurrentQuals[CurrentQuals %in% OtherQuals]
            localOtherQuals <- OtherQuals[OtherQuals %in% CurrentQuals]
            
            if (!all(localCurrentQual == localOtherQuals)) {
                
                stop(paste0('[[RepoReadWrite::ValidateXLS] The order of qualifiers in sheet ', names(Qual.list)[indexsName], ' and ', names(Qual.list)[indexOthersName], ' is not consistent.'))
            }
        }
    }
    cat(' ok.\n')
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for consistency of qualifier "TipoMicrodato" in all sheet...')
    for (sName in setdiff(SheetNames, 'VarSpec')) {
        
        sheet <- ExcelSheets.list[[sName]]
        NamesSheet <- unlist(lapply(names(sheet), function(names) {strsplit(names, '_')[[1]][1]}))
        if (!'TipoMicrodato' %in% NamesSheet) {
            
            stop(paste0('[RepoReadWrite::validateXLS] Qualifier "TipoMicrodato" is missing in sheet ', sName, '.'))
            
        }
    }
    cat(' ok.\n')
    
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for missing values in qualifier "TipoMicrodato" ...')
    for (sName in setdiff(SheetNames, 'VarSpec')) {
        
        sheet <- ExcelSheets.list[[sName]]
        indexTipoMicrodato <- grep('TipoMicrodato', names(sheet))
        TipoMicrodato <- sheet[[indexTipoMicrodato]]
        if (any(is.na(TipoMicrodato)) | any(TipoMicrodato == '')) {
            
            stop(paste0('[RepoReadWrite::validateXLS] There are missing values in column TipoMicrodato in sheet ', sName))
            
        }
        
    }
    cat(' ok.\n')
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for consistency of qualifier "TipoMicrodato" in all sheet...')
    UnitNameTipo <- list()
    for (sName in setdiff(SheetNames, 'VarSpec')) {
        
        sheet <- ExcelSheets.list[[sName]]
        TipoMicrodatoName <- names(sheet)[grep('TipoMicrodato', names(sheet))]
        Var <- sheet[, c('IDQual', 'NonIDQual', 'UnitName'), with = FALSE]
        Var[, Variable := ifelse(IDQual != '', IDQual, ifelse(NonIDQual != '', NonIDQual, UnitName))]
        Var[, TipoMicrodato := sheet[[TipoMicrodatoName]]]
        Var[, c('IDQual', 'NonIDQual', 'UnitName') := NULL]
        setnames(Var, 'Variable', 'UnitName')
        Var <- Var[!duplicated(Var, by = names(Var))]
        UnitNameTipo[[sName]] <- Var
    }
    UnitNameTipo <- rbindlist(UnitNameTipo)
    UnitNameTipo <- UnitNameTipo[!duplicated(UnitNameTipo, by = names(UnitNameTipo))]
    NotUniqueTipo <- UnitNameTipo[duplicated(UnitNameTipo, by = 'UnitName')]
    if (dim(NotUniqueTipo)[1] != 0) {
        
        stop(paste0('[RepoReadWrite::validateXLS] The following qualifier TipoMicrodato are not consistent ', print(NotUniqueTipo)))
        
    }
    cat(' ok.\n')
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for consistency of UnitNames with .. qualifiers ...')
    for (sName in setdiff(SheetNames, 'VarSpec')) {
        
        sheet <- ExcelSheets.list[[sName]]
        if (dim(sheet)[1] == 0) next
        DoubleDot <- as.logical(rowSums(as.matrix(sheet == '..')))
        DoubleDotUnitNames <- sheet[DoubleDot][['UnitName']]
        MetaUnitNames <- DoubleDotUnitNames[grepl('[', DoubleDotUnitNames, fixed = TRUE)]
        WrongUnitNames <- setdiff(DoubleDotUnitNames, MetaUnitNames)
        
        if (length(WrongUnitNames) != 0) {
            
            stop(paste0('[RepoReadWrite::validateXLS] The following UnitNames in sheet ', sName, ' are malformed: ', paste0(WrongUnitNames, collapse = ', '), '.'))
            
        }
    }
    cat(' ok.\n')
    
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for duplicates in IDDD + Qualifiers ...')
    for (sName in setdiff(SheetNames, 'VarSpec')) {
        
        sheet <- ExcelSheets.list[[sName]]
        NamesSheet <- unlist(lapply(names(sheet), function(names) {strsplit(names, '_')[[1]][1]}))
        setnames(sheet, names(sheet), NamesSheet)
        
        IDQual <- sheet[['IDQual']]
        IDQual <- IDQual[!is.na(IDQual) & IDQual != '']
        
        NonIDQual <- sheet[['NonIDQual']]
        NonIDQual <- NonIDQual[!is.na(NonIDQual) & NonIDQual != '']
        
        sheet <- sheet[!(IDQual != '' | NonIDQual != '')]
        
        byKey <- c('IDDD', IDQual, NonIDQual)
        DTdup <- sheet[duplicated(sheet, by = byKey)]
        
        if (dim(DTdup)[1] != 0) {
            
            stop(paste0('[RepoReadWrite::validateXLS] The following keys in sheet ', sName, ' are duplicated: ', DTdup, '\n\n'))
            
        }
    }
    cat(' ok.\n')
    
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for missing and not valid values in column "InFiles" ...')
    for (sName in setdiff(SheetNames, 'VarSpec')) {
      
      sheet <- ExcelSheets.list[[sName]]
      IDQual <- sheet[['IDQual']]
      IDQual <- IDQual[!is.na(IDQual) & IDQual != '']
      NonIDQual <- sheet[['NonIDQual']]
      NonIDQual <- NonIDQual[!is.na(NonIDQual) & NonIDQual != '']
      IDDD <- sheet[['IDDD']]
      IDDD <- IDDD[!is.na(IDDD) & IDDD != '']
      nquals <- length(c(IDQual, NonIDQual))
      indexInFiles <- grep('InFiles', names(sheet))
      InFiles <- sheet[[indexInFiles]]
      InFiles <- InFiles[(nquals + 1): (nquals + length(IDDD))]
      
      if (any(is.na(InFiles)) | any(InFiles == '')) {
        
        stop(paste0('[RepoReadWrite::validateXLS] There are missing values in column InFiles in sheet ', sName))
        
      }
      
      InFilesValues <- unique(unlist(lapply(InFiles, function(x){
        
          fileTypes <- unlist(strsplit(x, ','))
          fileTypes <- trim(fileTypes)
      })))
      if (any(!InFilesValues %chin% c('FI', 'FG', 'FD', 'FF', 'FP', 'FL', 'FT'))) {
        
        stop(paste0('[RepoReadWrite::validateXLS] Some values in column InFiles in sheet ', sName, ' are not valid. The valid values are: FG, FD, FF, FL, FT'))
        
      }
    }
    cat(' ok.\n')
    
    
    cat(paste0('\n[RepoReadWrite::validateXLS] The Excel file ', ExcelName, ' is valid.\n\n'))
    return(TRUE)
    
}

