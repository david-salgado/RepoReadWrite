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
#' @import data.table xlsx
#' 
#' @importFrom StQ ExtractNames
#'       
#' @export
ValidateXLS <- function(ExcelName){
    
    if (!requireNamespace('xlsx', quietly = TRUE)) stop('[RepoReadWrite::ValidateXLS] Package xlsx must be installed in the system.\n')
    wb <- loadWorkbook(ExcelName)
    SheetNames <- names(getSheets(wb))
    
    
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
        
        ExcelSheets.list[[sName]] <- read.xlsx2(ExcelName, 
                                                sheetName = sName, 
                                                colClasses = 'character',
                                                stringsAsFactors = F)
        OrigOrder <- dimnames(ExcelSheets.list[[sName]])[1][[1]]
        ExcelSheets.list[[sName]] <- as.data.table(ExcelSheets.list[[sName]])
        ExcelSheets.list[[sName]][, OrigOrder := as.integer(OrigOrder)]
        ExcelSheets.list[[sName]] <- ExcelSheets.list[[sName]][order(rank(OrigOrder)),]
        ExcelSheets.list[[sName]][, OrigOrder := NULL]
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
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for name consistency in VarSpec...\n')
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
    for (sName in setdiff(SheetNames, 'VarSpec')) {
        
        IDDD <- ExcelSheets.list[[sName]][['IDDD']]
        IDDDwUnderscore <- IDDD[grepl('_', IDDD)]
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
        auxLength <- apply(AuxExcel, 1 , FUN = nchar)
        auxLength <- apply(auxLength, 1, FUN = max)
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
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for consistency in the order of qualifiers in all sheet...\n')
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
    
    
    cat('\n[RepoReadWrite::ValidateXLS] Checking for missing values in qualifier "TipoMicrodato" ...\n')
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

    cat(paste0('\n[RepoReadWrite::validateXLS] The Excel file ', ExcelName, ' is valid.\n\n'))
    return(TRUE)
    
    
    
    
    
    # SheetNames <- setdiff(SheetNames, 'VarSpec')
    # IDQualTot <- c()
    # NonIDQualTot <- c()
    # IDDDTot <- c()
    # for (sName in SheetNames) {
    #     
    #     IDQual <- ExcelSheets.list[[sName]][['IDQual']]
    #     IDQual <- IDQual[!is.na(IDQual) & IDQual != '']
    #     NonIDQual <- ExcelSheets.list[[sName]][['NonIDQual']]
    #     NonIDQual <- NonIDQual[!is.na(NonIDQual) & NonIDQual != '']
    #     IDDD <- ExcelSheets.list[[sName]][['IDDD']]
    #     IDDD <- IDDD[!is.na(IDDD) & IDDD != '']
    #     
    #     IDQualTot <- unique(c(IDQualTot, IDQual))
    #     NonIDQualTot <- unique(c(NonIDQualTot, NonIDQual))
    #     IDDDTot <- unique(c(IDDDTot, IDDD))
    #     
    # #Validations
    # 
    #     #Duplicados
    #     DupIDQual <-  IDQual[duplicated(IDQual, by = key(IDQual))]
    #     DupNonIDQual <-  NonIDQual[duplicated(NonIDQual, by = key(NonIDQual))]
    #     
    #     if (length(DupIDQual) > 0) {
    #         stop(paste0('[RepoReadWrite::validateXLS] There are duplicated IDQual variables in sheet "', sName, '": ', DupIDQual, '.\n')) 
    #     }
    #     
    #     if (length(DupNonIDQual) > 0) {
    #         stop(paste0('[RepoReadWrite::validateXLS] There are duplicated NonIDQual variables in sheet "', sName, '": ', DupNonIDQual, '.\n')) 
    #     }
    #     
    #     #Columnas de calificadores
    #     colNames <- names(ExcelSheets.list[[sName]])
    #     
    #     difcolIDQual <- setdiff(IDQual, ExtractNames(colNames))
    #     if (length(difcolIDQual) > 0) {
    #         stop(paste0('[RepoReadWrite::validateXLS] There must be a column in sheet "', sName, '" with the following IDQual variables": ', difcolIDQual, '.\n')) 
    #     }
    # 
    #     difcolNonIDQual <- setdiff(NonIDQual, ExtractNames(colNames))
    #     if (length(difcolNonIDQual) > 0) {
    #         stop(paste0('[RepoReadWrite::validateXLS] There must be a column in sheet "', sName, '" with the following NonIDQual variables": ', difcolNonIDQual, '.\n')) 
    #     }
    #     
    #     #Coherencia con VarSpec
    #     difIDQual <- setdiff(IDQual, Name)
    #     if (length(difIDQual) > 0) {
    #         stop(paste0('[RepoReadWrite::validateXLS] The following variables in sheet "', sName, '" are not in "VarSpec": ', difIDQual, '.\n')) 
    #     }
    #     
    #     
    #     difNonIDQual <- setdiff(NonIDQual, Name)
    #     if (length(difNonIDQual) > 0) {
    #         stop(paste0('[RepoReadWrite::validateXLS] The following variables in sheet "', sName, '" are not in "VarSpec": ', difNonIDQual, '.\n')) 
    #     }
    #     
    # 
    #     difIDDD <- setdiff(IDDD, Name)
    #     if (length(difIDDD) > 0) {
    #         stop(paste0('[RepoReadWrite::validateXLS] The following variables in sheet "', sName, '" are not in "VarSpec": ', difIDDD, '.\n')) 
    #     }
    #     
    #     
    #     #Longitudes
    # 
    #     localNonIDQual <- c()
    #     for (aux in NonIDQual) {
    #         
    #         localNonIDQual <- c(localNonIDQual, colNames[grep(aux, colNames)])
    # 
    #     }
    #     AuxExcel <- ExcelSheets.list[[sName]][IDDD != '', localNonIDQual, with = F]
    #     AuxExcel[is.na(AuxExcel)] <- ''
    #     aux <- apply(AuxExcel, 1 , FUN = nchar)
    #     aux <- apply(aux, 1, FUN = max)
    #     for (Col in localNonIDQual) {
    #         
    #         if (aux[Col] > LengthVal[ExtractNames(Col)]) {
    #             
    #             stop('[RepoReadWrite::validateXLS] There are inconsistent values lengths in column "', Col, '" from sheet "', sName,'".')
    #         }
    #     }
    # }
    # 
    # 
    # difName <- setdiff(Name, c(IDQualTot, NonIDQualTot, IDDDTot))
    # if (length(difName) > 0) {
    #         stop('[RepoReadWrite::validateXLS] The following variables in Excel sheet "VarSpec" are not in any other valid sheet: ', 
    #                             toString(difName))
    # }
    # return(cat('[RepoReadWrite::validateXLS] Excel file validated.\n'))
}

