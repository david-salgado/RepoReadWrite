#' @title Check validity of xlsx file to create DD
#' 
#' @description This function checks the validity of the xlsx file which will be used to create the
#'  data dictionary with XML format.
#' 
#' @param ExcelName Character vector of length 1 with the name of the file to read.
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
#' @import data.table
#' 
#' @importFrom StQ ExtractNames
#' 
#' @importFrom openxlsx getSheetNames
#'       
#' @export
is.validXLSX <- function(ExcelName, verbose = FALSE){
    
    # Global Variables
    SheetNames <- openxlsx::getSheetNames(ExcelName)
    varRootSheetNames <- c('ID', 'MicroData', 'ParaData', 'Aggregates', 'AggWeights', 'Other')
    varSheetNames <- c()
    for (sh in varRootSheetNames){
        
        tempSheetName <- SheetNames[grep(sh, SheetNames)]
        varSheetNames <- c(varSheetNames, tempSheetName)
    }
    
    ExcelSheets.list <- list()
    for (sName in SheetNames) {
        
        ExcelSheets.list[[sName]] <- openxlsx::read.xlsx(ExcelName, sheet = sName)
        ExcelSheets.list[[sName]] <- as.data.table(ExcelSheets.list[[sName]])
        for (col in names(ExcelSheets.list[[sName]])) {
            
            ExcelSheets.list[[sName]] <- ExcelSheets.list[[sName]][, (col) := as.character(get(col))]
            ExcelSheets.list[[sName]] <- ExcelSheets.list[[sName]][is.na(get(col)), (col) := '']
        }
        OrigOrder <- dimnames(ExcelSheets.list[[sName]])[1][[1]]
        ExcelSheets.list[[sName]][, OrigOrder := as.integer(OrigOrder)]
        ExcelSheets.list[[sName]] <- ExcelSheets.list[[sName]][order(rank(OrigOrder)),]
        ExcelSheets.list[[sName]][, OrigOrder := NULL]
        if (dim(ExcelSheets.list[[sName]])[1] == 0) stop(paste0('[RepoReadWrite::ValidateXLS] The following sheet is empty: ', sName, '.\n'))
        ExcelSheets.list[[sName]] <- ExcelSheets.list[[sName]][!apply(is.na(ExcelSheets.list[[sName]]) | ExcelSheets.list[[sName]] == "", 1, all),]
        
    }
    
    quals_IDDD <- c()
    idQuals_IDDD <- c()
    nonIDQuals_IDDD <- c()
    quals_UnitName <- c()
    idQuals_UnitName <- c()
    nonIDQuals_UnitName <- c()
    for (sName in varSheetNames) {
        
        IDQual_IDDD <- ExcelSheets.list[[sName]][['IDQual']]
        IDQual_IDDD <- IDQual_IDDD[!is.na(IDQual_IDDD) & IDQual_IDDD != '']
        idQuals_IDDD <- c(idQuals_IDDD, IDQual_IDDD)
        
        IDQual_UnitName <- ExcelSheets.list[[sName]][IDQual %chin% IDQual_IDDD][['UnitName']]
        IDQual_UnitName <- IDQual_UnitName[IDQual_UnitName != '']
        idQuals_UnitName <- c(idQuals_UnitName, IDQual_UnitName)
        
        
        NonIDQual_IDDD <- ExcelSheets.list[[sName]][['NonIDQual']]
        NonIDQual_IDDD <- NonIDQual_IDDD[!is.na(NonIDQual_IDDD) & NonIDQual_IDDD != '']
        nonIDQuals_IDDD <- c(nonIDQuals_IDDD, NonIDQual_IDDD)
        
        NonIDQual_UnitName <- ExcelSheets.list[[sName]][NonIDQual %chin% NonIDQual_IDDD][['UnitName']]
        NonIDQual_UnitName <- NonIDQual_UnitName[NonIDQual_UnitName != '']
        nonIDQuals_UnitName <- c(nonIDQuals_UnitName, NonIDQual_UnitName)
        
        quals_IDDD <- c(quals_IDDD, IDQual_IDDD, NonIDQual_IDDD)
        quals_UnitName <- c(quals_UnitName, IDQual_UnitName, NonIDQual_UnitName)
    }
    quals_IDDD <- unique(quals_IDDD)
    quals_UnitName <- unique(quals_UnitName)
    idQuals_IDDD <- unique(idQuals_IDDD)
    idQuals_UnitName <- unique(idQuals_UnitName)
    nonIDQuals_IDDD <- unique(nonIDQuals_IDDD)
    nonIDQuals_UnitName <- unique(nonIDQuals_UnitName)

    Name <- ExcelSheets.list[['VarSpec']][['Name']]
    
    #######                                      CHECKS                                ######
    if (verbose) cat('\n[RepoReadWrite::ValidateXLS] Checking minimal compulsory sheet names: VarSpec, ID, MicroData...')
    CompulsorySheets <- setdiff(c('VarSpec', 'ID', 'MicroData'), SheetNames)
    if (length(CompulsorySheets) > 0) {
        
        stop(paste0('[RepoReadWrite::ValidateXLS] The following sheets must be in the Excel file:', CompulsorySheets, '.\n'))
    }
    if (verbose) cat(' ok.\n')
    
    if (verbose) cat('\n[RepoReadWrite::ValidateXLS] Checking optional (possibly compound) sheet names: ParaData, Aggregates, AggWeights, Other...')
    OptionalSheets <- setdiff(SheetNames, c('VarSpec', 'ID', 'MicroData'))
    OptionalSheets <- sapply(OptionalSheets, ExtractNames)
    OptionalValSheet <- setdiff(OptionalSheets, c('ParaData', 'Aggregates', 'AggWeights', 'Other'))
    if (length(OptionalValSheet) > 0) {
        
        stop(paste0('[RepoReadWrite::ValidateXLS] The following prefixes in the Excel sheet names are not valid:', OptionalValSheet, '.\n'))
    }
    if (verbose) cat(' ok.\n')

    if (verbose) cat('\n[RepoReadWrite::ValidateXLS] Checking no duplicates in column Name in the sheet VarSpec...')
    DupName <-  Name[duplicated(Name, by = 'Name')]
    if (length(DupName) > 0) {
        
        stop(paste0('[RepoReadWrite::validateXLS] The following variables in sheet VarSpec are duplicated: ', DupName, '.\n'))
    }
    if (verbose) cat(' ok.\n')
    
    if (verbose) cat('\n[RepoReadWrite::ValidateXLS] Checking column Type in the sheet VarSpec...')
    TypeVal <- ExcelSheets.list[['VarSpec']][['Type']]
    names(TypeVal) <- Name
    if (!all(TypeVal %in% c('STRING', 'NUMBER'))) {
        
        stop('[RepoReadWrite::validateXLS] Column Type of sheet VarSpec must have the value STRING or NUMBER.\n')
    }
    if (verbose) cat(' ok.\n')
    
    if (verbose) cat('\n[RepoReadWrite::ValidateXLS] Checking format of column Length in the sheet VarSpec...')
    LengthVal <- ExcelSheets.list[['VarSpec']][['Length']]
    names(LengthVal) <- Name
    if (any(is.na(LengthVal) | LengthVal <= 0)) {
        
        stop('[RepoReadWrite::validateXLS] Column Length of sheet VarSpec must be a positive integer.\n')
    }
    if (verbose) cat(' ok.\n')
    
    if (verbose) cat('\n[RepoReadWrite::ValidateXLS] Checking that all variables in VarSpec are included in the other sheet...')
    IDQualTot <- c()
    NonIDQualTot <- c()
    IDDDTot <- c()
    for (sName in varSheetNames) {
        
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
        stop('[RepoReadWrite::validateXLS] The following variables in Excel sheet VarSpec are not in any other valid sheet: ', 
             toString(difName))
    }
    if (verbose) cat(' ok.\n')
    
    if (verbose) cat('\n[RepoReadWrite::ValidateXLS] Checking correct syntax of column IDDD in each sheet...')
    specialchar <- c('_', "\\u00e1", "\\u00e9", "\\u00ed", "\\u00f3",
                     "\\u00fa", "\\u00f1", "\\u00fc", "\\u00e7")
    for (sName in varSheetNames) {
        
        IDDD <- ExcelSheets.list[[sName]][['IDDD']]
        IDDDwUnderscore <- unique(unlist(lapply(specialchar, function(char){
            
            return(IDDD[grepl(char, IDDD)])
        })))
        if (length(IDDDwUnderscore) != 0) {
            
            stop(paste0('[RepoReadWrite::validateXLS] The following variable identifiers (IDDD) in sheet ', sName, ' are not valid: ', paste0(IDDDwUnderscore, collapse = ', ')))
        }
    }
    if (verbose) cat(' ok.\n')
    
    if (verbose) cat('\n[RepoReadWrite::ValidateXLS] Checking no duplication in UnitNames (qualifiers not considered)...')
    unitNames <- c()

    for (sName in varSheetNames) {
        
        localUnitName <- ExcelSheets.list[[sName]][['UnitName']]
        localUnitName <- localUnitName[!is.na(localUnitName) & localUnitName != '']
        unitNames <- c(unitNames, localUnitName)

    }
    
    unitNames <- unitNames[!unitNames %in% quals_UnitName]
    dupUnitNames <- unitNames[duplicated(unitNames)]

    if (length(dupUnitNames) > 0) {
        
             stop(paste0('[RepoReadWrite::validateXLS] The following unitnames are duplicated: ', 
                         paste0(dupUnitNames, collapse = ' ,'), '.\n')) 
    }
    
    
    if (verbose) cat(' ok.\n')
    
    if (verbose) cat('\n[RepoReadWrite::ValidateXLS] Checking no duplication in qualifiers in sheet...\n')
    for (sName in varSheetNames) {
        
        if (verbose) cat(paste0('  ', sName, '...'))
        
        IDQual <- ExcelSheets.list[[sName]][['IDQual']]
        IDQual <- IDQual[!is.na(IDQual) & IDQual != '']

        NonIDQual <- ExcelSheets.list[[sName]][['NonIDQual']]
        NonIDQual <- NonIDQual[!is.na(NonIDQual) & NonIDQual != '']

        DupIDQual <-  IDQual[duplicated(IDQual, by = key(IDQual))]
        if (length(DupIDQual) > 0) {
            stop(paste0('[RepoReadWrite::validateXLS] There are duplicated unit qualifiers (IDQual) in sheet "', sName, '": ', DupIDQual, '.\n'))
        }

        DupNonIDQual <-  NonIDQual[duplicated(NonIDQual, by = key(NonIDQual))]
        if (length(DupNonIDQual) > 0) {
            stop(paste0('[RepoReadWrite::validateXLS] There are duplicated non-unit qualifiers (NonIDQual) in sheet "', sName, '": ', DupNonIDQual, '.\n'))
        }
        
        if (verbose) cat(' ok.\n')
    }
    
    if (verbose) cat('\n[RepoReadWrite::is.validXLSX] Checking no duplication in qualifier values (including IDDD) in sheet...\n')
    for (sName in varSheetNames) {
        
        if (verbose) cat(paste0('  ', sName, '...'))
        
        DupQual.dt <- ExcelSheets.list[[sName]]
        DupQual.dt <- DupQual.dt[!is.na(IDDD) & IDDD != '']
        colNames<- names(DupQual.dt)
        colNames <- unlist(sapply(quals_IDDD, function(pat) colNames[grep(pat, colNames)]))
        DupQual.dt <- DupQual.dt[duplicated(DupQual.dt, by = c('IDDD', colNames))][
            , c('IDDD', colNames), with = FALSE]

        # IDQual <- ExcelSheets.list[[sName]][['IDQual']]
        # IDQual <- IDQual[!is.na(IDQual) & IDQual != '']
        # 
        # NonIDQual <- ExcelSheets.list[[sName]][['NonIDQual']]
        # NonIDQual <- NonIDQual[!is.na(NonIDQual) & NonIDQual != '']
        # 
        # DupIDQual <-  IDQual[duplicated(IDQual, by = key(IDQual))]
        # if (length(DupIDQual) > 0) {
        #     stop(paste0('[RepoReadWrite::is.validXLSX] There are duplicated unit qualifiers (IDQual) in sheet "', sName, '": ', DupIDQual, '.\n')) 
        # }
        # 
        # DupNonIDQual <-  NonIDQual[duplicated(NonIDQual, by = key(NonIDQual))]
        # if (length(DupNonIDQual) > 0) {
        #     stop(paste0('[RepoReadWrite::is.validXLSX] There are duplicated non-unit qualifiers (NonIDQual) in sheet "', sName, '": ', DupNonIDQual, '.\n')) 
        # }
        if (dim(DupQual.dt)[1] > 0) {
            stop(paste0('[RepoReadWrite::is.validXLSX] There are duplicated qualifiers (including IDDD) in sheet ', sName, ': ', paste0(DupQual.dt[['IDDD']], collapse = ' ,'), '.\n'))
        }
        if (verbose) cat(' ok.\n')
    }

    if (verbose) cat('\n[RepoReadWrite::is.validXLSX] Checking qualifier column names according to columns IDQual and NonIDQual in sheet ...\n')
    for (sName in varSheetNames) {
        
        if (verbose) cat(paste0('  ', sName, '...'))
        
        colNames <- names(ExcelSheets.list[[sName]])
        IDQual <- ExcelSheets.list[[sName]][['IDQual']]
        IDQual <- IDQual[!is.na(IDQual) & IDQual != '']
        
        NonIDQual <- ExcelSheets.list[[sName]][['NonIDQual']]
        NonIDQual <- NonIDQual[!is.na(NonIDQual) & NonIDQual != '']
        
        difcolIDQual <- setdiff(IDQual, ExtractNames(colNames))
        if (length(difcolIDQual) > 0) {
            stop(paste0('[RepoReadWrite::is.validXLSX] There must be columns in sheet ', sName, ' with the following IDQual variables: ', paste0(difcolIDQual, collapse = ' ,'), '.\n')) 
        }
        
        difcolNonIDQual <- setdiff(NonIDQual, ExtractNames(colNames))
        if (length(difcolNonIDQual) > 0) {
            stop(paste0('[RepoReadWrite::is.validXLSX] There must be a column in sheet ', sName, ' with the following NonIDQual variables: ', paste0(difcolNonIDQual, collapse = ' ,'), '.\n')) 
        }
        if (verbose) cat(' ok.\n')
    }
    
    if (verbose) cat('\n[RepoReadWrite::is.validXLSX] Checking existence of columns UnitName, InFiles, VarDescription, table_column, filter, function in sheet ...\n')
    
    for (sName in varSheetNames) {
        
        if (verbose) cat(paste0('  ', sName, '...'))
        
        colNames <- names(ExcelSheets.list[[sName]])
        otherCol <- c("UnitName", "InFiles", "VarDescription", "table_column", "filter", "function")
        
        difcol <- setdiff(otherCol, colNames)
        if (length(difcol) > 0) {
            stop(paste0('[RepoReadWrite::is.validXLSX] The following columns are missing in sheet ', sName, ': ', paste0(difcol, collapse = ', '), '.\n')) 
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
            
            stop(paste0('[RepoReadWrite::is.validXLSX] The following columns do not appear as valid qualifiers in the sheet ', sName, ': ', paste0(wrongCol, collapse = ', '), '.\n')) 
        }
        if (verbose) cat(' ok.\n')
    }
    
    if (verbose) cat('\n[RepoReadWrite::is.validXLSX] Checking consistency in the order of columns UnitName, InFiles, VarDescription, table_column, filter, function in sheet...\n')
    for (sName in varSheetNames) {
        
        if (verbose) cat(paste0('  ', sName, '...'))
        
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
            
            stop(paste0('[RepoReadWrite::is.validXLSX] The order of qualifiers in sheet ', sName, ' is not ', paste0(otherCol , collapse = ', '), '.\n')) 
        }
        if (verbose) cat(' ok.\n')
    }
    
    if (verbose) cat('\n[RepoReadWrite::is.validXLSX] Checking that IDDDs are in column Name of VarSpec for sheet ...\n')
    for (sName in varSheetNames) {
        
        if (verbose) cat(paste0('  ', sName, '...'))
        
        IDQual <- ExcelSheets.list[[sName]][['IDQual']]
        IDQual <- IDQual[!is.na(IDQual) & IDQual != '']
        
        NonIDQual <- ExcelSheets.list[[sName]][['NonIDQual']]
        NonIDQual <- NonIDQual[!is.na(NonIDQual) & NonIDQual != '']
        
        IDDD <- ExcelSheets.list[[sName]][['IDDD']]
        IDDD <- IDDD[!is.na(IDDD) & IDDD != '']
        
        difIDQual <- setdiff(IDQual, Name)
        if (length(difIDQual) > 0) {
            stop(paste0('[RepoReadWrite::is.validXLSX] The following unit qualifiers (IDQual) in sheet "', sName, '" are not in the sheet VarSpec: ', 
                        paste0(difIDQual, collapse = ', '), 
                        '.\n')) 
        }
        
        
        difNonIDQual <- setdiff(NonIDQual, Name)
        if (length(difNonIDQual) > 0) {
            stop(paste0('[RepoReadWrite::is.validXLSX] The following non-unit qualifiers (NonIDQual) in sheet "', sName, '" are not in the sheet VarSpec: ', 
                        paste0(difNonIDQual, collapse = ', '),
                        '.\n'))
        }
        
        
        difIDDD <- setdiff(IDDD, Name)
        if (length(difIDDD) > 0) {
            stop(paste0('[RepoReadWrite::is.validXLSX] The following variables (IDDD) in sheet "', sName, '" are not in the sheet VarSpec: ', 
                        paste0(difIDDD, collapse = ', '),
                        '.\n')) 
        }
        if (verbose) cat(' ok.\n')
    }    
    
    if (verbose) cat('\n[RepoReadWrite::is.validXLSX] Checking consistency in length of qualifiers between column Length of VarSpec and sheet ...')
    QualLength <- data.table(Qual = character(0), Length = character(0))
    for (sName in varSheetNames) {
        
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
        
        stop(paste0('[RepoReadWrite::is.validXLSX] The following qualifiers do not have consistent lengths between the sheet VarSpec and the rest of sheets: ', 
                    paste0(InconsistLength, collapse = ', '), 
                    '.\n'))
        
    }
    if (verbose) cat(' ok.\n')
    
    if (verbose) cat('\n[RepoReadWrite::is.validXLSX] Checking consistency in the number of qualifiers for each variable in sheet ...')
    IDDD.list <- lapply(varSheetNames, function(sName){
        
                    IDDD <- ExcelSheets.list[[sName]][['IDDD']]
                    IDDD <- IDDD[!is.na(IDDD) & IDDD != '']
                    IDDD <- unique(IDDD)
                })
    names(IDDD.list) <- varSheetNames
    
    IDDDComm <- Reduce(c, IDDD.list)
    IDDDComm <- IDDDComm[duplicated(IDDDComm)]
    
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
                
                stop(paste0('[[RepoReadWrite::is.validXLSX] Variable (IDDD) ', iddd, ' has not all its non-unit qualifiers (NonIDQual) in sheet ', sheet))
            }
        }
    }
    if (verbose) cat(' ok.\n')

    if (verbose) cat('\n[RepoReadWrite::is.validXLSX] Checking consistency in the order of qualifiers in all sheet ...')
    Qual.list <- list()
    
    for (sName in varSheetNames) {
        
        colNames <- ExtractNames(names(ExcelSheets.list[[sName]]))
        IDQual <- colNames[colNames %in% idQuals_IDDD]
        NonIDQual <- colNames[colNames %in% nonIDQuals_IDDD]
        
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
                
                stop(paste0('[[RepoReadWrite::is.validXLSX] The order of qualifiers in sheet ', names(Qual.list)[indexsName], ' and ', names(Qual.list)[indexOthersName], ' is not consistent.'))
            }
        }
    }
    if (verbose) cat(' ok.\n')
    
    if (verbose) cat('\n[RepoReadWrite::is.validXLSX] Checking existence of column TipoMicrodato in sheet ...')
    for (sName in varSheetNames) {
        
        sheet <- ExcelSheets.list[[sName]]
        NamesSheet <- ExtractNames(names(sheet))
        if (!'TipoMicrodato' %in% NamesSheet) {
            
            stop(paste0('[RepoReadWrite::is.validXLSX] Qualifier TipoMicrodato is missing in sheet ', sName, '.'))
            
        }
    }
    if (verbose) cat(' ok.\n')
    
    
    if (verbose) cat('\n[RepoReadWrite::is.validXLSX] Checking absence of missing values in qualifier TipoMicrodato ...')
    for (sName in varSheetNames) {
        
        sheet <- ExcelSheets.list[[sName]]
        indexTipoMicrodato <- grep('TipoMicrodato', names(sheet))
        TipoMicrodato <- sheet[[indexTipoMicrodato]]
        if (any(is.na(TipoMicrodato)) | any(TipoMicrodato == '')) {
            
            stop(paste0('[RepoReadWrite::is.validXLSX] There are missing values in column TipoMicrodato in sheet ', sName))
            
        }
        
    }
    if (verbose) cat(' ok.\n')
    
    if (verbose) cat('\n[RepoReadWrite::is.validXLSX] Checking consistency of qualifier TipoMicrodato for each variable among all sheet ...')
    UnitNameTipo <- list()
    for (sName in varSheetNames) {
        
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
        
        stop(paste0('[RepoReadWrite::is.validXLSX] The following variables do not have consistent TipoMicrodato qualifier: ', paste0(NotUniqueTipo[['UnitName']], collapse = ' ,')))
        
    }
    if (verbose) cat(' ok.\n')
##    
    if (verbose) cat('\n[RepoReadWrite::is.validXLSX] Checking correct syntax of columns UnitNames with .. qualifiers ...')
    for (sName in varSheetNames) {
        
        if (verbose) cat(paste0('  ', sName, '...'))
        
        sheet <- ExcelSheets.list[[sName]]
        if (dim(sheet)[1] == 0) next
        DoubleDot <- as.logical(rowSums(as.matrix(sheet == '..')))
        DoubleDotUnitNames <- sheet[DoubleDot][['UnitName']]
        MetaUnitNames <- DoubleDotUnitNames[grepl('[', DoubleDotUnitNames, fixed = TRUE)]
        WrongUnitNames <- setdiff(DoubleDotUnitNames, MetaUnitNames)
        
        if (length(WrongUnitNames) != 0) {
            
            stop(paste0('[RepoReadWrite::is.validXLSX] The following UnitNames in sheet ', sName, ' are malformed: ', paste0(WrongUnitNames, collapse = ', '), ' (check square brackets).'))
            
        }
        
        if (verbose) cat(' ok.\n')
    }
    
    if (verbose) cat('Check manually the expression inside each square bracket!\n')
    
    
    # if (verbose) cat('\n[RepoReadWrite::is.validXLSX] Checking no duplication in IDDD + Qualifiers ...')
    # for (sName in varSheetNames) {
    #     
    #     sheet <- ExcelSheets.list[[sName]]
    #     NamesSheet <- unlist(lapply(names(sheet), ExtractNames))
    #     setnames(sheet, names(sheet), NamesSheet)
    #     
    #     IDQual <- sheet[['IDQual']]
    #     IDQual <- IDQual[!is.na(IDQual) & IDQual != '']
    #     
    #     NonIDQual <- sheet[['NonIDQual']]
    #     NonIDQual <- NonIDQual[!is.na(NonIDQual) & NonIDQual != '']
    #     
    #     sheet <- sheet[!(IDQual != '' | NonIDQual != '')]
    #     
    #     byKey <- c('IDDD', IDQual, NonIDQual)
    #     DTdup <- sheet[duplicated(sheet, by = byKey), ..(byKey)]
    # 
    #     if (dim(DTdup)[1] != 0) {
    #         
    #         stop(paste0('[RepoReadWrite::is.validXLSX] The following variables in sheet ', sName, ' are duplicated: ', DTdup, '\n\n'))
    #         
    #     }
    # }
    # if (verbose) cat(' ok.\n')
    
    
    if (verbose) cat('\n[RepoReadWrite::is.validXLSX] Checking no missing and invalid values in column InFiles ...')
    for (sName in varSheetNames) {
      
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
      InFiles <- InFiles[(nquals + 1) : (nquals + length(IDDD))]
      
      if (any(is.na(InFiles)) | any(InFiles == '')) {
        
        stop(paste0('[RepoReadWrite::is.validXLSX] There are missing values in column InFiles in sheet ', sName))
        
      }
      
      InFilesValues <- unique(unlist(lapply(InFiles, function(x){
        
          fileTypes <- unlist(strsplit(x, ','))
          fileTypes <- trim(fileTypes)
      })))
      invalidInFilesValues <- InFilesValues[!InFilesValues %chin% c('FI', 'FG', 'FD', 'FF', 'FP', 'FL', 'FT')]
      if (length(invalidInFilesValues) > 0) {
        
        stop(paste0('[RepoReadWrite::is.validXLSX] The following values in column InFiles of sheet ', sName, ' are invalid: ', paste0(invalidInFilesValues, collapse = ' ,'), '.\n'))
        
      }
    }
    if (verbose) cat(' ok.\n')
    
    
    if (verbose) cat(paste0('\n                       THE EXCEL FILE IS VALID!\n\n'))
    return(TRUE)
    
}

