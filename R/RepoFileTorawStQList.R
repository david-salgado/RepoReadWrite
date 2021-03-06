#' @title Produce an object of class \link{rawStQList} from files with key-value pair 
#' structure
#' 
#' @description \code{RepoFileTorawStQList} returns an object of class \link{rawStQList} from
#' files with the structure key-value pair.
#' 
#' @param SurveyCode Character vector of length 1 with the code of each survey.
#' 
#' @param IniPeriod Character vector of length 1 with the initial time period to be read (in the 
#' repository notation).
#' 
#' @param FinPeriod Character vector of length 1 with the final time period to be read (in the 
#' repository notation).
#' 
#' @param Base Character vector of length 1 with the year ('aaaa') of the base to which data are
#' referred. If it has no sense, "Base" is a empty character vector.
#' 
#' @param FileType Character vector of length 1 with the type of the file to be read (FI, FP, FF, FG
#'  or FD).
#' 
#' @param RepoPath Character vector of length 1 with the path of the repository from which files are
#' to be read.
#' 
#' @param Rot Logical vector of length 1 indicating whether rotated sample files are to be included 
#' (default value FALSE).
#' 
#' @param includeFI Logical vector of length 1 indicating whether ID variables will be included in
#' the  slot of class \code{\link[StQ]{VNC}} of the \link{DD} object.
#' 
##' @param perl Logical vector of length 1 indicating whether Perl is installed in the system or not.
#' 
#' @param sep Logical vector of length 1 containing the combination of characters used as separator 
#' in the input file (default value @@).
#' 
#' @param encoding Default value is "unknown". Other possible options are "UTF-8" and "Latin-1". 
#' Note: it is not used to re-encode the input, rather enables handling of encoded strings in their 
#' native encoding.
#' 
#' @return Object of class \link{rawStQList}.
#' 
#' @examples
#' \dontrun{
#'  RepoFileTorawStQList('E30163', 'T:/E30163/', 'FF', 'MM022016', 'MM032016', '', perl = TRUE)
#' }
#'
#' @include RepoXLSToDD.R ReadRepoFile.R
#' 
#' @import data.table RepoTime
#' 
#' @export
RepoFileTorawStQList <- function(SurveyCode, RepoPath, FileType, IniPeriod, FinPeriod, Base,  
                                 Rot = FALSE, includeFI = TRUE, perl = FALSE, sep = '@@',
                                 encoding = 'unknown'){
    
    StQ_Files <- IDDD <- Name <- QualType <- MetadataCode <- Type <- InFiles <- NULL
    ValueDescription <- ValueRegExp <- Name <- UnitName <- table_column <- filter <- NULL
    
    ## Validation
    
    if (length(FileType) != 1) {
        
        stop('[StQ::RepoFileToStQList] Only one FileType at a time is allowed.')
        
    }
    
    if (!FileType %in% c('FF', 'FD', 'FG', 'FI', 'FP', 'FL', 'FT')) {
        
        stop('[StQ::RepoFileToStQList] Only FI, FF, FG , FD, FP, FL or FT files are allowed.')
    }
    
    if (gregexpr('E[0-9]{5}', SurveyCode) == -1) stop('The survey code must of the form Ennnnn. Please, introduce a valid code.\n\n')

    
    IniPeriod.RepoTime <- try(newRepoTime(IniPeriod))
    if (inherits(IniPeriod.RepoTime, 'try-error')) stop('The initial time period does not have a valid format. Please, introduce a valid period.\n\n')
    
    FinPeriod.RepoTime <- try(newRepoTime(FinPeriod))
    if (inherits(FinPeriod.RepoTime, 'try-error')) stop('The final time period does not have a valid format. Please, introduce a valid period.\n\n')
    
    if (gregexpr('2[0-9]{3}', Base) == -1 & Base != '') stop('The year for the parameter Base is not correct. Please, introduce a valid year.\n\n')
    
    if (Rot != 'TRUE' & Rot != 'FALSE') stop(paste0(SurveyCode, '::: The parameter Rot must be TRUE or FALSE.\n\n'))
    
    
    
    ## Period construction
    cat(paste0(SurveyCode, '::: Generating sequence of time periods...'))
    IniRepoTime <- newRepoTime(IniPeriod)
    FinRepoTime <- newRepoTime(FinPeriod)
    Months <- Seq(IniRepoTime, FinRepoTime, Rot = Rot)
    MonthsNamesM <- getRepo(Months)
    cat(' ok.\n\n')
    
    ## Files names Construction
    FileNames <- lapply(seq(along = MonthsNamesM), function(Month.index){
        
        out <- c()
        if (FileType == 'FF' & Base != '') {
            
            FileNames.local <- list.files(RepoPath, paste0(FileType, '_B', substr(Base, 3, 4), '_V[1-9][0-9]*.', MonthsNamesM[Month.index]))
        } else {
            
            FileNames.local <- list.files(RepoPath, paste0(FileType, '_V[1-9][0-9]*.', MonthsNamesM[Month.index]))
        }

        if (length(FileNames.local) == 0) stop(paste0(SurveyCode, '::: Files ', FileType, ' for the period ', MonthsNamesM[Month.index], ' are missing.\n\n'))
        out <- c(out, FileNames.local)
        return(out) 
    })
    FileNames <- unlist(FileNames)
    
    
    ## DD Construction
    cat(paste0(SurveyCode, '::: Generating DDs for the survey... '))
    Version <- unlist(lapply(FileNames, function(Name){
        
        strsplit(strsplit(Name, '.', fixed = TRUE)[[1]][2], '_V')[[1]][2]
    }))
    
    
    DD.list <- lapply(unique(Version), function(vers){
        
        out <- RepoXLSToDD(paste0(RepoPath, SurveyCode, '.NombresVariables_V', vers, '.xlsx'))
        return(out)
    })
    names(DD.list) <- unique(Version)
    
    
    ## Read files
    rawStQ_Files <- list()
    cat(paste0(SurveyCode, '::: Reading files ', FileType, '...\n'))
    
    rawStQ_Files <- lapply(seq(along = MonthsNamesM), function(Month.index){
        
        NoFiles <- FALSE
        FileNames.local <- FileNames[grep(FileType, FileNames)]
        FileNames.local <- FileNames.local[grep(MonthsNamesM[Month.index], FileNames.local)]
        if (length(FileNames.local) == 0) NoFiles <- TRUE
        
        
        if (FileType %in% c('FL', 'FT')) {
            
            FileName <- FileNames.local
        } else {
            
            FileVersions <- lapply(FileNames.local, function(Name){
                
                if (FileType == 'FF') out <- strsplit(Name, '.D_', fixed = TRUE)[[1]][2]
                if (FileType %in% c('FD', 'FG', 'FI', 'FP')) out <- strsplit(Name, '.P_', fixed = TRUE)[[1]][2]
                return(out)
            })
            
            ThisFileVersion <- which.max(FileVersions) 
            FileName <- FileNames.local[ThisFileVersion]
        }
        
        DDVersion <- strsplit(strsplit(FileName, '.', fixed = TRUE)[[1]][2], '_V')[[1]][2]
        cat(paste0('     file ', FileName, '...ok\n'))
        FileName <- paste0(RepoPath, FileName)
        DDFile <- DD.list[[DDVersion]]
        out <- ReadRepoFile(FileName, DDFile, out = 'rawStQ', perl, sep, encoding)
        
        output <- list(DataMatrix = out, NoFiles = NoFiles)
        return(output)
        
    })
    
    cat(' ok;\n')
    
    ## Missing files
    MissPeriod <- unlist(lapply(StQ_Files, '[[', 'NoFiles'))
    FaltanPeriodos <- MonthsNamesM[MissPeriod]
    if (length(FaltanPeriodos) > 0) {
        
        cat(paste0(SurveyCode, '::: Files missing for the following periods: ', paste0(FaltanPeriodos, collapse = ', '), '.\n\n'))
        
    }
    
    ## rawStQList construction
    rawStQ_Files <-  lapply(rawStQ_Files, '[[', 'DataMatrix')
    names(rawStQ_Files) <- MonthsNamesM
    cat('...ok.\n\n')
    
    rawStQList <- BuildrawStQList(rawStQ_Files)
    cat(paste0(SurveyCode, '::: Data have been read successfully into an rawStQList.\n\n'))
    
    return(rawStQList)
    
}
