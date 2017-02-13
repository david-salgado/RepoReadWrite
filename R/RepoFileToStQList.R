#' @title Produce an object of class \linkS4class{StQList} from files with key-value pair structure 
#' 
#' @description \code{RepoFileToStQList} returns an object of class \linkS4class{StQList} from files
#' with key-value pair structure.
#' 
#' @param SurveyCode Character vector of length 1 with the code of each survey.
#' 
#' @param IniPeriod Character vector of length 1 with the initial time period to be read (in the 
#' repository notation).
#' 
#' @param FinPeriod Character vector of length 1 with the final time period to be read (in the 
#' repository notation).
#' 
#' @param FileTypes Character vector of length 1 with the type of the file to be read (FF, FG or FD).
#' 
#' @param RepoPath Character vector of length 1 with the path of the repository from which files are
#' to be read.
#' 
#' @param Rot Logical vector of length 1 indicating whether rotated sample files are to be included 
#' (default value FALSE).
#' 
#' @param includeFI Logical vector of length 1 indicating whether ID variables will be included in
#' the  slot of class \linkS4class{VarNameCorresp} of the \linkS4class{DD} object.
#' 
#' @param perl Logical vector of length 1 indicating whether Perl is installed in the system or not.
#' 
#' @param sep Logical vector of length 1 containing the combination of characters used as separator 
#' in the input file (default value @@).
#' 
#' @param encoding Default value is "unknown". Other possible options are "UTF-8" and "Latin-1". 
#' Note: it is not used to re-encode the input, rather enables handling of encoded strings in their 
#' native encoding.
#' 
#' @return Object of class \linkS4class{StQList}.
#' 
#' @examples
#' \dontrun{
#'  RepoFileToStQList('E30183', 'N:/UDMTD/UDTMDCOM/DepSel.Repositorio/DemIRIAaSP_IASS/E30183/', 
#'                     'FF', 'MM022016', 'MM022016')
#' }
#'
#' @include RepoXLSToVNC.R ReadRepoFile.R RepoDDToDD.R ReadRepoFile.R
#' 
#' @import data.table RepoTime
#' 
#' @export
RepoFileToStQList <- function(SurveyCode, RepoPath, FileTypes, IniPeriod, FinPeriod, Rot = FALSE, 
                              includeFI = TRUE, perl = FALSE, sep = '@@', encoding = 'unknown'){
    
        if (!FileTypes %in% c('FF', 'FD', 'FG')) {
            
            stop('[StQ::RepoFileToStQList] Only FI, FF, FG or FD files are allowed.')
        }
        
        ## Validation
  
        if (gregexpr('E[0-9]{5}', SurveyCode) == -1) stop('The survey code must of the form Ennnnn. Please, introduce a valid code.\n\n')
        cat(paste0(SurveyCode, '::: The survey code is ', SurveyCode, '.\n\n'))
        
        IniPeriod.RepoTime <- try(newRepoTime(IniPeriod))
        if (inherits(IniPeriod.RepoTime, 'try-error')) stop(paste0(SurveyCode, '::: The initial time period does not have a valid format. Please, introduce a valid period.\n\n'))
        cat(paste0(SurveyCode, '::: The initial time period is ', IniPeriod, '.\n\n'))
        
        FinPeriod.RepoTime <- try(newRepoTime(FinPeriod))
        if (inherits(FinPeriod.RepoTime, 'try-error')) stop(paste0(SurveyCode, '::: The final time period does not have a valid format. Please, introduce a valid period.\n\n'))
        cat(paste0(SurveyCode, '::: The final time period is ', FinPeriod, '.\n\n'))
        
        if (substr(FileTypes, 1, 1) %in% c("'", '"')) FileTypes <- substr(FileTypes, 2, nchar(FileTypes) - 1)
        FileTypes <- gsub('~', ' ', FileTypes)
        FileTypes <- strsplit(FileTypes, split = ',')[[1]]
        FileTypes <- stringi::stri_trim_both(FileTypes)
        if (!all(FileTypes %in% c('FF', 'FD', 'FG', 'FI', 'FP', 'FL', 'FT'))) stop(paste0(SurveyCode, '::: The types of the files to read must be FF, FD, FG, FI, FP, FL or FT. Please, introduce a valid type.\n\n'))
        cat(paste0(SurveyCode, '::: The types of the files to read are ', paste0(FileTypes, collapse = ', '), '.\n\n'))
        
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
          for (FileType in FileTypes) {
            
            FileNames.local <- list.files(RepoPath, paste0(FileType, '_V[1-9][0-9]*.', MonthsNamesM[Month.index]))
            
            if (length(FileNames.local) != 0) {
              
              out <- c(out, FileNames.local)
              
            }
          }  
          return(out) 
        })
        FileNames <- unlist(FileNames)
        
        
        ## DD Construction
        cat(paste0(SurveyCode, '::: Generating DDs for the survey... '))
        Version <- unlist(lapply(FileNames, function(Name){
          
          strsplit(strsplit(Name, '.', fixed = TRUE)[[1]][2], '_V')[[1]][2]
        }))
        
        
        DD.list <- lapply(unique(Version), function(vers){
          
          VNC <- RepoXLSToVNC(paste0(RepoPath, SurveyCode, '.NombresVariables_V', vers, '.xlsx'))
          DDName <- paste0(RepoPath, SurveyCode, '.DD_V', vers)
          out <- RepoDDToDD(DDName, VNC)
          return(out)
        })
        names(DD.list) <- unique(Version)
        
        
        ## Read files
        StQ_Files <- list()
        for (FileType in FileTypes) {
          
          cat(paste0(SurveyCode, '::: Reading files ', FileType, '...\n'))
          
          StQ_Files[[FileType]] <- lapply(seq(along = MonthsNamesM), function(Month.index){
            
            NoFiles <- FALSE
            FileNames.local <- FileNames[grep(FileType, FileNames)]
            FileNames.local <- FileNames.local[grep(MonthsNamesM[Month.index], FileNames.local)]
            if (length(FileNames.local) == 0) NoFiles <- TRUE
            
            if (FileType == 'FF') {
              
              FFVersion <- lapply(FileNames.local, function(Name){
                
                out <- strsplit(Name, 'D_')[[1]][2]
                return(out)
              })
              
              ThisFileVersion <- which.max(FFVersion) 
              FFName <- FileNames.local[ThisFileVersion]
              DDVersion <- strsplit(strsplit(FFName, '.', fixed = TRUE)[[1]][2], '_V')[[1]][2]
              cat(paste0('     file ', FFName, '...ok\n'))
              FFName <- paste0(RepoPath, FFName)
              DDFile <- DD.list[[DDVersion]]
              out <- ReadRepoFile(FFName, DDFile)
              
              
              #out <- list(out)
              names(out) <- FileNames.local[ThisFileVersion]
              output <- list(DataMatrix = out, NoFiles = NoFiles)
              return(output)
              
            } 
            
            if (FileType %in% c('FI', 'FP', 'FG', 'FD')) {
              
              DataFile <- lapply(seq(along = FileNames.local), function(i){
                
                FileName <- FileNames.local[i]
                cat(paste0('     file ', FileName, '...ok\n'))
                FileName <- paste0(RepoPath, FileName)
                DDVersion <- strsplit(strsplit(FileName, '.', fixed = TRUE)[[1]][2], '_V')[[1]][2]
                DDFile <- DD.list[[DDVersion]]
                out <- ReadRepoFile(FileName, DDFile)
                
                return(out)
                
              })
              
              names(DataFile) <- FileNames.local
              output <- list(DataMatrix = DataFile, NoFiles = NoFiles)
              return(output)
              
            }
            
          })
          cat(' ok;\n')
        }
        
        
        ## Missing files
        NoFiles <- lapply(names(StQ_Files), function(FileType){
          
          out <- unlist(lapply(StQ_Files[[FileType]], '[[', 'NoFiles'))
          FaltanPeriodos <- MonthsNamesM[out]
          if (length(FaltanPeriodos) > 0) {
            
            cat(paste0(SurveyCode, '::: Files ', FileType, ' missing for the following periods: ', paste0(FaltanPeriodos, collapse = ', '), '.\n\n'))
            
          }
          return(out)
        })
        names(NoFiles) <- names(StQ_Files)
        
        
        ## StQList construction
        StQ_Files <- lapply(names(StQ_Files), function(FileType){
          
          out <- lapply(StQ_Files[[FileType]], '[[', 'DataMatrix')
          return(out)
        })
        StQ_Files <- StQ_Files[[1]]
        names(StQ_Files) <- MonthsNamesM
        
        cat('...ok.\n\n')
        
        cat(paste0(SurveyCode, '::: Data have been read successfully.\n\n'))

        StQList <- BuildStQList(StQ_Files)
        
        return(StQList)
        
  }
