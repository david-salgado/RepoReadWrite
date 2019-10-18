#' @title Create the name of the next version of a file 
#'
#' @description \code{RepoNextFileVerNumber} returns the name of the next version of a file in a 
#' directory.
#' 
#' This function takes every file corresponding to the input file sort \code{FileType} for the input 
#' periods \code{Periods} in the input directory \code{Path} and generates for each of them the name 
#' of their next version.
#'
#' @param Periods Character vector with time periods included in the file names.
#' 
#' @param Path Character vector with the path of the search directory.
#' 
#' @param FileType Character vector with the sort (FF, FD, FG, DD, ...) of the files.
#' 
#' @param Base Character vector of length 1 with the year ('aaaa') of the base to which data are
#' referred. If it has no sense, "Base" is a empty character vector.
#'  
#' @param DorP Character vector with the type of version (D or P) of the files.
#' 
#' @return It returns a character vector of length equal to the length of \code{Periods} with the 
#' next version of each input file name.
#'
#' @examples
#' \dontrun{
#' RepoNextFileVerNumber(c('MM012016'), 'C:/Repo/E30183', 'FF', '2015')
#' }
#' 
#' @export
RepoNextFileVerNumber <- function(Periods, Path, FileType, Base, DorP = 'D'){
  
  Periods.RepoTime <- try(newRepoTime(Periods))
  if (inherits(Periods.RepoTime, 'try-error')) stop('[RepoReadWrite::RepoNextFileVerNumber] The time period does not have a valid format. Please, introduce a valid period.\n\n')
  
  if (gregexpr('2[0-9]{3}', Base) == -1 & Base != '') stop('[RepoReadWrite::RepoNextFileVerNumber] The year for the parameter Base is not correct. Please, introduce a valid year.\n\n')
  
  if (length(FileType) != 1) stop('[RepoReadWrite::RepoNextFileVerNumber] The input parameter FileType must be a character vector of length 1.\n')
  
  if (!FileType %in% c('FF', 'FD', 'FG', 'FA', 'FI', 'FP', 'FT', 'FL')) stop('[RepoReadWrite::RepoNextFileVerNumber] The allowed file types are FF, FD, FG, FA, FI, FP, FT, FL.\n')
      
  if (DorP != 'D' & DorP != 'P') stop('[RepoReadWrite::RepoNextFileVerNumber] DorP must be D or P.\n')
  
  Files <- list.files(Path)
  Files <- Files[grep(FileType, Files)]
  if (Base != '') Files <- Files[grep(paste0('B', substr(Base, 3, 4)), Files)]

  if (length(Files) == 0) {
      
      if (Base != '') warning(paste0('[RepoReadWrite::RepoNextFileVerNumber] Check if Base should be blank or not.'))
          
      matchFF <- pmatch('FF', FileType)
      if (!is.na(matchFF) & matchFF == 1) NextVer <- rep(paste0('.', DorP, '_1'), length(Periods))
      matchFA <- pmatch('FA', FileType)
      if (!is.na(matchFA) & matchFA == 1) NextVer <- rep(paste0('.', DorP, '_1'), length(Periods))
      matchFI <- pmatch('FI', FileType)
      if (!is.na(matchFI) & matchFI == 1) NextVer <- rep(paste0('.', DorP, '_1'), length(Periods))
      matchFP <- pmatch('FP', FileType)
      if (!is.na(matchFP) & matchFP == 1) NextVer <- rep(paste0('.', DorP, '_1'), length(Periods))
      matchFD <- pmatch('FD', FileType)
      if (!is.na(matchFD) & matchFD == 1) NextVer <- rep(paste0('.', DorP, '_1'), length(Periods))
      matchFG <- pmatch('FG', FileType)
      if (!is.na(matchFG) & matchFG == 1) NextVer <- rep(paste0('.', DorP, '_1'), length(Periods))
      
      names(NextVer) <- Periods
      
      return(NextVer)
  }
  
  nextVersions <- vector(mode = 'integer', length = length(Periods))
  names(nextVersions) <- Periods
  for (Per in Periods) {
    
    perFiles <- Files[grep(paste0(Per, '.[', DorP, ']_'), Files)]
    
    if (length(perFiles) == 0) nextVersions[Per] <- 1
    if (length(perFiles) != 0) {
      
      nextVersions[Per] <- max(sapply(perFiles, function(fileName){

        as.integer(strsplit(fileName, split = paste0(Per, '.[', DorP, ']_'))[[1]][2]) + 1
      
      }))
      
    }
  }
  
  nextVersions <- paste0('.', DorP, '_', nextVersions)
  names(nextVersions) <- Periods
  
  return(nextVersions)
  
}
