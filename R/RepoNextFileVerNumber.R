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
#' @return It returns a character vector of length equal to the length of \code{Periods} with the 
#' next version of each input file name.
#'
#' @examples
#' \dontrun{
#' RepoNextFileVerNumber(c('MM012016'), 'C:/Repo/E30183', 'FF', '2015')
#' }
#' 
#' @export
RepoNextFileVerNumber <- function(Periods, Path, FileType, Base){
  
  Periods.RepoTime <- try(newRepoTime(Periods))
  if (inherits(Periods.RepoTime, 'try-error')) stop('[RepoReadWrite::RepoNextFileVerNumber] The time period does not have a valid format. Please, introduce a valid period.\n\n')
  
  if (gregexpr('2[0-9]{3}', Base) == -1 & Base != '') stop('[RepoReadWrite::RepoNextFileVerNumber] The year for the parameter Base is not correct. Please, introduce a valid year.\n\n')
  
  if (length(FileType) != 1) stop('[RepoReadWrite::RepoNextFileVerNumber] The input parameter FileType must be a character vector of length 1.\n')
  
  if (!FileType %in% c('FF', 'FD', 'FG', 'FA', 'FI', 'FP', 'FT', 'FL')) stop('[RepoReadWrite::RepoNextFileVerNumber] The allowed file types are FF, FD, FG, FA, FI, FP, FT, FL.\n')
      
  Files <- list.files(Path)
  Files <- Files[grep(FileType, Files)]
  if (Base != '') Files <- Files[grep(paste0('B', substr(Base, 3, 4)), Files)]

  if (length(Files) == 0) {
      
      matchFF <- pmatch('FF', FileType)
      matchFA <- pmatch('FA', FileType)
      matchFX <- sum(c(matchFF, matchFA), na.rm = TRUE)
      if (!is.na(matchFX) & matchFX == 1) NextVer <- rep('.D_1', length(Periods))
      
      matchFI <- pmatch('FI', FileType)
      matchFP <- pmatch('FP', FileType)
      matchFD <- pmatch('FD', FileType)
      matchFG <- pmatch('FG', FileType)
      matchFY <- sum(c(matchFI, matchFP, matchFD, matchFG), na.rm = TRUE)
      if (!is.na(matchFY) & matchFY == 1) NextVer <- rep('.P_1', length(Periods))
      names(NextVer) <- Periods
      
      return(NextVer)
  }
  
  SelFiles <- c()
  for (Per in Periods) {
    
    aux <- strsplit(Files[grep(Per, Files)], Per)
    aux <- lapply(aux, function(x){
        
      x[1] <- paste0(x[1], Per)
      return(x)
    
    })
    
    if (length(aux) == 0) {
      
        TypesX <- c('FF', 'FA')
        Flag.TypeX <- lapply(TypesX, function(Type){
          
          length(FileType[grep(Type, FileType)])
        })
        Flag.TypeX <- sum(unlist(Flag.TypeX))
        if (Flag.TypeX > 0) aux <- matrix(c(Per, '.D_0'), ncol = 2)
      
        TypesY <- c('FI', 'FP', 'FD', 'FG')
        Flag.TypeY <- lapply(TypesY, function(Type){
          
                    length(FileType[grep(Type, FileType)])
                  })
        Flag.TypeY <- sum(unlist(Flag.TypeY))
        if (Flag.TypeY > 0) aux <- matrix(c(Per, '.P_0'), ncol = 2)
        
    } else {
      
      aux <- Reduce(rbind, aux, init = aux[[1]])
      
    }
      
    SelFiles <- rbind(SelFiles, aux)
  }

  OrderedPeriods <- unique(SelFiles[, 1])
  rownames(SelFiles) <- NULL
  SelFiles <- as.data.frame(SelFiles)
  SelFiles <- split(SelFiles, SelFiles[, 1])[OrderedPeriods]
  NextVer <- lapply(SelFiles, function(df){
      
      nVer <- unlist(strsplit(as.character(df[nrow(df), 2]), '_'))
      nVer[2] <- as.integer(nVer[2]) + 1
      nVer <- paste0(nVer[1], '_', nVer[2])
      return(nVer)
  })
  
  names(NextVer) <- Periods
  NextVer <- unlist(NextVer)
  
  return(NextVer)
  
}
