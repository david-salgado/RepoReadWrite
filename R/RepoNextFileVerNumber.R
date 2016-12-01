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
#' @return It returns a character vector of length equal to the length of \code{Periods} with the 
#' next version of each input file name.
#'
#' @examples
#' \dontrun{
#' RepoNextFileVerNumber(c('MM012016'), 'R:/E30183', 'FF')
#' }
#' 
#' @export
RepoNextFileVerNumber <- function(Periods, Path, FileType){
  
  Files <- list.files(Path)
  Files <- Files[grep(FileType, Files)]

  if (length(Files) == 0) {
      
      matchFF <- pmatch('FF', FileType)
      if (!is.na(matchFF)) NextVer <- rep('.D_1', length(Periods))
      
      matchFI <- pmatch('FI', FileType)
      matchFP <- pmatch('FP', FileType)
      matchFD <- pmatch('FD', FileType)
      matchFG <- pmatch('FG', FileType)
      matchFX <- sum(c(matchFI, matchFP, matchFD, matchFG), na.rm = TRUE)
      if (!is.na(matchFX) & matchFX == 1) NextVer <- rep('.P_1', length(Periods))
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
      
        if (length(FileType[grep('FF', FileType)]) > 0) aux <- matrix(c(Per, '.D_0'), ncol = 2)
        Types <- c('FI', 'FP', 'FD', 'FG')
        Flag.Type <- lapply(Types, function(Type){
          
                    length(FileType[grep(Type, FileType)])
                  })
        Flag.Type <- sum(unlist(Flag.Type))
        if (Flag.Type > 0) aux <- matrix(c(Per, '.P_0'), ncol = 2)
        
    } else if (length(aux) == 1) {
      
      aux <- aux[[1]]
      
    } else {
      
      aux <- Reduce(rbind, aux)
      
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
