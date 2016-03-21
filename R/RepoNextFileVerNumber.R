#' @title Create the name of the next version of a file
#'
#' @description \code{RepoNextFileVerNumber} returns the name of the next 
#' version of a file in a directory.
#' 
#' This function takes every file corresponding to the input file sort 
#' \code{FileType} for the input periods \code{Periods} in the input directory 
#' \code{Path} and generates for each of them the name of their next version.
#'
#' @param Periods Character vector with time periods included in the file names.
#' 
#' @param Path Character vector with the path of the search directory.
#' 
#' @param FileType Character vector with the sort (FF, FD, FG, DD, ...) of the
#' files.
#'  
#' @return It returns a character vector of length equal to the length of 
#' \code{Periods} with the full names of the next version of each input file 
#' name.
#'
#' @examples
#' \dontrun{
#' RepoTopn(Path, Type)
#' }
#' 
#' @export
RepoNextFileVerNumber <- function(Periods, Path, FileType){
  
  Files <- list.files(Path)
  Files <- Files[grep(FileType, Files)]
  if (length(Files) == 0) return(NULL)
  SelFiles <- c()
  for (Per in Periods){
    aux <- strsplit(Files[grep(Per, Files)], Per)
    aux <- lapply(aux, function(x){
      x[1] <- paste0(x[1], Per)
      return(x)
    })
    
    if (length(aux) == 0) {
      
        aux <- list(c(Per, '.D_0'))
    }
    
    
    if (length(aux) == 1) {
      
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
