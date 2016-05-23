#' @title Obtain index number of the last version of a file in a directory
#'
#' @description \code{RepoTopn} returns the index number of the last version of a file identified 
#' with a file name in a given directory.
#' 
#' This function searches in the input directory \code{Path} for the last version of a file with 
#' file name containing the string \code{FileNameString} and returns the corresponding index number.
#'
#' @param Path Character vector of length 1 specifying the path of the searching directory.
#' 
#' @param FileNameString Character vector of length 1 specifying the string of the file name whose 
#' last version is to be searched.
#'  
#' @return Integer vector of length 1 with the index number of the last version of the file.
#'
#' @examples
#' \dontrun{
#' RepoTopn('R:/E30183', 'FF_V1.MM122014')
#' }
#' 
#' @export
RepoTopn <- function(Path, FileNameString){
  
  Files <- list.files(Path)
  Files <- Files[grep(FileNameString, Files)]
  if (length(Files) == 0) {
      
      stop('[RepoReadWrite::RepoTopn] No file with this name.')
      
  }
  
  nVer <- unlist(lapply(as.list(Files), function(x){substr(x, nchar(x), nchar(x))}))
  out <- max(nVer)
  
return(out)
  
}
