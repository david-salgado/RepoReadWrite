#' @title Return index number of the last version of a file
#'
#' @description \code{RepoTopn} returns the index number of the last version of 
#' a file in an input directory.
#' 
#' This function searches in the input directory \code{Path} the last version of 
#' the input file name \code{FileNameString} and returns the corresponding index
#' number of such version.
#' 
#' This function is thought for files whose names has a suffix the version 
#' corresponding to its contents (either definitive or partial version).
#'
#' @param Path Character vector of length 1 with the path of the search 
#' directory.
#' 
#' @param FileNameString Character vector of length 1 with the name of the file
#' whose last version is queried.
#'  
#' @return Integer vector of length 1 expressing the last version index number
#' of the input file name in the input search directory.
#'
#' @examples
#' \dontrun{
#' RepoFileVersion('C:/Repo//E30183', 'FF_V1.MM122014')
#' }
#' 
#' @export
RepoFileVersion <- function(Path, FileNamePattern){
  
  Files <- list.files(Path)
  if (length(Files) == 0) stop('[RepoReadWrite::RepoFileVersion] Path not found.')
  Files <- Files[grep(FileNamePattern, Files)]
  if (length(Files) == 0) stop('[RepoReadWrite::RepoFileVersion] No files with this name pattern in this path.')
  nVer <- unlist(lapply(as.list(Files), function(x){substr(x, nchar(x), nchar(x))}))
  out <- max(nVer)
  
return(out)
  
}
