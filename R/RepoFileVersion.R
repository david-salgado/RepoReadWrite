#' @title Return index number of the last version of a file
#'
#' @description \code{RepoFileVersion} returns the index number of the last version of a file in an 
#' input directory.
#' 
#' This function searches in the input directory \code{Path} the last version of the input file name 
#' \code{FileNamePattern} and returns the corresponding index number of such version.
#' 
#' This function is thought for files whose names has a suffix with the version corresponding to its 
#' contents (either definitive or partial version).
#'
#' @param Path Character vector of length 1 with the path of the search directory.
#' 
#' @param FileNamePattern Character vector of length 1 with the name of the file whose last version 
#' is queried.
#'  
#' @return Integer vector of length 1 expressing the last version index number of the input file 
#' name in the input search directory.
#'
#' @examples
#' \dontrun{
#' RepoFileVersion('T:/E30183', 'FF_V1.MM122014')
#' RepoFileVersion('T:/E30183', 'FT_V1.MM122016')
#' RepoFileVersion('T:/E30183', 'FT_V1.MM122016')
#' }
#' 
#' @export
RepoFileVersion <- function(Path, FileNamePattern){
  
  if (length(FileNamePattern) != 1) stop('\n[RepoReadWrite::RepoFileVersion] The input parameter FileNamePattern must be a character vector of length 1.\n')
  
  FTpattern <- grep('FT_V', FileNamePattern)
  if (length(FTpattern) != 0) {
      
      cat('\n[RepoReadWrite::RepoFileVersion] FT files do not have version number.\n')
      return(character(0))
      
  }
  FLpattern <- grep('FL_V', FileNamePattern)
  if (length(FLpattern) != 0) {
      
      cat('\n[RepoReadWrite::RepoFileVersion] FL files do not have version number.\n')
      return(character(0))
      
  }

  Files <- list.files(Path)
  if (length(Files) == 0) stop('[RepoReadWrite::RepoFileVersion] Path not found.')
  Files <- Files[grep(FileNamePattern, Files)]
  if (length(Files) == 0) stop('[RepoReadWrite::RepoFileVersion] No files with this name pattern in this path.')
  nVer <- unlist(lapply(as.list(Files), function(x){substr(x, nchar(x), nchar(x))}))
  output <- max(nVer)
  return(output)
  
}
