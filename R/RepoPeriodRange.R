#' @title Obtain the time periods of a given input file type
#'
#' @description \code{RepoPeriodRange} returns the time periods of files of a 
#' given input file type \code{Type} (FF, FD, FG, FL, FT) in an input directory
#'  \code{Path} up to a last given input time period \code{Last} (included). By 
#'  default, it returns all time periods in the queried directory unless the 
#'  input parameter \code{Last} is specified.
#' 
#' @param Path Character vector of length 1 specifying the path of the searching 
#' directory.
#' 
#' @param Type Character vector of length 1 specifying the type of file whose 
#' time periods are queried. It can take values \code{FF}, \code{FG}, \code{FD}, 
#' \code{FL} or \code{FT}.
#' 
#' @param Last Character vector of length 1 specifying the last time period. By 
#' default it is the last of all of them.
#'  
#' @return Character vector with as many components as time periods present in 
#' the input directory up to the last one specified (included).
#'
#' @examples
#' \dontrun{
#' RepoPeriodRange(Path = 'R:/E30183', Type = 'FF', Last = 'MM102014')
#' }
#' 
#' @export
RepoPeriodRange <- function(Path, Type, Last = out[length(out)]){

    Files <- list.files(Path)
    if (length(Files) == 0) stop('[RepoReadWrite::RepoPeriodRange] Path not found.')    
    Files <- Files[grep(Type, Files)]
    if (length(Files) == 0) stop('[RepoReadWrite::RepoPeriodRange] No file with this type in this path.')    
    Files.desc <- strsplit(Files, ".", fixed = TRUE)
    out <- lapply(Files.desc, '[', i = 3)
    ord <- lapply(out, function(x){
        aux <- substr(x, 3, nchar(x))
        out <- paste0(substr(aux, 3, 6), '-', substr(aux, 1, 2), '-01')
        out <- as.Date(out)
        return(out)
        })
    out <- unique(unlist(out)[order(unlist(ord))])
    Last.index <- which(out == Last)
    out <- out[1:Last.index]
    
    return(out)
}
