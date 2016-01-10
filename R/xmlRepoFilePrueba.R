#' Lectura de un fichero xml...
#' 
#' \code{xmlRepoFilePrueba} devuelve un.....
#' 
#'
#' @import data.table
#'
#' @export
    xmlRepoFilePrueba <- function(FileName) {
    doc <- read_xml(FileName)
    children <- xml_children(doc) #list(52)
    childrenNodes <- lapply(children,xml_children) #list(52) of lists with variables
    varChildren <- unlist(lapply(childrenNodes,length))
    numVar <- max(varChildren)
    for (i in c(1:length(varChildren)))
    {
      if (varChildren[i] == numVar){
        childrenMax <- i
        break
      }
    }
    
    childrenMax <- childrenNodes[childrenMax] #list of a list with variables
    childrenMax <- childrenMax[[1]] #list of variables
    variables <- lapply(1:numVar, function(x) childrenMax[[x]]) #list(13) of variables
    
}