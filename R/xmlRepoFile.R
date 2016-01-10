#'#' Lectura de un fichero xml...
#' 
#' \code{xmlRepoFile} devuelve un.....
#' 
#'
#' @import data.table
#'
#' @export
    xmlRepoFile <- function(FileName) {
      doc <- xmlParse(FileName)
      nodes <- getNodeSet(doc, "//variable[@typeID]")
      dataFrameList <- lapply(nodes,xmlToDataFrame) #lista de dataFrames con  columna/s
    }