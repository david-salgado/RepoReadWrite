#' @title Produce an object of class \linkS4class{DD} from an xml DD file
#' 
#' @description This function is a constructor for the class \linkS4class{DD} 
#' using the contents of a DD file in xml format.
#' 
#' \code{xmlDDToDD} reads and transforms an xml file with the content of a DD 
#' file into an object of class \linkS4class{DD}. 
#' 
#' @param xmlDD \link{data.frame}?? with the content of the file \code{DD}.
#'  
#' @return Object of class \linkS4class{DD}.
#' 
#' @examples
#' data(RepoDD)
#' RepoDDToDD(RepoDD)
#' 
#' @import data.table
#'       
#' @export
#' Lectura de un fichero xml...
#' 
#' \code{xmlDDToDD} devuelve un.....
#' 
#' @export
xmlDDToDD <- function(xmlDD){
      variable <- unlist(lapply(xmlDD, function(x) x[1,1])) # Vector con los nombres de las variables
      class <- unlist(lapply(xmlDD, function(x) x[3,1])) # Vector con los tipos de las variables
      varQual <- c() # Lista con los elementos de xmlDD que tienen al menos un calificador
      varQ <- c() # Vector con el número de los elementos de xmlDD que tienen al menos un calificador
      varQual <- xmlDD[lapply(xmlDD,ncol) > 1]
      
      
      nomQual <- as.list(lapply(varQual, function(x) x[3]))
      nomQual <- lapply(nomQual,function(x) x[,1][!is.na(x[,1])])
      nummaxQual <- max(unlist(lapply(nomQual,length)))
      
      Qual <- vector('character', length(xmlDD) * nummaxQual)
      
      for (i in seq(along = xmlDD)){
        
        if(ncol(xmlDD[[i]]) > 1) varQ <- c(varQ,i)
      }
      
      
    }
