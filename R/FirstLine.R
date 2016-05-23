#' @title Generate the first line of a file
#' 
#' @description \code{FirstLine} returns a character \code{vector} of length 1 with the first line 
#' to appear in the file for the input object.
#' 
#' This function takes an object of class \linkS4class{StQ} as an input parameter and returns the 
#' first line to appear in the corresponding file when the input object is to be written (see 
#' function \link{WriteRepoFile}). The information contained in this first line is essentially the 
#' schema of the file. 
#' 
#' @param object Objeto de clase \code{\linkS4class{StQ}}.
#' 
#' @return \code{Character} \code{vector} of length 1 with the variables in the specified \code{StQ} 
#' object and theirs corresponding formats, that is, with the register desing of the file. Besides, 
#' it includes \code{NIV} as first variable, with value specified as input param, and \code{M} as 
#' final variable corresponding to the maximum register length in the file.
#' 
#' @examples
#' data(ExampleStQ)
#' FirstLine(ExampleStQ)
#' 
#' @seealso \link{ReadRepoFile}, \link{WriteRepoFile}
#' 
#' @rdname FirstLine
#' 
#' @export
setGeneric("FirstLine", function(object){standardGeneric("FirstLine")})
#' @rdname FirstLine
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "FirstLine",
    signature = c("data.table"),
    function(object){
        
        trim <- function(x){gsub(pattern = " ", replacement = "", x = x, 
                               useBytes = T, fixed = T)}
        
        Types <- lapply(object, class)
        Types <- unlist(lapply(Types, 
                               function(vec){
                                   if (vec == 'character') {
                                   
                                       '$'
                                       
                                   } else {
                                       
                                   ''
                                   
                                   }
                               }))
        
        NIV <- length(names(object)) - 2
        
        Lengths <- c()
        for (Col in names(object)) {
      
            if (class(object[[Col]]) == 'character') {
              
              aux <- max(nchar(object[[Col]]), na.rm = T)
              aux <- paste0(aux, '.')
              
            } else {
              
              auxCol <- trim(format(object[[Col]]))
              ColAsChar <- as.character(auxCol)
              ColNchar <- lapply(strsplit(as.character(ColAsChar), '\\.'), nchar)
    
              Integer <- max(unlist(lapply(ColNchar, '[', 1)), na.rm = T)
    
              if (all(unlist(lapply(ColNchar, length)) == 1)) {
                
                Decimal <- 0
                
              } else {
                
                Decimal <- max(unlist(lapply(ColNchar, '[', 2)), na.rm = T)
                
              }
    
              Integer <- Integer + Decimal + 1
              aux <- paste0(Integer, '.', Decimal)
    
            }
    
            Lengths <- c(Lengths, aux)
        }
    
        Types <- paste0(Types, Lengths)
          
        M <- ceiling(sum(as.numeric(Lengths)))      
        Var <- paste0(names(object), '=', Types)
        out <- paste0(Var, collapse = ',')
        out <- paste0(paste0('NIV=', as.character(NIV), ','), out)
        out <- paste0(out, paste0(',M=', M))
        
        out <- gsub('Valor', 'Value', out)
        
        return(out) 
    
    }
)
#' @rdname FirstLine
#' 
#' @import StQ 
#' 
#' @export
setMethod(
    f = "FirstLine",
    signature = c("StQ"),
    function(object){
        
        out <- FirstLine(getData(object))
            
        return(out) 
        
    }
)
