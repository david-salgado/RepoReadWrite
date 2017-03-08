#' @title Write a key-value pair ASCII file
#' 
#' @description \code{WriteRepoFile} writes the input key-value object in a fixed-width column ASCII 
#' file. 
#' 
#' This method reads the input object \code{object} and writes its content as a fixed-width column 
#' ASCII file with key-value pair structure with the specified input name \code{Name}. 
#' 
#' @param object Object to be written in the output file.
#' 
#' @param Name Character vectorof length 1 specifying the name of the output file. The file will be 
#' written in the working directory (see \link[base]{getwd}) unless the full path is specified.
#' 
#' @param sep Logical vector of length 1 containing the combination of characters used as separator 
#' in the file (default value @@).
#' 
#' @return These methods return the invisible \code{\link{NULL}} object writing as a side-effect the 
#' corresponding output file.
#' 
#' @examples
#' \dontrun{
#' # To write the output file in the administrator desktop:
#' Name <- 'C:/Users/Administrador/Desktop/E30103.FF_V1.MM032014.D_1'
#' WriteRepoFile(ExampleStQ, Name)
#' }
#' 
#' @seealso \code{\link{ReadRepoFile}}
#' 
#' @import data.table
#' 
#' @importFrom StQ getData StQTorawStQ getPeriods
#' 
#' 
#' @export
setGeneric("WriteRepoFile", function(object, Name, sep = '@@'){standardGeneric("WriteRepoFile")})


#' @rdname WriteRepoFile
#' 
#' @export
setMethod(
    f = "WriteRepoFile",
    signature = c("rawStQ"),
    function(object, Name, sep = '@@'){
        
        if (length(Name) != 1) {
            
            warning('\n[RepoReadWrite::WriteRepoFile] Only the first name will be used.\n')
        }
        Name <- Name[1]
        
        if (length(sep) != 1) stop('[RepoReadWrite::WriteRepoFile] The input parameter sep must a character vector of length 1.\n')
        
        # Este paso es necesario porque la función fwrite no admite separadores compuestos
        auxDT <- getData(object)
        colNames <- names(auxDT)
        setDT(auxDT)[, ROW := Reduce(function(...) paste(..., sep = sep), .SD[, mget(colNames)])]
        auxDT[, (setdiff(colNames, 'ROW')) := NULL]
        
        fwrite(auxDT, file = Name, quote = FALSE, na = ' ', row.names = FALSE, col.names = FALSE)
        cat(paste0('\nKey-value pair file written in ', Name), '\n')
        
        
        return(invisible(NULL))
        
    }
)
#' @rdname WriteRepoFile
#' 
#' @export
setMethod(
    f = "WriteRepoFile",
    signature = c("StQ"),
    function(object, Name, sep = '@@'){
        
        object <- StQTorawStQ(object)
        WriteRepoFile(object = object, Name = Name, sep = sep)
        return(invisible(NULL))
        
    }
)

#' @rdname WriteRepoFile
#' 
#' @export
setMethod(
    f = "WriteRepoFile",
    signature = c("StQList"),
    function(object, Name, sep = '@@'){
        
        Periods <- getPeriods(object)
        if (length(Periods) > 0) {
            
            if (length(Periods) != length(Name)) {
                
                stop(paste0('[RepoReadWrite::WriteRepoFile] ', object, ' and ', Name, ' must have the same length.\n'))
                
            }
            for (i in seq(along = Periods)) {
                
                WriteRepoFile(object = object[[i]], Name = Name[i], sep = sep) 
            }
        }

        return(invisible(NULL))
        
    }
)


#' @rdname WriteRepoFile
#' 
#' @export
setMethod(
    f = "WriteRepoFile",
    signature = c("rawStQList"),
    function(object, Name, sep = '@@'){
        
        Periods <- getPeriods(object)
        if (length(Periods) > 0) {
            
            if (length(Periods) != length(Name)) {
                
                stop(paste0('[RepoReadWrite::WriteRepoFile] ', object, ' and ', Name, ' must have the same length.\n'))
                
            }
            for (i in seq(along = Periods)) {
                
                WriteRepoFile(object = getData(object)[[i]], Name = Name[i], sep = sep) 
            }
        }
        
        return(invisible(NULL))
        
    }
)
