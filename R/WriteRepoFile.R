#' @title Write a key-value pair ASCII file
#' 
#' @description \code{WriteRepoFile} writes the input key-value object in a 
#' fixed-width column ASCII file. 
#' 
#' This method reads the input object \code{object} and writes its content as a 
#' fixed-width column ASCII file with key-value pair structure with the 
#' specified input name \code{Name}. 
#' 
#' @param object Object to be written in the output file.
#' 
#' @param Name Character vectorof length 1 specifying the name of the output 
#' file. The file will be written in the working directory 
#' (see \link[base]{getwd}) unless the full path is specified.
#' 
#' @return These methods return the invisible \code{\link{NULL}} object writing 
#' as a side-effect the corresponding output file.
#' 
#' @examples
#' # To write the output file in the administrator desktop:
#' Name <- 'C:/Users/Administrador/Desktop/E30103.FF_V1.MM032014.D_1'
#' WriteRepoFile(ExampleQ, Name)
#' 
#' @seealso \code{\link{ReadSASFile}}, \code{\link{ReadRepoFile}}, 
#' \code{\link{FirstLine}}
#' 
#' @include FirstLine.R
#' 
#' @export
setGeneric("WriteRepoFile", 
           function(object, Name){standardGeneric("WriteRepoFile")})

#' @rdname WriteRepoFile
#' 
#' @importFrom gdata write.fwf
#' 
#' @import data.table
#' 
#' @include FirstLine.R
#' 
#' @export
setMethod(
    f = "WriteRepoFile",
    signature = c("data.table", "FileName"),
    function(object, Name){
        
        FL <- FirstLine(object)
        
        Widths <- unlist(strsplit(FL, ','))
        Widths <- Widths[2:(length(Widths) - 1)]
        Widths <- unlist(lapply(strsplit(Widths, '='), '[', 2))
        Widths <- unlist(lapply(strsplit(Widths, '.', fixed = TRUE), '[', 1))
        Widths <- ifelse(substr(Widths, 1, 1) == '$', 
                         substr(Widths, 2, nchar(Widths)), 
                         Widths)
        Widths <- as.integer(Widths)
    
        write(x = FL, file = Name)
        auxData <- object
        write.fwf(auxData, Name, append = TRUE, sep='', colnames = FALSE, 
                  justify = 'right', na = '', width = Widths)
        cat(paste0('Key-value pair file written in ', Name, '.\n')    
        ) 
        return(invisible(NULL))
    }
)
#' @rdname WriteRepoFile
#' 
#' @importFrom StQ getData
#' 
#' @export
setMethod(
    f = "WriteRepoFile",
    signature = c("StQ", "FileName"),
    function(object, Name){
        
        WriteRepoFile(object = getData(object), Name = Name)
        return(invisible(NULL))
        
    }
)

#' @rdname WriteRepoFile
#' 
#' @import RepoTime
#' 
#' @export
setMethod(
    f = "WriteRepoFile",
    signature = c("StQList"),
    function(object, Name){
        
        if (Length(object@Periods) > 0) {
            
            if (Length(object@Periods) != length(Name)) {
                
                stop(paste0('[RepoReadWrite::WriteRepoFile] ', object, ' and ', Name, ' must have the same length.\n'))
                
            }
            for (i in 1:Length(object@Periods)) {
                
                WriteRepoFile(object = object@Data[[i]], Name = Name[i]) 
            }
        }

        return(invisible(NULL))
        
    }
)
