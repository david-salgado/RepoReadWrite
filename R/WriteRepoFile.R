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
#' @seealso \code{\link{ReadSASFile}}, \code{\link{ReadRepoFile}}, \code{\link{FirstLine}}
#' 
#' @include FirstLine.R
#' 
#' @export
setGeneric("WriteRepoFile", function(object, Name){standardGeneric("WriteRepoFile")})

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
    signature = c("data.table"),
    function(object, Name){
        
        object <- DatadtToDT(object)
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
        write.fwf(object, Name, append = TRUE, sep = '', colnames = FALSE,
                   na = '', width = Widths)
        cat(paste0('Key-value pair file written in ', Name), '.\n')
        
        return(invisible(NULL))
    }
)
#' @rdname WriteRepoFile
#' 
#' @export
setMethod(
    f = "WriteRepoFile",
    signature = c("StQ"),
    function(object, Name){
        
        WriteRepoFile(object = object, Name = Name)
        return(invisible(NULL))
        
    }
)

#' @rdname WriteRepoFile
#' 
#' @export
setMethod(
    f = "WriteRepoFile",
    signature = c("StQList"),
    function(object, Name){
        
        if (length(getPeriods.StQList(object)) > 0) {
            
            if (length(getPeriods.StQList(object)) != length(Name)) {
                
                stop(paste0('[RepoReadWrite::WriteRepoFile] ', object, ' and ', Name, ' must have 
                            the same length.\n'))
                
            }
            for (i in 1:length(getPeriods.StQList(object))) {
                
            WriteRepoFile(object = getData(object)[[i]], Name = Name[i]) 
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
    signature = c("rawStQ"),
    function(object, Name){
        
        #WriteRepoFile(object = getData(object), Name = Name)
        if (length(Name) != 1) {
            
            warning('[RepoReadWrite::WriteRepoFile] Only the first name will be used.')
        }
        Name <- Name[[1]]
        write.table(getData(object), file = Name, quote = FALSE, na = ' ', 
                    row.names = FALSE, col.names = FALSE)
        cat(paste0('Key-value pair file written in ', Name), '.\n')
        return(invisible(NULL))
        
    }
)

#' @rdname WriteRepoFile
#' 
#' @export
setMethod(
    f = "WriteRepoFile",
    signature = c("rawStQList"),
    function(object, Name){
        
        if (length(getPeriods.rawStQList(object)) > 0) {
            
            if (length(getPeriods.rawStQList(object)) != length(Name)) {
                
                stop(paste0('[RepoReadWrite::WriteRepoFile] ', object, ' and ', Name, ' must have 
                            the same length.\n'))
                
            }
            for (i in 1:length(getPeriods.rawStQList(object))) {
                
                WriteRepoFile(object = getData(object)[[i]], Name = Name[i]) 
            }
        }
        
        return(invisible(NULL))
        
    }
)
