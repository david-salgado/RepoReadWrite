#' Escritura de un fichero con estructura par clave-valor
#' 
#' \code{WriteRepoFile} reescribe el objeto de entrada con una estructura par
#' clave-valor y lo guarda en un fichero ASCII. 
#' 
#' Esta función lee el objeto de entrada \code{object} y reescribe su contenido
#' con una estructura par clave-valor. Esta nueva estructura se graba en un 
#' fichero ASCII con nombre \code{Name} especificado como argumento de entrada,
#' el cual debe definir la ruta completa de acceso al fichero (directorio y nombre
#' que queremos asignarle). 
#' 
#' También es necesario incluir como input de la función el número de 
#' calificadores de identificación (\code{NIV}) para que se incluya en la 
#' primera línea del fichero ASCII generado.
#' 
#' 
#' @param object Objeto que se desea escribir con estructura par clave-valor en
#' un fichero ASCII.
#' 
#' @param Name \code{\link{vector}} de tipo \code{character} de longitud 1 con el
#' nombre del fichero ASCII a escribir. Debe incluir la ruta completa en la que 
#' se quiere ubicar el mismo (directorio y nombre del fichero que se va a crear).
#' 
#' @param NIV \code{Vector} de tipo \code{integer} de longitud uno con el número
#' de niveles del fichero que se va a generar. Corresponde al número total de 
#' calificadores que aparecen en el fichero.Por defecto toma el valor 
#' \code{NIV} = \code{1}.
#' 
#' 
#' @return Fichero ASCII con los datos del objeto de entrada con estructura par
#' clave-valor y de nombre el especificado como input.
#' 
#' @examples
#' # Se escribirá un fichero ASCII en el escritorio del administrador:
#' Name <- 'C:/Users/Administrador/Desktop/E30103.FF_V1.MM032014.D_1' # Cambiar a discreción
#' WriteRepoFile(Example.Q, Name)
#' 
#' @seealso \link{ReadSASFile}, \link{ReadRepoFile}, \link{FirstLine}
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
        
        FL <- FirstLine(object)
        
        Widths <- unlist(strsplit(FL, ','))
        Widths <- Widths[2:(length(Widths) - 1)]
        Widths <- unlist(lapply(strsplit(Widths, '='), '[', 2))
        Widths <- unlist(lapply(strsplit(Widths, '.', fixed = TRUE), '[', 1))
        Widths <- ifelse(substr(Widths, 1, 1) == '$', substr(Widths, 2, nchar(Widths)), Widths)
        Widths <- as.integer(Widths)
    
        write(x = FL, file = Name)
        auxData <- object
        gdata::write.fwf(auxData, Name, append = TRUE, sep='', colnames = FALSE, justify = 'right', na = '', width = Widths)
        cat(paste0('Key-value pair file ', match.call()[['object']], ' written in ', Name, '\n')) 
        return(invisible(NULL))
    }
)
#' @rdname WriteRepoFile
#' 
#' @import StQ
#' 
#' @export
setMethod(
    f = "WriteRepoFile",
    signature = c("StQ"),
    function(object, Name){
        
        
        WriteRepoFile(StQ::getData(object), Name = Name)
        return(invisible(NULL))
        
    }
)    