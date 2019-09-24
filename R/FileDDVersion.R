#' @title Return version number of associated DD file.
#'
#' @description \code{FileDDVersion} returns the version number of the DD file 
#' (data dictionary) corresponding to the input file name.
#'
#' @param FileNames Character vector with the names of the files.
#'  
#' @return Integer vector with the DD number version of each filename.
#'
#' @examples
#' \dontrun{
#' FileDDVersion('E30183.FF_V1.MM122014.D_3')
#' FileDDVersion('T:/E30183.FF_B15_V1.MM122016.P_2')
#' FileDDVersion(c('E30183.FF_V1.MM122014.D_3', 'T:/E30183.FF_B15_V10.MM122016.P_2'))
#' }
#' 
#' @importFrom stringi stri_split_fixed
#' 
#' @export
FileDDVersion <- function(FileNames){
    
    simplFileNames <- basename(FileNames)
    
    regEx <- 'E[0-9]{5}\\.(DD|NombresVariables|FF|FD|FG|FL|FT|FI|FP)_(B[0-9]{2}_)*V[0-9]+'
    regExIndex <- gregexpr(regEx, simplFileNames)
    invalidFileNames <- FileNames[regExIndex == -1]
    if(length(invalidFileNames) != 0) {
        
        stop(paste0('[RepoReadWrite::FileDDVersion] The following FileNames are not valid:\n'), 
             paste0(invalidFileNames, collapse = ', '))
        
    }

    parsedFileName.dot <- stringi::stri_split_fixed(FileNames, pattern = '.')
    fileTypes <- sapply(parsedFileName.dot, `[`, 2)
    parsedFileTypes <- stringi::stri_split_fixed(fileTypes, pattern = '_V')
    DDversions <- sapply(parsedFileTypes, `[[`, 2)
    return(DDversions)        

}
