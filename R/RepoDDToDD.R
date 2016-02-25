#' @title Produce an object of class \linkS4class{DD} from a fixed-width column 
#' ASCII DD file
#' 
#' @description This function is a constructor for the class \linkS4class{DD} 
#' using the contents of a DD file in the original ASCII format.
#' 
#' \code{RepoDDToDD} transforms a \link{data.frame} with the content of a DD 
#' file with the original ASCII fixed-width columnwise format into an object of 
#' class \linkS4class{DD}. 
#' 
#' This function internally builds a \linkS4class{data.table} with columns
#'  \code{Variable}, \code{Sort}, \code{Class} and \code{Qual1} to 
#'  \code{Qual\emph{q}}.
#'  
#'  The column \code{Variable} contains the names of all 
#'  variables, both questionnaire variables and metadata. This internal 
#'  \linkS4class{data.table} is then used to initialize a \linkS4class{DD} 
#'  object.
#' 
#' The column \code{Sort} takes values \code{'IDQual'}, \code{'NonIDQual'} or 
#' \code{'IDDD'}, for statistical unit qualifiers, variable name qualifiers and
#' variable names, respectively.
#' 
#' The column \code{Class} specifies the class of the variable and takes values
#' \code{numeric} or \code{character}. 
#' 
#' The columns \code{Qual1} to \code{Qual\emph{q}} contain the names of the 
#' qualifiers of every variable name (row).
#' 
#' @param RepoDD \link{data.frame} with the content of the file \code{DD}.
#' 
#' @param VNC Object of class \linkS4class{VarNameCorresp}.
#' 
#' @param DDslot Character vector of length 1 with the name of DD slot in which
#' transformation will be made. Its default value is \code{MicroData}.
#'  
#' @return Object of class \linkS4class{DD}.
#' 
#' @examples
#' # An example with data created previosly:
#' library(data.table)
#' data(RepoDD)
#' data(VNC)
#' RepoDDToDD(RepoDD, VNC)
#' 
#' @import data.table
#'       
#' @export
    RepoDDToDD <- function(RepoDD, VNC, DDslot = 'MicroData'){
    
    # Comprobamoos que el slot del DD que se especifica realmente es uno de los slots del objeto DD
    if (DDslot != 'MicroData' & DDslot != 'Aggregates' & DDslot != 'AggWeights'
        & DDslot != 'Other'){
      stop(paste0('[Validity RepoDDToDD]"', DDslot, '" is not a slot of the DD input object.'))
    }
    
    
    RepoDD <- as.data.table(RepoDD)
    output <- copy(RepoDD)

    # Eliminamos columnas innecesarias
    output[, FICHORIG := NULL]
    output[, FORM := NULL]
    
    # Generamos columnas del slot DD: Variable, Sort y Class
    output[, Variable := '']
    output[NOMID != '', Variable := NOMID]
    output[NOMID != '', Sort := 'IDQual']
    
    output[NOMCALIFICADOR != '', Variable := NOMCALIFICADOR]
    output[NOMCALIFICADOR != '', Sort := 'NonIDQual']
    
    output[NOMIDDD != '', Variable := NOMIDDD]
    output[NOMIDDD != '', Sort := 'IDDD']

    output[, Class := ifelse(TIPO == 'NUMBER',
                             'numeric',
                             ifelse(TIPO == 'STRING', 'character', ''))]

    # Eliminamos columnas innecesarias
    output[, c('NOMID', 'NOMCALIFICADOR', 'NOMIDDD', 'TIPO') := NULL, with = F]
    
    # Segregamos una data.table con la fila de la variable de ponderaciones (TIPOCALIF1 = 1)
    # eliminando columnas en blanco
    index.NonIDQualUnit <- which(output[['TIPOCALIF1']] == 1)
    NonUnitDT <- output[index.NonIDQualUnit]
    if (dim(NonUnitDT)[1] > 0){
        
    
        nCal <- (length(names(NonUnitDT)) - 3L) / 2
        if (nCal >= 1){
          
          for (i in 1:nCal){
            
            if (all(NonUnitDT[[paste0('CALIF', i)]] == '')){
              NonUnitDT[, paste0('CALIF', i) := NULL, with = F]
              NonUnitDT[, paste0('TIPOCALIF', i) := NULL, with = F]
            }
            next
          }
        }
    
        PondName <- output[['Variable']][index.NonIDQualUnit]

        output <- output[!Variable %chin% NonUnitDT[['Variable']]]
    
    } else {
    
        NonUnitDT <- NonUnitDT[, c('Variable', 'Sort', 'Class'), with = F]    
        
    }
    
    nCal <- (length(names(output)) - 3L) / 2
    if (nCal >= 1){
      
      for (i in 1:nCal){
        
        if (all(output[[paste0('CALIF', i)]] == '')){
          output[, paste0('CALIF', i) := NULL, with = F]
          output[, paste0('TIPOCALIF', i) := NULL, with = F]
        }
      }
    }
    # Generamos columnas Qual1, Qual2, ... y eliminamos columnas en blanco
    NomID <- output[Sort == 'IDQual'][['Variable']]
    NomID <- NomID[NomID != '']

    for (i in seq(along = NomID)){
        
        output[Sort == 'IDDD', paste0('Qual', i) := NomID[i], with = F]
        
    }

    nCalif <- (length(names(NonUnitDT)) -
                   length(c('Variable', 'Sort', 'Class'))) / 2
    if (nCalif >= 1) {
        
        for (i in 1:nCalif){
            NonUnitDT[, paste0('TIPOCALIF', i):= NULL, with = F]
            setnames(NonUnitDT, paste0('CALIF', i), paste0('Qual', i))
        }
    }

    nCalif <- (length(names(output))  -
                   length(c('Variable', 'Sort', 'Class', NomID))) / 2

    if (nCalif >= 1) {
      
      for (i in 1:nCalif){
        output[, paste0('TIPOCALIF', i):= NULL, with = F]
        setnames(output, paste0('CALIF', i), paste0('Qual', i + length(NomID)))
      }
    }
    # Unimos las data.tables segregadas
    CommonVar <- intersect(names(output), names(NonUnitDT))
    setkeyv(output, CommonVar)
    setkeyv(NonUnitDT, CommonVar)
    output <- merge(output, NonUnitDT, all = T)
    
    # Ordenamos la data.table final
    Qual <- setdiff(names(output), c('Variable', 'Sort', 'Class'))
    nQual <- length(Qual)
    Qual <- paste0('Qual', 1:nQual)

    setcolorder(output, c('Variable', 'Sort', 'Class', Qual))
    
    for (col in names(output)){
        
      if (all(is.na(output[[col]])) | all(output[[col]] == '')) output[, col := NULL, with = F]
      output[is.na(get(col)), col := '', with = F]
        
    }
    
    # Otorgamos la clase DD a la data.table final
    if (DDslot == 'MicroData'){
      output <- new(Class = 'DD', MicroData = output, VarNameCorresp = VNC)
    }else if (DDslot == 'Aggregates'){
      output <- new(Class = 'DD', Aggregates = output, VarNameCorresp = VNC)
    }else if (DDslot == 'AggWeights'){
      output <- new(Class = 'DD', AggWeights = output, VarNameCorresp = VNC)
    }else{
      output <- new(Class = 'DD', Other = output, VarNameCorresp = VNC)
    }
    return(output)
}
