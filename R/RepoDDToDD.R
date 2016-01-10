#' Transforma el fichero DD del repositorio a su formato para R
#' 
#' Esta función es un constructor para la clase \linkS4class{DD} a partir del 
#' contenido del fichero DD correspondiente en el repositorio.
#' 
#' \code{RepoDDToDD} transforma un \code{\linkS4class{data.frame}} con el contenido
#' del fichero DD de definición de datos ubicado en el repositorio de microdatos
#' de la encuesta, en un \code{\linkS4class{data.table}} con el formato requerido para 
#' su inclusión como slot \code{DD} de un objeto de clase \code{\linkS4class{StQ}}.
#' 
#' Esta función toma el contenido del \code{data.frame} de entrada y 
#' construye un \code{data.table} con las columnas \code{Variable}, 
#' \code{Sort}, \code{Class} and \code{Qual\code{1}} to \code{Qual\code{q}}. 
#' 
#' La columna \code{Variable} contiene los nombres de todas las variables, tanto
#' de las del cuestionario como de las creadas durante el proceso.
#' 
#' La columna \code{Sort} toma los valores \code{'IDQual'}, \code{'NonIDQual'}, 
#' e \code{'IDDD'}, para calificadores de identificación de las unidades 
#' estadísticas, para calificadores de nombres de variables y para nombres de
#' variables otras variables en el cuestionario.
#' 
#' La columna \code{Class} especifica la clase de la variable, y toma los valores
#' \code{numeric} o \code{character}. 
#' 
#' Las columnas \code{Qual\code{1}} a \code{Qual\code{q}} contienen los nombres de los 
#' calificadores de cada variable (fila).
#' 
#' Se transforma el fichero de definición de datos para que su tratamiento desde
#' el punto de vista de la programación sea más eficiente.
#' 
#' @param RepoDD \code{data.frame} con el contenido del fichero 
#' \code{DD} de definición de datos ubicado en el repositorio de microdatos de 
#' la encuesta en su formato para R.
#'  
#' @return \code{data.table} con los datos del fichero \code{DD} 
#' en el formato adecuado para su inclusión como slot \code{DD} de un objeto de
#' clase \code{StQ}.
#' 
#' @examples
#' data(RepoDD)
#' RepoDDToDD(RepoDD)
#' 
#' @import data.table
#'       
#' @export
RepoDDToDD <- function(RepoDD){
    
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

    output[, Class := ifelse(TIPO == 'NUMBER', 'numeric', ifelse(TIPO == 'STRING', 'character', ''))]

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

    nCalif <- (length(names(NonUnitDT)) - length(c('Variable', 'Sort', 'Class'))) / 2
    if (nCalif >= 1) {
        
        for (i in 1:nCalif){
            NonUnitDT[, paste0('TIPOCALIF', i):= NULL, with = F]
            setnames(NonUnitDT, paste0('CALIF', i), paste0('Qual', i))
        }
    }

    nCalif <- (length(names(output))  - length(c('Variable', 'Sort', 'Class', NomID))) / 2

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
    output <- new(Class = 'DD', Data = output)
    
    return(output)
}
