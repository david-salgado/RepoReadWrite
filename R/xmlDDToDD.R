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
      
      #numQualVar <- c()
      #posiQual <- c()
      #numvarQ <- 1 # Variable para recorrer varQ y numQualVar
      
      #for (i in seq(along = xmlDD)){
        
        #if (ncol(xmlDD[[i]]) > 1){
        #varQual <- c(varQual,xmlDD[[i]])
      #varQ <- c(varQ,i)
      #}
      #}
      varQual <- xmlDD[lapply(xmlDD,ncol) > 1]
      
      
      #for (i in seq(3,length(varQual), by = 3)){
        
      #QualVar <- varQual[[i]] # Vector con la columna que tiene los nombres de los calificadores en el elemento de xmlDD en el que se está
        
      #for (j in c(1:numQualVar[numvarQ])){
          
      #   Qual[varQ[numvarQ] + (j - 1)] <- QualVar[posiQual[j]]
         # numvarQ <- numvarQ + 1
          # }
      
          #}
      
      nomQual <- as.list(lapply(varQual, function(x) x[3]))
      nomQual <- lapply(nomQual,function(x) x[,1][!is.na(x[,1])])
      nummaxQual <- max(unlist(lapply(nomQual,length)))
      
      Qual <- vector('character',length(xmlDD) * nummaxQual)
      
      for (i in seq(along = xmlDD)){
        
        if(ncol(xmlDD[[i]]) > 1) varQ <- c(varQ,i)
      }
      
      
    }