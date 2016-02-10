#' @title Produce an object of class \linkS4class{DD} from a xml file.
#' 
#' @description This function is a constructor for the class \linkS4class{DD} 
#' using the contents of a xml file.
#' 
#' \code{xmlToDD} read xml files with the definition and properties of every
#' variable for the Service Sector Activity Indicators survey and transform this
#' content into an object of class \linkS4class{DD}.
#' 
#' This function reads the content of a \linkS4class{data.table} with columns
#'  \code{Variable}, \code{Sort}, \code{Class} and \code{Qual1} to 
#'  \code{Qual\emph{q}}. The column \code{Variable} contains the names of all 
#'  variables, both questionnaire variables and metadata. This internal 
#'  \linkS4class{data.table} is then used to initialize a \linkS4class{DD} 
#' 
#' La columna \code{Variable} contiene los nombres de todas las variables, tanto
#' de las del cuestionario como de las creadas durante el proceso.
#' 
#' La columna \code{Sort} toma los valores \code{'IDQual'}, \code{'NonIDQual'}, 
#' e \code{'IDDD'}, para calificadores de identificación de las unidades 
#' estadísticas, para calificadores de nombres de variables y para nombres de
#' variables otras variables en el cuestionario.
#' 
#' La columna \code{Class} especifica la clase de la variable, y toma los
#' valores \code{numeric} o \code{character}. 
#' 
#' Las columnas \code{Qual\code{1}} a \code{Qual\code{q}} contienen los nombres 
#' de los calificadores de cada variable (fila).
#' 
#' Se transforma el fichero de definición de datos para que su tratamiento desde
#' el punto de vista de la programación sea más eficiente.
#' 
#' @param FileName \code{\link{vector}} de tipo \code{character} de longitud 1 
#' con el nombre del fichero que se desea leer. El nombre debe incluir la ruta
#' completa de ubicación del fichero (directorio en el que se encuentra el
#' fichero y su nombre).
#' 
#' @param VNC Object of class \linkS4class{VarNameCorresp}.
#' 
#' @param DDslot Character vector of length 1 with the name of DD slot in which
#' transformation will be made. Its default value is \code{MicroData}.
#' 
#' @return Objeto de clase \code{\linkS4class{DD}}, en el formato adecuado para
#' su inclusión como slot \code{DD} de un objeto de clase \code{StQ}.
#'  
#' 
#' @examples
#' FileName<-"N:/UDMTD/UDTMDCOM/DepSel.SoftwareR/INE.Packages.v2/Ejemplo_v2.xml"
#' DD <- xmlToDD(FileName)
#' 
#' @import data.table XML
#'
#' @export
    xmlToDD <- function(FileName, VNC, DDslot = 'MicroData'){
      
      # Comprobamoos que el slot del DD que se especifica realmente es uno de los slots del objeto DD
      if (DDslot != 'MicroData' & DDslot != 'Aggregates' & DDslot != 'AggWeights'
          & DDslot != 'Other'){
          stop(paste0('[Validity RepoDDToDD]"', DDslot, '" is not a slot of the DD input object.'))
      }
        
        
      doc <- xmlParse(FileName)
      nodes <- getNodeSet(doc, "//variable[@typeID]") #lista de clases 'XMLInternalElementNode'
      
      # Generamos una lista de dataframes con los datos de cada variable
      xmlDD <- lapply(nodes,xmlToDataFrame)
      
      # Generamos columnas del slot DD: Variable, Sort y Class
      Sort <- unlist(lapply(nodes,function(x) xmlGetAttr(x,"typeID")))
      Sort <- gsub("ID","IDQual",Sort)
      Sort <- gsub("Calificador","NonIDQual",Sort)
      Sort <- gsub("Variable","IDDD",Sort) 
      
      Variable <- unlist(lapply(xmlDD, function(x) x[1,1])) 
      Class <- unlist(lapply(xmlDD, function(x) x[3,1]))
      
      
      # Construimos un vector Qual que contenga los datos de Qual1, Qual2,... en ese orden
      
      varQual <- c() # Lista con los elementos de xmlDD que tienen al menos un calificador
      varQ <- c() # Vector con el número de los elementos de xmlDD que tienen al menos un calificador
      varQual <- xmlDD[lapply(xmlDD,ncol) > 1]
      
      
      nomQual <- as.list(lapply(varQual, function(x) x[3])) 
      nomQual <- lapply(nomQual,function(x) x[,1][!is.na(x[,1])])
      nummaxQual <- max(unlist(lapply(nomQual,length)))
      Qual <- vector('character',length(xmlDD) * nummaxQual)
      
      
      for (i in seq(along = xmlDD)){
        
        if(ncol(xmlDD[[i]]) > 1) varQ <- c(varQ,i)
      }
      
      for (i in seq(along = nomQual)){
        
        for (j in seq(along = nomQual[[i]])){
          
          pos <- varQ[i] + (j - 1) * length(xmlDD)
          Qual[pos] <- nomQual[[i]][j]
        }
      }
      
      
      # Construimos el data.table necesario para crear el objeto DD
      DDData <- data.table(Variable,Sort,Class)
      for (i in seq(1,nummaxQual)){
        
        posQual <- seq(length(xmlDD) * (i-1) + 1, length(xmlDD) * i)
        DDData <- data.table(DDData, aux = Qual[posQual])
        setnames(DDData, 'aux', paste0('Qual', i))
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
