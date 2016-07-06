#' @title Produce an object of class \linkS4class{DD} from a xml file
#' 
#' @description This function is a constructor for the class \linkS4class{DD} using the contents of 
#' a xml file.
#' 
#' \code{RepoDDToDD} read xml files with the definition and properties of every variable and
#' transform this content into an object of class \linkS4class{DD}. 
#' 
#' This function internally builds a \linkS4class{data.table} with columns \code{Variable}, 
#' \code{Sort}, \code{Class}, \code{Qual1} to \code{Qual}\emph{q} and ValueRegExp.
#'  
#' The column \code{Variable} contains the names of all variables, both questionnaire variables and 
#' metadata. This internal \linkS4class{data.table} is then used to initialize a \linkS4class{DD} 
#' object.
#' 
#' The column \code{Sort} takes values \code{'IDQual'}, \code{'NonIDQual'} or \code{'IDDD'}, for 
#' statistical unit qualifiers, variable name qualifiers and variable names, respectively.
#' 
#' The column \code{Class} specifies the class of the variable and takes values \code{numeric} or 
#' \code{character}. 
#' 
#' The columns \code{Qual1} to \code{Qual}\emph{q} contain the names of the qualifiers of every 
#' variable name (row).
#' 
#' The column \code{ValueRegExp} contains an expresion with the acceptable values for each variable.
#' 
#' @param FileName Character vector of length 1 with the name of the file to read. The file will be 
#' read from the working directory (see \link[base]{getwd}) unless the full path is specified.
#' 
#' @param VNC Object of class \linkS4class{VarNameCorresp}.
#' 
#' @param DDslot Character vector of length 1 with the name of DD slot in which transformation will 
#' be made. Its default value is \code{NULL}. If no DDslot is specified, variables in VNC components 
#' are assigned to the corresponding DD slot.
#'  
#' @return Object of class \linkS4class{DD}.
#' 
#' @examples
#' # An example with data created previosly:
#' library(data.table)
#' data(ExampleRepoDD)
#' data(ExampleVNC)
#' RepoDDToDD(ExampleRepoDD, ExampleVNC)
#' 
#' @import data.table 
#'
#' @export
RepoDDToDD <- function(FileName, VNC, DDslot = NULL){
    
    # Comprobamos que el slot del DD que se especifica realmente es uno de los slots del objeto DD
    if (!is.null(DDslot)) {
        if (!DDslot %in% c('ID', 'MicroData', 'ParaData', 'Aggregates' ,'AggWeights','Other')) {
            stop(paste0('[Validity RepoDDToDD]"', DDslot, '" is not a slot of the DD input object.'))
        }
    }
    
    doc <- xmlParse(FileName)
    nodes <- getNodeSet(doc, "//identifier[@identifierType]") #lista de clases 'XMLInternalElementNode'
    
    # Generamos listas de dataframes con los datos de cada variable y sus calificadores
    quals <- getNodeSet(doc, "//quals")
    
    data <- lapply(nodes,function(x){
                    as.data.table(xmlToDataFrame(union(x[1:3], x[5:7]), stringsAsFactors = FALSE))
            })
    
    quals <- lapply(quals, function(x){as.data.table(xmlToDataFrame(x, stringsAsFactors = FALSE))})
    
    # Generamos columnas del slot DD: Variable, Sort, Class y ValueRegExp
    Sort <- unlist(lapply(nodes,function(x) xmlGetAttr(x,"identifierType")))
    IDQual <- gsub("I","IDQual",Sort[Sort == 'I'])
    Sort <- c(IDQual, gsub("Q","NonIDQual",Sort[Sort != 'I']))
    Sort <- gsub("V","IDDD",Sort)
    
    Variable <- unlist(lapply(data, function(x) x[1])) 
    Class <- unlist(lapply(data, function(x) x[5]))
    
    values <- getNodeSet(doc, "//values")
    ValueRegExp <- unlist(lapply(values, function(x){as.data.table(xmlToDataFrame(x, stringsAsFactors = FALSE))[2]}))

    
    # Construimos un vector Qual que contenga los datos de Qual1, Qual2,... en ese orden
    existQual <- unlist(lapply(nodes, function(x){length(xmlChildren(x)) - 9})) #Vector con el valor 0 si la variable no tiene calificadores y 1 si tiene
    nummaxQual <- max(unlist(lapply(quals, function(x){dim(x)[1]})))
    Qual <- vector('character', length(nodes) * nummaxQual)
    contquals <- 0
 
    for (i in seq(along = (nodes))){
        
        if (existQual[i] == 1){
            
            contquals <- contquals + 1
            varQuals <- quals[[contquals]][['text']]
            if (length(varQuals) < nummaxQual){varQuals <- c(varQuals, rep("", nummaxQual - length(varQuals)))}
            for (j in seq(along = varQuals)){
                Qual[i + (j - 1) * length(nodes)] <- varQuals[j]
            }
        }
    }
    
           
    # Construimos el data.table necesario para crear el objeto DD
    DDData <- data.table(Variable, Sort, Class)
    for (i in seq(1,nummaxQual)) {
        
        posQual <- seq(length(nodes) * (i - 1) + 1, length(nodes) * i)
        DDData <- data.table(DDData, aux = Qual[posQual])
        setnames(DDData, 'aux', paste0('Qual', i))
    }

    DDData[, ValueRegExp := ValueRegExp]
    
        
    # Si no se especifica DDslot, cada variable se asigna al slot
    # correpondiente a la componente del VNC en el que aparece.
    
    if (is.null(DDslot)) {
        
        DD <- new(Class = "DD", VarNameCorresp = VNC)
        CommonNames <- intersect(names(VNC), slotNames(DD))
       
        for (Names in names(VNC)) {
            
            Var <- c(getIDQual(VNC[[Names]]), getIDDD(VNC[[Names]]), getNonIDQual(VNC[[Names]]))
            Var <- unique(Var[Var != ""])

            DDdt <- DDData[Variable %in% Var ]
           
            if (Names %in% CommonNames) {
                
                slot(DD, Names) <- new(Class = "DDdt", DDdt)
                
            } else {
                
                slot(DD, 'MicroData') <- slot(DD, 'MicroData') + new(Class = "DDdt", DDdt)
                
            }
        }

    } else if (DDslot == 'MicroData') {
                
        DD <- new(Class = 'DD', MicroData = new(Class = 'DDdt', DDData), VarNameCorresp = VNC)
        
    } else if (DDslot == 'ParaData') {

        DD <- new(Class = 'DD', ParaData = new(Class = 'DDdt', DDData), VarNameCorresp = VNC)
        
    } else if (DDslot == 'Aggregates') {
        
        DD <- new(Class = 'DD', Aggregates = new(Class = 'DDdt', DDData), VarNameCorresp = VNC)
        
    } else if (DDslot == 'AggWeights'){
        
        DD <- new(Class = 'DD', AggWeights = new(Class = 'DDdt', DDData), VarNameCorresp = VNC)
        
    } else {
        
        DD <- new(Class = 'DD', Other = new(Class = 'DDdt', DDData), VarNameCorresp = VNC)
    }
    
    
    return(DD)
}
