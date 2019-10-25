#' @title Produce an object of class \link{DD} from a DD file 
#' 
#' @description This function is a constructor for the class \link{DD} using the contents of 
#' the input DD file name (an xml file).
#' 
#' \code{RepoDDToDD} reads xml files with the definition and properties of every variable (a DD 
#' file) and transforms this content into an object of class \link{DD}. 
#' 
#' This function internally builds a \linkS4class{data.table} with columns \code{Variable}, 
#' \code{Sort}, \code{Class}, \code{Qual1} to \code{Qual}\emph{q} and \code{ValueRegExp}.
#'  
#' The column \code{Variable} contains the names of all variables, both questionnaire variables and 
#' metadata. This internal \linkS4class{data.table} is then used to initialize a \link{DD} 
#' object.
#' 
#' The column \code{Sort} takes values \code{'IDQual'}, \code{'NonIDQual'} or \code{'IDDD'}, for 
#' statistical unit qualifiers, variable name qualifiers and variable names, respectively.
#' 
#' The column \code{Class} specifies the class of the variable and takes values \code{numeric} or 
#' \code{character}. 
#' 
#' The column \code{Length} contains the highest length for each variable.
#' 
#' The columns \code{Qual1} to \code{Qual}\emph{q} contain the names of the qualifiers of every 
#' variable name (row).
#' 
#' The column \code{ValueRegExp} contains a regexp with the accepted values for each variable.
#' 
#' @param FileName Character vector of length 1 with the name of the file to read. The file will be 
#' read from the working directory (see \link[base]{getwd}) unless the full path is specified.
#' 
#' @param VNC Object of class \code{\link[StQ]{VNC}}.
#'  
#' @return Return an object of class \code{\link[StQ]{DD}}.
#' 
#' @examples
#' # An example with data created previosly:
#' \dontrun{
#' ExcelName <- 'T:/E30163/E30163.NombresVariables_V1.xlsx'
#' VNC <- RepoXLSToVNC(ExcelName)
#' RepoDDFileName <- 'T:/E30163/E30163.DD_V1'
#' RepoDDToDD(RepoDDFileName, VNC)
#' }
#' 
#' @import data.table XML StQ
#'
#' @export
RepoDDToDD <- function(FileName, VNC){
    
    doc <- xmlParse(FileName)
    nodes <- getNodeSet(doc, "//identifier[@identifierType]") #lista de clases 'XMLInternalElementNode'

    # Generamos listas de dataframes con los datos de cada variable y sus calificadores
    quals <- getNodeSet(doc, "//quals")

    data <- lapply(nodes, function(x){
        
        as.data.table(xmlToDataFrame(union(x[1], x[3:4]), stringsAsFactors = FALSE))
    })

    QualOrder <- lapply(quals, xmlChildren)

    quals <- lapply(quals, function(x){as.data.table(xmlToDataFrame(x, stringsAsFactors = FALSE))})
    PaddingDT <- lapply(vector(mode = "list", length = length(nodes) - length(quals)),
                        function(x){data.table(text = character(0))})

    quals <- c(PaddingDT, quals)
                
    # Generamos columnas del slot DD: Variable, Sort, Class, Length y ValueRegExp
    Sort <- unlist(lapply(nodes,function(x) xmlGetAttr(x, "identifierType")))
    Sort <- gsub("V", "IDDD", Sort)
    Sort <- gsub("Q", "NonIDQual", Sort)
    Sort[Sort == 'I'] <- 'IDQual'
    
    Variable <- unlist(lapply(data, function(x) x[1])) 

    Class <- unlist(lapply(data, function(x) x[2]))
    Class <- gsub('STRING', 'character', Class)
    Class <- gsub('NUMBER', 'numeric', Class)
    Length <- unlist(lapply(data, function(x) x[3]))
    
    #EnFicheros <- unlist(lapply(UnitNames,function(x) {lapply(x, function(y) {xmlGetAttr(y,"EnFicheros")})}))
    values <- getNodeSet(doc, "//values")
    ValueRegExp <- unlist(lapply(values, function(x){as.data.table(xmlToDataFrame(x, stringsAsFactors = FALSE))[2]}))
    
    # Construimos un vector Qual que contenga los datos de Qual1, Qual2,... en ese orden
    QualOrder <-  lapply(QualOrder, function(x){unlist(lapply(x, xmlGetAttr, "QualOrder"))})

    existQual <- unlist(lapply(nodes, function(x){length(xmlChildren(x)) - 7})) #Vector con el valor 0 si la variable no tiene calificadores y 1 si tiene
    nummaxQual <- max(unlist(lapply(quals, function(x){dim(x)[1]})))
    Qual <- vector('character', length(nodes) * nummaxQual)
    contquals <- 0

    for (i in seq(along = nodes)){
      
      varQualsinic <- quals[[i]][['text']]
      if (length(varQualsinic) == 0) next
      varQuals <- vector('character', length(varQualsinic))
      for (j in seq(along = varQualsinic)){
        
        Qual[i + (j - 1) * length(nodes)] <- varQualsinic[j]
      }
    }

           
    # Construimos el data.table necesario para crear el objeto DD
    DDData <- data.table(Variable, Sort, Class, Length)

    for (i in seq(1, nummaxQual)) {
        
        posQual <- seq(length(nodes) * (i - 1) + 1, length(nodes) * i)
        DDData <- data.table(DDData, aux = Qual[posQual])
        setnames(DDData, 'aux', paste0('Qual', i))
    }

    DDData[, ValueRegExp := ValueRegExp]

    # Si no se especifica DDslot, cada variable se asigna al slot
    # correspondiente a la componente del VNC en el que aparece.
    
    VNCnames <- names(VNC)
    DDnames <- unique(unlist(lapply(VNCnames, function(VNCname){ strsplit(VNCname, '_')[[1]][[1]] })))
    
    VarList_DD <- vector('list', length(DDnames))
    names(VarList_DD) <- DDnames

    for (i in seq(along = VNCnames)) {
        
        IDQuals <- VNC[[VNCnames[i]]][['IDQual']]
        IDQuals <- IDQuals[IDQuals != '']
        
        NonIDQuals <- VNC[[VNCnames[i]]][['NonIDQual']]
        NonIDQuals <- NonIDQuals[NonIDQuals != '']
        
        IDDDs <- VNC[[VNCnames[i]]][['IDDD']]
        IDDDs <- IDDDs[IDDDs != '']
        
        Var <- unique(c(IDQuals, NonIDQuals, IDDDs))
        DDdt <- DDData[Variable %in% Var ]
        localDDname <- strsplit(VNCnames[[i]], '_')[[1]][[1]]
        if (is.null(VarList_DD[[localDDname]])){
            
            VarList_DD[[localDDname]] <- DDdt
            
        } else {
            
            auxDT <- rbindlist(list(VarList_DD[[localDDname]], DDdt), fill = TRUE)
            setkeyv(auxDT, names(auxDT))
            auxDT <- auxDT[!duplicated(auxDT, by = key(auxDT))]
            VarList_DD[[localDDname]] <- auxDT
        }
            
    }

    DD <- BuildDD(c(list(VNC = VNC), VarList_DD))
    return(DD)    

    
}
