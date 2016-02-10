#' Lectura de un fichero SAS de microdatos
#' 
#' \code{ReadSASFile} devuelve un \code{\link{data.table}} con los datos del 
#' fichero SAS especificado.
#' 
#' Esta función lee el fichero SAS especificado en \code{SASFileName} y devuelve
#' su contenido en un \code{data.table} con estructura matricial (unidades en 
#' filas y variables en columnas). Los nombres de las variables se asignan de 
#' acuerdo a los especificados en el fichero Excel que se encuentra en el 
#' repositorio de la encuesta considerada.
#' Para llevar a cabo esta asignación de nombres es necesario especificar también
#' los argumentos de entrada \code{Exceldf}, que es un \code{data.frame} con el 
#' contenido de la correspondiente hoja del fichero Excel \code{Ennnnn.NombresVariables.xls}
#' del repositorio de la encuesta, y \code{DD} que es el fichero \code{DD} de 
#' definición de datos de la encuesta en su formato para R. 
#' 
#' El formato del fichero \code{DD} para R lo obtendremos a partir del fichero ASCII
#' \code{Ennnnn.DD_Vn} incluido en el repositorio de la encuesta haciendo uso de la 
#' función \code{\link{RepoDDToDD}} del paquete \code{\linkS4class{StQ}}. 
#' 
#' 
#' @param SASFileName \code{\link{vector}} de tipo \code{character} de longitud 1 con
#' la ruta completa de ubicación del fichero SAS que se desea leer.
#' 
#' @param Exceldf \code{\link{data.frame}} con el contenido del fichero Excel 
#' de equivalencias entre los nombres de las variables de la encuesta asignados 
#' por las distintas unidades del INE implicadas.
#' 
#' @param DD \code{\link{data.frame}} con el contenido del fichero \code{DD} de
#' definición de datos de la encuesta, en su versión para R.
#' 
#' @param DDslot Character vector of length 1 with the name of DD slot in which
#' variables to read are defined. Its default value is \code{MicroData}.
#' 
#' @return \code{\link{data.table}} con los datos del fichero SAS leído, con las
#' unidades en filas y las variables en columnas.
#' 
#' @examples
#' # Se asume que el fichero \code{SAS MM032014.sas7bdat} se encuentra en el escritorio
#' # del administrador:
#' SASName <- 'C:/Users/Administrador/Desktop/MM032014.sas7bdat'
#' # We assume data created previosly:
#' data(XLS)
#' data(RepoDD)
#' data(VNC)
#' DD <- RepoDDToDD(RepoDD, VNC)
#' Example.DM <- ReadSASFile(SASName, XLS, DD)
#' 
#' @seealso  \link{RepoDDToDD}, \link{ReadRepoFile}, \link{WriteRepoFile}
#' 
#' @importFrom haven read_sas
#' 
#' @import data.table
#' 
#' @export
ReadSASFile <- function(SASFileName, Exceldf, DD, DDslot = 'MicroData'){
   
    # Comprobamoos que el slot del DD que se especifica realmente es uno de los slots del objeto DD
    if (DDslot != 'MicroData' & DDslot != 'Aggregates' & DDslot != 'AggWeights' & DDslot != 'Other'){
      stop(paste0('[Validity ReadSASFile]"', DDslot, '" is not a slot of the DD input object.'))
    }
  
    out.SP <- haven::read_sas(SASFileName)    
    
    ColClasses <- unlist(lapply(out.SP, class))
    
    out.SP <- as.data.table(out.SP)
    
      
    Exceldf <- as.data.table(Exceldf)
    
    CalID <- Exceldf$CalificadoresID
    CalID <- CalID[!is.na(CalID)]
    CalID <- CalID[CalID != '']
    
    CalNoID <- Exceldf$CalificadoresNoID
    CalNoID <- CalNoID[!is.na(CalNoID)]
    CalNoID <- CalNoID[CalNoID != '']
    
    Cals <- union(CalID, CalNoID)
    VarSP <- Exceldf$SP
    VarSP <- VarSP[!is.na(VarSP) & VarSP !=""]
    MissVar <- setdiff(VarSP, names(out.SP))

    if (length(MissVar) > 0) cat(paste0('Las siguientes variables de la hoja de cálculo no se encuentran en el fichero SAS:\n\n', 
                                        paste0(MissVar, collapse = ' '), '\n\n'))
    VarSP <- intersect(VarSP, names(out.SP))
    out.SP <- out.SP[, VarSP, with = F]
    
    Exceldf <- Exceldf[is.na(Variables) & !is.na(CalificadoresID), Variables:= CalificadoresID]
    Exceldf <- Exceldf[is.na(Variables) & !is.na(CalificadoresNoID), Variables:= CalificadoresNoID]
    
    pasteNA <- function(x, y){
        out <- ifelse(is.na(y) | y == '', paste0(x, ''), paste(x, y, sep ="_"))
        return(out)
    }
    
    Exceldf <- Exceldf[, NewVar := Variables]
    Exceldf <- Exceldf[CalificadoresID != '' & Variables == '' & SP != '', NewVar := CalificadoresID]
    Exceldf <- Exceldf[CalificadoresNoID != '' & Variables == '' & SP != '', NewVar := CalificadoresNoID]
    for (Cal in Cals){
        Exceldf <- copy(Exceldf)[, NewVar:= pasteNA(NewVar, get(Cal))]
    }

    EquivalName <- names(out.SP)
    names(EquivalName) <- Exceldf$NewVar[Exceldf$SP %in% EquivalName]
    setnames(out.SP, EquivalName, names(EquivalName))
    
    if (DDslot == 'MicroData'){
      DD <- getData(DD)
    }else if (DDslot == 'Aggregates'){
      DD <- getAggr(DD)
    }else if (DDslot == 'AggWeights'){
      DD <- getAggWeights(DD)
    }else{
      DD <- getOtherDD(DD)
    }  
    
    if (nrow(DD) == 0){
      stop(paste0('[Validity ReadSASFile]Data to be read are not defined in the slot "', DDslot, '" of the DD input object.'))
    }
   
    ExtractNames <- function(NamesVector){
      NamesVector <- as.list(NamesVector)
      NamesVector <- unlist(lapply(NamesVector, function(name){strsplit(name, '_')[[1]][1]}))
      return(NamesVector)
    }

    DDVarNames <- unlist(lapply(as.list(names(out.SP)), ExtractNames))
    names(DDVarNames) <- names(out.SP)
    
    for (Var in names(out.SP)){

      out.SP[, Var := as(get(Var), DD[Variable == DDVarNames[Var], Class]), with = F]
      
    }
    
    return(out.SP)

}
