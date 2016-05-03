#' @title \linkS4class{data.table} with the content of the file DD
#'
#' @description \code{RepoDD} is a \linkS4class{data.table} with the content of 
#' the file DD containing the definition and properties of every variable for
#' the Service Sector Activity Indicators survey.
#' 
#' The columns of \code{RepoDD} are:
#' \itemize{
#'   \item NOMID: Names of the statistical unit qualifiers.
#'   \item NOMCALIFICADOR: Names of the variable qualifiers.
#'   \item NOMIDDD: Names of the variables for the microdata repository.
#'   \item TIPO: \code{STRING} for \code{character} class variables or 
#'   \code{NUMBER} for \code{double} or \code{integer} class variables.
#'   \item FORM: SAS format of the variables.
#'   \item CALIF\emph{j}: Name of the qualifier \emph{j} for each variable (note
#'   that unit qualifiers are not included by assumption; an exception occurs 
#'   with indices weights, taxes, ...).
#'   \item TIPOCALIF\emph{j}: Sort of qualifier \emph{j} (value \code{1} for 
#'   unit qualifiers and value \code{2} variable qualifiers).
#' }
#' 
#' 
#'
#' @docType data
#' @name RepoDD
#' @usage data(RepoDD)
#' @format \linkS4class{data.table} with 53 rows and 22 columns.
