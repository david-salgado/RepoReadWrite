#' @title Object of class \code{\link[StQ]{VNC}}
#'
#' @description \code{ExampleVNC} is an object of class \code{\link[StQ]{VNC}} with components
#' \code{MicroData} and \code{Aggregates}.
#'
#' The columns of component \code{MicroData} of this object are:
#' \itemize{
#'   \item \code{IDQual}: statistical unit qualifiers.
#'   \item \code{NonIDQual}: variable name qualifiers.
#'   \item \code{IDDD}: variable names.
#'   \item \code{ID}: first qualifier values.
#'   \item \code{EmplType}: second qualifier values.
#'   \item \code{UnitName}: variable names given by the production unit.
#' }
#'
#' And the columns of component \code{Aggregates} are:
#' \itemize{
#'   \item \code{IDQual}: statistical unit qualifiers.
#'   \item \code{NonIDQual}: variable name qualifiers.
#'   \item \code{IDDD}: variable names.
#'   \item \code{EmplType}: first qualifier values.
#'   \item \code{UnitName}: variable name given by the production unit.
#' }
#' 
#' @docType data
#'
#' @name ExampleVNC
#'
#' @usage data(ExampleVNC)
#'
#' @format Object of class \code{VarNameCorresp}. Its component \code{MicroData} has 15 rows and 7
#' columns and its component \code{Aggregates} has 3 rows and 6 columns.
NULL
