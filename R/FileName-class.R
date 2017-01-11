#' @title S4 class \code{FileName} for the names of files in the repository
#' 
#' @description An S4 class called \code{FileName} for the names of the files.
#'
#' @details  The valid names for a file is:
#' \itemize{
#'      \item For files with a data dictionary (DD)
#'      \code{E}\emph{nnnnn}\code{.DD_V}\emph{m}, where
#'          \itemize{
#'              \item \emph{nnnnn} is the code of the survey the data belongs to;
#'              \item \emph{m} is the version number of the data dictionary.
#'          }
#'      \item For (xlsx) files with the correspondence between different 
#'      statitiscal variable names
#'      \code{E}\emph{nnnnn}.VariableNames.xlsx, where \emph{nnnnn} is the code 
#'      of the survey the data belongs to.
#'      \item For files with direct identification variables (FI)
#'      \code{E}\emph{nnnnn}\code{.FI_V}\emph{m}\code{.}\emph{PPp...p}\code{.D_}\emph{n} or
#'      \code{E}\emph{nnnnn}\code{.FI_V}\emph{m}\code{.}\emph{PPp...p}\code{.P_}\emph{n}, where
#'              \itemize{
#'                  \item \emph{nnnnn} is the code of the survey the data belongs to;
#'                  \item \emph{m} is the version number of the data dictionary associated to the 
#'                  file;
#'                  \item \emph{PPp...p} denotes the reference time period in the notation of the 
#'                  repository;
#'                  \item \code{D} stands for the \code{d}efinitive version of the data;
#'                  \item \code{P} stands for the \code{p}rovisional version of the data;
#'                  \item \emph{n} stands for a correlative version number of the data.
#'              }
#'      \item For files with completely edited data (\code{FF})
#'          \code{E}\emph{nnnnn}\code{.FF_V}\emph{m}\code{.}\emph{PPp...p}\code{.D_}\emph{n} or
#'          \code{E}\emph{nnnnn}\code{.FF_V}\emph{m}\code{.}\emph{PPp...p}\code{.P_}\emph{n}, where
#'              \itemize{
#'                  \item \emph{nnnnn} is the code of the survey the data belongs to;
#'                  \item \emph{m} is the version number of the data dictionary associated to the 
#'                  file;
#'                  \item \emph{PPp...p} denotes the reference time period in the notation of the 
#'                  repository;
#'                  \item \code{D} stands for the \code{d}efinitive version of the data;
#'                  \item \code{P} stands for the \code{p}rovisional version of the data;
#'                  \item \emph{n} stands for a correlative version number of the data.
#'              }
#'      \item For files with partially edited data (\code{FD}):
#'          \code{E}\emph{nnnnn}\code{.FD_V}\emph{m}\code{.}\emph{PPp...p}\code{.D_}\emph{n} or
#'          \code{E}\emph{nnnnn}\code{.FD_V}\emph{m}\code{.}\emph{PPp...p}\code{.P_}\emph{n}, where
#'              \itemize{
#'                  \item \emph{nnnnn} is the code of the survey the data belongs to;
#'                  \item \emph{m} is the version number of the data dictionary associated to the 
#'                  file;
#'                  \item \emph{PPp...p} denotes the reference time period in the notation of the 
#'                  repository;
#'                  \item \code{D} stands for the \code{d}efinitive version of the data;
#'                  \item \code{P} stands for the \code{p}rovisional version of the data;
#'                  \item \emph{n} stands for a correlative version number of the data.
#'      }
#'      \item For files with raw data (FG)
#'          \code{E}\emph{nnnnn}\code{.FG_V}\emph{m}\code{.}\emph{PPp...p}\code{.D_}\emph{n} or
#'          \code{E}\emph{nnnnn}\code{.FG_V}\emph{m}\code{.}\emph{PPp...p}\code{.P_}\emph{n}, where
#'              \itemize{
#'                  \item \emph{nnnnn} is the code of the survey the data belongs to;
#'                  \item \emph{m} is the version number of the data dictionary associated to the 
#'                  file;
#'                  \item \emph{PPp...p} denotes the reference time period in the notation of the 
#'                  repository;
#'                  \item \code{D} stands for the \code{d}efinitive version of the data;
#'                  \item \code{P} stands for the \code{p}rovisional version of the data;
#'                  \item \emph{n} stands for a correlative version number of the data.
#'              }
#'      \item For files with editing validation intervals
#'          \code{E}\emph{nnnnn}\code{.FL_V}\emph{m}\code{.}\emph{PPp...p}\code{.D_}\emph{n} or
#'          \code{E}\emph{nnnnn}\code{.FL_V}\emph{m}\code{.}\emph{PPp...p}\code{.P_}\emph{n}, where
#'              \itemize{
#'                  \item \emph{nnnnn} is the code of the survey the data belongs to;
#'                  \item \emph{m} is the version number of the data dictionary associated to the 
#'                  file;
#'                  \item \emph{PPp...p} denotes the reference time period in the notation of the 
#'                  repository;
#'                  \item \code{D} stands for the \code{d}efinitive version of the data;
#'                  \item \code{P} stands for the \code{p}rovisional version of the data;
#'                  \item \emph{n} stands for a correlative version number of the data.
#'              }
#'      \item For files with a selection of units with auxiliary information for
#'      further editing as a result of a cross-sectional analysis upon the sample
#'          \code{E}\emph{nnnnn}\code{.FT_V}\emph{m}\code{.}\emph{PPp...p}\code{.D_}\emph{n} or
#'          \code{E}\emph{nnnnn}\code{.FT_V}\emph{m}\code{.}\emph{PPp...p}\code{.P_}\emph{n}, where
#'              \itemize{
#'                  \item \emph{nnnnn} is the code of the survey the data belongs to;
#'                  \item \emph{m} is the version number of the data dictionary associated to the 
#'                  file;
#'                  \item \emph{PPp...p} denotes the reference time period in the notation of the 
#'                  repository;
#'                  \item \code{D} stands for the \code{d}efinitive version of the data;
#'                  \item \code{P} stands for the \code{p}rovisional version of the data;
#'                  \item \emph{n} stands for a correlative version number of the data.
#'              }
#' }
#'
#' @examples
#' # Empty object
#' x <- new(Class = 'FileName')
#' x
#' str(x)
#' 
#' # Example with one time interval
#' FF.MM032014 <- new(Class = 'FileName', c('E30183.FF_V1.MM032014.D_1'))
#' FF.MM032014
#' str(FF.MM032014)
#' 
#' @export
setClass(Class = "FileName",
         contains = 'character',
         prototype = character(0),
         validity = function(object){
             
             DD.Syntax <- "E[0-9]{5}\\.DD_V[0-9]+$"
             ValidDD <- regexpr(DD.Syntax, object) != -1
             
             F.Syntax <- "E[0-9]{5}\\.((FF)|(FG)|(FD)|(FI)|(FT)|(FL)|(FA))_V[0-9]+\\.((Q(Q|R)(1|2)((0[1-9])|(1[0-2]))[0-9]{4})|(M(M|R)((0[1-9])|(1[0-2]))[0-9]{4})|(B(B|R)[1-6][0-9]{4})|(T(T|R)[1-4][0-9]{4})|(S(S|R)[1-2][0-9]{4})|(A(A|R)[0-9]{4}))\\.(D|P)_[0-9]+$"
             ValidF <- regexpr(F.Syntax, object) != -1
             
             xlsx.Syntax <- "E[0-9]{5}\\.((NombresVariables|VariableNames)_V[0-9]+)\\.xls[x]*$"
             Validxls <- regexpr(xlsx.Syntax, object) != -1
             
             Validany <- ValidDD | ValidF | Validxls
             
             Validany[is.na(Validany)] <- TRUE
             
             
             if (!all(Validany)) {
                 
                 indexNotValid <- which(!Validany)
                 InvalidKeys <- object[indexNotValid]
                 stop(paste0('[RepoReadWrite::validity FileName] Not valid keys detected:\n ',
                             paste0(InvalidKeys, collapse = ',\n')),
                      call. = FALSE)
             }
             
             
             return(value = TRUE)
         }
)
