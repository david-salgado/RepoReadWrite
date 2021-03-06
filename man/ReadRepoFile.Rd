% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReadRepoFile.R
\name{ReadRepoFile}
\alias{ReadRepoFile}
\title{Read a file with a key-value pair structure.}
\usage{
ReadRepoFile(
  FileName,
  DD,
  out = "StQ",
  perl = FALSE,
  sep = "@@",
  encoding = "unknown",
  verbose = TRUE
)
}
\arguments{
\item{FileName}{Character vector of length 1 with the name of the file to read. The file will be 
read from the working directory (see \link[base]{getwd}) unless the full path is specified.}

\item{DD}{Object of class \code{\link[StQ]{DD}} with the definition and characteristics of the data 
contained in the file to read.}

\item{out}{Character vector of length 1 indicating whether to output an \code{\link[StQ]{StQ}} 
object (\code{out} = 'StQ'; default) or a \code{\link[StQ]{rawStQ}} object 
(\code{out} = 'rawStQ').}

\item{perl}{Logical vector of length 1 indicating whether Perl is installed in the system or not.}

\item{sep}{Character vector of length 1 containing the combination of characters used as separator 
in the input file (default value @).}

\item{encoding}{Character vector of length 1 with default value is "unknown". Other possible 
options are "UTF-8" and "Latin-1". 
Note: it is not used to re-encode the input, rather enables handling of encoded strings in their 
native encoding.}

\item{verbose}{Logical vector of length 1 indicating whether report timings are shown or not.}
}
\value{
Return an object of class \code{\link[StQ]{StQ}} or class \code{\link[StQ]{rawStQ}} with all data 
from the input file.
}
\description{
\code{ReadRepoFile} returns an \code{\link[StQ]{StQ}} (default) or a 
\code{\link[StQ]{rawStQ}} object with the content of the file corresponding to the 
input name.
}
\examples{
\dontrun{
#We assume that the key-value ASCII file \code{E30183.FF_V1.MM032014.D_1} is in the administrator 
desktop (change accordingly otherwise): 
ExcelName <- 'T:/E30163/E30163.NombresVariables_V1.xlsx'
DD <- RepoXLSToDD(ExcelName)
RepoName <- 'T:/E30163/E30163.FF_V1.MM032014.D_1'
Example.StQ <- ReadRepoFile(RepoName, DD, perl = TRUE)
str(Example.StQ)
}

}
\seealso{
\code{\link{WriteRepoFile}}
}
