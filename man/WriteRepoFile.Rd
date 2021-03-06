% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WriteRepoFile.R
\name{WriteRepoFile}
\alias{WriteRepoFile}
\alias{WriteRepoFile,rawStQ-method}
\alias{WriteRepoFile,StQ-method}
\alias{WriteRepoFile,StQList-method}
\alias{WriteRepoFile,rawStQList-method}
\title{Write a key-value pair ASCII file}
\usage{
WriteRepoFile(object, Name, sep = "@@")

\S4method{WriteRepoFile}{rawStQ}(object, Name, sep = "@@")

\S4method{WriteRepoFile}{StQ}(object, Name, sep = "@@")

\S4method{WriteRepoFile}{StQList}(object, Name, sep = "@@")

\S4method{WriteRepoFile}{rawStQList}(object, Name, sep = "@@")
}
\arguments{
\item{object}{Object to be written in the output file.}

\item{Name}{Character vectorof length 1 specifying the name of the output file. The file will be 
written in the working directory (see \link[base]{getwd}) unless the full path is specified.}

\item{sep}{Character vector of length 1 containing the combination of characters used as separator 
in the file (default value @).}
}
\value{
These methods return the invisible \code{\link{NULL}} object writing as a side-effect the 
corresponding output file.
}
\description{
\code{WriteRepoFile} writes the input key-value object in a fixed-width column ASCII 
file. 

This method reads the input object \code{object} and writes its content as a fixed-width column 
ASCII file with key-value pair structure with the specified input name \code{Name}.
}
\examples{
\dontrun{
# To write the output file in the administrator desktop:
Name <- 'C:/Users/Administrador/Desktop/E30103.FF_V1.MM032014.D_1'
WriteRepoFile(ExampleStQ, Name)
}

}
\seealso{
\code{\link{ReadRepoFile}}
}
