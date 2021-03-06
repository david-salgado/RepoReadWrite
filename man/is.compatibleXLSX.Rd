% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is.compatibleXLSX.R
\name{is.compatibleXLSX}
\alias{is.compatibleXLSX}
\title{Check compatibility of two xlsx files to create DD (Data Dictionaries).}
\usage{
is.compatibleXLSX(oldVersion, newVersion, verbose = FALSE)
}
\arguments{
\item{oldVersion}{Character vector of length 1 with the name of the file with the old version of 
the data dictionary.}

\item{newVersion}{Character vector of length 1 with the name of the file with the new version of 
the data dictionary.}

\item{verbose}{\code{TRUE} or \code{FALSE} (default) to request verbose mode.}
}
\value{
\code{TRUE} or \code{FALSE}.
}
\description{
This function checks the validity of the xlsx file used as GUI to create the data 
dictionary with XML format.
}
\details{
The input xlsx file is a naive user interface to generate a data dictionary in XML 
format with the specification of each single key-value pair in the dictionary. The function 
performs a sequence of checks over each sheet of the input xlsx file. These checks are necessary 
(though not sufficient) for the construction of the XML file with the function 
\code{\link{RepoXLSToDD}}.
}
\examples{
\dontrun{
ExcelName_old <- 'T:/E30163/E30163.NombresVariables_V1.xlsx'
ExcelName_new <- 'T:/E30163/E30163.NombresVariables_V2.xlsx'
is.compatibleXLSX(ExcelName_old, ExcelName_new)
}

}
