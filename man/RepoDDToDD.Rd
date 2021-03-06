% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RepoDDToDD.R
\name{RepoDDToDD}
\alias{RepoDDToDD}
\title{Produce an object of class \link{DD} from a DD file}
\usage{
RepoDDToDD(FileName, VNC)
}
\arguments{
\item{FileName}{Character vector of length 1 with the name of the file to read. The file will be 
read from the working directory (see \link[base]{getwd}) unless the full path is specified.}

\item{VNC}{Object of class \code{\link[StQ]{VNC}}.}
}
\value{
Return an object of class \code{\link[StQ]{DD}}.
}
\description{
This function is a constructor for the class \link{DD} using the contents of 
the input DD file name (an xml file).

\code{RepoDDToDD} reads xml files with the definition and properties of every variable (a DD 
file) and transforms this content into an object of class \link{DD}. 

This function internally builds a \linkS4class{data.table} with columns \code{Variable}, 
\code{Sort}, \code{Class}, \code{Qual1} to \code{Qual}\emph{q} and \code{ValueRegExp}.
 
The column \code{Variable} contains the names of all variables, both questionnaire variables and 
metadata. This internal \linkS4class{data.table} is then used to initialize a \link{DD} 
object.

The column \code{Sort} takes values \code{'IDQual'}, \code{'NonIDQual'} or \code{'IDDD'}, for 
statistical unit qualifiers, variable name qualifiers and variable names, respectively.

The column \code{Class} specifies the class of the variable and takes values \code{numeric} or 
\code{character}. 

The column \code{Length} contains the highest length for each variable.

The columns \code{Qual1} to \code{Qual}\emph{q} contain the names of the qualifiers of every 
variable name (row).

The column \code{ValueRegExp} contains a regexp with the accepted values for each variable.
}
\examples{
# An example with data created previosly:
\dontrun{
ExcelName <- 'T:/E30163/E30163.NombresVariables_V1.xlsx'
VNC <- RepoXLSToVNC(ExcelName)
RepoDDFileName <- 'T:/E30163/E30163.DD_V1'
RepoDDToDD(RepoDDFileName, VNC)
}

}
