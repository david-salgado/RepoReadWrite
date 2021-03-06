% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RepoFileVersion.R
\name{RepoFileVersion}
\alias{RepoFileVersion}
\title{Return index number of the last version of a file}
\usage{
RepoFileVersion(Path, FileNamePattern)
}
\arguments{
\item{Path}{Character vector of length 1 with the path of the search directory.}

\item{FileNamePattern}{Character vector of length 1 with the name of the file whose last version 
is queried.}
}
\value{
Integer vector of length 1 expressing the last version index number of the input file 
name in the input search directory.
}
\description{
\code{RepoFileVersion} returns the index number of the last version of a file in an 
input directory.

This function searches in the input directory \code{Path} the last version of the input file name 
\code{FileNamePattern} and returns the corresponding index number of such version.

This function is thought for files whose names has a suffix with the version corresponding to its 
contents (either definitive or partial version).
}
\examples{
\dontrun{
RepoFileVersion('T:/E30183', 'FF_V1.MM122014')
RepoFileVersion('T:/E30183', 'FT_V1.MM122016')
RepoFileVersion('T:/E30183', 'FT_V1.MM122016')
}

}
