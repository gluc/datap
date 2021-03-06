% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_syntax.R
\name{CheckSyntax}
\alias{CheckSyntax}
\title{Loads a context definition and checks its syntax.}
\usage{
CheckSyntax(con)
}
\arguments{
\item{con}{a connection containing the meta data}
}
\description{
Loads a context definition and checks its syntax.
}
\examples{
filePath <- system.file("extdata", "context1.yaml", package="datap")
errors <- CheckSyntax(filePath)
print(errors)

}

