% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readNHLCsvsFunction.R
\name{readNHLCSVs}
\alias{readNHLCSVs}
\title{Read NHL csv saved Models}
\usage{
readNHLCSVs(modelDate = "12_23_2016", modelPosition = "All")
}
\arguments{
\item{modelDate}{"1_1_2017"}

\item{modelPosition}{"ALL","S","G"... Both,Skaters Only,Goalies Only..}
}
\value{
pulls model into r
}
\description{
Read NHL csv saved Models
}
\examples{
nhl12_23_2016 <- readNHLCSVs(modelDate = "12_23_2016",modelPosition="All")
}

