% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nbaLinRegFitProjectionsFunction.R
\name{nbaLinRegProjections}
\alias{nbaLinRegProjections}
\title{NBA Linear Regression Model Projections Function}
\usage{
nbaLinRegProjections(modelDate = "2_12_2017", modelSite = "Draftkings",
  lmFit = lmer10, lmModel = "lmer10", errorHandleProp = NULL,
  errorHandleValue = NULL, cookie = labsC)
}
\arguments{
\item{modelDate}{"12_27_2016"}

\item{modelSite}{Draftkings or Fanduel}

\item{lmFit}{lmer10}

\item{lmModel}{"lmer10"}
}
\value{
nba data frame of daily projections using a linear model to predict values
}
\description{
NBA Linear Regression Model Projections Function
}
\examples{
nbaLinRegProjections(modelDate="11_15_2016",modelSite="Draftkings",lmFit=lmer10,lmModel = "lmer10")
}

