% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constant.R
\name{regressoR.direct.constant}
\alias{regressoR.direct.constant}
\title{Create a Direct Constant Interpolation from the Data}
\usage{
regressoR.direct.constant(metric, transformation.x = NULL,
  transformation.y = NULL, metric.transformed = NULL, q = 0)
}
\arguments{
\item{metric}{an instance of \code{RegressionQualityMetric}}

\item{transformation.x}{the transformation along the \code{x}-axis, or
\code{NULL} if none was applied to the data}

\item{transformation.y}{the transformation along the \code{y}-axis, or
\code{NULL} if none was applied to the data}

\item{metric.transformed}{the transformed metric for the first fitting step}

\item{q}{the ignored quality parameter}
}
\value{
On success, an instance of \code{\link{DirectFitModel.new}},
  \code{NULL} on failure.
}
\description{
A direct constant interpolation is computed.
}
