% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{summary.SUMMERmodel.svy}
\alias{summary.SUMMERmodel.svy}
\title{Summary method for the smoothing model and output from \code{smoothSurvey}.}
\usage{
\method{summary}{SUMMERmodel.svy}(object, ...)
}
\arguments{
\item{object}{output from \code{\link{smoothSurvey}}}

\item{...}{not used}
}
\description{
This function is the summary method for class \code{SUMMERmodel.svy}.
}
\examples{
\dontrun{
data(DemoData2)
data(DemoMap2)
fit0 <- smoothSurvey(data=DemoData2,  
Amat=DemoMap2$Amat, responseType="binary", 
responseVar="tobacco.use", strataVar="strata", 
weightVar="weights", regionVar="region", 
clusterVar = "~clustid+id", CI = 0.95)
summary(fit0)
}
}
\seealso{
\code{\link{summary.SUMMERmodel.svy}}
}
\author{
Zehang Li
}
