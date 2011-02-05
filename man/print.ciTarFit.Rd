\name{print.ciTarFit}
\alias{print.ciTarFit}
\title{Printing Outputs from Threshold Cointegration Regression}
\description{Show main outputs from threshold cointegration regression on screen.}
\usage{\method{print}{ciTarFit}(x, \dots)}
\arguments{
  \item{x}{an object of class 'ciTarFit'.}
  \item{\dots}{additional arguments to be passed.}
}
\details{
This is the print method for \code{ciTarFit} to show main outputs from the threshold cointegration regression.
}
\value{
Four main outputs from threshold cointegration regression are shown: long-run regression between the two price variables, threshold cointegration regression, hypothesis test of no cointegration, and hypothesis of no asymmetric adjustment.
}
\author{Changyou Sun (\email{csun@cfr.msstate.edu})}
\seealso{\code{\link{ciTarFit}}.}
\examples{
data(daVi); data(daCh)    
f1 <- ciTarFit(y=daVi, x=daCh, model="tar", lag=3, thresh=0)
f1
}
\keyword{methods}
