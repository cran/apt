\name{summary.ciTarFit}
\alias{summary.ciTarFit}
\title{Summary of Results from Threshold Cointegration Regression}
\description{This summarizes the main results from threshold cointegration regression.}
\usage{\method{summary}{ciTarFit}(object, digits=3, \dots)}
\arguments{
  \item{object}{an object of class 'ciTarFit'.}
  \item{digits}{number of digits for rounding outputs.}
  \item{\dots}{additional arguments to be passed.}
}
\details{
This wraps up the outputs from threshold cointegration regression in two data frames, one for diagnostic statistics and the other for coeffients.
}
\value{A list with two data frames. \code{dia} contains the main model specifications and hypothesis test results. \code{out} contains the regression results for both the long run (LR) and threshold cointegration (CI).
}
\author{Changyou Sun (\email{csun@cfr.msstate.edu})}
\seealso{\code{\link{ciTarFit}}.}
\examples{
data(daVi); data(daCh)
(f4 <- ciTarFit(y=daVi, x=daCh, model="mtar",lag=3, thresh=-0.451))
f4.sum <- summary(f4)
names(f4.sum)
f4.sum$dia
f4.sum$out
f4.sum 
} 
\keyword{methods}
