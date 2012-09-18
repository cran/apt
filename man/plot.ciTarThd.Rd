\name{plot.ciTarThd}
\alias{plot.ciTarThd}
\title{Plot for Threshold Value Selection}
\description{Plot the values of SSE, AIC and BIC for threshold cointegration regressions by threshold value.}
\usage{\method{plot}{ciTarThd}(x, \dots)}
\arguments{
  \item{x}{an object of class 'ciTarThd'.}
  \item{\dots}{additional arguments to be passed.}
}
\details{
This function generates three plots in one window. They reveals the relationship between SSE (sum of squared errors), AIC, BIC and threshold value. The best threshold value is associated with the lowest SSE value. 
}
\value{A plot of three graphs (SSE, AIC, and BIC versus threshold value).}
\references{
Chan, K.S. 1993. Consistency and limiting distribution of the least squares estimator of a threshold autoregressive model. The Annals of Statistics 21(1):520-533.

Enders, W., and C.W.J. Granger. 1998. Unit-root tests and asymmetric adjustment with an example using the term structure of interest rates. Journal of Business & Economic Statistics 16(3):304-311.
}
\author{Changyou Sun (\email{csun@cfr.msstate.edu})}
\seealso{\code{\link{ciTarThd}} and \code{\link{ciTarFit}}.}
\examples{
# see example at daVich
}
\keyword{methods}
