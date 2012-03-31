\name{plot.ciTarLag}
\alias{plot.ciTarLag}
\title{Plot of AIC and BIC for Threshold Cointegration by Lag}
\description{Plot the values of AIC and BIC for threshold cointegration regressions by lag length.}
\usage{\method{plot}{ciTarLag}(x, \dots)}
\arguments{
  \item{x}{an object of class 'ciTarLag'.}
  \item{\dots}{additional arguments to be passed.}
}
\details{
This demonstrates the trend of AIC and BIC changes of threshold cointegration regressions by lag. It facilitates the selection of the best lag for a threshold cointegration model.
}
\value{A panel of two graphs.}
\references{
Enders, W., and C.W.J. Granger. 1998. Unit-root tests and asymmetric adjustment with an example using the term structure of interest rates. Journal of Business & Economic Statistics 16(3):304-311.
}
\author{Changyou Sun (\email{csun@cfr.msstate.edu})}
\seealso{\code{\link{ciTarLag}}; \code{\link{ciTarThd}}; and \code{\link{ciTarFit}}.}

\examples{
data(daVi); data(daCh)
t.tar <- -8.041; t.mtar <- -0.451
mx <- 4
(g4 <-ciTarLag(y=daVi, x=daCh, model="mtar",maxlag=mx, 
    adjust=TRUE, thresh=t.mtar))
plot(g4)
}
\keyword{methods}
