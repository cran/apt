\name{ecmAsyTest}
\alias{ecmAsyTest}
\title{Hypothesis Tests on Asymmetric Error Correction Model}
\description{Conduct several F-tests on the coefficients from asymmetric ECM.}
\usage{
ecmAsyTest(w, digits = 3, ...)
}
\arguments{
  \item{w}{an object of 'ecmAsyFit' class.}
  \item{digits}{number of digits used in rounding outputs.}
  \item{\dots}{additional arguments to be passed.}
}
\details{
There are two ECM equations for the two price series. In each equation, four types of hypotheses are tested; equilibrium adjustment path symmetry on the error correction terms (H1), Granger causality test (H2), distributed lag symmetry at each lag (H3), and cumulative asymmetry of all lags (H4). The latter two tests are only feasible and availabe for models with split variables. The number of H3 tests is equal to the number of lags. 
}
\value{
Return a list object with the following components: 
    \item{H1ex}{H01 in equation x: equilibrium adjustment path symmetry} 
    \item{H1ey}{H01 in equation y: equilibrium adjustment path symmetry} 
    \item{H2xx}{H02 in equation x: x does not Granger cause x} 
    \item{H2yx}{H02 in equation y: x does not Granger cause y} 
    \item{H2xy}{H02 in equation x: y does not Granger cause x} 
    \item{H2yy}{H02 in equation y: y does not Granger cause y} 
    \item{H3xx}{H03 in equation x: distributed lag symmetry of x at each lag} 
    \item{H3yx}{H03 in equation y: distributed lag symmetry of x at each lag} 
    \item{H3xy}{H03 in equation x: distributed lag symmetry of y at each lag} 
    \item{H3yy}{H03 in equation y: distributed lag symmetry of y at each lag}
    \item{H4xx}{H04 in equation x: cumulative asymmetry of x for all lags}  
    \item{H4yx}{H04 in equation y: cumulative asymmetry of x for all lags}  
    \item{H4xy}{H04 in equation x: cumulative asymmetry of y for all lags} 
    \item{H4yy}{H04 in equation y: cumulative asymmetry of y for all lags}
    \item{out}{summary of the four types of hypothesis tests}
}
\references{Frey, G., and M. Manera. 2007. Econometric models of asymmetric price transmission. Journal of Economic Surveys 21(2):349-415.}
\author{Changyou Sun (\email{csun@cfr.msstate.edu})}
\seealso{\code{\link{ecmAsyFit}} and \code{\link{ecmDiag}}.}

\examples{

data(daVi); data(daCh)
t.mtar <- -0.451
aem <- ecmAsyFit(y=daVi, x=daCh, lag=4, model="mtar", split=TRUE, thresh=t.mtar)
aem
summary(aem)
ecmDiag(aem, 3)
(tes <- ecmAsyTest(aem)$out)
}
\keyword{regression}