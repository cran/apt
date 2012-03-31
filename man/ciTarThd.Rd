\name{ciTarThd}
\alias{ciTarThd}
\title{
Threshold Selection for Threshold Cointegration Regression
}
\description{
Select the best threshold for threshold cointegration regression by sum of squared errors
}
\usage{
ciTarThd(y, x, model = c("tar", "mtar"), lag, th.range = 0.15, 
    digits = 3, ...)
}
\arguments{
  \item{y}{dependent or left-side variable for the long-run regression.}
  \item{x}{independent or right-side variable for the long-run regression.}
  \item{model}{a choice of two models, either tar or mtar.}
  \item{lag}{number of lags.} 
  \item{th.range}{the percentage of observations to be excluded from the search.}
  \item{digits}{number of digits used in rounding outputs.}
  \item{\dots}{additional arguments to be passed.}
}
\details{
The best threshold is determined by fitting the regression for possible threshold values, sorting the results by sum of squared errors (SSE), and selecting the best with the lowest SSE. To have sufficient observations on either side of the threshold value, certain percentage of observations on the top and bottoms are excluded from the search path. This is usually set as 0.15 by the \code{th.range} (Chan 1993).
}
\value{
Return a list object of class "ciTarThd" with the following components: 
  \item{model}{model choice} 
  \item{lag}{number of lags} 
  \item{th.range}{the percentage of observations excluded} 
  \item{th.final}{the best threshold value}
  \item{ssef}{the best (i.e., lowest) value of SSE}
  \item{obs.tot}{total number of observations in the raw data}
  \item{obs.CI}{number of observations used in the threshold cointegration regression}      
  \item{basic}{a brief summary of the major outputs}
  \item{path}{a data frame of the search record (number of regression, threshold value, SSE, AIC, and BIC values).}
}
\references{
Chan, K.S. 1993. Consistency and limiting distribution of the least squares estimator of a threshold autoregressive model. The Annals of Statistics 21(1):520-533.

Enders, W., and C.W.J. Granger. 1998. Unit-root tests and asymmetric adjustment with an example using the term structure of interest rates. Journal of Business & Economic Statistics 16(3):304-311.
}
\author{Changyou Sun (\email{csun@cfr.msstate.edu})}
\seealso{
\code{\link{plot.ciTarThd}} for the graphical trend; \code{\link{ciTarFit}}; and \code{\link{ciTarLag}} 
}
\examples{
data(daVi); data(daCh)

# 1. best threshold value for tar
t3 <- ciTarThd(y=daVi, x=daCh, model="tar", lag=0); 
(th.tar <- t3$basic)
plot(t3)

# 2. best threshold value for mtar 
t4 <- ciTarThd(y=daVi, x=daCh, model="mtar", lag=0) 
(th.mtar <- t4$basic)
plot(t4)

# 3. best threshold by lag; 
# The following two loops are executable.
# But it may take about 5 to 10 minutes to run 
# because there are over 1,000 linear regressions in the loops.

\dontrun{
for (i in 1:12) {
     t3a <- ciTarThd(y=daVi, x=daCh, model="tar", lag=i) 
     th.tar[i+2] <- t3a$basic[,2]
 }
th.tar

for (i in 1:12) {
     t4a <- ciTarThd(y=daVi, x=daCh, model="mtar", lag=i) 
     th.mtar[i+2] <- t4a$basic[,2]
 }
th.mtar
}

# 4. The final result of threshold selection are as follows.
# The best threshold values do not vary much by lag. 
# This is not suprising because more lagged values do not 
# reduce SSE much.

t.tar <- -8.041; t.mtar <- -0.451    # lag = 0 to 4
t.tar <- -8.701; t.mtar <- -0.451    # lag = 5 to 12
}
\keyword{regression}