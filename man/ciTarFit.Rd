\name{ciTarFit}
\alias{ciTarFit}
\title{
Fitting Threshold Cointegration
}
\description{
Fit a threshold cointegration regression between two time series.
}
\usage{
ciTarFit(y, x, model = c("tar", "mtar"), lag, thresh, 
    small.win, ...)
}
\arguments{
  \item{y}{dependent or left-side variable for the long-run regression.}
  \item{x}{independent or right-side variable for the long-run regression.}
  \item{model}{a choice of two models: tar or mtar.}
  \item{lag}{number of lags for the threshold cointegration regression.}
  \item{thresh}{a threshold value.}
  \item{small.win}{value of a small window for fitting the threshold cointegration regression.}
  \item{\dots}{additional arguments to be passed.}
}
\details{
This is the main function for threshold autoregression regression (TAR) in assessing the nonlinear threshold relation between two time series variables. It can be used to estimate four types of threshold cointegration regressions. These four types are TAR with a threshold value of zero; consistent TAR with a nonzero threshold; MTAR (momentum TAR) with a threshold value of zero; and consistent MTAR with a nonzero thresold. The option of small window is used in model selection because a comparison of AIC and BIC values should be based on the same number of regression observations.
}
\value{
Return a list object of class "ciTarFit" with the following components: 
  \item{y}{dependend variable} 
  \item{x}{independent variable} 
  \item{model}{model choice} 
  \item{lag}{number of lags} 
  \item{thresh}{threshold value} 
  \item{data.LR}{data used in the long-run regression} 
  \item{data.CI}{data used in the threshold cointegration regression} 
  \item{z}{residual from the long-run regression} 
  \item{lz}{lagged residual from the long-run regression} 
  \item{ldz}{lagged residual with first difference from the long-run regression} 
  \item{LR}{long-run regression} 
  \item{CI}{threshold cointegration regression} 
  \item{f.phi}{test with the null hypothesis of no threshold cointegration} 
  \item{f.apt}{test with the null hypothesis of no asymmetric price transmission in the long run}
  \item{sse}{value of sum of squared errors} 
  \item{aic}{value of Akaike Information Criterion} 
  \item{bic}{value of Bayesian Information Criterion.}
}
\references{
Balke, N.S., and T. Fomby. 1997. Threshold cointegration. International Economic Review 38(3):627-645.

Enders, W., and C.W.J. Granger. 1998. Unit-root tests and asymmetric adjustment with an example using the term structure of interest rates. Journal of Business & Economic Statistics 16(3):304-311.

Enders, W., and P.L. Siklos. 2001. Cointegration and threshold adjustment. Journal of Business and Economic Statistics 19(2):166-176.
}
\author{Changyou Sun (\email{csun@cfr.msstate.edu})}
\seealso{
\code{\link{print.ciTarFit}}; \code{\link{summary.ciTarFit}}; \code{\link{ciTarLag}} for lag selection; and \code{\link{ciTarThd}} for threshold selection; 
}
\examples{
data(daVi); data(daCh)
t.tar <- -8.041; t.mtar <- -0.451
mya <- myb <- 3       

(f1 <- ciTarFit(y=daVi, x=daCh, model="tar", lag=mya, thresh=0))
(f2 <- ciTarFit(y=daVi, x=daCh, model="tar", lag=mya, thresh=t.tar))
(f3 <- ciTarFit(y=daVi, x=daCh, model="mtar",lag=myb, thresh=0))
(f4 <- ciTarFit(y=daVi, x=daCh, model="mtar",lag=myb, thresh=t.mtar))
 
r0 <- cbind(summary(f1)$dia, summary(f2)$dia, summary(f3)$dia, 
    summary(f4)$dia)
(r1 <- r0[, c(1,2,4,6,8)])
col.name <- c("item","tar","c.tar","mtar","c.mtar")
diag <- r1[c(1:4, 6:7, 12:14, 8, 9, 11), col.name]
rownames(diag) <- 1:nrow(diag); diag

e1 <- summary(f1)$out; e2 <- summary(f2)$out 
e3 <- summary(f3)$out; e4 <- summary(f4)$out
(r2 <- rbind(e1, e2, e3, e4))

ee <- list(e1, e2, e3, e4); vect <- NULL
for (i in 1:4) {
  ef <- data.frame(ee[i]); ef
  vect2 <- c(paste(ef[3, "estimate"], ef[3, "sign"], sep=""), 
         paste("(", ef[3, "t.value"], ")", sep=""),
         paste(ef[4, "estimate"], ef[4, "sign"], sep=""), 
         paste("(", ef[4, "t.value"], ")", sep=""))    
  vect <- cbind(vect, vect2)
}
item <- c("pos.coeff","pos.t.value", "neg.coeff","neg.t.value")
ve <- data.frame(cbind(item, vect)); colnames(ve) <- col.name
( tab3.b <- rbind(diag, ve)[c(1:2, 13:16, 3:12), ])
}
\keyword{regression}