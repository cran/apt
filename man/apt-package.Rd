\name{apt-package}
\alias{apt}
\alias{apt-package}
\docType{package}
\title{Asymmetric Price Transmission}

\description{
This package focuses on asymmetric price transmission between two time series. The name of functions and datasets reveals the categories they belong to. A prefix of \code{da} is for datasets, \code{ci} for cointegration, and \code{ecm} for error correction model.

This package focuses on the price transmission between \emph{two} price variables. Therefore, objectives like fitting an error correction model for more than two variables are beyond the scope of this package. 

As a teaching demo, a graphical user interface is also developed for the main functions in this package. Several packages are needed to run the two included GUIs: \code{guiCor} and \code{guiApt}. See the note at \code{guiCor} for installation detail.
}

\details{
\tabular{ll}{
Package: \tab apt\cr
Type: \tab Package\cr
Version: \tab 2.3\cr
Date: \tab 2010-11-03 (first built); 2014-11-28 (last) \cr
Depends: \tab R (>= 3.0.0), erer, gWidgets \cr
Imports: \tab car, urca, copula \cr
Suggests: \tab RGtk2, gWidgetsRGtk2, cairoDevice \cr
License: \tab GPL \cr
LazyLoad: \tab yes\cr
}
}

\author{Changyou Sun (\email{cs258@msstate.edu})}
\keyword{package} 