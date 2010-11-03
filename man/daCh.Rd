\name{daCh}
\alias{daCh}
\docType{data}
\title{Import price of wooden beds from China}
\description{
This data set contains the unit import price of wooden beds from China to the United States.
}
\usage{data(daCh)}
\format{A monthly time series from Janurary 2002 to Janurary 2010 with 97 observations.}
\details{
Under the Harmonized Tariff Schedule (HTS) system, the commodity of wooden beds is classified as HTS 9403.50.9040. The monthly cost-insurance-freight values in dollar and quantities in piece are reported by country from U.S. ITC (2010). The unit price (\$/piece) is calculated as the ratio of value over quantity.
}
\source{
U.S. ITC, 2010. Interactive tariff and trade data web. \url{http://dataweb.usitc.gov} (Assecced on March 1, 2010).
}
\references{
Sun, C. 2011. Price dynamics in the import wooden bed market of the United States. Under preparation.  
}
\examples{
data(daCh)
plot(daCh)
}
\keyword{datasets}