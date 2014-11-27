\name{guiApt}
\alias{guiApt}
\title{Graphical User Interface for the apt Package}

\description{
  This graphical user interface (GUI) contains most functions in the \code{apt} package. Users can call these functions on this interface. 
}

\usage{guiApt(pos = 1)}

\arguments{
  \item{pos}{ specifying where the seven tabs should be placed: bottom (\code{pos = 1}; default), left (\code{pos = 2}), top (\code{pos = 3}), or right (\code{pos = 4}).}
}

\details{
The \code{gWidgets} package is used to create this GUI. The objective in this application is to develop a GUI for the \code{apt} package and allow users to call individual functions on the interface and conduct threshold cointegration analysis in an intuitive way. The GUI has menus and toolbars at the top. Then seven tabbed notebook container widgets are used to hold messages and individual function calls.

The inputs are two single time series data, and they should be processed within R first before a GUI can be used for analyses. Outputs can be displayed within the interface, but for simplicity, the R console is used to display all outputs in this application.

See \code{\link{guiCor}} for package installation issues.
}

\value{
No value returnd from this function call. A GUI is generated as the side effect.
}

\references{
Lawrence, M.F., and J. Verzani. 2012. Programming graphical user interfaces in R. Ed. CRC Press. 466 P.
}

\author{Changyou Sun (\email{cs258@msstate.edu})}

\seealso{\code{\link{guiCor}}}

\examples{
  \dontrun{
    library(RGtk2); library(gWidgetsRGtk2)
    options(guiToolkit = "RGtk2")  # may need this for some computers
    
    data(daVich); prVi <- daVich[, 1] ; prCh <- daVich[, 2]    
    guiApt()
    guiApt(2) # tabs on the left side
  }
}
\keyword{misc}