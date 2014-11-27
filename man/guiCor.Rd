\name{guiCor}
\alias{guiCor}
\title{Graphical User Interface for Showing Variable Correlation}

\description{
  This graphical user interface (GUI) can show the correlation between two random variables with normal copula distribution. 
}

\usage{guiCor()}

\details{
The \code{gWidgets} package is used to create this GUI. In this application, two random variables are generated from R and then plotted on a graph. Their correlation changes with each random sample. When many plots are drawn and compared, the change in correlation pattern can be revealed. To make the pattern more predictable, we need a random number generator to draw variables randomly and consistently. The \code{rcoupla()} function in the \code{copula} package is a good choice for this objective. The relation between the variables is specified through a normal copula, as defined by \code{normalCopula()}. The correlation between two normally distributed variables is controlled by a single parameter with the value range [-1, 1]. In showing these pairs of variables on the screen, a number of graphics parameter values can also be manipulated to improve the presentation, e.g., the size and color of points.

Two versions of the graph are considered. In the static version, the goal is to show one plot each time. The appearance of the plot can be affected by four choices: the number of points, their color, size, and the correlation parameter between the two variables. In the dynamic version, a \code{for} loop is employed to show many plots continuously. Only one choice is left for users, i.e., the number of plots. Other choices are fixed: the number of points is 1,000, the point size is 3, and the color is changed automatically with each plot.

Important installation notes: Several packages are needed to run this GUI: \code{gWidgets}, \code{RGtk2}, \code{cairoDevice}, and \code{gWidgetsRGtk2}. \code{gWidgets} is on the "Depends" list so it will be installed with \code{apt} together. \code{RGtk2}, \code{cairoDevice}, and \code{gWidgetsRGtk2} are on the "Suggests" list so they will not be installed by default. Users can install them individually, when they want to use \code{guiCor}  and \code{guiApt}. 

 \code{RGtk2} should be installed first. It can be challenging to install this package. I am using Microsoft Windows system so the following note is specifically for Windows users. First, install this package by \code{install.packages("RGtk2")}. Then load/attach it by \code{library(RGtk2)}. When a small window pops up and ask you to install \code{GTK+}, choose yes. Then shut down the whole R program, restart it, and load/attach this library again. Install other packages, i.e., \code{cairoDevice} and \code{gWidgetsRGtk2}. Finally, install \code{apt}. Submit \code{library(apt); help("guiCor")}, and copy the sample codes below to initiate the GUI. If you can go through the process as described, then that is perfect.
 
 Many problems may arise in loading and attaching the \code{RGk2} package, depending on the operating system and hardware on a computer. A typical and annoying problem is that the computer may ask you to installl the GTK+ again and again. This is because the \code{RGk2} package cannot find the \code{GTK+} software, or the \code{GTK+} installed on your computer does not meet the requirement (e.g., 32 or 64 bit).  \code{GTK+} is an outside and independent software program and it has many versions. A typical error message is like this: "Failed to load RGtk2 dynamic library." If that is the case, read the error message carefully and download the suggested version of \code{GTK+} manually from the internet, unzip it, and place all the folders in one place on your local drive. Then add a directory in the path of environmental variable through the control panel on your computer. For example, I download a zip file with the name of \code{gtk+-bundle_2.22.1-20101229_win64.zip}. The following directory is added and recognized: \code{C:\\CSprogram\\myRsoftware\\gtk22211win64\\bin}.
 
 In sum, running the GUI needs the toolkit of \code{GTK+}, which is independent from R. It needs to be installed on a local drive and recognized by R. The package of \code{rattle} faces similar challenges as it uses this toolkit too. Search the internet for similar problems and solutions.  
}

\value{
No value returnd from this function call. A GUI is generated as the side effect.
}

\references{
Lawrence, M.F., and J. Verzani. 2012. Programming graphical user interfaces in R. Ed. CRC Press. 466 P.

Nelsen, R.B. 2006. An Introduction to Copulas. 2nd Ed. Springer. 269 P.
}

\author{Changyou Sun (\email{cs258@msstate.edu})}

\seealso{\code{\link{guiApt}}}

\examples{
  \dontrun{
    library(copula)
    library(RGtk2)
    library(cairoDevice)
    library(gWidgetsRGtk2)
    options(guiToolkit = "RGtk2")  # may need this for some computers 
    
    guiCor()
  }
}
\keyword{misc}