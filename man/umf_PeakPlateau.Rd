\name{umf_PeakPlateau}
\alias{umf_PeakPlateau}
\title{
umf_PeakPlateau
}
\description{
Returns the peaks and plateau values for the columns of a data frame
}
\usage{
umf_PeakPlateau(Final, Initial, n = 6, Data, PeakName = delta_peak, PlateauName = delta_plateau)
}
\arguments{
  \item{Final}{
A numerical value.
}
  \item{Initial}{
A numerical value.
}
  \item{n}{
The number of values prior to Initial to be used. Suggested n = 6.
}
  \item{Data}{
A data frame containing 2 or more columns.
}
  \item{PeakName}{
Specify a name for the peak output.
}
 \item{PlateauName}{
Specify a name for the plateau output.
}
}
\details{
This function computes the peaks and plateaus for the columns of a data frame.
}
\value{
Returns a list of peaks and plateaus for the columns of a data frame.
}

\author{
Eric S. Davis <ericscottdavis@outlook.com>
}

\examples{
Data <- data.frame("Time" = c(1,3,5,7,8,9,9.1,9.2,9.3,10, 11, 12, 13),
                       "y1" = c(1,1,1,1,2,3,6,16,7,5,-3,2,1),
                       "y2" = c(1,1,1,1,2,3,6,6,7,5,-12,2,1))
umf_PeakPlateau(10, 7, n = 6, Data)

## The function is currently defined as
function(Final, Initial, n = 6, Data, PeakName = delta_peak, PlateauName = delta_plateau){
  delta_peak <- umf_FindPeaks(Initial, Final, n, Data)
  delta_plateau <- umf_GetMean(Final, n, Data) - umf_GetMean(Initial, n, Data)
  peak_plat <- cbind(delta_peak, delta_plateau)
  colnames(peak_plat) <- c(PeakName, PlateauName)
  return(peak_plat)
}
}
