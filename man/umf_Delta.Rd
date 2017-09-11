\name{umf_Delta}
\alias{umf_Delta}
\title{
umf_Delta
}
\description{
Returns the change in values in the columns of a data frame
}
\usage{
umf_Delta(Final, Initial, n = 6, Data, method = "delta")
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
  \item{method}{
Contains two methods for analysis. "delta" (default) uses the average of the final values in the range. "deltaMin" uses the minimum value in the range between Final and Initial.
}
}
\details{
This function computes the difference between Final and Initial values in a data frame.
It contains two methods for analysis. "delta" (default) uses the average of the final values in the range. "deltaMin" uses the minimum value in the range between Final and Initial.
}
\value{
Returns a list of differences between Final and Initial.
}

\author{
Eric S. Davis <ericscottdavis@outlook.com>
}

\examples{
Data <- data.frame("Time" = c(1,3,5,7,8,9,9.1,9.2,9.3,10, 11, 12, 13),
                       "y1" = c(1,1,1,1,2,3,6,16,7,5,-3,2,1),
                       "y2" = c(1,1,1,1,2,3,6,6,7,5,-12,2,1))
umf_Delta(7, 2, Data)

## The function is currently defined as
function(Final, Initial, n = 6, Data, method = "delta"){
  if (method == "delta"){
    delta <- umf_GetMean(Final, n, Data) - umf_GetMean(Initial, n, Data)
    return(delta)
  }
  if (method == "deltaMin"){
    slice <- umf_Slice(Initial, Final, Data)
    deltaMin  <- min(slice) - umf_GetMean(Initial, n, Data)
    return(deltaMin)
  }
}
}
