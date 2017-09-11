\name{umf_Baseline}
\alias{umf_Baseline}
\title{
umf_Baseline
}
\description{
Returns a list of baseline values for columns of a data frame.
}
\usage{
umf_Baseline(Timepoint, n = 6, Data)
}
\arguments{
  \item{Timepoint}{
A numerical value.
}
  \item{n}{
The number of values prior to Timepoint to be averaged. Suggested n = 6.
}
  \item{Data}{
A data frame containing 2 or more columns.
}
}
\details{
Uses umf_GetMean() function to obtain baseline values for a dataset.
}
\value{
Returns a list of means from Timepoint to Timepoint - n previous values in each column of Data.
}

\author{
Eric S. Davis <ericscottdavis@outlook.com>
}

\examples{
Data <- data.frame("Time" = c(1,3,5,7,8,9,9.1,9.2,9.3,10, 11, 12, 13),
                       "y1" = c(1,1,1,1,2,3,6,16,7,5,-3,2,1),
                       "y2" = c(1,1,1,1,2,3,6,6,7,5,-12,2,1))
umf_Baseline(7, 2, Data)

## The function is currently defined as
function(Timepoint, n = 6, Data){
  baseline <- umf_GetMean(Timepoint, n, Data)
  return(baseline)
}