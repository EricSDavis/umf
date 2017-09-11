\name{umf_GetMean}
\alias{umf_GetMean}
\title{
umf_GetMean
}
\description{
Returns the mean of Timepoint to Timepoint - n previous values in each column of Data
}
\usage{
umf_GetMean(Timepoint, n, Data)
}
\arguments{
  \item{Timepoint}{
A numerical value.
}
  \item{n}{
The number of values prior to Timepoint to be averaged. For a single value set n = 0.
}
  \item{Data}{
A data frame containing 2 or more columns.
}
}
\details{
This function searches for the nearest (previous) existing value to Timepoint (using umf_NearestValue() and umf_PreviousValue()), takes a slice of the data set (using umf_Slice()), and returns the average of every column of Data from Timepoint to Timepoint - n previous values.
}
\value{
Returns a list of means from Timepoint to Timepoint - n previous values in each column of Data
}

\author{
Eric S. Davis <ericscottdavis@outlook.com>
}

\examples{
Data <- data.frame("Time" = c(1,3,5,7,8,9,9.1,9.2,9.3,10, 11, 12, 13),
                       "y1" = c(1,1,1,1,2,3,6,16,7,5,-3,2,1),
                       "y2" = c(1,1,1,1,2,3,6,6,7,5,-12,2,1))
umf_GetMean(7, 2, Data)

## The function is currently defined as
function(Timepoint, n, Data){
  NV <- umf_PreviousValues(Timepoint, n, Data)
  Slice <- umf_Slice(NV[length(NV)], Timepoint, Data)
  Average <- c()
  for (i in 2:length(colnames(Data))) {
    Average[i] <- mean(Slice[[i]])
  }
  Average <- Average[2:length(Average)]
  return(Average)
}
}
