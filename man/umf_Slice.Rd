\name{umf_Slice}
\alias{umf_Slice}
\title{
umf_Slice
}
\description{
Returns a slice of the input data frame between Timepoint1 and Timepoint2.
}
\usage{
umf_Slice(Timepoint1, Timepoint2, Data)
}
\arguments{
  \item{Timepoint1}{
Starting point of the slice of Data.
}
  \item{Timepoint2}{
Ending point of the slice of Data.
}
  \item{Data}{
A data frame containing one or more columns.
}
}
\details{
This function takes the nearest (previous) existing value (see umf_NearestValue()) for Timepoint1 and Timepoint2 from the first column of Data and returns a sliced data frame of Data.

Designed to work with data that contains increasing time values in the first column of Data. Timepoint2 must be greater than Timepoint1.
}
\value{
Returns a sliced data frame of Data.
}

\author{
Eric S. Davis <ericscottdavis@outlook.com>
}

\examples{

Data <- data.frame("Time" = c(1,3,5,7,8,9,9.1,9.2,9.3,10, 11, 12, 13),
                       "y1" = c(1,1,1,1,2,3,6,16,7,5,-3,2,1),
                       "y2" = c(1,1,1,1,2,3,6,6,7,5,-12,2,1))
umf_Slice(3, 10, Data)

## The function is currently defined as
function (Timepoint1, Timepoint2, Data) 
{
    Value1 <- umf_NearestValue(Timepoint1, Data)
    Value2 <- umf_NearestValue(Timepoint2, Data)
    RowValueI <- as.numeric(rownames(Data)[Data[1] == Value1])
    RowValueF <- as.numeric(rownames(Data)[Data[1] == Value2])
    Data <- Data[RowValueI:RowValueF, ]
    return(Data)
  }
}

