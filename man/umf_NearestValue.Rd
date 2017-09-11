\name{umf_NearestValue}
\alias{umf_NearestValue}
\title{
umf_NearestValue
}
\description{
Returns nearest (previous) existing value from the first column of values in a data frame.
}
\usage{
umf_NearestValue(Value, Data)
}

\arguments{
  \item{Value}{
A numerical value to be searched for in the first column of a data frame.
}
  \item{Data}{
A data frame containing one or more columns.
}
}
\details{
This function starts at Value and subtracts one in a loop until it finds an existing value in the first column of Data and returns this new value.

An error will be produced if Value is smaller than the first entry of Data. This function is built to work with whole numbers. It will not produce the nearest (previous) entry for decimal numbers between whole integers.
}
\value{
Returns nearest (previous) existing value from the first column of values in a data frame.
}
\references{
None
}
\author{
Eric S. Davis <ericscottdavis@outlook.com>
}
\examples{
Data <- data.frame("Time" = c(1,3,5,7,8,9,9.1,9.2,9.3,10, 11, 12, 13),
                       "y1" = c(1,1,1,1,2,3,6,16,7,5,-3,2,1),
                       "y2" = c(1,1,1,1,2,3,6,6,7,5,-12,2,1))
                       
umf_NearestValue(16, Data) #Should return 13

## The function is currently defined as
function (Value, Data) 
{
    continue <- TRUE
    Column <- Data[[1]]
    while (continue) {
        if (Value < Column[1]) {
            return(paste0("Please enter a number greater than ", 
                Column[1]))
        }
        for (i in 1:length(Column)) {
            if (Column[i] == Value) {
                Value = Value
                continue <- FALSE
                return(Value)
            }
        }
        Value <- Value - 1
    }
  }
}