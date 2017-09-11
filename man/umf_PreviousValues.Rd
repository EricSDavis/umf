\name{umf_PreviousValues}
\alias{umf_PreviousValues}
\title{
umf_PreviousValues
}
\description{
Returns n previous values that exist in the first column of a data frame
}
\usage{
umf_PreviousValues(Value, n, Data)
}
\arguments{
  \item{Value}{
A numerical value to be searched for in the first column of a data frame.
}
 \item{n}{
Enter n previous values.
}
 \item{Data}{
A data frame containing one or more columns.
}
}
\details{
This function uses umf_NearestValue to find to return n previous values in a list from a data frame.
}
\value{
Returns a list of n previous values.
}

\author{
Eric S. Davis <ericscottdavis@outlook.com>
}

\examples{


## The function is currently defined as
function(Value, n, Data){
  NV <- c()
  NV[1] <- umf_NearestValue(Value, Data) - 1
  for (i in 1:n){
    NV[i+1] <- umf_NearestValue(NV[i], Data)-1
  }
  NV <- NV + 1
  return(NV)
}
}