\name{umf_Potentiator}
\alias{umf_Potentiator}
\title{
umf_Potentiator
}
\description{
Applies the CFTR potentiator equation to the columns of a data frame.
}
\usage{
umf_Potentiator(Final, Initial, n = 6, Data)
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
}
\details{
This function applies the CFTR potentiator equation to the columns of a data frame.
The equation is defined as y2 - (slope*(x2-x1)+y1). Note: slope uses a different set of x and y values.
}
\value{
Returns a list of values per the equation described.
}

\author{
Eric S. Davis <ericscottdavis@outlook.com>
}

\examples{
Data <- data.frame("Time" = c(1,3,5,7,8,9,9.1,9.2,9.3,10, 11, 12, 13),
                       "y1" = c(1,1,1,1,2,3,6,16,7,5,-3,2,1),
                       "y2" = c(1,1,1,1,2,3,6,6,7,5,-12,2,1))
umf_Potentiator(10, 7, n = 6, Data)

## The function is currently defined as
function(Final, Initial, n = 6, Data){
  slice <- umf_Slice(Initial, Final, Data)
  begining <- umf_PreviousValues(Initial, n-2, Data)[length(umf_PreviousValues(Initial, n-2, Data))]
  slice2 <- umf_Slice(begining, Initial, Data)
  Y2 <- c()
  X2 <- c()
  X1 <- c()
  Y1 <- c()
  SY2 <- c()
  SX2 <- c()
  SX1 <- c()
  SY1 <- c()
  for (i in 2:length(colnames(slice))){
    Y2[i-1] <- max(slice[i])
    X2[i-1] <- (slice[[1]][which.max(slice[[i]])])
    X1[i-1] <- slice[[1]][1]
    Y1[i-1] <- slice[[i]][1]
    SY2[i-1] <- slice2[[i]][length(slice2[[2]])]
    SX2[i-1] <- slice2[[1]][length(slice2[[1]])]
    SX1[i-1] <- slice2[[1]][1]
    SY1[i-1] <- slice2[[i]][1]
  }
  slope <- ((SY2 - SY1)/(SX2 - SX1))
  formula <- (Y2 - (slope*(X2-X1)+Y1))
  return(formula)
}
}
