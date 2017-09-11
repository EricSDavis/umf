\name{umf_FindPeaks}
\alias{umf_FindPeaks}
\title{
umf_FindPeaks
}
\description{
Returns a list of the highest peaks (+ or -) for each column of a data frame (excluding the first column) subracted from the average of timepoint - n previous points.
}
\usage{
umf_FindPeaks(Timepoint1, Timepoint2, n, Data)
}
\arguments{
  \item{Timepoint1}{
Define range to search beginning with Timepoint1.
}
  \item{Timepoint2}{
Define range to search ending with Timepoint2.
}
  \item{n}{
Subtract the peak from the average of Timepoint1 - n values. For no average, use n = 0.
}
  \item{Data}{
A data frame with 2 or more columns.
}
}
\details{
This function returns a list of the highest peaks (+ or -) for each column in Data. It defines two slices of data (using umf_Slice() and umf_PreviousValues()) based on the timepoints given, and returns the max() or min() subtracted from the mean() starting point.
}
\value{
Returns a list of the highest peaks (+ or -) for each column in Data.
}

\author{
Eric S. Davis <ericscottdavis@outlook.com>
}

\examples{
Data <- data.frame("Time" = c(1,3,5,7,8,9,9.1,9.2,9.3,10, 11, 12, 13),
                       "y1" = c(1,1,1,1,2,3,6,16,7,5,-3,2,1),
                       "y2" = c(1,1,1,1,2,3,6,6,7,5,-12,2,1))
umf_FindPeaks(7, 13, 2, Data)                      
                       
## The function is currently defined as
function (Timepoint1, Timepoint2, n, Data){
  MaxDiff <- c()
  MinDiff <- c()
  Diff <- c()
  NV <- umf_PreviousValues(Timepoint1, n, Data)
  Slice1 <- umf_Slice(NV[length(NV)], Timepoint1, Data)
  Slice2 <- umf_Slice(Timepoint1, Timepoint2, Data)
  for (i in 2:length(colnames(Data))){  
    MaxDiff[i] <- abs(max(Slice2[[i]])) - mean(Slice1[[i]])
    MinDiff[i] <- abs(min(Slice2[[i]])) - mean(Slice1[[i]])
  }
  for (i in 2:length(MaxDiff)){
    if (MaxDiff[i] > MinDiff[i]){
      Diff[i] <- MaxDiff[i]
    } else if (MinDiff[i] > MaxDiff[i]){
      Diff[i] <- -MinDiff[i]
    }
  }
  Diff <- Diff[2:length(Diff)]
  return(Diff)
}
