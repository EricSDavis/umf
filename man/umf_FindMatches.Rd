\name{umf_FindMatches}
\alias{umf_FindMatches}
\title{
umf_FindMatches
}
\description{
Returns a vector of common values between two lists with optional partial matching.
}
\usage{
umf_FindMatches(Seq1, Seq2, unique=T, partial=T)
}
\arguments{
  \item{Seq1}{
A list of one or more items to search against Seq2
}
  \item{Seq2}{
A list of one or more items to be searched against by Seq1
}
  \item{unique}{
If unique=T, only the unique results will be displayed
}
\arguments{
  \item{partial}{
If partial=T, partial matching will be returned. If partial=F, strict matching (perfect matches) will be returned.
}
}
\details{
Uses partial or complete string matching to return common values between two vectors.
}
\value{
Returns a vector of matches between the two input vectors.
}

\author{
Eric S. Davis <ericscottdavis@outlook.com>
}

\examples{
list <- c("ABC", "BCA", "DCB", "DCBA")
patterns <- c("DCB", "BC")

umf_FindMatches(patterns, list, unique=T, partial=T)
umf_FindMatches(patterns, list, unique=T, partial=F)

## The function is currently defined as
function(Seq1, Seq2, unique=T, partial=T){
  common <- c()
  if (partial == T){
    for (i in seq(1:length(Seq1))){
      common <- c(common, Seq2[grep(Seq1[i], Seq2)])
    }
    if (unique == T){
      return(unique(common))
    }else{
      return(common)
    }
  }else{
    for (i in seq(1:length(Seq1))){
      for (j in seq(1:length(Seq2))){
        if(Seq1[i] == Seq2[j]){
          common <- c(common, Seq2[j])
        }
      }
    }
    return(common)
  }
}
}
