umf_FindMatches <-
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
