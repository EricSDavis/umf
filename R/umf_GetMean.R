umf_GetMean <- 
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