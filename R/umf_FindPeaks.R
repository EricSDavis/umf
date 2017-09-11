umf_FindPeaks <- 
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
