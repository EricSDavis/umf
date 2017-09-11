umf_Slice <-
function(Timepoint1, Timepoint2, Data){
  Value1 <- umf_NearestValue(Timepoint1, Data)
  Value2 <- umf_NearestValue(Timepoint2, Data)
  RowValueI <- as.numeric(rownames(Data)[Data[1] == Value1])
  RowValueF <- as.numeric(rownames(Data)[Data[1] == Value2])
  Data <- Data[RowValueI:RowValueF,]
  return(Data)
}
