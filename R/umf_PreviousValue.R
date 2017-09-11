umf_PreviousValues <- 
function(Value, n, Data){
  NV <- c()
  NV[1] <- umf_NearestValue(Value, Data) - 1
  for (i in 1:n){
    NV[i+1] <- umf_NearestValue(NV[i], Data)-1
  }
  NV <- NV + 1
  return(NV)
}