umf_Baseline <- 
function(Timepoint, n = 6, Data){
  baseline <- umf_GetMean(Timepoint, n, Data)
  return(baseline)
}