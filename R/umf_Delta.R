umf_Delta <- 
function(Final, Initial, n = 6, Data, method = "delta"){
  if (method == "delta"){
    delta <- umf_GetMean(Final, n, Data) - umf_GetMean(Initial, n, Data)
    return(delta)
  }
  if (method == "deltaMin"){
    slice <- umf_Slice(Initial, Final, Data)
    deltaMin  <- min(slice) - umf_GetMean(Initial, n, Data)
    return(deltaMin)
  }
}