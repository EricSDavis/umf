umf_PeakPlateau <- 
function(Final, Initial, n = 6, Data, PeakName = delta_peak, PlateauName = delta_plateau){
  delta_peak <- umf_FindPeaks(Initial, Final, n, Data)
  delta_plateau <- umf_GetMean(Final, n, Data) - umf_GetMean(Initial, n, Data)
  peak_plat <- cbind(delta_peak, delta_plateau)
  colnames(peak_plat) <- c(PeakName, PlateauName)
  return(peak_plat)
}
