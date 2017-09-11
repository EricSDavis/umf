umf_Potentiator <- 
function(Final, Initial, n = 6, Data){
  slice <- umf_Slice(Initial, Final, Data)
  begining <- umf_PreviousValues(Initial, n-2, Isc)[length(umf_PreviousValues(Initial, n-2, Data))]
  slice2 <- umf_Slice(begining, Initial, Data)
  Y2 <- c()
  X2 <- c()
  X1 <- c()
  Y1 <- c()
  SY2 <- c()
  SX2 <- c()
  SX1 <- c()
  SY1 <- c()
  for (i in 2:length(colnames(slice))){
    Y2[i-1] <- max(slice[i])
    X2[i-1] <- (slice[[1]][which.max(slice[[i]])])
    X1[i-1] <- slice[[1]][1]
    Y1[i-1] <- slice[[i]][1]
    SY2[i-1] <- slice2[[i]][length(slice2[[2]])]
    SX2[i-1] <- slice2[[1]][length(slice2[[1]])]
    SX1[i-1] <- slice2[[1]][1]
    SY1[i-1] <- slice2[[i]][1]
  }
  slope <- ((SY2 - SY1)/(SX2 - SX1))
  formula <- (Y2 - (slope*(X2-X1)+Y1))
  return(formula)
}
