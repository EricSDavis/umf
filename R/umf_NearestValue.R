umf_NearestValue <-
function (Value, Data){
  continue <- TRUE
  Column <- Data[[1]]
  while(continue){
    if (Value < Column[1]){
      return(paste0("Please enter a number greater than ", Column[1]))
    }
    for (i in 1:length(Column)){
      if (Column[i] == Value){
        Value = Value
        continue <- FALSE
        return(Value)
      }
    }
    Value <- Value - 1
  }
}
