umf_UssingAnalysis <- 
  function (file_path, drugs=c("Amil", "FSK", "VX770", "CFTRinh", "UTP"), n=6, method=c("delta", "deltaMin")){  
  library(umf)
  library(readxl)
  Protocol <- read_excel((file = file_path), sheet = "Protocol")
  Isc <- read_excel((file = file_path), sheet = "Isc")
  TER <- read_excel((file = file_path), sheet = "TER")
  results <- list()
  
  #Suppress Warning Messages
  options(warn=-1) #options(warn=0) turns warnings back on
  
  #Store Raw Data into class
  results$Isc_Data <- Isc
  results$TER_Data <- TER
  
  #Generate ProtocolValues
  ProtocolValues <- c()
  values <- c()
  for(i in 1:length(Protocol[[1]])){
    ProtocolValues[i] <- Protocol[[1]][i]
  }
  
  #Append the last value to ProtocolValues
  End <- as.numeric(Isc[length(Isc[[1]]), 1])
  ProtocolValues <- c(ProtocolValues, End)
  results$ProtocolValues <- ProtocolValues
  
  #Generate ProtocolList (will come from user input)
  ProtocolList <- drugs
  ProtocolList <- c(ProtocolList, "End")
  results$ProtocolList <- ProtocolList
  
  #Build Analysis for Isc and TER
  IscTable <- c()
  TERTable <- c()
  for (i in 1:length(ProtocolList)) {
    if (ProtocolList[i] == "Amil"){
      IscTable[[i]] <- rbind("Amiloride" = umf_Delta(ProtocolValues[i+1], ProtocolValues[i], n, Isc, method=method))
      TERTable[[i]] <- rbind("Amiloride" = umf_Delta(ProtocolValues[i+1], ProtocolValues[i], n, TER, method=method))
    }
    if (ProtocolList[i] == "FSK"){
      IscTable[[i]] <- rbind("Forskolin" = umf_PeakPlateau(ProtocolValues[i+1], ProtocolValues[i], n, Isc, "FSK_Peak", "FSK_Plateau"))
      IscTable[[i]] <- t(IscTable[[i]])
      TERTable[[i]] <- rbind("Forskolin" = umf_PeakPlateau(ProtocolValues[i+1], ProtocolValues[i], n, TER, "FSK_Peak", "FSK_Plateau"))
      TERTable[[i]] <- t(TERTable[[i]])
    }
    if (ProtocolList[i] == "VX770" | ProtocolList[i] == "Gen"){
      IscTable[[i]] <- rbind("VX770" = umf_Potentiator(ProtocolValues[i+1], ProtocolValues[i], n, Isc))
      TERTable[[i]] <- rbind("VX770" = umf_Potentiator(ProtocolValues[i+1], ProtocolValues[i], n, TER))
    }
    if (ProtocolList[i] == "CFTRinh"){
      IscTable[[i]] <- rbind("CFTRinh-172" = umf_Delta(ProtocolValues[i+1], ProtocolValues[i], n, Isc, method=method))
      TERTable[[i]] <- rbind("CFTRinh-172" = umf_Delta(ProtocolValues[i+1], ProtocolValues[i], n, TER, method=method))
    }
    if (ProtocolList[i] == "UTP"){
      IscTable[[i]] <- rbind("UTP"= umf_PeakPlateau(ProtocolValues[i+1], ProtocolValues[i], n, Isc, "UTP_Peak", "UTP_Plateau"))
      IscTable[[i]] <- t(IscTable[[i]])
      TERTable[[i]] <- rbind("UTP"= umf_PeakPlateau(ProtocolValues[i+1], ProtocolValues[i], n, TER, "UTP_Peak", "UTP_Plateau"))
      TERTable[[i]] <- t(TERTable[[i]])
    }
  }
  IscTable[[1]] <- rbind("Baseline" = umf_Baseline(ProtocolValues[1], n, Isc), IscTable[[1]])
  TERTable[[1]] <- rbind("Baseline" = umf_Baseline(ProtocolValues[1], n, TER), TERTable[[1]])


  #Bind Analysis to Table
  IscTable_List <- list()
  TERTable_List <- list()
  for (i in 1:length(ProtocolList)){
    IscTable_List[i] <- c(IscTable[i])
    TERTable_List[i] <- c(TERTable[i])
  }

  IscData_names <- attributes(Isc)
  IscData_names <- IscData_names$names
  IscData_names <- IscData_names[2:length(IscData_names)]
  TERData_names <- attributes(TER)
  TERData_names <- TERData_names$names
  TERData_names <- TERData_names[2:length(TERData_names)]

  IscDelta_Table <- do.call(rbind, IscTable_List)
  colnames(IscDelta_Table) <- IscData_names
  TERDelta_Table <- do.call(rbind, TERTable_List)
  colnames(TERDelta_Table) <- TERData_names

  results$Isc_Table <- IscDelta_Table
  results$TER_Table <- TERDelta_Table
  
  results$Protocol <- rbind.data.frame(ProtocolValues)
  colnames(results$Protocol) <- ProtocolList
  
  product <- new("Ussing", 
                 Protocol=results$Protocol, 
                 Isc_Data=results$Isc_Data, 
                 TER_Data=results$TER_Data,
                 ProtocolValues=results$ProtocolValues,
                 ProtocolList=results$ProtocolList,
                 Isc_Table=results$Isc_Table,
                 TER_Table=results$TER_Table)

  return(product)
}
