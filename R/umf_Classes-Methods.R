# Define 'Ussing' class
setClass("Ussing", slots = c(Protocol="data.frame", 
                             Isc_Data="data.frame",
                             TER_Data="data.frame",
                             ProtocolValues="numeric",
                             ProtocolList="character",
                             Isc_Table="matrix",
                             TER_Table="matrix"))

# Set Methods for viewing data of class 'Ussing'
setMethod("plot", signature(x="Ussing", y="missing"),
           function(x, y, ...){
             par(ask=F)
             IscPlot <- function(){
             matplot(x@Isc_Data[1]/60, x@Isc_Data[2:length(x@Isc_Data)],
                     type = "p", xlab="Time (min)", ylab="Isc",
                     main="Short-Circuit Current",
                     ylim=c(min(x@Isc_Data[2:length(x@Isc_Data)]),
                            max(x@Isc_Data[2:length(x@Isc_Data)]*1.2)), ...)
             abline(v=x@ProtocolValues[1:length(x@ProtocolValues)-1]/60)
             text(x=x@ProtocolValues[1:length(x@ProtocolValues)-1]/60,
                  y=(max(x@Isc_Data[2:length(x@Isc_Data)])*1.2),
                  srt = 90,
                  pos = 2,
                  x@ProtocolList[1:length(x@ProtocolList)-1])
             }
             TERPlot <- function(){
             matplot(x@TER_Data[1]/60, x@TER_Data[2:length(x@TER_Data)],
                     type = "p", xlab="Time (min)", ylab="TER",
                     main="Transepithelial Resistance",
                     ylim=c(min(x@TER_Data[2:length(x@TER_Data)]),
                            max(x@TER_Data[2:length(x@TER_Data)]*1.2)), ...)
             abline(v=x@ProtocolValues[1:length(x@ProtocolValues)-1]/60)
             text(x=x@ProtocolValues[1:length(x@ProtocolValues)-1]/60,
                  y=(max(x@TER_Data[2:length(x@TER_Data)])*1.2),
                  srt = 90,
                  pos = 2,
                  x@ProtocolList[1:length(x@ProtocolList)-1])
             }
             IscSumPlot <- function(){
             boxplot(t(x@Isc_Table), main="Isc Summary")
             }
             TERSumPlot <- function(){
             boxplot(t(x@TER_Table), main="TER Summary")
             }
             opar <- par(no.readonly = T)
             par(cex = 0.6)
             m <- rbind(c(1,2), c(3,4))
             layout(m)
             IscPlot()
             TERPlot()
             IscSumPlot()
             TERSumPlot()
             par(ask=F)
             par(opar)
          })

setMethod("summary", "Ussing",
          function(object, type){
            if(missing(type)){
              print("Please enter 'Isc' or 'TER'")
            }else if(type == "Isc"){
              summary(t(object@Isc_Table))
            }else if(type == "TER"){
              summary(t(object@TER_Table))
            }else{
              print("Please enter 'Isc' or 'TER'")
            }
          })

setMethod("show" , "Ussing" ,
           function(object){
             cat("Ussing Analysis\n")
             cat("Protocol:\n")
             print.data.frame(object@Protocol, row.names = F)
             cat("\n")
             cat("Isc Table:\n")
             print(object@Isc_Table)
             cat("\n")
             cat("TER Table:\n")
             print(object@TER_Table)
             cat("\n")
           })

setGeneric("view.plots", function(x) {
    standardGeneric("view.plots")
  })

setMethod("view.plots", signature(x="Ussing"),
          function(x){
          IscPlot <- function(){
            matplot(x@Isc_Data[1]/60, x@Isc_Data[2:length(x@Isc_Data)],
                    type = "p", xlab="Time (min)", ylab="Isc",
                    main="Short-Circuit Current",
                    ylim=c(min(x@Isc_Data[2:length(x@Isc_Data)]),
                           max(x@Isc_Data[2:length(x@Isc_Data)]*1.2)))
            abline(v=x@ProtocolValues[1:length(x@ProtocolValues)-1]/60)
            text(x=x@ProtocolValues[1:length(x@ProtocolValues)-1]/60,
                 y=(max(x@Isc_Data[2:length(x@Isc_Data)])*1.2),
                 srt = 90,
                 pos = 2,
                 x@ProtocolList[1:length(x@ProtocolList)-1])
          }
          TERPlot <- function(){
            matplot(x@TER_Data[1]/60, x@TER_Data[2:length(x@TER_Data)],
                    type = "p", xlab="Time (min)", ylab="TER",
                    main="Transepithelial Resistance",
                    ylim=c(min(x@TER_Data[2:length(x@TER_Data)]),
                           max(x@TER_Data[2:length(x@TER_Data)]*1.2)))
            abline(v=x@ProtocolValues[1:length(x@ProtocolValues)-1]/60)
            text(x=x@ProtocolValues[1:length(x@ProtocolValues)-1]/60,
                 y=(max(x@TER_Data[2:length(x@TER_Data)])*1.2),
                 srt = 90,
                 pos = 2,
                 x@ProtocolList[1:length(x@ProtocolList)-1])
          }
          IscSumPlot <- function(){
            boxplot(t(x@Isc_Table), main="Isc Summary")
          }
          TERSumPlot <- function(){
            boxplot(t(x@TER_Table), main="TER Summary")
          }
          opar <- par(no.readonly = T)
          par(ask=F)
          IscPlot()
          par(ask=T)
          TERPlot()
          par(ask=T)
          IscSumPlot()
          par(ask=T)
          TERSumPlot()
          par(ask=F)
          par(opar)
        })