#' The slide function
#'
#' This function transforms the input data file by shifting any given toilet technologies to another category until a goal is reached for that category.
#' @param data The input dataframe (but only the urban or rural fraction, they have to be analyzed one by one)
#' @param precision The precision of the iterations (0.001 does not take too long; 0.01 is much faster)
#' @param moved A percentage indicating how many percentage points need to be moved (i.e., how much the slider bar button has moved)
#' @param toColumns The column(s) where you want the percentages to increase
#' @param fromColumns The columns(s) where you want the percentages to decrease
#' @keywords pathogens
#' @export
#' @examples
#' # Rules:
#' # 1a. If the red button moves to the right, then toColumn=c("openDefecation") and fromColumns=c("pitSlab","pitNoSlab","bucketLatrine","containerBased")
#' # 1b. If the red button moves to the left, then toColumn=c("pitSlab") and fromColumns=c("openDefecation","other","hangingToilet")
#' # 2a. If the orange button moves to the right, then toColumn=c("pitSlab") and fromColumns=c("compostingToilet")
#' # 2a. If the orange button moves to the left, then toColumn=c("compostingToilet") and fromColumns=c("pitSlab","pitNoSlab","bucketLatrine","containerBased")
#' # 3a. If the yellow button moves to the right, then toColumn=c("compostingToilet") and fromColumns=c("flushSeptic","flushPit","flushOpen","flushUnknown")
#' # 3b. If the yellow button moves to the left, then toColumn=c("flushSeptic","flushPit") and fromColumns=c("compostingToilet")
#' # 4a. If the green button moves to the right, then toColumn=c("flushSeptic","flushPit") and fromColumns=c("flushSewer")
#' # 4b. If the green button moves to the left, then toColumn=c("flushSewer") and fromColumns=c("flushSeptic","flushPit","flushOpen","flushUnknown")
#'
#' # This is an example of eliminating flushOpen and flushUnknown and replacing them with flushSeptic and flushPit
#' res1<-slide(data=read.csv("data/InputFile.csv"),precision=0.001,moved="all",fromColumns=c("flushOpen_urb","flushUnknown_urb"),toColumns=c("flushSeptic_urb","flushPit_urb"),context="urban")
#' res2<-slide(data=res1,precision=0.001,moved="all",fromColumns=c("flushOpen_rur","flushUnknown_rur"),toColumns=c("flushSeptic_rur","flushPit_rur"),context="rural")
#' rowSums(res2[,c(13:25)])
#' rowSums(res2[,c(36:48)])
#'
#' # This is an example of sliding the green button to the left by 10% in both urban and rural settings
#' res3<-slide(data=read.csv("data/InputFile.csv"),precision=0.001,moved=0.1,fromColumns=c("flushSeptic_urb","flushPit_urb","flushOpen_urb","flushUnknown_urb"),toColumns=c("flushSewer_urb"),context="urban")
#' res4<-slide(data=res3,precision=0.001,moved=0.1,fromColumns=c("flushSeptic_rur","flushPit_rur","flushOpen_rur","flushUnknown_rur"),toColumns=c("flushSewer_rur"),context="rural")
#' rowSums(res4[,c(13:25)])
#' rowSums(res4[,c(36:48)])

slide<-function(data,precision,moved,fromColumns,toColumns,context){
  mf<-data.frame(urban=data$fraction_urban_pop,rural=1-data$fraction_urban_pop)
  current<-sum(data$population*mf[,context]*data[,toColumns])/sum(data$population*mf[,context]);current
  available<-rowSums(as.data.frame(data[,fromColumns]));available
  spliceFr<-data[,fromColumns]/available
  alreadyThere<-rowSums(as.data.frame(data[,toColumns]));alreadyThere
  spliceTo<-data[,toColumns]/alreadyThere
  if(moved=="all"){
    data[,toColumns]<-data[,toColumns]+available*spliceTo
    data[,fromColumns]<-0
    dataOut<-data
  }else{
    dataAside<-data.frame()
    goal<-current+moved
    while(current<goal){
      available<-rowSums(as.data.frame(data[,fromColumns]))
      alreadyThere<-rowSums(as.data.frame(data[,toColumns]))
      alreadyThere[which(available==0)]<-999
      ofNeed<-which.min(alreadyThere)
      alreadyThere<-rowSums(as.data.frame(data[,toColumns]))
      spliceFr<-data[,fromColumns]/available
      spliceTo<-data[,toColumns]/alreadyThere
      if(length(fromColumns)>1){sF<-spliceFr[ofNeed,]}else{sF<-spliceFr[ofNeed]}
      if(length(toColumns)>1){sT<-spliceTo[ofNeed,]}else{sT<-spliceTo[ofNeed]}
      wouldBeNegative<-any( data[ofNeed,fromColumns]-precision*sF < 0 )
      if(wouldBeNegative==F | is.na(wouldBeNegative)){ #checks to see if the values in the fromColumns would become negative and if so, then it dumps all remaining percentages into the toColumn
        data[ofNeed,toColumns]<-data[ofNeed,toColumns]+precision*sT
        data[ofNeed,fromColumns]<-data[ofNeed,fromColumns]-precision*sF
      }else{
        data[ofNeed,toColumns]<-data[ofNeed,toColumns]+sum(data[ofNeed,fromColumns])*sT
        data[ofNeed,fromColumns]<-0
      }
      current<-sum(data$population*mf[,context]*data[,toColumns])/sum(data$population*mf[,context]);data;current;goal
    }
  }
  return(data)
}
