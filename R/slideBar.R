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
  before<-data.frame(Sewered=rep(NA,3),Flush_Onsite=rep(NA,3),Composting_Toilets=rep(NA,3),Dry_Onsite=rep(NA,3),No_Facility=rep(NA,3));rownames(before)=c("Urban","Rural","Overall")
  before["Urban","Sewered"]<-sum(data$population*data$fraction_urban_pop*data$flushSewer_urb)/(sum(data$population*data$fraction_urban_pop)+0.000000000001)
  before["Urban","Flush_Onsite"]<-sum(data$population*data$fraction_urban_pop*(data$flushSeptic_urb+data$flushPit_urb+data$flushOpen_urb+data$flushUnknown_urb))/(sum(data$population*data$fraction_urban_pop)+0.000000000001)
  before["Urban","Composting_Toilets"]<-sum(data$population*data$fraction_urban_pop*(data$compostingToilet_urb))/(sum(data$population*data$fraction_urban_pop)+0.000000000001)
  before["Urban","Dry_Onsite"]<-sum(data$population*data$fraction_urban_pop*(data$pitSlab_urb+data$pitNoSlab_urb+data$bucketLatrine_urb+data$containerBased_urb))/(sum(data$population*data$fraction_urban_pop)+0.000000000001)
  before["Urban","No_Facility"]<-sum(data$population*data$fraction_urban_pop*(data$other_urb+data$hangingToilet_urb+data$openDefecation_urb))/(sum(data$population*data$fraction_urban_pop)+0.000000000001)
  before["Rural","Sewered"]<-sum(data$population*(1-data$fraction_urban_pop)*data$flushSewer_rur)/(sum(data$population*(1-data$fraction_urban_pop))+0.000000000001)
  before["Rural","Flush_Onsite"]<-sum(data$population*(1-data$fraction_urban_pop)*(data$flushSeptic_rur+data$flushPit_rur+data$flushOpen_rur+data$flushUnknown_rur))/(sum(data$population*(1-data$fraction_urban_pop))+0.000000000001)
  before["Rural","Composting_Toilets"]<-sum(data$population*(1-data$fraction_urban_pop)*(data$compostingToilet_rur))/(sum(data$population*(1-data$fraction_urban_pop))+0.000000000001)
  before["Rural","Dry_Onsite"]<-sum(data$population*(1-data$fraction_urban_pop)*(data$pitSlab_rur+data$pitNoSlab_rur+data$bucketLatrine_rur+data$containerBased_rur))/(sum(data$population*(1-data$fraction_urban_pop))+0.000000000001)
  before["Rural","No_Facility"]<-sum(data$population*(1-data$fraction_urban_pop)*(data$other_rur+data$hangingToilet_rur+data$openDefecation_rur))/(sum(data$population*(1-data$fraction_urban_pop))+0.000000000001)
  before["Overall","Sewered"]<-(before["Urban","Sewered"]*sum(data$population*data$fraction_urban_pop)+before["Rural","Sewered"]*sum(data$population*(1-data$fraction_urban_pop)))/sum(data$population)
  before["Overall","Flush_Onsite"]<-(before["Urban","Flush_Onsite"]*sum(data$population*data$fraction_urban_pop)+before["Rural","Flush_Onsite"]*sum(data$population*(1-data$fraction_urban_pop)))/sum(data$population)
  before["Overall","Composting_Toilets"]<-(before["Urban","Composting_Toilets"]*sum(data$population*data$fraction_urban_pop)+before["Rural","Composting_Toilets"]*sum(data$population*(1-data$fraction_urban_pop)))/sum(data$population)
  before["Overall","Dry_Onsite"]<-(before["Urban","Dry_Onsite"]*sum(data$population*data$fraction_urban_pop)+before["Rural","Dry_Onsite"]*sum(data$population*(1-data$fraction_urban_pop)))/sum(data$population)
  before["Overall","No_Facility"]<-(before["Urban","No_Facility"]*sum(data$population*data$fraction_urban_pop)+before["Rural","No_Facility"]*sum(data$population*(1-data$fraction_urban_pop)))/sum(data$population)
  before$rowSum<-rowSums(before);before

  mf<-data.frame(urban=data$fraction_urban_pop,rural=1-data$fraction_urban_pop) #this loads in the fraction of the population that is urban
  current<-sum(data$population*mf[,context]*data[,toColumns])/sum(data$population*mf[,context]) #this calculates the current overall fraction using the technologies in toColumns
  available<-rowSums(as.data.frame(data[,fromColumns])) #this calculates a vector of available fractions using any of the technologies in fromColumns
  spliceFr<-data[,fromColumns]/available #this calculates the proportion of people using each of the technologies in the fromColumns
  alreadyThere<-rowSums(as.data.frame(data[,toColumns])) #this calculates
  spliceTo<-data[,toColumns]/alreadyThere #this calculates the proportion of people using each of the technologies in the toColumns
  if(moved=="all"){ #if all of the toilets from the fromColumns will be moved to the toColumns, then it sets the fromColumns equal to zero and puts the available population into the toColumns (using spliceTo to distribute according to what's commonly used)
    data[,toColumns]<-data[,toColumns]+available*spliceTo
    data[,fromColumns]<-0
  }else{
    goal<-current+moved #otherwise, a goal is set, equal to current + how much is moved
    while(current<goal){ #this loop runs as long as current is less than the goal
      available<-rowSums(as.data.frame(data[,fromColumns])) #this recalculates available (same as line 37) to update during each loop
      alreadyThere<-rowSums(as.data.frame(data[,toColumns])) #this recalculates alreadyThere (same as line 39) to update during each loop
      # note: which is a command that finds the row number associated with a conditional statement. For example, if alreadyThere is a vector with c(2,4,3,0,2,1,0), then the command alreadyThere[which(available==0)] will return c(4,7) because the number zero is located in positions 4 and 7 of the vector
      alreadyThere[which(available==0)]<-999 #the loop looks for the region with the lowest percentages, and then moves small percentage points (according to precision) into the toColumns category with the lowest amount. But when there is none left in the fromColumns (i.e., available = 0), then it must look for the next lowest number, so this just temporarily sets that number to 999 to avoid it calculating a value of infinity (which crashes the script)
      ofNeed<-which.min(alreadyThere) #this line calculates the lowest value in the vector alreadyThere (ignoring the ones where available=0, since there is none left to give for those regions), the which.min command is like the which command, but finds the minimum value.
      alreadyThere<-rowSums(as.data.frame(data[,toColumns])) #then, I set alreadyThere back to what it should be after determining which region is in need
      spliceFr<-data[,fromColumns]/available #same as line 38
      spliceTo<-data[,toColumns]/alreadyThere #same as line 40
      if(length(fromColumns)>1){sF<-spliceFr[ofNeed,]}else{sF<-spliceFr[ofNeed]} #these next two lines are probably just necessary in R---if fromColumns has only one column, then it's a vector, and rowSums will not work; if it has more than one column, then it's a dataframe (matrix) and rowSums command will work. I couldn't find any other way around it.
      if(length(toColumns)>1){sT<-spliceTo[ofNeed,]}else{sT<-spliceTo[ofNeed]}
      wouldBeNegative<-any( data[ofNeed,fromColumns]-precision*sF < 0 ) #before dumping the values of the fromColumns into the row of the toColumns with the greatest need (ofNeed), it first checks to see if that would make the fromColumns row negative (which is not possible), and if so, then it will just set it to zero and dump everything left into the toColumns
      if(wouldBeNegative==F | is.na(wouldBeNegative)){ #checks to see if the values in the fromColumns would become negative and if so, then it dumps all remaining percentages into the toColumn; I put the is.na command in there because at one point there was a situation that made it crash because wouldBeNegative was NA... later revisions to teh script may have made that situation go away, I don't know if it's still necessary to have the is.na command in there, but it's not hurting...
        data[ofNeed,toColumns]<-data[ofNeed,toColumns]+precision*sT #btw precision is how much of the percentage points are dumped at a time (during each loop), so it runs slower when precision is lower (but the end values come out being closer to the target)
        data[ofNeed,fromColumns]<-data[ofNeed,fromColumns]-precision*sF
      }else{
        data[ofNeed,toColumns]<-data[ofNeed,toColumns]+sum(data[ofNeed,fromColumns])*sT
        data[ofNeed,fromColumns]<-0
      }
      current<-sum(data$population*mf[,context]*data[,toColumns])/sum(data$population*mf[,context]) #this recalculates current so that the loop can check to see if the current percentage is still less than the goal.
    }
  }
  after<-data.frame(Sewered=rep(NA,3),Flush_Onsite=rep(NA,3),Composting_Toilets=rep(NA,3),Dry_Onsite=rep(NA,3),No_Facility=rep(NA,3));rownames(after)=c("Urban","Rural","Overall")
  after["Urban","Sewered"]<-sum(data$population*data$fraction_urban_pop*data$flushSewer_urb)/(sum(data$population*data$fraction_urban_pop)+0.000000000001)
  after["Urban","Flush_Onsite"]<-sum(data$population*data$fraction_urban_pop*(data$flushSeptic_urb+data$flushPit_urb+data$flushOpen_urb+data$flushUnknown_urb))/(sum(data$population*data$fraction_urban_pop)+0.000000000001)
  after["Urban","Composting_Toilets"]<-sum(data$population*data$fraction_urban_pop*(data$compostingToilet_urb))/(sum(data$population*data$fraction_urban_pop)+0.000000000001)
  after["Urban","Dry_Onsite"]<-sum(data$population*data$fraction_urban_pop*(data$pitSlab_urb+data$pitNoSlab_urb+data$bucketLatrine_urb+data$containerBased_urb))/(sum(data$population*data$fraction_urban_pop)+0.000000000001)
  after["Urban","No_Facility"]<-sum(data$population*data$fraction_urban_pop*(data$other_urb+data$hangingToilet_urb+data$openDefecation_urb))/(sum(data$population*data$fraction_urban_pop)+0.000000000001)
  after["Rural","Sewered"]<-sum(data$population*(1-data$fraction_urban_pop)*data$flushSewer_rur)/(sum(data$population*(1-data$fraction_urban_pop))+0.000000000001)
  after["Rural","Flush_Onsite"]<-sum(data$population*(1-data$fraction_urban_pop)*(data$flushSeptic_rur+data$flushPit_rur+data$flushOpen_rur+data$flushUnknown_rur))/(sum(data$population*(1-data$fraction_urban_pop))+0.000000000001)
  after["Rural","Composting_Toilets"]<-sum(data$population*(1-data$fraction_urban_pop)*(data$compostingToilet_rur))/(sum(data$population*(1-data$fraction_urban_pop))+0.000000000001)
  after["Rural","Dry_Onsite"]<-sum(data$population*(1-data$fraction_urban_pop)*(data$pitSlab_rur+data$pitNoSlab_rur+data$bucketLatrine_rur+data$containerBased_rur))/(sum(data$population*(1-data$fraction_urban_pop))+0.000000000001)
  after["Rural","No_Facility"]<-sum(data$population*(1-data$fraction_urban_pop)*(data$other_rur+data$hangingToilet_rur+data$openDefecation_rur))/(sum(data$population*(1-data$fraction_urban_pop))+0.000000000001)
  after["Overall","Sewered"]<-(after["Urban","Sewered"]*sum(data$population*data$fraction_urban_pop)+after["Rural","Sewered"]*sum(data$population*(1-data$fraction_urban_pop)))/sum(data$population)
  after["Overall","Flush_Onsite"]<-(after["Urban","Flush_Onsite"]*sum(data$population*data$fraction_urban_pop)+after["Rural","Flush_Onsite"]*sum(data$population*(1-data$fraction_urban_pop)))/sum(data$population)
  after["Overall","Composting_Toilets"]<-(after["Urban","Composting_Toilets"]*sum(data$population*data$fraction_urban_pop)+after["Rural","Composting_Toilets"]*sum(data$population*(1-data$fraction_urban_pop)))/sum(data$population)
  after["Overall","Dry_Onsite"]<-(after["Urban","Dry_Onsite"]*sum(data$population*data$fraction_urban_pop)+after["Rural","Dry_Onsite"]*sum(data$population*(1-data$fraction_urban_pop)))/sum(data$population)
  after["Overall","No_Facility"]<-(after["Urban","No_Facility"]*sum(data$population*data$fraction_urban_pop)+after["Rural","No_Facility"]*sum(data$population*(1-data$fraction_urban_pop)))/sum(data$population)
  after$rowSum<-rowSums(after);after
  output<-list(dataFile=data,slideBar_before=round(before,3),slideBar_after=round(after,3))
  return(output)
}

#res<-slide(data=read.csv("data/input_file_new_kla_div.csv"),precision=0.001,moved=0.2,fromColumns=c("flushOpen_urb","flushUnknown_urb","flushSeptic_urb","flushPit_urb"),toColumns=c("flushSewer_urb"),context="urban")
#write.csv(res$dataFile,"move_sewered_twenty_percent_to_the_left_FULL.csv")
#write.csv(res$before,"move_sewered_twenty_percent_to_the_BEFORE.csv")
#write.csv(res$after,"move_sewered_twenty_percent_to_the_AFTER.csv")

