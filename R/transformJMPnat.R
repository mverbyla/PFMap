#' The t_JMP function
#'
#' This function transforms national or subnational sanitation data from the UNICEF/WHO Joint Monitoring Program and produces it in a format that can be used directly by the getLoadings function.
#' @param context Either "urban" or "rural" context
#' @keywords pathogens
#' @export
#' @examples
#' t_JMP("urban")
#'
#'
t_JMPnat<-function(context="urban",myRegions="all"){

  sat<-read.csv("data/jmpSanFac.csv",header=T);head(sat)
  colnames(sat)[4]<-"iso3"
  harmon<-data.frame(classific_id=1:17,san=c("flushSewer","flushSeptic","flushPit","flushUnknown","flushOpen",
                                              "pitSlab","pitSlab","pitSlab","pitNoSlab","hangingToilet","bucketLatrine",
                                              "other","compostingToilet","pitSlab","openDefecation","other","other"))
  sat<-merge(sat,harmon,by="classific_id");head(sat)
  sat$year<-as.numeric(paste("20",stringr::str_sub(sat$source,-2,-1),sep=""))
  st<-merge(aggregate(year ~ iso3, sat, max), sat);head(st)
  sat<-aggregate(percentage~iso3+country+context+year+source+san,data=st,FUN=sum);head(sat)

  suppressWarnings(if(myRegions=="all"){sat<-sat}else{sat<-sat[sat$iso3 %in% myRegions,]})
  trt<-read.csv("data/jmpTreatment.csv",header=T)
  suppressWarnings(if(myRegions=="all"){trt<-trt}else{trt<-trt[trt$ISO3 %in% myRegions,]})
  pop<-read.csv("data/population.csv",header=T)   #bring in the inputs CSV file
  suppressWarnings(if(myRegions=="all"){pop<-pop}else{pop<-pop[pop$region %in% myRegions,]})
  #assume<-data.frame(urban=c(0.01,0.2,0.3),rural=c(0.99,0.1,0.1))
  #rownames(assume)<-c("coverBury","sewageTreated","fecalSludgeTreated")

  if(context=="urban"){
    pop$excreted<-pop$excreted_urban
    pop$population<-pop$population*pop$fr_urban
    pop<-pop[,-which(names(pop) %in% c("fr_urban","excreted_urban","excreted_rural"))]
    ag<-aggregate(percentage~iso3+san+source+year,data=sat[sat$context=="Urban",],FUN=sum);head(ag)
    tr<-trt[trt$Residence.Type=="urban",]
    #defaultSurveys<-read.csv("data/surveys.csv",header=T)  # here I could include a function that filters out all other surveys except for the ones chosen by JMP
    d<-aggregate(percentage~san+iso3,data=ag,FUN=mean) #here I take the average of the percentages from different surveys conducted
    w<-tidyr::spread(d,san,percentage)
  }
  if(context=="rural"){
    pop$excreted<-pop$excreted_rural
    pop$population<-pop$population*(1-pop$fr_urban)
    pop<-pop[,-which(names(pop) %in% c("fr_urban","excreted_urban","excreted_rural"))]
    ag<-aggregate(percentage~iso3+san+source+year,data=sat[sat$context=="Rural",],FUN=sum)
    tr<-trt[trt$Residence.Type=="rural",]
    d<-aggregate(percentage~san+iso3,data=ag,FUN=mean)
    w<-tidyr::spread(d,san,percentage)
  }
  names(tr)[1]<-"iso3"
  x<-tidyr::spread(tr,Safely.managed.element,Coverage)
  cb<-aggregate(`Disposed insitu`~iso3,data=x,FUN=sum)
  sl<-aggregate(`Sewage treated`~iso3,data=x,FUN=sum)
  et<-aggregate(`Faecal sludge treated`~iso3,data=x,FUN=sum)
  cb[,-1]<-cb[,-1]/100
  sl[,-1]<-sl[,-1]/100
  et[,-1]<-et[,-1]/100
  t<-merge(merge(cb,sl,by="iso3",all=T),et,by="iso3",all=T)
  names(t)<-c("iso3","coverBury","sewageTreated","fecalSludgeTreated")
  out<-merge(w,t,by="iso3",all=T)
  #out[is.na(out)] <- 0
  names(out)[1]<-"region"

  if(is.null(out$flushSewer)){out$flushSewer<-NA}
  if(is.null(out$flushSeptic)){out$flushSeptic<-NA}
  if(is.null(out$flushPit)){out$flushPit<-NA}
  if(is.null(out$flushOpen)){out$flushOpen<-NA}
  if(is.null(out$flushUnknown)){out$flushUnknown<-NA}
  if(is.null(out$pitSlab)){out$pitSlab<-NA}
  if(is.null(out$pitNoSlab)){out$pitNoSlab<-NA}
  if(is.null(out$bucketLatrine)){out$bucketLatrine<-NA}
  if(is.null(out$hangingToilet)){out$hangingToilet<-NA}
  if(is.null(out$openDefecation)){out$openDefecation<-NA}
  if(is.null(out$containerBased)){out$containerBased<-NA}
  if(is.null(out$compostingToilet)){out$compostingToilet<-NA}
  if(is.null(out$compostingTwinSlab)){out$compostingTwinSlab<-NA}
  if(is.null(out$compostingTwinNoSlab)){out$compostingTwinNoSlab<-NA}
  if(is.null(out$other)){out$other<-NA}
  out<-merge(pop,out,by="region",all=T)
  out<-out[c("region","name","iso2","isonum","population","excreted","flushSewer","flushSeptic","flushPit","flushOpen","flushUnknown","pitSlab","pitNoSlab","compostingTwinSlab","compostingTwinNoSlab","compostingToilet","bucketLatrine","containerBased","hangingToilet","openDefecation","other","coverBury","sewageTreated","fecalSludgeTreated")]
  out$isWatertight<-out$fecalSludgeTreated
  out$hasLeach<-out$fecalSludgeTreated
  out$onsiteDumpedLand<-0.1
  out$emptyFrequency<-3
  out$pitAdditive<-"None"
  out$urine<-"Fresh Urine"

  return(out)
}
my_inputU=t_JMPnat(context="urban",myRegions=c("HND","UGA"))
write.csv(my_inputU,"inputURBAN.csv")
my_inputR=t_JMPnat(context="rural",myRegions="all")
write.csv(my_inputR,"inputRURAL.csv")
df1<-read.csv("inputURBAN.csv")
df2<-read.csv("inputRURAL.csv")
df1[complete.cases(df1),]
