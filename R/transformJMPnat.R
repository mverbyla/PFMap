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
t_JMPnat<-function(context,myRegions="all"){

  sat<-read.csv("data/jmpSanFac.csv",header=T);head(sat)
  suppressWarnings(if(myRegions=="all"){sat<-sat}else{sat<-sat[sat$iso3 %in% myRegions,]})
  trt<-read.csv("data/jmpTreatment.csv",header=T)
  suppressWarnings(if(myRegions=="all"){trt<-trt}else{trt<-trt[trt$ISO3 %in% myRegions,]})
  pop<-read.csv("data/population.csv",header=T)   #bring in the inputs CSV file
  suppressWarnings(if(myRegions=="all"){pop<-pop}else{pop<-pop[pop$region %in% myRegions,]})
  assume<-data.frame(urban=c(0.01,0.2,0.3),rural=c(0.99,0.1,0.1))
  rownames(assume)<-c("coverBury","sewageTreated","fecalSludgeTreated")

  if(context=="urban"){
    ag<-aggregate(urban~iso3+san+source+year,data=sat,FUN=sum);head(ag)
    defaultSurveys<-read.csv("data/surveys.csv",header=T)
    # here I could include a function that filters out all other surveys except for the ones chosen by JMP
#    d<-
    d<-aggregate(urban~san+iso3,data=ag,FUN=mean) #here I take the average of the percentages from different surveys conducted
    #### for two lines above, choose one or the other, not necessarily both...
    w<-tidyr::spread(d,san,urban)
    #w$add<-NA
    #w[which(rowSums(w[,-1],na.rm=T)!=100),]$add<-(100-rowSums(w[which(rowSums(w[,-1],na.rm=T)!=100),][,-1],na.rm=T))  #this checks if the sum equals 100
    w[,-1]<-w[,-1]/100

    tr<-trt[trt$Residence.Type=="urban",]
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
    out[is.na(out)] <- 0
    names(out)[1]<-"region"
  }
  if(context=="rural"){
    ag<-aggregate(rural~iso3+san+source+year,data=sat,FUN=sum)
    d<-aggregate(rural~san+iso3,data=ag,FUN=mean)
    w<-tidyr::spread(d,san,rural)
    #w$add<-NA
    #w[which(rowSums(w[,-1],na.rm=T)!=100),]$add<-(100-rowSums(w[which(rowSums(w[,-1],na.rm=T)!=100),][,-1],na.rm=T))  #this checks if the sum equals 100
    w[,-1]<-w[,-1]/100

    tr<-trt[trt$Residence.Type=="rural",]
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
    out[is.na(out)] <- 0
    names(out)[1]<-"region"
  }

  if(is.null(out$flushSewer)){out$flushSewer<-0}
  if(is.null(out$flushSeptic)){out$flushSeptic<-0}
  if(is.null(out$flushPit)){out$flushPit<-0}
  if(is.null(out$flushOpen)){out$flushOpen<-0}
  if(is.null(out$flushUnknown)){out$flushUnknown<-0}
  if(is.null(out$pitSlab)){out$pitSlab<-0}
  if(is.null(out$pitNoSlab)){out$pitNoSlab<-0}
  if(is.null(out$bucketLatrine)){out$bucketLatrine<-0}
  if(is.null(out$hangingToilet)){out$hangingToilet<-0}
  if(is.null(out$openDefecation)){out$openDefecation<-0}
  if(is.null(out$containerBased)){out$containerBased<-0}
  if(is.null(out$compostingToilet)){out$compostingToilet<-0}
  if(is.null(out$compostingTwinSlab)){out$compostingTwinSlab<-0}
  if(is.null(out$compostingTwinNoSlab)){out$compostingTwinNoSlab<-0}
  if(is.null(out$other)){out$other<-0}
  out<-merge(pop,out,by="region",all=T)
  out<-out[c("region","name","iso2","isonum","population","fr_urban","excreted_urban","excreted_rural","flushSewer","flushSeptic","flushPit","flushOpen","flushUnknown","pitSlab","pitNoSlab","compostingTwinSlab","compostingTwinNoSlab","compostingToilet","bucketLatrine","containerBased","hangingToilet","openDefecation","other","coverBury","sewageTreated","fecalSludgeTreated")]
  out$isWatertight<-out$fecalSludgeTreated
  out$hasLeach<-out$fecalSludgeTreated
  out$onsiteDumpedLand<-0.1
  out$emptyFrequency<-3
  out$pitAdditive<-"None"
  out$urine<-"Fresh Urine"
  if(any(is.na(out$coverBury))){out[is.na(out$coverBury),]$coverBury<-assume["coverBury",context]}
  if(any(is.na(out$fecalSludgeTreated))){out[is.na(out$fecalSludgeTreated),]$fecalSludgeTreated<-assume["fecalSludgeTreated",context]}
  if(any(is.na(out$sewageTreated))){out[is.na(out$sewageTreated),]$sewageTreatd<-assume["sewageTreated",context]}

  return(out)
}
my_input=t_JMPnat(context="urban",myRegions=c("HND","UGA"))
