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
t_JMPnat<-function(context="national",myRegions="all"){

  # Get file list
  #setwd("data/processed")
  #file_list <- list.files()

  # Read all csv files in the folder and create a list of dataframes
  #ldf <- lapply(file_list , read.csv)

  # Combine each dataframe in the list into a single dataframe
  #df.final <- do.call("rbind", ldf)
  #setwd("..")
  #setwd("..")
  #write.csv(df.final,"data/jmpSanFac.csv")

  sat<-read.csv("data/jmpSanFac.csv",header=T);head(sat)
  sat$year<-as.numeric(paste("20",stringr::str_sub(sat$source,-2,-1),sep=""))
  sat[sat$year>2020,"year"]<-as.numeric(paste("19",stringr::str_sub(sat[sat$year>2020,"source"],-2,-1),sep=""))
  sat$source_ID<-stringr::str_sub(sat$source,-(stringr::str_length(sat$source)-stringr::str_locate(sat$source," - ")[,"end"]),-1)
  colnames(sat)[colnames(sat)=="alpha.3"]<-"iso3"
  harmon<-data.frame(classific_id=1:17,san=c("flushSewer","flushSeptic","flushPit","flushUnknown","flushOpen",
                                              "pitSlab","pitSlab","pitSlab","pitNoSlab","hangingToilet","bucketLatrine",
                                              "other","compostingToilet","pitSlab","openDefecation","other","other"))
  sat<-merge(sat,harmon,by="classific_id");head(sat)
  sat$uniqueID<-paste(sat$iso3,sat$source_ID,sep=".")
  sums<-aggregate(percentage ~ iso3 + context + source_ID + uniqueID,data=sat,FUN=sum)
  complete<-sums[which(sums$percentage==1),]

  sat2<-sat[sat$uniqueID %in% unique(complete$uniqueID),]

  st<-merge(aggregate(year ~ iso3, sat2, max), sat2);head(st)  #this takes surveys from the most recent year
  sat<-aggregate(percentage~iso3+country+context+year+source+san,data=st,FUN=sum);head(sat)
  sums<-aggregate(percentage ~ iso3 + context + source,data=sat,FUN=sum)
  complete<-sums[which(sums$percentage==1),]

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
    d<-aggregate(percentage~san+iso3,data=ag,FUN=mean)
    sources<-unique(data.frame(ag$iso3,ag$source))
    sources<-with(sources,sources[order(ag.iso3),])
    w<-tidyr::spread(d,san,percentage)
  }
  if(context=="rural"){
    pop$excreted<-pop$excreted_rural
    pop$population<-pop$population*(1-pop$fr_urban)
    pop<-pop[,-which(names(pop) %in% c("fr_urban","excreted_urban","excreted_rural"))]
    ag<-aggregate(percentage~iso3+san+source+year,data=sat[sat$context=="Rural",],FUN=sum)
    tr<-trt[trt$Residence.Type=="rural",]
    d<-aggregate(percentage~san+iso3,data=ag,FUN=mean)
    sources<-unique(data.frame(ag$iso3,ag$source))
    sources<-with(sources,sources[order(ag.iso3),])
    w<-tidyr::spread(d,san,percentage)
  }
  if(context=="national"){
    pop$excreted<-pop$excreted_rural+pop$excreted_urban
    pop<-pop[,-which(names(pop) %in% c("fr_urban","excreted_urban","excreted_rural"))]
    ag<-aggregate(percentage~iso3+san+source+year,data=sat[sat$context=="National",],FUN=sum)
    tr<-trt[trt$Residence.Type=="total",]
    d<-aggregate(percentage~san+iso3,data=ag,FUN=mean)
    sources<-unique(data.frame(ag$iso3,ag$source))
    sources<-with(sources,sources[order(ag.iso3),])
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
  out$pitAdditive<-0
  out$urine<-0
  out$twinPits<-0
  out<-out[,!(names(out) %in% c("population","excreted","compostingTwinSlab","compostingTwinNoSlab"))]
  check1<-rowSums(out[,names(out) %in% c("flushSewer","flushSeptic","flushPit","flushOpen","flushUnknown","pitSlab","pitNoSlab","compostingToilet","bucketLatrine","containerBased","hangingToilet","openDefecation","other")],na.rm=T)
  out$warning<-""
  out$warning[which(check1>1.1 | (check1<0.9 & check1!=0))]<-"Does not add up to 100%"

  return(list(source=sources,output=out))
}

#my_inputU=t_JMPnat(context="urban",myRegions="all")
#write.csv(my_inputU$output,"inputURBAN.csv")
#write.csv(my_inputU$source,"inputURBANsources.csv")
#my_inputR=t_JMPnat(context="rural",myRegions="all")
#write.csv(my_inputR$output,"inputRURAL.csv")
#write.csv(my_inputR$source,"inputRURALsources.csv")
#my_inputN=t_JMPnat(context="national",myRegions="all")
#write.csv(my_inputN$output,"inputNATIONAL.csv")
#write.csv(my_inputN$source,"inputNATIONALsources.csv")
#df1<-read.csv("inputURBAN.csv")
#df2<-read.csv("inputRURAL.csv")
#df3<-read.csv("inputNATIONAL.csv")
#nrow(df3)
#df1$region
#df2$region
#df3<-df3[df3$region!="CHI",]
#df3$region
#missing<-vector()
#for(i in 1:nrow(df1)){
#  missing[i]<-all(is.na(df1[i,names(df1) %in% c("flushSewer","flushSeptic","flushPit","flushOpen","flushUnknown","pitSlab","pitNoSlab","compostingToilet","bucketLatrine","containerBased","hangingToilet","openDefecation","other")]))
#}
#whichNA<-which(missing)
#noUrban<-which(rowSums(df1[,names(df1) %in% c("flushSewer","flushSeptic","flushPit","flushOpen","flushUnknown","pitSlab","pitNoSlab","compostingToilet","bucketLatrine","containerBased","hangingToilet","openDefecation","other")],na.rm=T)==0);noUrban
#noRural<-which(rowSums(df2[,names(df2) %in% c("flushSewer","flushSeptic","flushPit","flushOpen","flushUnknown","pitSlab","pitNoSlab","compostingToilet","bucketLatrine","containerBased","hangingToilet","openDefecation","other")],na.rm=T)==0);noRural
#noNational<-which(rowSums(df3[,names(df3) %in% c("flushSewer","flushSeptic","flushPit","flushOpen","flushUnknown","pitSlab","pitNoSlab","compostingToilet","bucketLatrine","containerBased","hangingToilet","openDefecation","other")],na.rm=T)==0);noNational
#df1[noUrban,names(df1) %in% c("flushSewer","flushSeptic","flushPit","flushOpen","flushUnknown","pitSlab","pitNoSlab","compostingToilet","bucketLatrine","containerBased","hangingToilet","openDefecation","other")]<-df3[noUrban,names(df3) %in% c("flushSewer","flushSeptic","flushPit","flushOpen","flushUnknown","pitSlab","pitNoSlab","compostingToilet","bucketLatrine","containerBased","hangingToilet","openDefecation","other")]
#df2[noRural,names(df2) %in% c("flushSewer","flushSeptic","flushPit","flushOpen","flushUnknown","pitSlab","pitNoSlab","compostingToilet","bucketLatrine","containerBased","hangingToilet","openDefecation","other")]<-df3[noRural,names(df3) %in% c("flushSewer","flushSeptic","flushPit","flushOpen","flushUnknown","pitSlab","pitNoSlab","compostingToilet","bucketLatrine","containerBased","hangingToilet","openDefecation","other")]
#df1$notes<-""
#df1$notes[noUrban]<-"Urban sanitation technologies data was not available, so national data was used."
#df2$notes<-""
#df2$notes[noRural]<-"Rural sanitation technologies data was not available, so national data was used."
#
#noUrban<-is.na(df1[,names(df1) %in% c("coverBury","sewageTreated","fecalSludgeTreated")]);which(apply(noUrban, 1, all))
#noRural<-is.na(df2[,names(df2) %in% c("coverBury","sewageTreated","fecalSludgeTreated")]);which(apply(noRural, 1, all))
#noNational<-is.na(df3[,names(df3) %in% c("coverBury","sewageTreated","fecalSludgeTreated")]);which(apply(noNational, 1, all))
#
#write.csv(df1,"inputURBAN_withNational.csv")
#write.csv(df2,"inputRURAL_withNational.csv")
###

df<-read.csv("data/REGRESSIONtreatment.csv")
par(mfrow = c(2, 2))
plot(df$hdi,df$sewageTreated_urb)
plot(df$hdi,df$sewageTreated_rur)
plot(df$hdi,df$Fsmanaged_urb)
plot(df$hdi,df$Fsmanaged_rur)
df$lnsewageTreated_urb<-log(df$sewageTreated_urb+0.000000001)
df$lnsewageTreated_rur<-log(df$sewageTreated_rur+0.000000001)
df$lnFsmanaged_urb<-log(df$Fsmanaged_urb+0.000000001)
df$lnFsmanaged_rur<-log(df$Fsmanaged_rur+0.000000001)
plot(df$hdi,df$sewageTreated_urb)
plot(df$hdi,df$lnsewageTreated_urb)
plot(df$hdi,df$sewageTreated_rur)
plot(df$hdi,df$lnsewageTreated_rur)
plot(df$hdi,df$Fsmanaged_urb)
plot(df$hdi,df$lnFsmanaged_urb)
plot(df$hdi,df$Fsmanaged_rur)
plot(df$hdi,df$lnFsmanaged_rur)
####
fit1<-lm(lnsewageTreated_urb~hdi,data=df)
summary(fit1)
plot(fit1)
####
fit2<-lm(lnsewageTreated_rur~hdi,data=df)
summary(fit2)
plot(fit2)
####
fit3<-lm(Fsmanaged_urb~hdi,data=df)
summary(fit3)
plot(fit3)
mean(df$Fsmanaged_urb)
####
fit4<-lm(Fsmanaged_rur~hdi,data=df)
summary(fit4)
plot(fit4)
mean(df$Fsmanaged_rur)
