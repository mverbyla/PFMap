#' The t_JMP function
#'
#' This function transforms national or subnational sanitation data from the UNICEF/WHO Joint Monitoring Program and produces it in a format that can be used directly by the getLoadings function.
#' @param contx Either "urban" or "rural" context
#' @keywords pathogens
#' @export
#' @examples
#' t_JMP("urban")
#'
#'
t_JMPsubnat<-function(contx,myRegions="all"){

  san<-read.csv("../JMP/san_fac_subnational.csv",header=T);head(san)   #bring in the sanitation CSV file
  if(myRegions=="all"){san<-san}else{san<-san[san$iso3 %in% myRegions,]}
  har<-read.csv("data/jmpHarmonize.csv",header=T);head(har)            #our customized harmonization file
  sat<-merge(san[,c("iso3","year","name","survey","sanitation_facility","region","value")],har,by="sanitation_facility")
  head(sat)
  trt<-read.csv("data/jmpTreatment.csv",header=T);head(trt)
  pop<-read.csv("data/isoraster_level1_0.1.csv",header=T);head(pop)   #bring in the inputs CSV file
  names(sat)[names(sat)=="name"]<-"country"
  names(pop)[names(pop)=="name"]<-"region"
  st<-merge(aggregate(year ~ iso3, sat, max), sat);head(st)

  test1<-merge(x=pop[,c("iso3","country","region","iso")],y=st,by=c("iso3","country","region"),all=F);head(test1)
  test2<-aggregate(value~iso3+country+iso+region+year+survey+category,data=test1,FUN=sum);head(test2)

  #d<-aggregate(value~iso+category,data=test2,FUN=mean)
  w<-tidyr::spread(test2,category,value);head(w)
  #w$add<-NA
  #w[which(rowSums(w[,unlist(lapply(w, is.numeric))][,-1],na.rm=T)!=100),]$add<-(100-rowSums(w[which(rowSums(w[,unlist(lapply(w, is.numeric))][,-1],na.rm=T)!=100),][,unlist(lapply(w, is.numeric))][,-1],na.rm=T))
  w[,unlist(lapply(w, is.numeric))][,-1]<-w[,unlist(lapply(w, is.numeric))][,-1]/100
  head(w)
  #w$other<-w$other+w$add
  #w$add<-NA
  #max(abs(w$add[is.na(w$add)==F]))

  tr<-trt[trt$Residence.Type==contx,]
  names(tr)[1]<-"iso3"
  x<-tidyr::spread(tr[,-c(2,4,7)],Safely.managed.element,Coverage)
  test3<-merge(pop,x,by="iso3",all=F);head(test3)

  names(test3)<-c("iso3","X","ID","iso","country","region","population","fr_urban","excreted_urban","excreted_rural","context","year","coverBury","fecalSludgeTreated","sewageTreated");head(test3)
  t<-test3[,c("iso","population","fr_urban","excreted_urban","excreted_rural","coverBury","fecalSludgeTreated","sewageTreated")]
  t[,c("coverBury","fecalSludgeTreated","sewageTreated")]<-t[,c("coverBury","fecalSludgeTreated","sewageTreated")]/100
  head(w)
  head(t)
  out<-merge(w,t,by="iso",all=F);head(out)
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
  head(out)
  out<-out[c("iso","iso3","country","region","year","survey","population","fr_urban","excreted_urban","excreted_rural","flushSewer","flushSeptic","flushPit","flushOpen","flushUnknown","pitSlab","pitNoSlab","compostingTwinSlab","compostingTwinNoSlab","compostingToilet","bucketLatrine","containerBased","hangingToilet","openDefecation","other","coverBury","sewageTreated","fecalSludgeTreated")]
  out$isWatertight<-out$fecalSludgeTreated
  out$hasLeach<-out$fecalSludgeTreated
  out$emptyFrequency<-3
  out$pitAdditive<-"None"
  out$urine<-"Fresh Urine"
  names(out)[names(out)=="region"]<-"isoName"
  out$region<-paste(out$iso,out$isoName)
  #out<-merge(out,pop[,c("iso","excreted_urban","excreted_rural")],by="iso")
  head(out)
  return(out)
}
df<-t_JMPsubnat(contx="urban",myRegions="all")
write.csv(df,"subnat_output.csv")
