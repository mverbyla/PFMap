#' The t_JMP function
#'
#' This function transforms country-level sanitation data from the UNICEF/WHO Joint Monitoring Program and produces it in a format that can be used directly by the getLoadings function.
#' @param contx Either "urban" or "rural" context
#' @keywords pathogens
#' @export
#' @examples
#' t_JMP("urban")
#'
#'
t_JMP<-function(contx){
  sat<-read.csv("data/jmpSanFac.csv",header=T)
  trt<-read.csv("data/jmpTreatment.csv",header=T)
  if(contx=="urban"){
    ag<-aggregate(urban~iso3+san+source+year,data=sat,FUN=sum)
    d<-aggregate(urban~san+iso3,data=ag,FUN=mean)
    w<-tidyr::spread(d,san,urban)
    w$add<-NA
    w[which(rowSums(w[,-1],na.rm=T)!=100),]$add<-(100-rowSums(w[which(rowSums(w[,-1],na.rm=T)!=100),][,-1],na.rm=T))
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
    names(out)[1]<-"region"
  }
  if(contx=="rural"){
    ag<-aggregate(rural~iso3+san+source+year,data=sat,FUN=sum)
    d<-aggregate(rural~san+iso3,data=ag,FUN=mean)
    w<-tidyr::spread(d,san,rural)
    w$add<-NA
    w[which(rowSums(w[,-1],na.rm=T)!=100),]$add<-(100-rowSums(w[which(rowSums(w[,-1],na.rm=T)!=100),][,-1],na.rm=T))
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
    names(out)[1]<-"region"
  }

  return(out)
}

t_JMP("urban")
t_JMP("rural")
