estimate<-function(nodes,pathogenType){
  # transformation of user data to make predictions
  nodes$lhrt<-NA
  nodes$lhrt[nodes$subType=="anaerobic pond"|nodes$subType=="facultative pond"|nodes$subType=="maturation pond"]<-log(nodes$retentionTime[nodes$subType=="anaerobic pond"|nodes$subType=="facultative pond"|nodes$subType=="maturation pond"])
  nodes$SQRThrt<-sqrt(nodes$retentionTime)
  nodes$SQRTht<-sqrt(as.double(nodes$holdingTime))
  nodes$ldepth<-NA
  nodes$ldepth[nodes$subType=="anaerobic pond"|nodes$subType=="facultative pond"|nodes$subType=="maturation pond"]<-log(nodes$depth[nodes$subType=="anaerobic pond"|nodes$subType=="facultative pond"|nodes$subType=="maturation pond"])
  nodes$temp<-nodes$temperature
  nodes$temp2<-nodes$temperature^2
  nodes$temp3<-nodes$temperature^3
  nodes$ltemp<-NA
  nodes$ltemp[nodes$subType=="sludge drying bed"]<-log(nodes$temperature[nodes$subType=="sludge drying bed"])
  nodes$ltemp<-log(nodes$temperature)
  nodes$SQRTmoist<-sqrt(as.double(nodes$moistureContent))
  nodes$pathogen<-pathogenType
  nodes$fit<-0;nodes$upr<-0;nodes$lwr<-0
  # execution of models
  if(any(nodes$subType=="anaerobic pond")==TRUE){nodes[nodes$subType=="anaerobic pond",c("fit","lwr","upr")]<-predict(fit_ap,nodes[nodes$subType=="anaerobic pond",],interval="confidence")^2}
  if(any(nodes$subType=="facultative pond")==TRUE){nodes[nodes$subType=="facultative pond",c("fit","lwr","upr")]<-predict(fit_fp,nodes[nodes$subType=="facultative pond",],interval="confidence")^2}
  if(any(nodes$subType=="maturation pond")==TRUE){nodes[nodes$subType=="maturation pond",c("fit","lwr","upr")]<-predict(fit_mp,nodes[nodes$subType=="maturation pond",],interval="confidence")^2}
  if(any(nodes$subType=="sludge drying bed")==TRUE){
    if(pathogenType=="Virus"){nodes[nodes$subType=="sludge drying bed",c("fit","lwr","upr")]<-1}else{nodes[nodes$subType=="sludge drying bed",c("fit","lwr","upr")]<-predict(fit_db,nodes[nodes$subType=="sludge drying bed",],interval="confidence")^2}
  }
  if(any(nodes$subType=="trickling filter")==TRUE){
    if(pathogenType=="Helminth"){nodes[nodes$subType=="trickling filter",c("fit","lwr","upr")]<-1}else{nodes[nodes$subType=="trickling filter",c("fit","lwr","upr")]<-predict(fit_tf,nodes[nodes$subType=="trickling filter",],interval="confidence")^2}
  }
  if(any(nodes$subType=="settler/sedimentation")==TRUE){
    if(pathogenType=="Protozoa"|pathogenType=="Helminth"){nodes[nodes$subType=="settler/sedimentation",c("fit","lwr","upr")]<-0}else{nodes[nodes$subType=="settler/sedimentation",c("fit","lwr","upr")]<-predict(fit_sd,nodes[nodes$subType=="settler/sedimentation",],interval="confidence")^2}
  }
  return(nodes)
}
