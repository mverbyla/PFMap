#' The getLRV function
#'
#' This function predicts the pathogen log reduction value for a wastewater or fecal sludge treatment plant sketched using the K2P Sketcher Tool (http://tools.waterpathogens.org/sketcher/)
#' @param sketch A JSON file containing information about the wastewater or fecal sludge treatment plant. This file must be in a very specific format and can be created using the K2P Sketcher Tool (http://tools.waterpathogens.org/sketcher/)
#' @param inFecalSludge Number of pathogens conveyed each year to the treatment plant in fecal sludge
#' @param inSewage Number of pathogens conveyed each year to the treatment plant in sewerage
#' @param pathogenType Pathogen group of interest (Virus, Bacteria, Protozoa, Helminths)
#' @keywords pathogens
#' @export
#' @examples
#' getLRV(mySketch="http://data.waterpathogens.org/dataset/a1423a05-7680-4d1c-8d67-082fbeb00a50/resource/e7852e8f-9603-4b19-a5fa-9cb3cdc63bb8/download/sketch_lubigi.json",pathogenType="Virus",inFecalSludge=10000000000,inSewage=10000000000)
#'
#'
#'
getLRV<-function(mySketch="http://data.waterpathogens.org/dataset/a1423a05-7680-4d1c-8d67-082fbeb00a50/resource/e7852e8f-9603-4b19-a5fa-9cb3cdc63bb8/download/sketch_lubigi.json",pathogenType="Virus",inFecalSludge=10000000000,inSewage=10000000000){
  k2pdata<-read.csv("http://data.waterpathogens.org/dataset/eda3c64c-479e-4177-869c-93b3dc247a10/resource/9e172f8f-d8b5-4657-92a4-38da60786327/download/treatmentdata.csv",header=T)
  lambdas<-c(Virus=0.2,Bacteria=0.3,Protozoa=0.6,Helminths=0.99) # these lambda values are based on data from the literature (Chauret et al., 1999; Lucena et al., 2004; Ramo et al., 2017; Rose et al., 1996; Tanji et al., 2002; Tsai et al., 1998)
  lambda<-as.numeric(lambdas[pathogenType])

  results<-data.frame(In_Fecal_Sludge=inFecalSludge,In_Sewage=inSewage,Sludge_Biosolids=NA,Liquid_Effluent=NA,Centralized_LRV=NA)

  sketch=jsonlite::read_json(mySketch,simplifyVector = T)
  #pData=read.csv(myData,header=T)
  sketch$temperature<-as.double(sketch$temperature)
  sketch$retentionTime<-as.double(sketch$retentionTime)
  sketch$depth<-as.double(sketch$depth)

  res<-suppressWarnings(getNodes(sketch = sketch, nodes = sketch[,-c(2,3)]))
  nodes<-res$nodes
  arrows<-res$arrows

  # transform the K2P data and fit the models
  k2pdata<-suppressWarnings(transformData(k2pdata))
  fit_ap<-lm(SQRTlrv ~ SQRThrt+temp+factor(pathogen),data=subset(k2pdata,technology_description=="Anaerobic Pond"))
  fit_fp<-lm(SQRTlrv ~ lhrt+temp+factor(pathogen),data=subset(k2pdata,technology_description=="Facultative Pond"|technology_description=="Maturation Pond"))
  fit_mp<-lm(SQRTlrv ~ lhrt+temp+factor(pathogen),data=subset(k2pdata,technology_description=="Facultative Pond"|technology_description=="Maturation Pond"))
  fit_db<-lm(SQRTlrv ~ SQRTht+SQRTmoist+factor(pathogen),data=subset(k2pdata,technology_description=="Sludge Drying Bed")) #maybe later we can add back SQRT(moist) as a factor
  fit_tf<-lm(SQRTlrv ~ factor(pathogen),data=subset(k2pdata,technology_description=="Trickling Filter"))
  fit_sd<-lm(SQRTlrv ~ factor(pathogen),data=subset(k2pdata,technology_description=="Sedimentation"))

  # find the LRVs for each pathogen group, then solve the DAG!
  pathogenGroups<-c("Virus","Bacteria","Protozoa","Helminth")

  warnings<-vector(mode="character",length=0)

  if(results$In_Fecal_Sludge>0 & any(nodes$subType=="fecal sludge")==FALSE){ #if the onsite system produces fecal sludge but the treatment plant does not accept any
    results$To_Surface<-results$In_Fecal_Sludge
    warnings[length(warnings)+1]<-"Warning: The onsite sanitation technologies in your system produce fecal sludge, but according to your sketch, the treatment plant does not accept fecal sludge."
    results$In_Fecal_Sludge<-0
    skipFS<-TRUE
  }else{skipFS<-FALSE}
  if(results$In_Sewage>0 & any(nodes$subType=="sewerage")==FALSE){ #if the onsite system produces sewerage but the treatment plant does not accept any
    results$To_Surface<-results$In_Sewage
    warnings[length(warnings)+1]<-"Warning: The onsite sanitation technologies in your system produce sewage, but according to your sketch, the treatment plant does not accept sewage."
    results$In_Sewage<-0
    skipWW<-TRUE
  }else{skipWW<-FALSE}

  nodes$loading_output<-NA
  arrows$loading<-NA
  if(skipFS==FALSE){nodes[nodes$subType=="fecal sludge",]$loading_output<-results$In_Fecal_Sludge}
  if(skipWW==FALSE){nodes[nodes$subType=="sewerage",]$loading_output<-results$In_Sewage}
  # get the LRVs for each node
  nodes<-estimate(nodes,pathogenType=pathogenType)

  nodeLRVs<-nodes[,c("name","fit","lwr","upr")]

  # solve the DAG
  solved<-solveit(nodes,arrows,lambda)
  # store the results
  arrowLoads<-solved$arrows
  results$Centralized_LRV<-round(solved$lrv,2)
  if(any(solved$nodes$matrix=="liquid")){results$Liquid_Effluent<-solved$nodes[solved$nodes$ntype=="end use" & solved$nodes$matrix=="liquid",]$loading_output}else{results$Liquid_Effluent<-0}
  if(any(solved$nodes$matrix=="solid")){results$Sludge_Biosolids<-sum(solved$nodes[solved$nodes$ntype=="end use" & solved$nodes$matrix=="solid",]$loading_output)}else{results$Sludge_Biosolids<-0}

  references<-unique(k2pdata[nodes$subType %in% tolower(unique(k2pdata$technology_description)),]$bib_id)

  return(list(Results=results,References=references))
}
