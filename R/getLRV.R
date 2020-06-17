#' The getLRV function
#'
#' This function predicts the pathogen log reduction value for a wastewater or fecal sludge treatment plant sketched using the K2P Sketcher Tool (http://tools.waterpathogens.org/sketcher/)
#' @param sketch A JSON file containing information about the wastewater or fecal sludge treatment plant. This file must be in a very specific format and can be created using the K2P Sketcher Tool (http://tools.waterpathogens.org/sketcher/)
#' @param inFecalSludge Number of pathogens conveyed each year to the treatment plant in fecal sludge
#' @param inSewage Number of pathogens conveyed each year to the treatment plant in sewerage
#' @param pathogenType Pathogen group of interest (Virus, Bacteria, Protozoa, Helminth)
#' @keywords pathogens
#' @export
#' @examples
#' getLRV(mySketch="http://data.waterpathogens.org/dataset/1c681f80-82fa-4ed8-a572-cb98c886781a/resource/a87a552b-2f7c-4267-ac07-526eba1f4b68/download/lubigisewageandfecalsludgetreatmentsystem.json",pathogenType="Virus",inFecalSludge=10000000000,inSewage=10000000000)
#'
#' getLRV(mySketch="http://data.waterpathogens.org/dataset/bd53bbc6-a8f3-4d10-95b6-35f36e274b3c/resource/5cf1c3e0-a28c-4293-81e9-12586dd8941c/download/kirinyawastewatertreatmentplant-jinja6.json",pathogenType="Virus",inFecalSludge=10000000000,inSewage=10000000000)
#'
getLRV<-function(mySketch="http://data.waterpathogens.org/dataset/1c681f80-82fa-4ed8-a572-cb98c886781a/resource/a87a552b-2f7c-4267-ac07-526eba1f4b68/download/lubigisewageandfecalsludgetreatmentsystem.json"
                 ,
                 myLRVdata="http://data.waterpathogens.org/dataset/eda3c64c-479e-4177-869c-93b3dc247a10/resource/9e172f8f-d8b5-4657-92a4-38da60786327/download/treatmentdata.csv"
                 ,
                 pathogenType="Virus"
                 ,
                 inFecalSludge=10000000000
                 ,
                 inSewage=10000000000
                 ){

  k2pdata<-read.csv(myLRVdata,header=T)
  suppressWarnings(k2pdata$SQRTlrv<-sqrt(k2pdata$lrv))
  suppressWarnings(k2pdata$llrv<-log(k2pdata$lrv))
  suppressWarnings(k2pdata$pathogen<-k2pdata$pathogen_group)
  suppressWarnings(k2pdata$lhrt<-log(k2pdata$hrt_days))
  suppressWarnings(k2pdata$SQRThrt<-sqrt(k2pdata$hrt_days))
  suppressWarnings(k2pdata$SQRTht<-sqrt(k2pdata$holdingtime_days))
  suppressWarnings(k2pdata$ldepth<-log(k2pdata$depth_meters))
  suppressWarnings(k2pdata$temp<-k2pdata$temperature_celsius)
  suppressWarnings(k2pdata$temp2<-k2pdata$temperature_celsius^2)
  suppressWarnings(k2pdata$temp3<-k2pdata$temperature_celsius^3)
  suppressWarnings(k2pdata$ltemp<-log(k2pdata$temperature_celsius))
  suppressWarnings(k2pdata$SQRTmoist<-sqrt(k2pdata$moisture_content_percent))
  lambdas<-c(Virus=0.2,Bacteria=0.3,Protozoa=0.6,Helminth=0.99) # these lambda values are based on data from the literature (Chauret et al., 1999; Lucena et al., 2004; Ramo et al., 2017; Rose et al., 1996; Tanji et al., 2002; Tsai et al., 1998)
  lambda<-as.numeric(lambdas[pathogenType])

  results<-data.frame(In_Fecal_Sludge=inFecalSludge,In_Sewage=inSewage,Sludge_Biosolids=NA,Liquid_Effluent=NA,Centralized_LRV=NA)

  sketch=jsonlite::read_json(mySketch,simplifyVector = T)
  #pData=read.csv(myData,header=T)
  sketch$temperature<-as.double(sketch$temperature)
  sketch$retentionTime<-as.double(sketch$retentionTime)
  sketch$depth<-as.double(sketch$depth)

  ########((((((((this is the beginning of the old getNodes function))))))))
  #res<-suppressWarnings(getNodes(sketch = sketch, nodes = sketch[,-c(2,3)]))
  drop <- c("x","y","parents","children")
  nodes = sketch[,!(names(sketch) %in% drop)]

  nodes$number_inputs<-NA
  nodes$number_outputs<-NA
  for(i in 1:nrow(sketch)){
    nodes$number_inputs[i]<-length(sketch[["parents"]][[i]])
    nodes$number_outputs[i]<-length(sketch[["children"]][[i]])
  }
  nodes$loading_output=NA
  sn<-sketch[,c("parents","children")]
  sn$me<-as.character(sketch[,c("name")])
  numParents<-rep(NA,length(sn[,1]))
  rem<-NA;j=0
  suppressWarnings(  # this for loop turns all NULL parents and children to NA values, and it counts the number of parents (numParents) each node has
    for(i in 1:length(sn[,1])){
      numParents[i]<-if(is.null(length(sn[i,1][[1]]))){0}else{length(sn[i,1][[1]])}
      if(is.null(sn$parents[[i]]) | rlang::is_empty(sn$parents[[i]])){sn[i,1][[1]]<-NA}
      if(is.null(sn$children[[i]]) | rlang::is_empty(sn$children[[i]])){sn[i,2][[1]]<-NA}
      if(is.na(sn[[1]][[i]])){
        j=j+1;rem[j]<-i
      }
    }
  )
  orph<-which(numParents==0)
  arrows<-data.frame(us_node=rep(NA,sum(numParents)),ds_node=rep(NA,sum(numParents)))
  sn<-sn[-orph,];rownames(sn)<-1:nrow(sn)

  m=1
  for(i in 1:nrow(sn)){
    for(j in 1:length(sn[i,"parents"][[1]])){
      arrows$us_node[m]<-sn[i,"parents"][[1]][j]
      arrows$ds_node[m]<-sn$me[i]
      m=m+1
    }
  }


  arrows$loading<-NA
  rownames(nodes)<-nodes$name
  arrows$siblings<-nodes[arrows$us_node,"number_outputs"]
  arrows$flowtype<-nodes[arrows$ds_node,"matrix"]
  arrows$siblings_solid<-NA
  arrows$siblings_liquid<-NA
  arrows$iamsolid<-NA
  for(i in 1:nrow(arrows)){
    arrows$siblings_solid[i]<-sum(arrows$flowtype[which(arrows$us_node==arrows$us_node[i])]=="solid")
    arrows$siblings_liquid[i]<-sum(arrows$flowtype[which(arrows$us_node==arrows$us_node[i])]=="liquid")
    if(arrows$flowtype[i]=="solid"){arrows$iamsolid[i]<-TRUE}else{arrows$iamsolid[i]<-FALSE}
  }

  ####(((((((this is the end of the old getNodes function)))))))

  # transform the K2P data and fit the models
  # k2pdata<-suppressWarnings(transformData(k2pdata))
  fit_ap<-lm(SQRTlrv ~ SQRThrt+temp+factor(pathogen),data=subset(k2pdata,technology_description=="Anaerobic Pond"))
  fit_fp<-lm(SQRTlrv ~ lhrt+temp+factor(pathogen),data=subset(k2pdata,technology_description=="Facultative Pond"|technology_description=="Maturation Pond"))
  fit_mp<-lm(SQRTlrv ~ lhrt+temp+factor(pathogen),data=subset(k2pdata,technology_description=="Facultative Pond"|technology_description=="Maturation Pond"))
  fit_db<-lm(SQRTlrv ~ SQRTht+SQRTmoist+factor(pathogen),data=subset(k2pdata,technology_description=="Sludge Drying Bed"))
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


  ####(((((((this is the beginning of the old estimate, or getLRVs function)))))))

  # get the LRVs for each node
  #nodes<-estimate(nodes,pathogenType=pathogenType)



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

    ####placeholder LRVs until we get more data into the database####
    if(any(nodes$subType=="biogas reactor")==TRUE){nodes[nodes$subType=="biogas reactor",c("fit","lwr","upr")]<-c(1,0,2)}
    if(any(nodes$subType=="co-composting")==TRUE){nodes[nodes$subType=="co-composting",c("fit","lwr","upr")]<-c(1,0,2)}
    if(any(nodes$subType=="activated sludge")==TRUE){nodes[nodes$subType=="activated sludge",c("fit","lwr","upr")]<-c(1,0,2)}
    if(any(nodes$subType=="uasb reactor")==TRUE){nodes[nodes$subType=="uasb reactor",c("fit","lwr","upr")]<-c(1,0,2)}
    if(any(nodes$subType=="media filer")==TRUE){nodes[nodes$subType=="media filer",c("fit","lwr","upr")]<-c(1,0,2)}
    if(any(nodes$subType=="imhoff tank")==TRUE){nodes[nodes$subType=="imhoff tank",c("fit","lwr","upr")]<-c(1,0,2)}
    if(any(nodes$subType=="aerated pond")==TRUE){nodes[nodes$subType=="aerated pond",c("fit","lwr","upr")]<-c(1,0,2)}
    if(any(nodes$subType=="ss")==TRUE){nodes[nodes$subType=="ss",c("fit","lwr","upr")]<-c(1,0,2)}
    if(any(nodes$subType=="fws")==TRUE){nodes[nodes$subType=="fws",c("fit","lwr","upr")]<-c(1,0,2)}
    if(any(nodes$subType=="anaerobic baffled reactor")==TRUE){nodes[nodes$subType=="anaerobic baffled reactor",c("fit","lwr","upr")]<-c(1,0,2)}
    ####

    ####(((((((this is the end of the old estimate function)))))))

  nodeLRVs<-nodes[,c("name","subType","fit","lwr","upr")]


  #######(((((((SOLVE IT SOLVE IT SOLVE IT)))))))
  #######(((((((SOLVE IT SOLVE IT SOLVE IT)))))))
  #######(((((((SOLVE IT SOLVE IT SOLVE IT)))))))
  # solve the DAG
  i=1;j=1;stuck=1   # here, j is an index for the nodes, i is an index for the arrows, stuck prevents the loop from getting infinitely stuck
  nN<-nodes$name
  keepGoing=TRUE
  # this next monstrosity of a line finds all arrows who's parents are a source, then divides the parent's source load by the number of siblings to calculate the load in these "special" arrows.
  arrows[which(arrows$us_node %in% nodes[nodes$ntype=="source",]$name),]$loading<-nodes[arrows[which(arrows$us_node %in% nodes[nodes$ntype=="source",]$name),]$us_node,]$loading_output/arrows[which(arrows$us_node %in% nodes[nodes$ntype=="source",]$name),]$siblings
  while (keepGoing==TRUE){       ##### each loop focuses on a single node (nN[j+1]) and the arrow (i+1) that is going into it
    if(any(arrows$ds_node==(nN[j]))==TRUE & is.na(sum(arrows[which(arrows$ds_node==(nN[j])),]$loading))==FALSE){       #2. DO I KNOW THE LOADINGS OF ARROWS COMING INTO ME               # if there are any arrows coming into me (Node nN[j])...
      nodes[nN[j],]$loading_output=10^(log10(sum(arrows[which(arrows$ds_node==(nN[j])),]$loading))-nodes[nN[j],]$fit)  # then get the sum of all arrows coming into me (nN[j]), minus my LRV, to calculate my output loading
    }
    if(arrows[i,]$iamsolid==TRUE & arrows[i,]$siblings_liquid>0){    #CALCULATES THE LOADING IN THIS ARROW     # if this arrow is a solid but has liquid siblings
      arrows[i,]$loading=nodes[arrows[i,]$us_node,]$loading_output*lambda/arrows[i,]$siblings_solid          # then use the factor lambda to divide the loading up between liquid vs. solid
    }else{
      if(arrows[i,]$iamsolid==FALSE & arrows[i,]$siblings_solid>0){   #CALCULATES THE LOADING IN THIS ARROW     # if this arrow is a liquid but has solid siblings
        arrows[i,]$loading=nodes[arrows[i,]$us_node,]$loading_output*(1-lambda)/arrows[i,]$siblings_liquid     # then use the factor lambda to divide the loading up between liquid vs. solid
      }else{arrows[i,]$loading=nodes[arrows[i,]$us_node,]$loading_output/arrows[i,]$siblings}                  # otherwise this arrow only has siblings that are the same as it (could be liquid or solid, but they're all the same), so just divide the loading by the number of siblings
    }
    stuck<-stuck+1
    if(i==(nrow(arrows))){i=1} else {i=i+1}
    if(j==(nrow(nodes))){j=1} else {j=j+1} ;arrows;nodes[,c("subType","loading_output")];i;j;nN[j]
    if(stuck==1000){keepGoing = FALSE} else {keepGoing = (any(is.na(arrows$loading)) == TRUE | any(is.na(nodes$loading_output)) == TRUE)}
  }

  lrv=round(log10(sum(nodes$loading_output[nodes$ntype=="source"])/sum(nodes$loading_output[nodes$ntype=="end use"])),2)
  references<-unique(k2pdata[nodes$subType %in% tolower(unique(k2pdata$technology_description)),]$bib_id)

  #######(((((((I SOLVED IT!)))))))
  #######(((((((I SOLVED IT!)))))))
  #######(((((((I SOLVED IT!)))))))

  # store the results
  #arrowLoads<-solved$arrows
  results$Centralized_LRV<-lrv
  if(any(nodes$matrix=="liquid")){results$Liquid_Effluent<-nodes[nodes$ntype=="end use" & nodes$matrix=="liquid",]$loading_output}else{results$Liquid_Effluent<-0}
  if(any(nodes$matrix=="solid")){results$Sludge_Biosolids<-sum(nodes[nodes$ntype=="end use" & nodes$matrix=="solid",]$loading_output)}else{results$Sludge_Biosolids<-0}

  loadings=results
  loadings$Percent_Liquid<-round(loadings$Liquid_Effluent/(loadings$Liquid_Effluent+loadings$Sludge_Biosolids)*100,1)
  loadings$Percent_Solid<-round(loadings$Sludge_Biosolids/(loadings$Liquid_Effluent+loadings$Sludge_Biosolids)*100,1)

  arrows$relativeLoading<-arrows$loading/(results$In_Fecal_Sludge+results$In_Sewage)
  arrows$us_node_type<-nodes[arrows$us_node,]$subType
  arrows$ds_node_type<-nodes[arrows$ds_node,]$subType

  #****#****#****#****#
  uPs<-paste(unique(nodes$subType[nodes$ntype=="unit process"]), collapse = ', ')
  methods<-paste("treats ",nodes$subType[nodes$ntype=="source"][1],if(length(nodes$subType[nodes$ntype=="source"]==2)){paste(" and",nodes$subType[nodes$ntype=="source"][2])},
                 " using the following technologies: ",
                 uPs,".",sep=""
                 );methods
  nodes[nodes$ntype=="unit process",]
  #****#****#****#****#

  solved<-list(arrows=arrows[,c("us_node","ds_node","loading","flowtype","us_node_type","ds_node_type","relativeLoading")],
               nodes=nodes[,c("name","ntype","subType","temperature","retentionTime","depth","useCategory","moistureContent","holdingTime","matrix","loading_output","pathogen")],
               loadings=loadings,
               references=references)

  return(solved)
}

