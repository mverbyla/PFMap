# getLRV function for the pathogen flow model (to use with sketcher tool)
getLRV(mySketch="http://data.waterpathogens.org/dataset/a1423a05-7680-4d1c-8d67-082fbeb00a50/resource/e7852e8f-9603-4b19-a5fa-9cb3cdc63bb8/download/sketch_lubigi.json",pathogenType,
       inFecalSludge=10000000000,inSewage=10000000000){
  k2pdata<-read.csv("http://data.waterpathogens.org/dataset/eda3c64c-479e-4177-869c-93b3dc247a10/resource/9e172f8f-d8b5-4657-92a4-38da60786327/download/treatmentdata.csv",header=T)

}

results<-data.frame(In_Fecal_Sludge=inFecalSludge,
                    In_Sewage=inSewage,
                    Sludge_Biosolids=NA,
                    Liquid_Effluent=NA,
                    Centralized_LRV=NA)

sketch=jsonlite::read_json(mySketch,simplifyVector = T)
#pData=read.csv(myData,header=T)
sketch$temperature<-as.double(sketch$temperature)
sketch$retentionTime<-as.double(sketch$retentionTime)
sketch$depth<-as.double(sketch$depth)

res<-suppressWarnings(getNodes(sketch = sketch, nodes = sketch[,-c(2,3)]))
nodes<-res$nodes
arrows<-res$arrows

sketch
nodes
arrows
results

# transform the K2P data and fit the models
k2pdata<-transformData(k2pdata);head(k2pdata)
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
  warnings[length(warnings)+1]<-"Warning: The onsite sanitation technologies in your system produce fecal sludge, but according to your sketch, the treatment plant does not accept fecal sludge.")
  results$In_Fecal_Sludge<-0
  skipFS<-TRUE
}else{skipFS<-FALSE}
if(sum(results[mySketch,]$In_Sewage)==0 | (any(results[mySketch,]$In_Sewage>0)==TRUE & any(nodes$subType=="sewerage")==FALSE)){ #if the onsite system produces sewerage but the treatment plant does not accept any
  results$to_surface<-results$to_surface+results$In_Sewage
  warnings[length(warnings)+1]<-paste("Warning: The onsite sanitation technologies in",mySketch,"produce sewerage, but according to your sketch, the treatment plant does not appear to be accepting sewerage.")
  results[mySketch,]$In_Sewage<-0
  skipWW<-TRUE
}else{skipWW<-FALSE}

nodes[[k]]$loading_output<-NA
arrows[[k]]$loading<-NA
if(skipFS==FALSE){nodes[nodes$subType=="fecal sludge",]$loading_output<-results[mySketch,]$In_Fecal_Sludge}
if(skipWW==FALSE){nodes[nodes$subType=="sewerage",]$loading_output<-results[mySketch,]$In_Sewage}
# get the LRVs for each node
nodes<-estimate(nodes,pathogenType=pathogenType)

nodeLRVs[[pathogenType]]<-nodes[,c("name","fit","lwr","upr")]

# solve the DAG
solved<-solveit(nodes[[k]],arrows[[k]],lambda=lambdas[h])
#myNodes[[k]][[pathogenGroups[h]]]<-solved$arrows[,c("name","pathogen","loading_output","fit","lwr","upr")]
# store the results
arrowLoads[[k]][[pathogenGroups[h]]]<-solved$arrows
results[[h]][sketches[k],]$Centralized_LRV<-round(solved$lrv,2)
if(any(solved$nodes$matrix=="liquid")){results[[h]][sketches[k],]$Liquid_Effluent<-solved$nodes[solved$nodes$ntype=="end use" & solved$nodes$matrix=="liquid",]$loading_output}else{results[[h]][sketches[k],]$Liquid_Effluent<-0}
if(any(solved$nodes$matrix=="solid")){results[[h]][sketches[k],]$Sludge_Biosolids<-sum(solved$nodes[solved$nodes$ntype=="end use" & solved$nodes$matrix=="solid",]$loading_output)}else{results[[h]][sketches[k],]$Sludge_Biosolids<-0}



# First the references
ref.names <- sketches
references <- vector("list", length(ref.names))
names(references) <- ref.names
for(k in 1:length(sketches)){
  references[[k]]<-unique(k2pdata[nodes[[k]]$subType %in% tolower(unique(k2pdata$technology_description)),]$bib_id)
}
references

####### ###### ###### ###### #######
###### outputs from central #######
###### ###### ###### ###### ######
##### ###### ###### ###### #####
#### ###### ###### ###### ####
### ###### ###### ###### ###
## ###### ###### ###### ##
# ###### ###### ###### #
###### ###### ######
###### ###### ###### ###### ######
### ###### ###### ###### ###
###### ###### ######
# ###### #
##

###### central reduction ######

results$virus$Scenario<-row.names(results$virus)
results$bacteria$Scenario<-row.names(results$bacteria)
results$protozoa$Scenario<-row.names(results$protozoa)
results$helminth$Scenario<-row.names(results$helminth)
mResults<-melt(results)
centralReduction<-mResults[mResults$variable=="Centralized_LRV",c("Scenario","value","L1")]
colnames(centralReduction)<-c("Scenario","LRV","Pathogen")
centralReduction$Pathogen<-c(rep("1-Virus",length(sketches)),rep("2-Bacteria",length(sketches)),rep("3-Protozoa",length(sketches)),rep("4-Helminth",length(sketches)))
write.csv(centralReduction,paste("outputs/centralReduction_",onsiteData,".csv",sep=""))
cR<-ggplot(centralReduction,aes(x=Scenario,y=LRV,fill=Scenario)) +
  geom_bar(stat = "identity",position = "dodge") +
  facet_grid(~Pathogen) +
  coord_cartesian(ylim = c(0, 3)) +
  ggtitle("Pathogen Reduction by Centralized Treatment Plant",subtitle=paste(onsiteScenarios[which(onsiteScenarios$data==onsiteData),"scenarios"])) +
  theme(legend.position="none",axis.text.x = element_text(size = 10, angle = 60, hjust = 1))
fileCR<-paste("outputs",paste("centralReduction_",onsiteData,".png",sep=""),sep="/")
ggsave(filename=fileCR, plot=cR, width = 6, height = 3, dpi = 300, units = "in", device='png')

###### central loading ######

centralLoading<-mResults[mResults$variable=="In_Fecal_Sludge"|mResults$variable=="In_Sewage"|mResults$variable=="Liquid_Effluent"|mResults$variable=="Sludge_Biosolids",]
colnames(centralLoading)<-c("Scenario","Variable","Loading","Pathogen")
write.csv(centralLoading,file=paste("outputs/centralLoading_",onsiteData,".csv",sep=""))
centralLoading$Pathogen<-c(rep("1-Virus",length(sketches)*4),rep("2-Bacteria",length(sketches)*4),rep("3-Protozoa",length(sketches)*4),rep("4-Helminth",length(sketches)*4))
cl1<-subset(centralLoading,Variable=="Liquid_Effluent"|Variable=="Sludge_Biosolids")
cl2<-subset(centralLoading,Variable=="In_Sewage"|Variable=="In_Fecal_Sludge")
colnames(cl1)<-c("Scenario","Matrix","Loading","Pathogen")
colnames(cl2)<-c("Scenario","Matrix","Loading","Pathogen")

ska<-ggplot(cl1,aes(x=Scenario,y=Loading,fill=Matrix)) +
  geom_bar(stat="identity") +
  facet_grid(~Pathogen) +
  coord_cartesian(ylim = c(0, 5e14)) +
  ggtitle("Pathogens Emitted to the Environment",subtitle=paste(onsiteScenarios[which(onsiteScenarios$data==onsiteData),"scenarios"])) +
  #ggtitle("Pathogens Emitted to the Environment") +
  theme(axis.text.x = element_text(size = 10, angle = 60, hjust = 1))
skb<-ggplot(cl2,aes(x=Scenario,y=Loading,fill=Matrix)) +
  geom_bar(stat="identity") +
  facet_grid(~Pathogen) +
  coord_cartesian(ylim = c(0, 1e16)) +
  ggtitle("Pathogens Received at Treatment Plant",subtitle=paste(onsiteScenarios[which(onsiteScenarios$data==onsiteData),"scenarios"])) +
  #ggtitle("Pathogens Received at Treatment Plant") +
  theme(axis.text.x = element_text(size = 10, angle = 60, hjust = 1))

sk<-grid.arrange(skb,ska,ncol=1)

fileSk<-paste("outputs",paste("centralLoading_",onsiteData,".png",sep=""),sep="/")
ggsave(filename=fileSk, plot=sk, width = 8, height = 8, dpi = 300, units = "in", device='png')
