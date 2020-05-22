getNodes<-function(sketch,nodes){
  nodes$number_inputs<-NA
  nodes$number_outputs<-NA
  for(i in 1:nrow(sketch)){
    nodes$number_inputs[i]<-length(sketch[[2]][[i]])
    nodes$number_outputs[i]<-length(data.frame(sketch[i,3])[1,])
  };nodes
  nodes$loading_output=NA
  sn<-sketch[,c("parents","children")]
  sn$me<-as.numeric(row.names(sn))
  lenny<-rep(NA,length(sn[,1]))
  rem<-NA;j=0
  for(i in 1:length(sn[,1])){
    lenny[i]<-if(is.null(dim(sn[i,1][[1]]))){0}else{length(sn[i,1][[1]])}
    if(is.null(dim(sn[i,1][[1]]))){sn[i,1][[1]]<-NA}
    if(is.null(dim(sn[i,2][[1]]))){sn[i,2][[1]]<-NA}
    if(is.na(sn[[1]][[i]])){
      j=j+1;rem[j]<-i
    }
  };rem
  lenny
  arrows<-data.frame(us_node=rep(NA,sum(lenny)),ds_node=rep(NA,sum(lenny)))
  sn<-sn[-rem,];rownames(sn)<-1:nrow(sn)
  m=0
  for(i in 1:sum(lenny)){
    for(j in 1:length(sn[i,"parents"][[1]][1,])){
      repl<-as.numeric(gsub(".*?([0-9]+).*", "\\1", sn[i,"parents"][[1]][1,j]))
      if(length(repl)>0){
        arrows$us_node[m+1]<-repl
        arrows$ds_node[m+1]<-sn$me[i]
        m=m+1
      }
    }
  }
  arrows$us_node
  arrows$ds_node
  arrows$loading<-NA
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
  return(list(nodes=nodes,arrows=arrows))
}
