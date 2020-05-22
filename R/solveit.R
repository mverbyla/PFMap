solveit<-function(nodes,arrows,lambda){
  i=1;j=1
  while (any(is.na(arrows$loading)) == TRUE | any(is.na(nodes$loading_output)) == TRUE){
    if(nodes[j,]$ntype=="source"){arrows$loading[i]=nodes$loading_output[arrows$us_node[i]]/arrows$siblings[i]}
    if(any(arrows$ds_node==(j+1))==TRUE){
      nodes$loading_output[j+1]=10^(log10(sum(arrows$loading[which(arrows$ds_node==(j+1))]))-nodes$fit[j+1])
    }
    if(arrows$iamsolid[i+1]==TRUE & arrows$siblings_liquid[i+1]>0){ #if I'm a solid but I have liquid siblings
      arrows$loading[i+1]=nodes$loading_output[arrows$us_node[i+1]]*lambda/arrows$siblings_solid[i+1]
    }else{
      if(arrows$iamsolid[i+1]==FALSE & arrows$siblings_solid[i+1]>0){ #if I'm a liquid but I have solid siblings
        arrows$loading[i+1]=nodes$loading_output[arrows$us_node[i+1]]*(1-lambda)/arrows$siblings_liquid[i+1]
      }else{arrows$loading[i+1]=nodes$loading_output[arrows$us_node[i+1]]/arrows$siblings[i+1]} #otherwise I only have siblings that are like me, could be liquid or solid but we're all the same
    }
    #arrows$loading[i+1]=nodes$loading_output[arrows$us_node[i+1]]/arrows$divideby[i+1]
    if(i==(nrow(arrows)-1)){i=1} else {i=i+1}
    if(j==(nrow(nodes)-1)){j=1} else {j=j+1};arrows;nodes[,c("subType","loading_output")]
  }
  lrv=log10(sum(nodes$loading_output[nodes$ntype=="source"])/sum(nodes$loading_output[nodes$ntype=="end use"]))
  return(list(arrows=arrows,nodes=nodes,lrv=lrv))
}
