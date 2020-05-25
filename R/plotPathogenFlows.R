#' The plotPathogenFlows function
#'
#' This function plots a Sankey flow chart showing pathogen loadings through a system sketched using the K2P Sketcher Tool (http://tools.waterpathogens.org/sketcher/)
#' @param sketcherResults A data frame output from the getLRV function
#' @keywords pathogens
#' @export
#' @examples
#' plotPathogenFlows(getLRV(mySketch="http://data.waterpathogens.org/dataset/afafd87c-b592-44c5-bb42-0a5a8415e54b/resource/83057ee9-402d-4b9b-8ab3-92053ee94c63/download/lubigisewageandfecalsludgetreatmentsystemv4.json",pathogenType="Virus",inFecalSludge=10000000000,inSewage=10000000000))
#'
plotPathogenFlows<-function(sketcherResults){
  #require(igraph)
  #require(networkD3)
  myNodes<-data.frame(name=make.names(sketcherResults$nodes$subType,unique=T),id=nodes$name)
  myNodes$newID<-rownames(myNodes)
  myLinks<-data.frame(source=sketcherResults$arrows$us_node,target=sketcherResults$arrows$ds_node,value=sketcherResults$arrows$relativeLoading*100)
  lookup<-myNodes[,c("id","newID")]
  myLinks$source<-as.integer(lookup$newID[match(myLinks$source,lookup$id)])-1
  myLinks$target<-as.integer(lookup$newID[match(myLinks$target,lookup$id)])-1

  p <- networkd3::sankeyNetwork(Links = myLinks, Nodes = myNodes, Source = "source",
                     Target = "target", Value = "value", NodeID = "name",
                     units = "%", fontSize = 12, nodeWidth = 30)
  return(p)
}
