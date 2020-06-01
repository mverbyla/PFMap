#### Load necessary packages and data ####
#library(shiny)
#install.packages("Rcpp")  #do this locally first, then test the app again if it works locally but not on shiny website

#library(ggraph)
library(igraph)
library(networkD3)
library(htmlwidgets)

#mySketch<-"http://data.waterpathogens.org/dataset/d6c03a14-ed8e-4136-896b-89e806f61bd6/resource/b5f5254b-5a5b-4570-810e-c7155c4a6f27/download/wastestabilizationpondsystem.json"
#mySketch<-"http://data.waterpathogens.org/dataset/afafd87c-b592-44c5-bb42-0a5a8415e54b/resource/83057ee9-402d-4b9b-8ab3-92053ee94c63/download/lubigisewageandfecalsludgetreatmentsystemv4.json"
#sketcherResults<-getLRV(pathogenType="Virus",mySketch);exportJSON<-jsonlite::toJSON(sketcherResults);write(exportJSON,"data/WSPsystem-virus.json");sketcherResults<-getLRV(pathogenType="Bacteria",mySketch);exportJSON<-jsonlite::toJSON(sketcherResults);write(exportJSON,"data/WSPsystem-bacteria.json");sketcherResults<-getLRV(pathogenType="Protozoa",mySketch);exportJSON<-jsonlite::toJSON(sketcherResults);write(exportJSON,"data/WSPsystem-protozoa.json");sketcherResults<-getLRV(pathogenType="Helminth",mySketch);exportJSON<-jsonlite::toJSON(sketcherResults);write(exportJSON,"data/WSPsystem-helminth.json")

createLinks<-function(input){
  sketcherResults<-jsonlite::fromJSON(paste("data/",input,".json",sep=""),simplifyVector = T)
  myNodes<-data.frame(name=make.names(sketcherResults$nodes$subType,unique=T),id=sketcherResults$nodes$name)
  myNodes$newID<-rownames(myNodes)
  myLinks<-data.frame(source=sketcherResults$arrows$us_node,target=sketcherResults$arrows$ds_node,value=sketcherResults$arrows$relativeLoading*100)
  lookup<-myNodes[,c("id","newID")]
  myLinks$source<-as.integer(lookup$newID[match(myLinks$source,lookup$id)])-1
  myLinks$target<-as.integer(lookup$newID[match(myLinks$target,lookup$id)])-1
  return(list(myLinks=myLinks,myNodes=myNodes))
}
df1<-createLinks("WSPsystem-virus")
df2<-createLinks("WSPsystem-bacteria")
df3<-createLinks("WSPsystem-protozoa")
df4<-createLinks("WSPsystem-helminth")

#### Server ####
server <- function(input, output) {

  output$sankey1 <- renderSankeyNetwork({
    sankeyNetwork(Links = df1$myLinks, Nodes = df1$myNodes, Source = "source",
                  Target = "target", Value = "value", NodeID = "name",
                  units = "%", fontSize = 12, nodeWidth = 30)
  })

  output$sankey2 <- renderSankeyNetwork({
    sankeyNetwork(Links = df2$myLinks, Nodes = df2$myNodes, Source = "source",
                  Target = "target", Value = "value", NodeID = "name",
                  units = "%", fontSize = 12, nodeWidth = 30)
  })

  output$sankey3 <- renderSankeyNetwork({
    sankeyNetwork(Links = df3$myLinks, Nodes = df3$myNodes, Source = "source",
                  Target = "target", Value = "value", NodeID = "name",
                  units = "%", fontSize = 12, nodeWidth = 30)
  })

  output$sankey4 <- renderSankeyNetwork({
    sankeyNetwork(Links = df4$myLinks, Nodes = df4$myNodes, Source = "source",
                  Target = "target", Value = "value", NodeID = "name",
                  units = "%", fontSize = 12, nodeWidth = 30)
  })

}

#### UI ####

ui <- shinyUI(fluidPage(

  titlePanel("Pathogen Flow Diagram: Lubigi Treatment System (Kampala, Uganda)"),

  verticalLayout(
    mainPanel(
      tabsetPanel(
        tabPanel("Viruses", sankeyNetworkOutput("sankey1")),
        tabPanel("Bacteria", sankeyNetworkOutput("sankey2")),
        tabPanel("Protozoa", sankeyNetworkOutput("sankey3")),
        tabPanel("Helminths", sankeyNetworkOutput("sankey4"))
      )
    )
  )
))

#### Run ####
shinyApp(ui = ui, server = server)
