#' The PathEmPlot function
#'
#' This function takes the output from the getLoadings function and puts it into a format that can be plotted as a stacked bar plot using https://reaviz.io/?path=/story/charts-bar-chart-vertical-multi-series--stacked.
#' @param myLoadings The output from the getLoadings function
#' @keywords pathogens
#' @export
#' @examples
#' pathEmPlot(myLoadings=getLoadings())
#'
#' [
#' [
#'   {
#'     "Category": "1 Flush Toilets (Sewered)",
#'     "region": "HND",
#'     "emissionsPerCapita": 283449937507.16
#'   },
#'   {
#'     "Category": "2 Flush Toilets (Onsite)",
#'     "region": "HND",
#'     "emissionsPerCapita": 771922982401.523
#'   },
#'   {
#'     "Category": "3 Composting Toilets",
#'     "region": "HND",
#'     "emissionsPerCapita": 6163234018.8351
#'   },
#'   {
#'     "Category": "4 Dry Toilets",
#'     "region": "HND",
#'     "emissionsPerCapita": 6163183904.1368
#'   },
#'   {
#'     "Category": "5 Open Defecation",
#'     "region": "HND",
#'     "emissionsPerCapita": 366495976591.075
#'   },
#'   {
#'     "Category": "1 Flush Toilets (Sewered)",
#'     "region": "UGA",
#'     "emissionsPerCapita": 363549225541.818
#'   },
#'   {
#'     "Category": "2 Flush Toilets (Onsite)",
#'     "region": "UGA",
#'     "emissionsPerCapita": 791391988400.548
#'   },
#'   {
#'     "Category": "3 Composting Toilets",
#'     "region": "UGA",
#'     "emissionsPerCapita": 6943531405.2761
#'   },
#'   {
#'     "Category": "4 Dry Toilets",
#'     "region": "UGA",
#'     "emissionsPerCapita": 379008140777.892
#'   },
#'   {
#'     "Category": "5 Open Defecation",
#'     "region": "UGA",
#'     "emissionsPerCapita": 730164207658.016
#'   }
#'   ]
#' ]
#'
pathEmPlot<-function(myLoadings){
  for(i in 1:length(myLoadings$det)){
    myLoadings$det[[i]]$emitPC <- round((myLoadings$det[[i]]$toSurface + myLoadings$det[[i]]$toGroundwater) / myLoadings$input$population[i] * t(myLoadings$input[i,][,c("flushSewer","flushSeptic","flushPit","flushOpen","flushUnknown","pitSlab","pitNoSlab","compostingTwinSlab","compostingTwinNoSlab","compostingToilet","bucketLatrine","containerBased","hangingToilet","openDefecation","other")])[,1],0)
    myLoadings$det[[i]]$emitPC[is.na(myLoadings$det[[i]]$emitPC)]<-0
    plot1<-data.frame(sanTechCat=c("1 Flush Toilets (Sewered)",rep("2 Flush Toilets (Onsite)",4),rep("4 Dry Toilets",2),rep("3 Composting Toilets",3),rep("4 Dry Toilets",2),rep("5 Open Defecation",3)),myLoadings$det[[i]][,c("sanitationTechnology","emitPC")])
    plot2<-aggregate(emitPC~sanTechCat,data=plot1,FUN=sum)
    plotMe<-if(i==1){data.frame(Category=plot2[,1],plot2[i+1])}else{cbind(plotMe,plot2[,2])}
    names(plotMe)[i+1]<-as.character(myLoadings$input$region[i])
  }
  plotMe<-tidyr::gather(plotMe,key=region,value=emissionsPerCapita,-Category)
  plotMe$emissionsPerCapita<-as.numeric(plotMe$emissionsPerCapita)
  #ggplot2::ggplot(data=plotMe,ggplot2::aes(x=region,y=emissionsPerCapita,fill=Category)) +
  #  ggplot2::geom_bar(stat="identity") + ggplot2::theme_classic() +
  #  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  return(jsonlite::toJSON(list(plotMe),pretty = T))
}
pathEmPlot(my)
