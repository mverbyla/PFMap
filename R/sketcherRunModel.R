############
### CORS ###
############

#' @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}


######################################################
################### GET LOADINGS #####################
######################################################

#* API to predict annual loadings of water pathogens from onsite sanitation system technologies used in a given region of the world
#* @param onsiteData Filepath to a CSV file containing the onsite sanitation data in your area of interest
#* @param pathogenType The pathogen type you want to analyze (Virus, Bacteria, Protozoa, Helminth)
#* @get /loadings
#* @serializer unboxedJSON
function(onsiteData="http://data.waterpathogens.org/dataset/5374462b-5bb5-456f-bfc0-816ea572666d/resource/4d9e5fba-9280-4b8b-acce-d1c87952acc1/download/onsitedata_example.csv",pathogenType="Virus"){
  devtools::install_github('mverbyla/pathogenflows')
  loadings<-pathogenflows::getLoadings(onsiteData,pathogenType)
  print(loadings)
  return(loadings)
}


######################################################
################## ANALYZE SKETCH ####################
######################################################

#* API to estimate the log reduction value for a sketched wastewater or fecal sludge treatment plant
#* @param mySketch Filepath to a JSON file containing a sketch of your treatment plant
#* @param pathogenType The pathogen type you want to analyze (Virus, Bacteria, Protozoa, Helminth)
#* @param inFecalSludge The number of pathogens per day taken in at the plant in fecal sludge received
#* @param inSewage The number of pathogens per day taken in at the plant in sewerage
#* @get /lrv
#* @serializer unboxedJSON
function(mySketch="http://data.waterpathogens.org/dataset/a1423a05-7680-4d1c-8d67-082fbeb00a50/resource/e7852e8f-9603-4b19-a5fa-9cb3cdc63bb8/download/sketch_lubigi.json", pathogenType="Virus", inFecalSludge=10000000000, inSewage=10000000000){
  devtools::install_github('mverbyla/pathogenflows')
  lrv<-pathogenflows::getLRV(mySketch,pathogenType,inFecalSludge,inSewage)
  print(lrv)
  return(lrv)
}

