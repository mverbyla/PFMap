#' The getLoadings function
#'
#' This function predicts the pathogen loadings from onsite sanitation systems for data available through the UNICEF/WHO Joint Monitoring Program and provides an output that can be used directly by the Pathogen Mapping Tool.
#' @param inputDF A dataframe containing your onsite sanitation data. An example template can be found at http://data.waterpathogens.org/dataset/pasteaddresshere?
#' @param pathogenType Specify either "Virus","Bacteria","Protozoa", or "Helminth"
#' @param context Specify either "urban" or "rural"
#' @keywords pathogens
#' @export
#' @examples
#' getLoadings(inputDF,pathogenType="Virus",context="urban")
#'
#' $output
#' region excreted to_groundwater   to_surface retained_in_soil      decayed In_Fecal_Sludge    In_Sewage  stillViable Onsite_LRV Onsite_PR
#' 1    HND 3.63e+18   5.840389e+16 1.963546e+18     5.738629e+17 5.042712e+17    1.179983e+14 5.297977e+17 2.551866e+18       0.15    0.2970
#' 2    UGA 1.67e+19   1.565005e+17 1.731193e+18     1.408505e+18 1.340068e+19    0.000000e+00 3.127723e+15 1.890822e+18       0.95    0.8868

getLoadings<-function(inputDF,pathogenType="Virus",context="urban"){

  df1<-inputDF
  assume<-data.frame(urban=c(0,0.25,0.5),rural=c(0.99,0.25,0.1))
  rownames(assume)<-c("coverBury","sewerLeak","emptiedTreatment")

  pathogenGroups<-c("Virus","Bacteria","Protozoa","Helminth")
  index<-which(pathogenGroups==pathogenType)
  # &&&&& START GWPP Inputs &&&&&
  lambdas<-c(lambdaV=0.2,lambdaB=0.3,lambdaP=0.6,lambdaH=0.99) # these lambda values are based on data from the literature (Chauret et al., 1999; Lucena et al., 2004; Ramo et al., 2017; Rose et al., 1996; Kinyua et al. 2016; Tanji et al., 2002; Tsai et al., 1998)
  vzReduction<-c(vzV=0.1,vzB=0.01,vzP=0.001,vzH=0.0001) # currently assuming 1-, 2-, 3-, and 4-log reduction of viruses, bacteria, protozoa, and helminth eggs, respectively, between pits and groundwater

  #persistence model in pits
  persist<-read.csv("http://data.waterpathogens.org/dataset/eda3c64c-479e-4177-869c-93b3dc247a10/resource/f99291ab-d536-4536-a146-083a07ea49b9/download/k2p_persistence.csv",header=T)
  persist<-persist[persist$matrix=="Fecal sludge",]
  persist$ln_removal<--persist$log10_reduction*log(10)
  N<-length(unique(persist$experiment_id))
  persist$ind<-NA       # this will be an index variable to distinguish each independent experiment
  for(j in 1:length(persist$experiment_id)){
    persist$ind[j]<-which(data.frame(unique(persist$experiment_id))==persist$experiment_id[j])
  }
  k<-rep(NA,N);group<-rep(NA,N);addit<-rep(NA,N); pH<-rep(NA,N);urine<-rep(NA,N);moisture<-rep(NA,N);temperature<-rep(NA,N);r2<-rep(NA,N);num<-rep(NA,N);authors<-rep(NA,N)

  for(z in 1:N){   #in this loop, we calculate the k value for the log linear decay:                 Ct = Co*exp(-k*t)
    time<-persist[persist$ind==z,]$time_days   #get the time only for the present experiment
    lnrv<-persist[persist$ind==z,]$ln_removal  #get the ln reduction only for the present experiment
    # since we calculated the ln reduction, then equation gets algebraically rearranged like this:    ln(Ct/Co) = -k*t
    # lnrv is ln(Ct/Co), so our linear model is like this: lnrv~time
    fit<-lm(lnrv~time)
    num[z]<-length(time)
    r2[z]<-suppressWarnings(summary(fit)$r.squared)
    k[z]<-fit$coefficients[2]
    authors[z]<-paste(unique(persist[persist$ind==z,]$bib_id),as.character(unique(persist[persist$ind==z,]$authors)))
    group[z]<-as.character(unique(persist[persist$ind==z,]$microbial_group))
    addit[z]<-as.character(unique(persist[persist$ind==z,]$additive))
    pH[z]<-as.numeric(median(persist[persist$ind==z,]$pH))
    urine[z]<-as.character(unique(persist[persist$ind==z,]$urine))
    moisture[z]<-as.numeric(max(persist[persist$ind==z,]$moisture_content_percent))
    temperature[z]<-as.numeric(median(persist[persist$ind==z,]$temperature_celsius))
  }
  kPit<-data.frame(microbial_group=group,k=k,num=num,additive=addit,pH=pH,temp=temperature,moisture=moisture,urine=urine,r2=r2)
  kPit=kPit[-c(30,46,47,73,204,205),] #removing data points that are outliers
  kPit<-kPit[kPit$r2>0.7,] #only keeping data with good log linear fit (r2>0.7)
  kPit<-kPit[kPit$k<0,] #removing any data showing growth
  kPit<-kPit[kPit$num>4,] #removing any results from experiments done with fewer than 4 data points
  kPit$lk<-log(-kPit$k)
  kPit$ltemp<-log(kPit$temp)
  fit_kPit<-lm(lk~factor(microbial_group)+factor(urine)*pH+ltemp+moisture+additive,data=kPit)
  summary(fit_kPit)
  # I need to find out here how to make pitAdditive dependent on the index m (geographic location or treatment plant service area)
  kValueV<-suppressWarnings(exp(predict(fit_kPit,newdata=data.frame(urine="Fresh Urine",pH=7,ltemp=log(30),moisture=80,additive="None",microbial_group="Virus"))))
  kValueB<-suppressWarnings(exp(predict(fit_kPit,newdata=data.frame(urine="Fresh Urine",pH=7,ltemp=log(30),moisture=80,additive="None",microbial_group="Bacteria"))))
  kValueP<-suppressWarnings(exp(predict(fit_kPit,newdata=data.frame(urine="Fresh Urine",pH=7,ltemp=log(30),moisture=80,additive="None",microbial_group="Protozoa"))))
  kValueH<-suppressWarnings(exp(predict(fit_kPit,newdata=data.frame(urine="Fresh Urine",pH=7,ltemp=log(30),moisture=80,additive="None",microbial_group="Helminth"))))

  kValues<-c(Virus=kValueV,Bacteria=kValueB,Protozoa=kValueP,Helminth=kValueH);kValues #the units here are 1/days
  # &&&&& END GWPP Inputs &&&&&

  loops<-nrow(df1)

  loadings.names <- c("virus", "bacteria", "protozoa", "helminth")
  loadings <- vector("list", length(loadings.names))
  names(loadings) <- loadings.names
  for(o in 1:4){
    loadings[[o]] <- vector("list",loops)
    names(loadings[[o]]) <- df1$region
  }

  onsite_results.names <- c("virus", "bacteria", "protozoa", "helminth")
  onsite_results <- vector("list", length(onsite_results.names))
  names(onsite_results) <- onsite_results.names

  loadings<-loadings[[index]]
  onsite_results<-onsite_results[[index]]
  myJMP1<-read.csv("http://data.waterpathogens.org/dataset/86741b90-62ab-4dc2-941c-60c85bfe7ffc/resource/9113d653-0e10-4b4d-9159-344c494f7fc7/download/jmp_assumptions.csv",header=T)

  for(m in 1:loops){  # m is an index for region or subregion
    df<-df1[m,]
    myJMP<-myJMP1
    region<-df$region
    population<-df$population
    excreted<-if(context=="urban"){df$excreted_urban}else{if(context=="rural"){df$excreted_rural}else{NA}}
    isWatertight<-df$isWatertight
    hasLeach<-df$hasLeach
    emptyFrequency<-df$emptyFrequency
    pitAdditive<-df$pitAdditive

    coverBury<-if(is.na(df$coverBury)){assume["coverBury",context]}else{df$coverBury}
    emptiedTreatment<-if(is.na(df$fecalSludgeTreated)){assume["emptiedTreatment",context]}else{df$fecalSludgeTreated}
    sewerLeak<-if(is.na(df$sewageTreated)){assume["sewerLeak",context]}else{1-df$sewageTreated}

    flushSewer<-if(is.null(df$flushSewer)){0}else{if(is.na(df$flushSewer)){0}else{df$flushSewer}}
    flushSeptic<-if(is.null(df$flushSeptic)){0}else{if(is.na(df$flushSeptic)){0}else{df$flushSeptic}}
    flushPit<-if(is.null(df$flushPit)){0}else{if(is.na(df$flushPit)){0}else{df$flushPit}}
    flushOpen<-if(is.null(df$flushOpen)){0}else{if(is.na(df$flushOpen)){0}else{df$flushOpen}}
    flushUnknown<-if(is.null(df$flushUnknown)){0}else{if(is.na(df$flushUnknown)){0}else{df$flushUnknown}}
    pitSlab<-if(is.null(df$pitSlab)){0}else{if(is.na(df$pitSlab)){0}else{df$pitSlab}}
    pitNoSlab<-if(is.null(df$pitNoSlab)){0}else{if(is.na(df$pitNoSlab)){0}else{df$pitNoSlab}}
    compostingTwinSlab<-if(is.null(df$compostingTwinSlab)){0}else{if(is.na(df$compostingTwinSlab)){0}else{df$compostingTwinSlab}}
    compostingTwinNoSlab<-if(is.null(df$compostingTwinNoSlab)){0}else{if(is.na(df$compostingTwinNoSlab)){0}else{df$compostingTwinNoSlab}}
    compostingToilet<-if(is.null(df$compostingToilet)){0}else{if(is.na(df$compostingToilet)){0}else{df$compostingToilet}}
    bucketLatrine<-if(is.null(df$bucketLatrine)){0}else{if(is.na(df$bucketLatrine)){0}else{df$bucketLatrine}}
    containerBased<-if(is.null(df$containerBased)){0}else{if(is.na(df$containerBased)){0}else{df$containerBased}}
    hangingToilet<-if(is.null(df$hangingToilet)){0}else{if(is.na(df$hangingToilet)){0}else{df$hangingToilet}}
    openDefecation<-if(is.null(df$openDefecation)){0}else{if(is.na(df$openDefecation)){0}else{df$openDefecation}}
    other<-if(is.null(df$other)){0}else{if(is.na(df$other)){0}else{df$other}}


    daysperyear<-366
    decayTimeUNSAFE<-daysperyear  # this is the average time interval between unsafe pit emptying events (default is set to 1 year, assuming it happens at the beginning of the rainy season)

    myJMP$percentage<-c(flushSewer,
                        flushSeptic,
                        flushPit,
                        flushOpen,
                        flushUnknown,
                        pitSlab,
                        pitNoSlab,
                        compostingTwinSlab,
                        compostingTwinNoSlab,
                        compostingToilet,
                        bucketLatrine,
                        containerBased,
                        hangingToilet,
                        openDefecation,
                        other)

    myJMP$tankWatertight<-c(0,isWatertight,rep(0,13))
    myJMP$leachSystem<-c(0,hasLeach,rep(0,13))
    myJMP$cover_bury<-c(0,coverBury,coverBury,0,0,coverBury,0,coverBury,0,coverBury,0,coverBury,0,0,coverBury)
    myJMP$tankOutlet<-myJMP$flushOnsite*(1-myJMP$leachSystem)

    myJMP$DRY_TOILET<-(1-myJMP$flushSewer)*(1-myJMP$flushOnsite)

    myJMP$FLUSH_TOILET_sewered<-myJMP$flushSewer
    myJMP$FLUSH_TOILET_containedNotWT<-(1-myJMP$flushSewer)*myJMP$flushOnsite*(1-myJMP$tankWatertight)
    myJMP$FLUSH_TOILET_containedWT_noLeach<-myJMP$flushOnsite*myJMP$tankWatertight*myJMP$tankOutlet
    myJMP$FLUSH_TOILET_containedWT_Leach<-myJMP$flushOnsite*myJMP$tankWatertight*myJMP$leachSystem

    myJMP$safeEmpty<-emptiedTreatment*(1-myJMP$FLUSH_TOILET_sewered-myJMP$cover_bury)
    myJMP$unsafeEmpty<-1-myJMP$FLUSH_TOILET_sewered-myJMP$cover_bury-myJMP$safeEmpty #includes "flooding out" latrines in the rainy season

    # ^^^^^ END JMP data calculations ^^^^^

    loadings[[m]]<-data.frame(myJMP[,c("name","classification","percentage","initiallyContained","flushSewer","FLUSH_TOILET_containedNotWT","FLUSH_TOILET_containedWT_Leach","FLUSH_TOILET_containedWT_noLeach","unsafeEmpty","safeEmpty")])

    # &&&&& START PFM Onsite Calculations &&&&&

    i=index
    loadings[[m]]$lamda<-c(0,lambdas[i],lambdas[i],0,0,1,1,1,1,1,0,1,0,0,0)
    loadings[[m]]$excreted<-excreted*loadings[[m]]$percentage  #Eq. 1: Pathogen Loading Model (Column J) #/year
    loadings[[m]]$initContained<-loadings[[m]]$excreted*loadings[[m]]$initiallyContained  #Eq. 2: Number Initially Contained (Column O) #/year
    loadings[[m]]$notContained<-loadings[[m]]$excreted-loadings[[m]]$initContained
    loadings[[m]]$inLiquid<-loadings[[m]]$initContained*(1-loadings[[m]]$lamda)*(1-loadings[[m]]$flushSewer)
    loadings[[m]]$inSolid<-loadings[[m]]$initContained*loadings[[m]]$lamda*(1-loadings[[m]]$flushSewer)
    loadings[[m]]$toGW<-loadings[[m]]$inLiquid*(loadings[[m]]$FLUSH_TOILET_containedNotWT+loadings[[m]]$FLUSH_TOILET_containedWT_Leach)*vzReduction[[i]]   #Eq. 5: Number Reaching Groundwater (Column AI) #/year
    loadings[[m]]$inVZ<-loadings[[m]]$inLiquid*(loadings[[m]]$FLUSH_TOILET_containedNotWT+loadings[[m]]$FLUSH_TOILET_containedWT_Leach)-loadings[[m]]$toGW
    loadings[[m]]$coveredBuried<-loadings[[m]]$inSolid*coverBury
    loadings[[m]]$totalSubsurface<-loadings[[m]]$inVZ+loadings[[m]]$coveredBuried
    loadings[[m]]$toSW_liq<-loadings[[m]]$inLiquid*loadings[[m]]$FLUSH_TOILET_containedWT_noLeach+loadings[[m]]$initContained*loadings[[m]]$flushSewer*sewerLeak

    for(j in 1:15){  # decay for toilets with UNSAFE emptying practices
      if(j==8|j==9){ #i.e., if there are twin pits
        remaining<-exp(-kValues[i]*(seq(decayTimeUNSAFE,1,by=-1)+decayTimeUNSAFE-1))
        remaining<-replace(remaining,which(remaining<0.001),0.001)
        loadings[[m]]$toSW_sol[j]<-sum(remaining*loadings[[m]]$inSolid[j]/daysperyear*loadings[[m]]$unsafeEmpty[j])
        loadings[[m]]$unsafeDecay[j]<-sum((1-remaining)*loadings[[m]]$inSolid[j]/daysperyear*loadings[[m]]$unsafeEmpty[j])
      }else{ #i.e., there is only a single pit
        remaining<-exp(-kValues[i]*seq(decayTimeUNSAFE,1,by=-1))
        remaining<-replace(remaining,which(remaining<0.001),0.001)
        loadings[[m]]$toSW_sol[j]<-sum(remaining*loadings[[m]]$inSolid[j]/daysperyear*loadings[[m]]$unsafeEmpty[j])
        loadings[[m]]$unsafeDecay[j]<-sum((1-remaining)*loadings[[m]]$inSolid[j]/daysperyear*loadings[[m]]$unsafeEmpty[j])
      }
    }
    loadings[[m]]$toWWTP<-loadings[[m]]$initContained*loadings[[m]]$flushSewer*(1-sewerLeak)
    for(j in 1:15){  # decay for toilets with SAFE emptying practices
      if(j==8|j==9){ #i.e., if there are twin pits
        remaining<-exp(-kValues[i]*(seq(daysperyear*emptyFrequency,1,by=-1)+daysperyear*emptyFrequency-1))
        remaining<-replace(remaining,which(remaining<0.001),0.001)
        loadings[[m]]$toFSTP[j]<-sum(remaining*loadings[[m]]$inSolid[j]/daysperyear*loadings[[m]]$safeEmpty[j])/emptyFrequency
        # decay = Σ [ (1 - exp(-0.7559879*10)) * 4.4e17/366 * 1 ]
        loadings[[m]]$safeDecay[j]<-sum((1-remaining)*loadings[[m]]$inSolid[j]/daysperyear*loadings[[m]]$safeEmpty[j])/emptyFrequency
      }else{   #i.e., there is only a single pit
        remaining<-exp(-kValues[i]*(seq(daysperyear*emptyFrequency,1,by=-1)))
        remaining<-replace(remaining,which(remaining<0.001),0.001)
        loadings[[m]]$toFSTP[j]<-sum(remaining*loadings[[m]]$inSolid[j]/daysperyear*loadings[[m]]$safeEmpty[j])/emptyFrequency
        loadings[[m]]$safeDecay[j]<-sum((1-remaining)*loadings[[m]]$inSolid[j]/daysperyear*loadings[[m]]$safeEmpty[j])/emptyFrequency
      }
    }
    loadings[[m]]$totalDecayed<-loadings[[m]]$unsafeDecay+loadings[[m]]$safeDecay
    loadings[[m]]$toSW<-loadings[[m]]$notContained+loadings[[m]]$toSW_sol+loadings[[m]]$toSW_liq
    loadings[[m]]$stillViable<-(loadings[[m]][,"toGW"]+loadings[[m]][,"toSW"]+loadings[[m]][,"toWWTP"]+loadings[[m]][,"toFSTP"])
    loadings[[m]]$LRV_byTech<-round(log10(loadings[[m]][,"excreted"]/loadings[[m]][,"stillViable"]),2)
    LRV_byTechnology <- as.data.frame(t(loadings[[m]]$LRV_byTech))
    colnames(LRV_byTechnology) <- paste("LRV_",loadings[[m]]$name,sep="")
    LRV_byTechnology[is.na(LRV_byTechnology)]<-NA

    ### CALCULATE EMISSIONS ###
    excreted=sum(loadings[[m]]$excreted)
    to_groundwater=sum(loadings[[m]]$toGW)
    to_surface=sum(loadings[[m]]$toSW)
    retained_in_soil=sum(loadings[[m]]$totalSubsurface)
    decayed=sum(loadings[[m]]$totalDecayed)
    In_Sewage=sum(loadings[[m]]$toWWTP)
    In_Fecal_Sludge=sum(loadings[[m]]$toFSTP)

    loadings[[m]]<-loadings[[m]][,c("name","classification","percentage","excreted","toGW","toSW","totalSubsurface","totalDecayed","toFSTP","toWWTP","stillViable","LRV_byTech")]
    names(loadings[[m]])<-c("id","sanitationTechnology","percentage","excreted","toGroundwater","toSurface","inSubsurface","decayed","fecalSludge","sewerage","stillViable","onsiteLRV")

    newRow<-data.frame(region=df$region,excreted,to_groundwater,to_surface,retained_in_soil,decayed,
                       In_Fecal_Sludge,In_Sewage,stillViable=(to_groundwater+to_surface+In_Sewage+In_Fecal_Sludge),
                       Onsite_LRV=round(log10(excreted/(to_groundwater+to_surface+In_Sewage+In_Fecal_Sludge)),2),
                       Onsite_PR=round(((excreted-(to_groundwater+to_surface+In_Sewage+In_Fecal_Sludge))/excreted),4))
    onsite_results=rbind(onsite_results,newRow)
  }

  #return(list(detailed=loadings,summary=onsite_results[complete.cases(onsite_results),]))
  returned<-list(input=df1,
                 det=loadings,
                 output=onsite_results[complete.cases(onsite_results),]
                 )
  return(returned)

}

my=getLoadings(inputDF=my_input,context="urban")
