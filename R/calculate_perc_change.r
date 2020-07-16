rm(list=ls(all=TRUE))

#MODIFYING sewageTreated

#inputs are the input dataframe and the treatment goal to be achieved
#data<-read.csv("C:\\Users\\hofst023\\OneDrive - WageningenUR\\Oude D schijf\\Onderzoek\\K2P\\test\\Input_file_new_KLA_div.csv")
data<-read.csv("data/Input_file_new_KLA_div.csv")
goal_f_tr<-0.9

##just to test whether it works, I modified the data in several columns (this should not normally be part of the code):
#temp<-c(0.4,0.9,1,0.3,0.6)
#temp1<-c(0.3,0.5,0.4,0,0.7)
#data$sewageTreated_urb<-temp
#data$sewageTreated_rur<-temp1
#data$fraction_urban_pop<-c(0.7,0.8,0.9,0.3,0.5)
##after this the normal code starts

#calculate current population treated & total pop
pop_tr<-sum(data$population*data$fraction_urban_pop*data$flushSewer_urb*data$sewageTreated_urb+data$population*(1-data$fraction_urban_pop)*data$flushSewer_rur*data$sewageTreated_rur)
pop_tot<-sum(data$population*data$fraction_urban_pop*data$flushSewer_urb+data$population*(1-data$fraction_urban_pop)*data$flushSewer_rur)
data$pop_tr<-data$population*data$fraction_urban_pop*data$flushSewer_urb*data$sewageTreated_urb+data$population*(1-data$fraction_urban_pop)*data$flushSewer_rur*data$sewageTreated_rur
data$pop_tot<-data$population*data$fraction_urban_pop*data$flushSewer_urb+data$population*(1-data$fraction_urban_pop)*data$flushSewer_rur

#calculate current fraction treated (weighed by population)
f_tr<-pop_tr/pop_tot  #THIS IS THE STARTING VALUE FOR THE SLIDER BAR - CURRENT FRACTION TREATED

#set new fraction to be treated and calculate pop that requires treatment
goal_pop_tr<-goal_f_tr*pop_tot
factor_to_be_added<-goal_f_tr-f_tr

a<-which(data$sewageTreated_urb+factor_to_be_added>1 | data$sewageTreated_rur+factor_to_be_added>1)

data$f_str_new_urb<-0
data$f_str_new_rur<-0
data$new_pop_tr<-0

#if simply adding the factor is not possible, because of the cap of 1, then first calculate the
#population still to be distributed and distribute based on population
while(length(a)>0){    #a while loop is sometimes dangerous, as you can end up in a never ending loop - can be solved with a for loop and a breakout statement
  for(i in 1:length(a)){
    if(data$sewageTreated_urb[a[i]]+factor_to_be_added>1 && data$sewageTreated_rur[a[i]]+factor_to_be_added>1){
      data$f_str_new_urb[a[i]]<-1
      data$f_str_new_rur[a[i]]<-1
    }else if(data$sewageTreated_urb[a[i]]+factor_to_be_added>1){
      data$f_str_new_urb[a[i]]<-1
      data$f_str_new_rur[a[i]]<-data$sewageTreated_rur[a[i]]+factor_to_be_added+((data$population[a[i]]*data$fraction_urban_pop[a[i]]*data$flushSewer_urb[a[i]]*(data$sewageTreated_urb[a[i]]+factor_to_be_added)+data$population[a[i]]*(1-data$fraction_urban_pop[a[i]])*data$flushSewer_rur[a[i]]*(data$sewageTreated_rur[a[i]]+factor_to_be_added))-((data$population[a[i]]*data$fraction_urban_pop[a[i]]*data$flushSewer_urb[a[i]]*1)+data$population[a[i]]*(1-data$fraction_urban_pop[a[i]])*data$flushSewer_rur[a[i]]*(data$sewageTreated_rur[a[i]]+factor_to_be_added)))/(data$population[a[i]]*(1-data$fraction_urban_pop[a[i]])*data$flushSewer_rur[a[i]])
      if(data$f_str_new_rur[a[i]]>1){
        data$f_str_new_rur[a[i]]<-1
      }
    }else{
      data$f_str_new_urb[a[i]]<-data$sewageTreated_urb[a[i]]+factor_to_be_added+((data$population[a[i]]*data$fraction_urban_pop[a[i]]*data$flushSewer_urb[a[i]]*(data$sewageTreated_urb[a[i]]+factor_to_be_added)+data$population[a[i]]*(1-data$fraction_urban_pop[a[i]])*data$flushSewer_rur[a[i]]*(data$sewageTreated_rur[a[i]]+factor_to_be_added))-((data$population[a[i]]*(1-data$fraction_urban_pop[a[i]])*data$flushSewer_rur[a[i]]*1)+data$population[a[i]]*data$fraction_urban_pop[a[i]]*data$flushSewer_urb[a[i]]*(data$sewageTreated_urb[a[i]]+factor_to_be_added)))/(data$population[a[i]]*data$fraction_urban_pop[a[i]]*data$flushSewer_urb[a[i]])
      data$f_str_new_rur[a[i]]<-1
      if(data$f_str_new_urb[a[i]]>1){
        data$f_str_new_urb[a[i]]<-1
      }
    }

    data$new_pop_tr[a[i]]<-(data$population[a[i]]*data$fraction_urban_pop[a[i]]*data$flushSewer_urb[a[i]]*data$f_str_new_urb[a[i]]+data$population[a[i]]*(1-data$fraction_urban_pop[a[i]])*data$flushSewer_rur[a[i]]*data$f_str_new_rur[a[i]])-(data$population[a[i]]*data$fraction_urban_pop[a[i]]*data$flushSewer_urb[a[i]]*data$sewageTreated_urb[a[i]]+data$population[a[i]]*(1-data$fraction_urban_pop[a[i]])*data$flushSewer_rur[a[i]]*data$sewageTreated_rur[a[i]])
  }

  pop_already<-sum(data$pop_tr)+sum(data$new_pop_tr)
  pop_still<-goal_pop_tr-pop_already
  r<-which(data$f_str_new_urb==0)
  factor_to_be_added<-pop_still/sum(data$pop_tot[r])

  print(factor_to_be_added)

  a<-which((data$sewageTreated_urb+factor_to_be_added>1 | data$sewageTreated_rur+factor_to_be_added>1) & data$f_str_new_urb==0)

  if(length(a)==0){
    b<-which(data$f_str_new_urb==0)
    if(length(b)>0){
      for(i in 1:length(b)){
       if(data$sewageTreated_urb[b[i]]+factor_to_be_added>1 && data$sewageTreated_rur[b[i]]+factor_to_be_added<1){
          data$f_str_new_urb[b[i]]<-1
          data$f_str_new_rur[b[i]]<-data$sewageTreated_rur[b[i]]+factor_to_be_added+((data$population[b[i]]*data$fraction_urban_pop[b[i]]*data$flushSewer_urb[b[i]]*(data$sewageTreated_urb[b[i]]+factor_to_be_added)+data$population[b[i]]*(1-data$fraction_urban_pop[b[i]])*data$flushSewer_rur[b[i]]*data$flushSewer_rur[b[i]]*(data$sewageTreated_rur[b[i]]+factor_to_be_added))-((data$population[b[i]]*data$fraction_urban_pop[b[i]]*data$flushSewer_urb[b[i]]*1)+data$population[b[i]]*(1-data$fraction_urban_pop[b[i]])*data$flushSewer_rur[b[i]]*(data$sewageTreated_rur[b[i]]+factor_to_be_added)))/(data$population[b[i]]*(1-data$fraction_urban_pop[b[i]])*data$flushSewer_rur[b[i]])
        }else if (data$sewageTreated_rur[b[i]]+factor_to_be_added>1 && data$sewageTreated_urb[b[i]]+factor_to_be_added<1){
          data$f_str_new_urb[b[i]]<-data$sewageTreated_urb[b[i]]+factor_to_be_added+((data$population[b[i]]*data$fraction_urban_pop[b[i]]*data$flushSewer_urb[b[i]]*(data$sewageTreated_urb[b[i]]+factor_to_be_added)+data$population[b[i]]*(1-data$fraction_urban_pop[b[i]])*data$flushSewer_rur[b[i]]*(data$sewageTreated_rur[b[i]]+factor_to_be_added))-((data$population[b[i]]*(1-data$fraction_urban_pop[b[i]])*data$flushSewer_rur[b[i]]*1)+data$population[b[i]]*data$fraction_urban_pop[b[i]]*data$flushSewer_urb[b[i]]*(data$sewageTreated_urb[b[i]]+factor_to_be_added)))/(data$population[b[i]]*data$fraction_urban_pop[b[i]]*data$flushSewer_urb[b[i]])
          data$f_str_new_rur[b[i]]<-1
        }else{
          data$f_str_new_urb[b[i]]<-data$sewageTreated_urb[b[i]]+factor_to_be_added
          data$f_str_new_rur[b[i]]<-data$sewageTreated_rur[b[i]]+factor_to_be_added
        }
        data$new_pop_tr[b[i]]<-(data$population[b[i]]*data$fraction_urban_pop[b[i]]*data$flushSewer_urb[b[i]]*data$f_str_new_urb[b[i]]+data$population[b[i]]*(1-data$fraction_urban_pop[b[i]])*data$flushSewer_rur[b[i]]*data$f_str_new_rur[b[i]])-(data$population[b[i]]*data$fraction_urban_pop[b[i]]*data$flushSewer_urb[b[i]]*data$sewageTreated_urb[b[i]]+data$population[b[i]]*(1-data$fraction_urban_pop[b[i]])*data$flushSewer_rur[b[i]]*data$sewageTreated_rur[b[i]])
      }
    }
  }

}

if(length(a)==0 & sum(data$f_str_new_urb)==0 & sum(data$f_str_new_rur)==0){
  data$f_str_new_urb<-data$sewageTreated_urb+factor_to_be_added
  data$f_str_new_rur<-data$sewageTreated_rur+factor_to_be_added
}

f_str_new<-(sum(data$pop_tr)+sum(data$new_pop_tr))/sum(data$pop_tot)

#output are the new fractions sewageTreated: data$f_str_new_urb and data$f_str_new_rur

#MODIFYING faecalSludgeTreated and coverBury ***************************************************************

##THIS IS NOT CURRENTLY WORKING - NOT SURE HOW TO DIVERT / DEAL WITH FECALSLUDGETREATED AND COVERBURY YET

#inputs are the input dataframe and the treatment goal to be achieved
data<-read.csv("C:\\Users\\hofst023\\OneDrive - WageningenUR\\Oude D schijf\\Onderzoek\\K2P\\test\\Input_file_new_KLA_div.csv")
goal_f_tr<-0.9

##just to test whether it works, I modify the data in several columns (this should not normally be part of the code):
#temp<-c(0.4,0.9,1,0.3,0.6)
#temp1<-c(0.3,0.5,0.4,0,0.7)
#data$fecalSludgeTreated_urb<-temp
#data$fecalSludgeTreated_rur<-temp1
#data$fraction_urban_pop<-c(0.7,0.8,0.9,0.3,0.5)
##after this the normal code starts

#calculate current population treated & total pop
pop_tr<-sum(data$population*data$fraction_urban_pop*(data$flushSeptic_urb+data$flushPit_urb+data$flushOpen_urb+data$flushUnknown_urb+data$pitSlab_urb+data$pitNoSlab_urb+data$bucketLatrine_urb+data$containerBased_urb+data$compostingTwinSlab_urb+data$compostingTwinNoSlab_urb+data$compostingToilet_urb)*data$fecalSludgeTreated_urb+data$population*(1-data$fraction_urban_pop)*(data$flushSeptic_rur+data$flushPit_rur+data$flushOpen_rur+data$flushUnknown_rur+data$pitSlab_rur+data$pitNoSlab_rur+data$bucketLatrine_rur+data$containerBased_rur+data$compostingTwinSlab_rur+data$compostingTwinNoSlab_rur+data$compostingToilet_rur)*data$fecalSludgeTreated_rur)
pop_tot<-sum(data$population*data$fraction_urban_pop*(data$flushSeptic_urb+data$flushPit_urb+data$flushOpen_urb+data$flushUnknown_urb+data$pitSlab_urb+data$pitNoSlab_urb+data$bucketLatrine_urb+data$containerBased_urb+data$compostingTwinSlab_urb+data$compostingTwinNoSlab_urb+data$compostingToilet_urb)+data$population*(1-data$fraction_urban_pop)*(data$flushSeptic_rur+data$flushPit_rur+data$flushOpen_rur+data$flushUnknown_rur+data$pitSlab_rur+data$pitNoSlab_rur+data$bucketLatrine_rur+data$containerBased_rur+data$compostingTwinSlab_rur+data$compostingTwinNoSlab_rur+data$compostingToilet_rur))
data$pop_tr<-data$population*data$fraction_urban_pop*(data$flushSeptic_urb+data$flushPit_urb+data$flushOpen_urb+data$flushUnknown_urb+data$pitSlab_urb+data$pitNoSlab_urb+data$bucketLatrine_urb+data$containerBased_urb+data$compostingTwinSlab_urb+data$compostingTwinNoSlab_urb+data$compostingToilet_urb)*data$fecalSludgeTreated_urb+data$population*(1-data$fraction_urban_pop)*(data$flushSeptic_rur+data$flushPit_rur+data$flushOpen_rur+data$flushUnknown_rur+data$pitSlab_rur+data$pitNoSlab_rur+data$bucketLatrine_rur+data$containerBased_rur+data$compostingTwinSlab_rur+data$compostingTwinNoSlab_rur+data$compostingToilet_rur)*data$fecalSludgeTreated_rur
data$pop_tot<-data$population*data$fraction_urban_pop*(data$flushSeptic_urb+data$flushPit_urb+data$flushOpen_urb+data$flushUnknown_urb+data$pitSlab_urb+data$pitNoSlab_urb+data$bucketLatrine_urb+data$containerBased_urb+data$compostingTwinSlab_urb+data$compostingTwinNoSlab_urb+data$compostingToilet_urb)+data$population*(1-data$fraction_urban_pop)*(data$flushSeptic_rur+data$flushPit_rur+data$flushOpen_rur+data$flushUnknown_rur+data$pitSlab_rur+data$pitNoSlab_rur+data$bucketLatrine_rur+data$containerBased_rur+data$compostingTwinSlab_rur+data$compostingTwinNoSlab_rur+data$compostingToilet_rur)

#calculate current fraction treated (weighed by population)
f_tr<-pop_tr/pop_tot

#set new fraction to be treated and calculate pop that requires treatment
goal_pop_tr<-goal_f_tr*pop_tot
factor_to_be_added<-goal_f_tr-f_tr

a<-which(data$fecalSludgeTreated_urb+factor_to_be_added>1 | data$fecalSludgeTreated_rur+factor_to_be_added>1)

data$f_ftr_new_urb<-0
data$f_ftr_new_rur<-0
data$new_pop_tr<-0

#if simply adding the factor is not possible, because of the cap of 1, then first calculate the
#population still to be distributed and distribute based on population
while(length(a)>0){    #a while loop is sometimes dangerous, as you can end up in a never ending loop - can be solved with a for loop and a breakout statement
  for(i in 1:length(a)){
    if(data$fecalSludgeTreated_urb[a[i]]+factor_to_be_added>1 && data$fecalSludgeTreated_rur[a[i]]+factor_to_be_added>1){
      data$f_ftr_new_urb[a[i]]<-1
      data$f_ftr_new_rur[a[i]]<-1
    }else if(data$fecalSludgeTreated_urb[a[i]]+factor_to_be_added>1){
      data$f_ftr_new_urb[a[i]]<-1
      data$f_ftr_new_rur[a[i]]<-data$fecalSludgeTreated_rur[a[i]]+factor_to_be_added+((data$population[a[i]]*data$fraction_urban_pop[a[i]]*(data$flushSeptic_urb[a[i]]+data$flushPit_urb[a[i]]+data$flushOpen_urb[a[i]]+data$flushUnknown_urb[a[i]]+data$pitSlab_urb[a[i]]+data$pitNoSlab_urb[a[i]]+data$bucketLatrine_urb[a[i]]+data$containerBased_urb[a[i]]+data$compostingTwinSlab_urb[a[i]]+data$compostingTwinNoSlab_urb[a[i]]+data$compostingToilet_urb[a[i]])*(data$fecalSludgeTreated_urb[a[i]]+factor_to_be_added)+data$population[a[i]]*(1-data$fraction_urban_pop[a[i]])*(data$flushSeptic_rur[a[i]]+data$flushPit_rur[a[i]]+data$flushOpen_rur[a[i]]+data$flushUnknown_rur[a[i]]+data$pitSlab_rur[a[i]]+data$pitNoSlab_rur[a[i]]+data$bucketLatrine_rur[a[i]]+data$containerBased_rur[a[i]]+data$compostingTwinSlab_rur[a[i]]+data$compostingTwinNoSlab_rur[a[i]]+data$compostingToilet_rur[a[i]])*(data$fecalSludgeTreated_rur[a[i]]+factor_to_be_added))-((data$population[a[i]]*data$fraction_urban_pop[a[i]]*(data$flushSeptic_urb[a[i]]+data$flushPit_urb[a[i]]+data$flushOpen_urb[a[i]]+data$flushUnknown_urb[a[i]]+data$pitSlab_urb[a[i]]+data$pitNoSlab_urb[a[i]]+data$bucketLatrine_urb[a[i]]+data$containerBased_urb[a[i]]+data$compostingTwinSlab_urb[a[i]]+data$compostingTwinNoSlab_urb[a[i]]+data$compostingToilet_urb[a[i]])*1)+data$population[a[i]]*(1-data$fraction_urban_pop[a[i]])*(data$flushSeptic_rur[a[i]]+data$flushPit_rur[a[i]]+data$flushOpen_rur[a[i]]+data$flushUnknown_rur[a[i]]+data$pitSlab_rur[a[i]]+data$pitNoSlab_rur[a[i]]+data$bucketLatrine_rur[a[i]]+data$containerBased_rur[a[i]]+data$compostingTwinSlab_rur[a[i]]+data$compostingTwinNoSlab_rur[a[i]]+data$compostingToilet_rur[a[i]])*(data$fecalSludgeTreated_rur[a[i]]+factor_to_be_added)))/(data$population[a[i]]*(1-data$fraction_urban_pop[a[i]])*(data$flushSeptic_rur[a[i]]+data$flushPit_rur[a[i]]+data$flushOpen_rur[a[i]]+data$flushUnknown_rur[a[i]]+data$pitSlab_rur[a[i]]+data$pitNoSlab_rur[a[i]]+data$bucketLatrine_rur[a[i]]+data$containerBased_rur[a[i]]+data$compostingTwinSlab_rur[a[i]]+data$compostingTwinNoSlab_rur[a[i]]+data$compostingToilet_rur[a[i]]))
      if(data$f_ftr_new_rur[a[i]]>1){
        data$f_ftr_new_rur[a[i]]<-1
      }
    }else{
      data$f_ftr_new_urb[a[i]]<-data$fecalSludgeTreated_urb[a[i]]+factor_to_be_added+((data$population[a[i]]*data$fraction_urban_pop[a[i]]*(data$flushSeptic_urb[a[i]]+data$flushPit_urb[a[i]]+data$flushOpen_urb[a[i]]+data$flushUnknown_urb[a[i]]+data$pitSlab_urb[a[i]]+data$pitNoSlab_urb[a[i]]+data$bucketLatrine_urb[a[i]]+data$containerBased_urb[a[i]]+data$compostingTwinSlab_urb[a[i]]+data$compostingTwinNoSlab_urb[a[i]]+data$compostingToilet_urb[a[i]])*(data$fecalSludgeTreated_urb[a[i]]+factor_to_be_added)+data$population[a[i]]*(1-data$fraction_urban_pop[a[i]])*(data$flushSeptic_rur[a[i]]+data$flushPit_rur[a[i]]+data$flushOpen_rur[a[i]]+data$flushUnknown_rur[a[i]]+data$pitSlab_rur[a[i]]+data$pitNoSlab_rur[a[i]]+data$bucketLatrine_rur[a[i]]+data$containerBased_rur[a[i]]+data$compostingTwinSlab_rur[a[i]]+data$compostingTwinNoSlab_rur[a[i]]+data$compostingToilet_rur[a[i]])*(data$fecalSludgeTreated_rur[a[i]]+factor_to_be_added))-((data$population[a[i]]*(1-data$fraction_urban_pop[a[i]])*(data$flushSeptic_rur[a[i]]+data$flushPit_rur[a[i]]+data$flushOpen_rur[a[i]]+data$flushUnknown_rur[a[i]]+data$pitSlab_rur[a[i]]+data$pitNoSlab_rur[a[i]]+data$bucketLatrine_rur[a[i]]+data$containerBased_rur[a[i]]+data$compostingTwinSlab_rur[a[i]]+data$compostingTwinNoSlab_rur[a[i]]+data$compostingToilet_rur[a[i]])*1)+data$population[a[i]]*data$fraction_urban_pop[a[i]]*(data$flushSeptic_urb[a[i]]+data$flushPit_urb[a[i]]+data$flushOpen_urb[a[i]]+data$flushUnknown_urb[a[i]]+data$pitSlab_urb[a[i]]+data$pitNoSlab_urb[a[i]]+data$bucketLatrine_urb[a[i]]+data$containerBased_urb[a[i]]+data$compostingTwinSlab_urb[a[i]]+data$compostingTwinNoSlab_urb[a[i]]+data$compostingToilet_urb[a[i]])*(data$fecalSludgeTreated_urb[a[i]]+factor_to_be_added)))/(data$population[a[i]]*data$fraction_urban_pop[a[i]]*(data$flushSeptic_urb[a[i]]+data$flushPit_urb[a[i]]+data$flushOpen_urb[a[i]]+data$flushUnknown_urb[a[i]]+data$pitSlab_urb[a[i]]+data$pitNoSlab_urb[a[i]]+data$bucketLatrine_urb[a[i]]+data$containerBased_urb[a[i]]+data$compostingTwinSlab_urb[a[i]]+data$compostingTwinNoSlab_urb[a[i]]+data$compostingToilet_urb[a[i]]))
      data$f_ftr_new_rur[a[i]]<-1
      if(data$f_ftr_new_urb[a[i]]>1){
        data$f_ftr_new_urb[a[i]]<-1
      }
    }

    data$new_pop_tr[a[i]]<-(data$population[a[i]]*data$fraction_urban_pop[a[i]]*(data$flushSeptic_urb[a[i]]+data$flushPit_urb[a[i]]+data$flushOpen_urb[a[i]]+data$flushUnknown_urb[a[i]]+data$pitSlab_urb[a[i]]+data$pitNoSlab_urb[a[i]]+data$bucketLatrine_urb[a[i]]+data$containerBased_urb[a[i]]+data$compostingTwinSlab_urb[a[i]]+data$compostingTwinNoSlab_urb[a[i]]+data$compostingToilet_urb[a[i]])*data$f_ftr_new_urb[a[i]]+data$population[a[i]]*(1-data$fraction_urban_pop[a[i]])*(data$flushSeptic_rur[a[i]]+data$flushPit_rur[a[i]]+data$flushOpen_rur[a[i]]+data$flushUnknown_rur[a[i]]+data$pitSlab_rur[a[i]]+data$pitNoSlab_rur[a[i]]+data$bucketLatrine_rur[a[i]]+data$containerBased_rur[a[i]]+data$compostingTwinSlab_rur[a[i]]+data$compostingTwinNoSlab_rur[a[i]]+data$compostingToilet_rur[a[i]])*data$f_ftr_new_rur[a[i]])-(data$population[a[i]]*data$fraction_urban_pop[a[i]]*(data$flushSeptic_urb[a[i]]+data$flushPit_urb[a[i]]+data$flushOpen_urb[a[i]]+data$flushUnknown_urb[a[i]]+data$pitSlab_urb[a[i]]+data$pitNoSlab_urb[a[i]]+data$bucketLatrine_urb[a[i]]+data$containerBased_urb[a[i]]+data$compostingTwinSlab_urb[a[i]]+data$compostingTwinNoSlab_urb[a[i]]+data$compostingToilet_urb[a[i]])*data$fecalSludgeTreated_urb[a[i]]+data$population[a[i]]*(1-data$fraction_urban_pop[a[i]])*(data$flushSeptic_rur[a[i]]+data$flushPit_rur[a[i]]+data$flushOpen_rur[a[i]]+data$flushUnknown_rur[a[i]]+data$pitSlab_rur[a[i]]+data$pitNoSlab_rur[a[i]]+data$bucketLatrine_rur[a[i]]+data$containerBased_rur[a[i]]+data$compostingTwinSlab_rur[a[i]]+data$compostingTwinNoSlab_rur[a[i]]+data$compostingToilet_rur[a[i]])*data$fecalSludgeTreated_rur[a[i]])
  }

  pop_already<-sum(data$pop_tr)+sum(data$new_pop_tr)
  pop_still<-goal_pop_tr-pop_already
  r<-which(data$f_ftr_new_urb==0)
  factor_to_be_added<-pop_still/sum(data$pop_tot[r])

  print(factor_to_be_added)

  a<-which((data$fecalSludgeTreated_urb+factor_to_be_added>1 | data$fecalSludgeTreated_rur+factor_to_be_added>1) & data$f_ftr_new_urb==0)

  if(length(a)==0){
    b<-which(data$f_ftr_new_urb==0)
    if(length(b)>0){
      for(i in 1:length(b)){
        if(data$fecalSludgeTreated_urb[b[i]]+factor_to_be_added>1 && data$fecalSludgeTreated_rur[b[i]]+factor_to_be_added<1){
          data$f_ftr_new_urb[b[i]]<-1
          data$f_ftr_new_rur[b[i]]<-data$fecalSludgeTreated_rur[b[i]]+factor_to_be_added+((data$population[b[i]]*data$fraction_urban_pop[b[i]]*(data$flushSeptic_urb[b[i]]+data$flushPit_urb[b[i]]+data$flushOpen_urb[b[i]]+data$flushUnknown_urb[b[i]]+data$pitSlab_urb[b[i]]+data$pitNoSlab_urb[b[i]]+data$bucketLatrine_urb[b[i]]+data$containerBased_urb[b[i]]+data$compostingTwinSlab_urb[b[i]]+data$compostingTwinNoSlab_urb[b[i]]+data$compostingToilet_urb[b[i]])*(data$fecalSludgeTreated_urb[b[i]]+factor_to_be_added)+data$population[b[i]]*(1-data$fraction_urban_pop[b[i]])*(data$flushSeptic_rur[b[i]]+data$flushPit_rur[b[i]]+data$flushOpen_rur[b[i]]+data$flushUnknown_rur[b[i]]+data$pitSlab_rur[b[i]]+data$pitNoSlab_rur[b[i]]+data$bucketLatrine_rur[b[i]]+data$containerBased_rur[b[i]]+data$compostingTwinSlab_rur[b[i]]+data$compostingTwinNoSlab_rur[b[i]]+data$compostingToilet_rur[b[i]])*(data$flushSeptic_rur[b[i]]+data$flushPit_rur[b[i]]+data$flushOpen_rur[b[i]]+data$flushUnknown_rur[b[i]]+data$pitSlab_rur[b[i]]+data$pitNoSlab_rur[b[i]]+data$bucketLatrine_rur[b[i]]+data$containerBased_rur[b[i]]+data$compostingTwinSlab_rur[b[i]]+data$compostingTwinNoSlab_rur[b[i]]+data$compostingToilet_rur[b[i]])*(data$fecalSludgeTreated_rur[b[i]]+factor_to_be_added))-((data$population[b[i]]*data$fraction_urban_pop[b[i]]*(data$flushSeptic_urb[b[i]]+data$flushPit_urb[b[i]]+data$flushOpen_urb[b[i]]+data$flushUnknown_urb[b[i]]+data$pitSlab_urb[b[i]]+data$pitNoSlab_urb[b[i]]+data$bucketLatrine_urb[b[i]]+data$containerBased_urb[b[i]]+data$compostingTwinSlab_urb[b[i]]+data$compostingTwinNoSlab_urb[b[i]]+data$compostingToilet_urb[b[i]])*1)+data$population[b[i]]*(1-data$fraction_urban_pop[b[i]])*(data$flushSeptic_rur[b[i]]+data$flushPit_rur[b[i]]+data$flushOpen_rur[b[i]]+data$flushUnknown_rur[b[i]]+data$pitSlab_rur[b[i]]+data$pitNoSlab_rur[b[i]]+data$bucketLatrine_rur[b[i]]+data$containerBased_rur[b[i]]+data$compostingTwinSlab_rur[b[i]]+data$compostingTwinNoSlab_rur[b[i]]+data$compostingToilet_rur[b[i]])*(data$fecalSludgeTreated_rur[b[i]]+factor_to_be_added)))/(data$population[b[i]]*(1-data$fraction_urban_pop[b[i]])*(data$flushSeptic_rur[b[i]]+data$flushPit_rur[b[i]]+data$flushOpen_rur[b[i]]+data$flushUnknown_rur[b[i]]+data$pitSlab_rur[b[i]]+data$pitNoSlab_rur[b[i]]+data$bucketLatrine_rur[b[i]]+data$containerBased_rur[b[i]]+data$compostingTwinSlab_rur[b[i]]+data$compostingTwinNoSlab_rur[b[i]]+data$compostingToilet_rur[b[i]]))
        }else if (data$fecalSludgeTreated_rur[b[i]]+factor_to_be_added>1 && data$fecalSludgeTreated_urb[b[i]]+factor_to_be_added<1){
          data$f_ftr_new_urb[b[i]]<-data$fecalSludgeTreated_urb[b[i]]+factor_to_be_added+((data$population[b[i]]*data$fraction_urban_pop[b[i]]*(data$flushSeptic_urb[b[i]]+data$flushPit_urb[b[i]]+data$flushOpen_urb[b[i]]+data$flushUnknown_urb[b[i]]+data$pitSlab_urb[b[i]]+data$pitNoSlab_urb[b[i]]+data$bucketLatrine_urb[b[i]]+data$containerBased_urb[b[i]]+data$compostingTwinSlab_urb[b[i]]+data$compostingTwinNoSlab_urb[b[i]]+data$compostingToilet_urb[b[i]])*(data$fecalSludgeTreated_urb[b[i]]+factor_to_be_added)+data$population[b[i]]*(1-data$fraction_urban_pop[b[i]])*(data$flushSeptic_rur[b[i]]+data$flushPit_rur[b[i]]+data$flushOpen_rur[b[i]]+data$flushUnknown_rur[b[i]]+data$pitSlab_rur[b[i]]+data$pitNoSlab_rur[b[i]]+data$bucketLatrine_rur[b[i]]+data$containerBased_rur[b[i]]+data$compostingTwinSlab_rur[b[i]]+data$compostingTwinNoSlab_rur[b[i]]+data$compostingToilet_rur[b[i]])*(data$fecalSludgeTreated_rur[b[i]]+factor_to_be_added))-((data$population[b[i]]*(1-data$fraction_urban_pop[b[i]])*(data$flushSeptic_rur[b[i]]+data$flushPit_rur[b[i]]+data$flushOpen_rur[b[i]]+data$flushUnknown_rur[b[i]]+data$pitSlab_rur[b[i]]+data$pitNoSlab_rur[b[i]]+data$bucketLatrine_rur[b[i]]+data$containerBased_rur[b[i]]+data$compostingTwinSlab_rur[b[i]]+data$compostingTwinNoSlab_rur[b[i]]+data$compostingToilet_rur[b[i]])*1)+data$population[b[i]]*data$fraction_urban_pop[b[i]]*(data$flushSeptic_urb[b[i]]+data$flushPit_urb[b[i]]+data$flushOpen_urb[b[i]]+data$flushUnknown_urb[b[i]]+data$pitSlab_urb[b[i]]+data$pitNoSlab_urb[b[i]]+data$bucketLatrine_urb[b[i]]+data$containerBased_urb[b[i]]+data$compostingTwinSlab_urb[b[i]]+data$compostingTwinNoSlab_urb[b[i]]+data$compostingToilet_urb[b[i]])*(data$fecalSludgeTreated_urb[b[i]]+factor_to_be_added)))/(data$population[b[i]]*data$fraction_urban_pop[b[i]]*(data$flushSeptic_urb[b[i]]+data$flushPit_urb[b[i]]+data$flushOpen_urb[b[i]]+data$flushUnknown_urb[b[i]]+data$pitSlab_urb[b[i]]+data$pitNoSlab_urb[b[i]]+data$bucketLatrine_urb[b[i]]+data$containerBased_urb[b[i]]+data$compostingTwinSlab_urb[b[i]]+data$compostingTwinNoSlab_urb[b[i]]+data$compostingToilet_urb[b[i]]))
          data$f_ftr_new_rur[b[i]]<-1
        }else{
          data$f_ftr_new_urb[b[i]]<-data$fecalSludgeTreated_urb[b[i]]+factor_to_be_added
          data$f_ftr_new_rur[b[i]]<-data$fecalSludgeTreated_rur[b[i]]+factor_to_be_added
        }
        data$new_pop_tr[b[i]]<-(data$population[b[i]]*data$fraction_urban_pop[b[i]]*(data$flushSeptic_urb[b[i]]+data$flushPit_urb[b[i]]+data$flushOpen_urb[b[i]]+data$flushUnknown_urb[b[i]]+data$pitSlab_urb[b[i]]+data$pitNoSlab_urb[b[i]]+data$bucketLatrine_urb[b[i]]+data$containerBased_urb[b[i]]+data$compostingTwinSlab_urb[b[i]]+data$compostingTwinNoSlab_urb[b[i]]+data$compostingToilet_urb[b[i]])*data$f_ftr_new_urb[b[i]]+data$population[b[i]]*(1-data$fraction_urban_pop[b[i]])*(data$flushSeptic_rur[b[i]]+data$flushPit_rur[b[i]]+data$flushOpen_rur[b[i]]+data$flushUnknown_rur[b[i]]+data$pitSlab_rur[b[i]]+data$pitNoSlab_rur[b[i]]+data$bucketLatrine_rur[b[i]]+data$containerBased_rur[b[i]]+data$compostingTwinSlab_rur[b[i]]+data$compostingTwinNoSlab_rur[b[i]]+data$compostingToilet_rur[b[i]])*data$f_ftr_new_rur[b[i]])-(data$population[b[i]]*data$fraction_urban_pop[b[i]]*(data$flushSeptic_urb[b[i]]+data$flushPit_urb[b[i]]+data$flushOpen_urb[b[i]]+data$flushUnknown_urb[b[i]]+data$pitSlab_urb[b[i]]+data$pitNoSlab_urb[b[i]]+data$bucketLatrine_urb[b[i]]+data$containerBased_urb[b[i]]+data$compostingTwinSlab_urb[b[i]]+data$compostingTwinNoSlab_urb[b[i]]+data$compostingToilet_urb[b[i]])*data$fecalSludgeTreated_urb[b[i]]+data$population[b[i]]*(1-data$fraction_urban_pop[b[i]])*(data$flushSeptic_rur[b[i]]+data$flushPit_rur[b[i]]+data$flushOpen_rur[b[i]]+data$flushUnknown_rur[b[i]]+data$pitSlab_rur[b[i]]+data$pitNoSlab_rur[b[i]]+data$bucketLatrine_rur[b[i]]+data$containerBased_rur[b[i]]+data$compostingTwinSlab_rur[b[i]]+data$compostingTwinNoSlab_rur[b[i]]+data$compostingToilet_rur[b[i]])*data$fecalSludgeTreated_rur[b[i]])
      }
    }
  }

}

if(length(a)==0 & sum(data$f_ftr_new_urb)==0 & sum(data$f_ftr_new_rur)==0){
  data$f_ftr_new_urb<-data$fecalSludgeTreated_urb+factor_to_be_added
  data$f_ftr_new_rur<-data$fecalSludgeTreated_rur+factor_to_be_added
}

f_ftr_new<-(sum(data$pop_tr)+sum(data$new_pop_tr))/sum(data$pop_tot)

#output are the new fractions faecalSludgeTreated: data$f_ftr_new_urb and data$f_ftr_new_rur

