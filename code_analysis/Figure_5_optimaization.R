
# Load
library(arm)
library(plyr)
library(ggplot2)
library(mgcv)
library(gridExtra)
library(MortalityLaws)
library(Cairo)
library(doSNOW)
library(MASS)
#library(metR)


# Load header
source("code_analysis/ggSurface.R")
source("code_analysis/inhull.R")
source("code_analysis/convertBrass.R")
source("code_analysis/optim_predictions.R")

standard<-read.csv("data_read/wilmoth_standard.csv")

# read in data
data.5m<-read.csv("data_cleaned/5mdata.csv", stringsAsFactors=T, header = TRUE)
data.60m<-read.csv("data_cleaned/60mdata.csv", stringsAsFactors=T, header = TRUE)

data.5f<-read.csv("data_cleaned/5fdata.csv", stringsAsFactors=T, header = TRUE)
data.60f<-read.csv("data_cleaned/60fdata.csv", stringsAsFactors=T, header = TRUE)

# load GAMs

load("code_analysis/analysis_output/ASM_doSNOW_allGAMs_results.rdata")

GAM.5m<-results[[1]][[14]]
GAM.5f<-results[[3]][[14]]
GAM.60m<-results[[2]][[14]]
GAM.60f<-results[[4]][[14]]

#OR

# load("code_analysis/analysis_output/GAM_5m.rdata")
# load("code_analysis/analysis_output/GAM_5f.rdata")
# load("code_analysis/analysis_output/GAM_60m.rdata")
# load("code_analysis/analysis_output/GAM_60f.rdata")

# set year
year.plot<-2017

# subset data to 2017 only
dataset.plot.5m<-data.5m[which(data.5m$Year == 2017),]
dataset.plot.60m<-data.60m[which(data.60m$Year == 2017),]
dataset.plot.5f<-data.5f[which(data.5f$Year == 2017),]
dataset.plot.60f<-data.60f[which(data.60f$Year == 2017),]

# calculate median GDP per capita
med.GDP.5m<-round(median(dataset.plot.5m$GDP, na.rm=T))
med.GDP.60m<-round(median(dataset.plot.60m$GDP, na.rm=T))
med.GDP.5f<-round(median(dataset.plot.5f$GDP, na.rm=T))
med.GDP.60f<-round(median(dataset.plot.60f$GDP, na.rm=T))
med.GDP<-med.GDP.5m

# combine data from males and females into age only subsets
data.l5<-rbind(dataset.plot.5m,dataset.plot.5f)
data.l60<-rbind(dataset.plot.60m,dataset.plot.60f)

# create a list of the data subsets
dataset_plot<-list(data.l5,data.l60)

# create a list of the GAMs for males and females
GAM.m<-list(GAM.5m,GAM.60m)

GAM.f<-list(GAM.5f,GAM.60f)

# create a list of the GAM lists for the optim function 
GAM<-list(GAM.m,GAM.f)


G<-4000 # number of iterations

for (g in 1:G){
  
  g<-as.numeric(g)
  

# Put together in a list
init_list<-list()

# select columns for the initial values 
cols<-c("abp.kcal","pbp.kcal","fat.kcal","carb.kcal")

# tag dataset columns - should be same columns for both datasets
tag<-which(names(data.l5)%in%cols)

init_list[[1]]<-mvrnorm(1, apply(rbind(data.l5[,tag],data.l60[,tag]), 2, mean), cov(rbind(data.l5[,tag],data.l60[,tag])))


sexes<-c("Males","Females")

optim_predictions(g=g,init_list=init_list,sexes=sexes)

}

#################################################################
################## Load Prediction Files #######################
#################################################################

file_num<-c(1:4000)

sexes<-c("Males","Females")

ages<-c(0, 1, seq(5, 100, 5))
colnames<-c("Age","qx","abp.kcal","pbp.kcal","carb.kcal","fat.kcal")
Age_col<-rep(as.numeric(ages),length(file_num))

######## Males #########

predictions.males.min<-data.frame(Age_col,matrix(NA,(22*length(file_num)),5))

predictions.males.max<-data.frame(Age_col,matrix(NA,(22*length(file_num)),5))

names(predictions.males.min)<-colnames
names(predictions.males.max)<-colnames

############# set j=1 for sexes[1] MALES #############
#LifeTable
j=1

for (g in c(1:4000)){
  filename<-sprintf("code_analysis/analysis_output/Predictions_g%s_%s.Rdata",g,sexes[j])
  if(file.exists(filename)) {
    load(sprintf("code_analysis/analysis_output/Predictions_g%s_%s.Rdata",g,sexes[j]))
    for (x in 1:22){
      i<-(22*(g-1))+x
      predictions.males.min$qx[i]<-predictions[[1]][[x]][1]
      predictions.males.min$abp.kcal[i]<-predictions[[1]][[x]][2]
      predictions.males.min$pbp.kcal[i]<-predictions[[1]][[x]][3]
      predictions.males.min$carb.kcal[i]<-predictions[[1]][[x]][4]
      predictions.males.min$fat.kcal[i]<-predictions[[1]][[x]][5]
      predictions.males.max$qx[i]<-predictions[[2]][[x]][1]
      predictions.males.max$abp.kcal[i]<-predictions[[2]][[x]][2]
      predictions.males.max$pbp.kcal[i]<-predictions[[2]][[x]][3]
      predictions.males.max$carb.kcal[i]<-predictions[[2]][[x]][4]
      predictions.males.max$fat.kcal[i]<-predictions[[2]][[x]][5]
    }
  }
  else{
    for (x in 1:22){
      i<-(22*(g-1))+x
      predictions.males.min$qx[i]<-NA
      predictions.males.min$abp.kcal[i]<-NA
      predictions.males.min$pbp.kcal[i]<-NA
      predictions.males.min$carb.kcal[i]<-NA
      predictions.males.min$fat.kcal[i]<-NA
      predictions.males.max$qx[i]<-NA
      predictions.males.max$abp.kcal[i]<-NA
      predictions.males.max$pbp.kcal[i]<-NA
      predictions.males.max$carb.kcal[i]<-NA
      predictions.males.max$fat.kcal[i]<-NA
    }
  }
  
}


#################################################################

######## Females #########

predictions.females.min<-data.frame(Age_col,matrix(NA,(22*length(file_num)),5))

predictions.females.max<-data.frame(Age_col,matrix(NA,(22*length(file_num)),5))

names(predictions.females.min)<-colnames
names(predictions.females.max)<-colnames

j=2

for (g in c(1:4000)){
  filename<-sprintf("code_analysis/analysis_output/Predictions_g%s_%s.Rdata",g,sexes[j])
  if(file.exists(filename)) {
    load(sprintf("code_analysis/analysis_output/Predictions_g%s_%s.Rdata",g,sexes[j]))
    for (x in 1:22){
      i<-(22*(g-1))+x
      predictions.females.min$qx[i]<-predictions[[1]][[x]][1]
      predictions.females.min$abp.kcal[i]<-predictions[[1]][[x]][2]
      predictions.females.min$pbp.kcal[i]<-predictions[[1]][[x]][3]
      predictions.females.min$carb.kcal[i]<-predictions[[1]][[x]][4]
      predictions.females.min$fat.kcal[i]<-predictions[[1]][[x]][5]
      predictions.females.max$qx[i]<-predictions[[2]][[x]][1]
      predictions.females.max$abp.kcal[i]<-predictions[[2]][[x]][2]
      predictions.females.max$pbp.kcal[i]<-predictions[[2]][[x]][3]
      predictions.females.max$carb.kcal[i]<-predictions[[2]][[x]][4]
      predictions.females.max$fat.kcal[i]<-predictions[[2]][[x]][5]
    }
  }
  else{
    for (x in 1:22){
      i<-(22*(g-1))+x
      predictions.females.min$qx[i]<-NA
      predictions.females.min$abp.kcal[i]<-NA
      predictions.females.min$pbp.kcal[i]<-NA
      predictions.females.min$carb.kcal[i]<-NA
      predictions.females.min$fat.kcal[i]<-NA
      predictions.females.max$qx[i]<-NA
      predictions.females.max$abp.kcal[i]<-NA
      predictions.females.max$pbp.kcal[i]<-NA
      predictions.females.max$carb.kcal[i]<-NA
      predictions.females.max$fat.kcal[i]<-NA
    }
  }}

#################################################################
################## Find the Minimal Value #######################
#################################################################

head(predictions.males.min)
head(predictions.females.min)

ages<-c(0, 1, seq(5, 100, 5))
min_data_males<-data.frame(Age=ages)
max_data_males<-min_data_males
min_data_females<-min_data_males
max_data_females<-min_data_males


########### Males - min qx ###########

for (z in 1:length(ages)){
  age_data<-predictions.males.min[which(predictions.males.min$Age==ages[z]),]	
  age_data_min<-age_data[which(unlist(age_data$qx)==min(unlist(age_data$qx))),]
  min_data_males$qx[z]<-age_data_min$qx
  min_data_males$abp.kcal[z]<-age_data_min$abp.kcal
  min_data_males$pbp.kcal[z]<-age_data_min$pbp.kcal
  min_data_males$carb.kcal[z]<-age_data_min$carb.kcal
  min_data_males$fat.kcal[z]<-age_data_min$fat.kcal
}


########### Males - max qx ###########

for (z in 1:length(ages)){
  age_data<-predictions.males.max[which(predictions.males.max$Age==ages[z]),]	
  age_data_max<-age_data[which(unlist(age_data$qx)==min(unlist(age_data$qx))),]
  max_data_males$qx[z]<-age_data_max$qx
  max_data_males$abp.kcal[z]<-age_data_max$abp.kcal
  max_data_males$pbp.kcal[z]<-age_data_max$pbp.kcal
  max_data_males$carb.kcal[z]<-age_data_max$carb.kcal
  max_data_males$fat.kcal[z]<-age_data_max$fat.kcal
}

max_data_males$qx<-unlist(max_data_males$qx)*-1

################### if missing datasets ###########

for (z in 1:length(ages)){
  age_data<-predictions.males.min[which(predictions.males.min$Age==ages[z]),]	
  age_data<-age_data[-which(is.na(age_data$qx)),]
  age_data_min<-age_data[which(unlist(age_data$qx)==min(unlist(age_data$qx))),]
  min_data_males$qx[z]<-age_data_min$qx
  min_data_males$abp.kcal[z]<-age_data_min$abp.kcal
  min_data_males$pbp.kcal[z]<-age_data_min$pbp.kcal
  min_data_males$carb.kcal[z]<-age_data_min$carb.kcal
  min_data_males$fat.kcal[z]<-age_data_min$fat.kcal
}


for (z in 1:length(ages)){
  age_data<-predictions.males.max[which(predictions.males.max$Age==ages[z]),]	
  age_data<-age_data[-which(is.na(age_data$qx)),]
  age_data_max<-age_data[which(unlist(age_data$qx)==min(unlist(age_data$qx))),]
  max_data_males$qx[z]<-age_data_max$qx
  max_data_males$abp.kcal[z]<-age_data_max$abp.kcal
  max_data_males$pbp.kcal[z]<-age_data_max$pbp.kcal
  max_data_males$carb.kcal[z]<-age_data_max$carb.kcal
  max_data_males$fat.kcal[z]<-age_data_max$fat.kcal
}

max_data_males$qx<-unlist(max_data_males$qx)*-1

########### Females - min qx ###########

for (z in 1:length(ages)){
  age_data<-predictions.females.min[which(predictions.females.min$Age==ages[z]),]	
  age_data_min<-age_data[which(unlist(age_data$qx)==min(unlist(age_data$qx))),]
  min_data_females$qx[z]<-age_data_min$qx
  min_data_females$abp.kcal[z]<-age_data_min$abp.kcal
  min_data_females$pbp.kcal[z]<-age_data_min$pbp.kcal
  min_data_females$carb.kcal[z]<-age_data_min$carb.kcal
  min_data_females$fat.kcal[z]<-age_data_min$fat.kcal
}


########### Females - max qx ###########

for (z in 1:length(ages)){
  age_data<-predictions.females.max[which(predictions.females.max$Age==ages[z]),]	
  age_data_max<-age_data[which(unlist(age_data$qx)==min(unlist(age_data$qx))),]
  max_data_females$qx[z]<-age_data_max$qx
  max_data_females$abp.kcal[z]<-age_data_max$abp.kcal
  max_data_females$pbp.kcal[z]<-age_data_max$pbp.kcal
  max_data_females$carb.kcal[z]<-age_data_max$carb.kcal
  max_data_females$fat.kcal[z]<-age_data_max$fat.kcal
}

max_data_females$qx<-unlist(max_data_females$qx)*-1

################### if missing datasets ###########

for (z in 1:length(ages)){
  age_data<-predictions.females.min[which(predictions.females.min$Age==ages[z]),]	
  age_data<-age_data[-which(is.na(age_data$qx)),]
  age_data_min<-age_data[which(unlist(age_data$qx)==min(unlist(age_data$qx))),]
  min_data_females$qx[z]<-age_data_min$qx
  min_data_females$abp.kcal[z]<-age_data_min$abp.kcal
  min_data_females$pbp.kcal[z]<-age_data_min$pbp.kcal
  min_data_females$carb.kcal[z]<-age_data_min$carb.kcal
  min_data_females$fat.kcal[z]<-age_data_min$fat.kcal
}


for (z in 1:length(ages)){
  age_data<-predictions.females.max[which(predictions.females.max$Age==ages[z]),]	
  age_data<-age_data[-which(is.na(age_data$qx)),]
  age_data_max<-age_data[which(unlist(age_data$qx)==max(unlist(age_data$qx))),]
  max_data_females$qx[z]<-age_data_max$qx
  max_data_females$abp.kcal[z]<-age_data_max$abp.kcal
  max_data_females$pbp.kcal[z]<-age_data_max$pbp.kcal
  max_data_females$carb.kcal[z]<-age_data_max$carb.kcal
  max_data_females$fat.kcal[z]<-age_data_max$fat.kcal
}

max_data_females$qx<-unlist(max_data_females$qx)*-1

############ SAVE ##############

save(min_data_males,file="code_analysis/analysis_output/min_data_males_4000.rdata")
save(min_data_females,file="code_analysis/analysis_output/min_data_females_4000.rdata")
