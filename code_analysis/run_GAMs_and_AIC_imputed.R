# Clean up 
rm(list=ls())

library(mgcv)
library(arm)
library(dplyr)
library(ggplot2)
library(doSNOW)


# read in imputed data
data<-read.csv("data_cleaned/imputed_data.csv",header=TRUE)


data$Country<-as.factor(data$Country)
data$Sex<-as.factor(data$Sex)
glimpse(data)

colnames(data)[which(names(data)=="Protein_Total_2901_g")]<-c("protein.g")
colnames(data)[which(names(data)=="Animal_Protein_2941_kcal")]<-c("abp.kcal")
colnames(data)[which(names(data)=="Vegetal_Protein_2903_kcal")]<-c("pbp.kcal")
colnames(data)[which(names(data)=="Fat_Total_2901_kcal")]<-c("fat.kcal")
colnames(data)[which(names(data)=="Carbohydrate_kcal")]<-c("carb.kcal")
colnames(data)[which(names(data)=="GDP_perCapita")]<-c("GDP")
glimpse(data)

tag_na<-which(is.na(data$seS_t)==TRUE)
length(tag_na)
# 0 


age5.data<-data[,-12]
age60.data<-data[,-11]
glimpse(age5.data)
glimpse(age60.data)


age5m.data<-age5.data[(which(age5.data$Sex=="Males")),]
age60m.data<-age60.data[(which(age60.data$Sex=="Males")),]

age5f.data<-age5.data[(which(age5.data$Sex=="Females")),]
age60f.data<-age60.data[(which(age60.data$Sex=="Females")),]

glimpse(age5.data)
glimpse(age60.data)
glimpse(age5m.data)
glimpse(age60m.data)
glimpse(age5f.data)
glimpse(age60f.data)

write.table(age5m.data,"5mdata_imp.csv", sep=",", row.names=F, col.names=names(age5m.data))
write.table(age60m.data,"60mdata_imp.csv", sep=",", row.names=F, col.names=names(age60m.data))
write.table(age5f.data,"5fdata_imp.csv", sep=",", row.names=F, col.names=names(age5f.data))
write.table(age60f.data,"60fdata_imp.csv", sep=",", row.names=F, col.names=names(age60f.data))

load("code_analysis/GAM_Formulas.rdata")


add_SEsq<-function(dataset){
  weights_SE<-(1/dataset[,"seS_t"]^2)/mean(1/dataset[,"seS_t"]^2)
  dataset<-cbind(dataset,weights_SE)
  return(dataset)
}

data.5f.SEsq<-add_SEsq(data.5f)
data.5m.SEsq<-add_SEsq(data.5m)
data.60f.SEsq<-add_SEsq(data.60f)
data.60m.SEsq<-add_SEsq(data.60m)


predict_weights<-function(dataset,year){
  weight.gam.new<-gam(weights_SE~Year+s(Country,bs="re"),data=dataset)
  Countries<-unique(dataset[,"Country"])
  predict.val<-data.frame(Year=rep(2017,length(Countries)),Country=Countries)
  predictions<-predict(weight.gam.new,newdata=predict.val, type="response", newdata.guaranteed=T)
  predicted<-cbind(predict.val,predictions)
  keep<-match(dataset[,"Country"],predicted[,"Country"])
  weight_pred<-predicted[keep,"predictions"]
  dataset<-cbind(dataset,weight_pred)
  wgts.new<-dataset[,"weight_pred"]
  #output<-list(dataset,wgts.new)
  #return(output)
  return(wgts.new)
}


run.aic.weights<-function(var,i,k_nut,dataset,year,k_carb){
  Weights<-predict_weights(dataset,year)
  n<-dim(dataset)[1]
  gamma<-log(n)/2
  GAM<-gam(as.formula(paste0(var,formulas[[i]])),data=dataset,gamma=gamma,family="betar",weights=Weights)
  return(GAM)
}

dataset<-list(data.5m.SEsq,data.60m.SEsq,data.5f.SEsq,data.60f.SEsq)


# Fit single GAM for all datasets

vars<-c("l5","l60","l5","l60")


# I am going to run over clusters (one per sublist)
cl<-makeCluster(length(dataset), outfile="")

registerDoSNOW(cl)
# run function for each part across the eight cores
results<-foreach(j = 1:length(dataset)) %dopar% {
  library(mgcv)
  
  out<-list()
  
  out[[1]]<-run.aic.weights(vars[j],14,100,dataset[[j]],2017,5)
  
  
  return(out)
  
} 


save(results,file="ASM_doSNOW_results_imputed.rdata")

# save separately
GAM.5m<-results[[1]][[1]]
save(GAM.5m,file="code_analysis/analysis_output/GAM_5m_imp.rdata")
GAM.60m<-results[[2]][[1]]
save(GAM.60m,file="code_analysis/analysis_output/GAM_60m_imp.rdata")
GAM.5f<-results[[3]][[1]]
save(GAM.5f,file="code_analysis/analysis_output/GAM_5f_imp.rdata")
GAM.60f<-results[[4]][[1]]
save(GAM.60f,file="code_analysis/analysis_output/GAM_60f_imp.rdata")

# Table S19
summary(GAM.5m)
summary(GAM.5f)

# Table S20
summary(GAM.60m)
summary(GAM.60f)

