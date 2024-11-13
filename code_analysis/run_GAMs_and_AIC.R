
library(dplyr)
library(mgcv)
library(doSNOW)
library(arm)
library(ggplot2)


data.5m<-read.csv("data_cleaned/5mdata.csv", stringsAsFactors=T, header = TRUE)
data.60m<-read.csv("data_cleaned/60mdata.csv", stringsAsFactors=T, header = TRUE)

data.5f<-read.csv("data_cleaned/5fdata.csv", stringsAsFactors=T, header = TRUE)
data.60f<-read.csv("data_cleaned/60fdata.csv", stringsAsFactors=T, header = TRUE)

data.5m$Year<-as.numeric(data.5m$Year)
data.5f$Year<-as.numeric(data.5f$Year)
data.60m$Year<-as.numeric(data.60m$Year)
data.60f$Year<-as.numeric(data.60f$Year)

dataset<-list(data.5m,data.60m,data.5f,data.60f)
names(dataset)<-c('5m','60m','5f','60f')

load("code_analysis/GAM_Formulas.rdata")

# Calculate weights from SE and add as dataset col
add_SEsq<-function(dataset){
  weights_SE<-(1/dataset[,"seS"]^2)/mean(1/dataset[,"seS"]^2)
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

# Fit multiple GAMs for all datasets

vars<-c("l5","l60","l5","l60")

# I am going to run over clusters (one per sublist)
cl<-makeCluster(length(formulas), outfile="")

registerDoSNOW(cl)
# run function for each part across multiple cores
results<-foreach(i = 1:length(formulas)) %dopar% {
  library(mgcv)
  
  out<-list()
  
  out[[1]]<-run.aic.weights(vars[1],i,100,data.5m.SEsq,2017,5)
  out[[2]]<-run.aic.weights(vars[2],i,100,data.60m.SEsq,2017,5)
  out[[3]]<-run.aic.weights(vars[3],i,100,data.5f.SEsq,2017,5)
  out[[4]]<-run.aic.weights(vars[4],i,100,data.60f.SEsq,2017,5)
  
  return(out)
  
} 

save(results,file="code_analysis/analysis_output/ASM_doSNOW_allGAMs_results.rdata")


summaries.5m<-lapply(results[[1]],summary)
dev_5m<-unlist(lapply(summaries.5m, "[[", 14)) * 100
AIC_5m<-unlist(lapply(results[[1]], AIC))
delta_5m<-AIC_5m - min(AIC_5m)
weights_5m<-exp(-0.5 * delta_5m) / sum(exp(-0.5 * delta_5m))

sumEDF.5m<-c()

for (i in c(1:length(summaries.5m)))(
  sumEDF.5m[i]<-sum(summaries.5m[[i]]$edf)
)

summaries.5f<-lapply(results[[3]],summary)
dev_5f<-unlist(lapply(summaries.5f, "[[", 14)) * 100
AIC_5f<-unlist(lapply(results[[3]], AIC))
delta_5f<-AIC_5f - min(AIC_5f)
weights_5f<-exp(-0.5 * delta_5f) / sum(exp(-0.5 * delta_5f))

sumEDF.5f<-c()

for (i in c(1:length(summaries.5f)))(
  sumEDF.5f[i]<-sum(summaries.5f[[i]]$edf)
)


summaries.60m<-lapply(results[[2]],summary)
dev_60m<-unlist(lapply(summaries.60m, "[[", 14)) * 100
AIC_60m<-unlist(lapply(results[[2]], AIC))
delta_60m<-AIC_60m - min(AIC_60m)
weights_60m<-exp(-0.5 * delta_60m) / sum(exp(-0.5 * delta_60m))

sumEDF.60m<-c()

for (i in c(1:length(summaries.60m)))(
  sumEDF.60m[i]<-sum(summaries.60m[[i]]$edf)
)

summaries.60f<-lapply(results[[4]],summary)
dev_60f<-unlist(lapply(summaries.60f, "[[", 14)) * 100
AIC_60f<-unlist(lapply(results[[4]], AIC))
delta_60f<-AIC_60f - min(AIC_60f)
weights_60f<-exp(-0.5 * delta_60f) / sum(exp(-0.5 * delta_60f))

sumEDF.60f<-c()

for (i in c(1:length(summaries.60f)))(
  sumEDF.60f[i]<-sum(summaries.60f[[i]]$edf)
)


for(i in 1:length(formula)){
  formula[i]<-paste0("l5",formulas[i])
}


f<-length(formulas)
colnames<-c("Model","AIC","dev","delta","weight","sum(EDF)", "Sex","Formula")
AIC.df.5m<-data.frame(Model=c(1:f),AIC_5m,dev_5m,delta_5m, weights_5m,sumEDF.5m,sex=rep("Males",f),formula=formula)
AIC.df.5f<-data.frame(Model=c(1:f),AIC_5f,dev_5f,delta_5f, weights_5f,sumEDF.5f,sex=rep("Females",f),formula=formula)

for(i in 1:length(formula)){
  formula[i]<-paste0("l60",formulas[i])
}

AIC.df.60m<-data.frame(Model=c(1:f),AIC_60m,dev_60m,delta_60m, weights_60m,sumEDF.60m,sex=rep("Males",f),formula=formula)
AIC.df.60f<-data.frame(Model=c(1:f),AIC_60f,dev_60f,delta_60f, weights_60f,sumEDF.60f,sex=rep("Females",f),formula=formula)

# Tables S13 and S14
write.table(AIC.df.5m, file="code_analysis/analysis_output/AIC_table_l5m.csv", sep=",", row.names=F, col.names=colnames(AIC.df.5m))
write.table(AIC.df.5f, file="code_analysis/analysis_output/AIC_table_l5f.csv", sep=",", row.names=F, col.names=colnames(AIC.df.5f))

write.table(AIC.df.60m, file="code_analysis/analysis_output/AIC_table_l60m.csv", sep=",", row.names=F, col.names=colnames(AIC.df.60m))
write.table(AIC.df.60f, file="code_analysis/analysis_output/AIC_table_l60f.csv", sep=",", row.names=F, col.names=colnames(AIC.df.60f))


AIC.df<-rbind(AIC.df.5m,AIC.df.60m,AIC.df.5f,AIC.df.60f)

write.table(AIC.df, file="code_analysis/analysis_output/ASM_AIC_df.txt", sep="\t", row.names=F)

# Table S16
summary(results[[1]][[14]])
summary(results[[3]][[14]])

# Table S17
summary(results[[2]][[14]])
summary(results[[4]][[14]])

# save GAM to be used in analysis
# GAM.5m<-results[[1]][[]] # specify which gam to save
# save(GAM.5m,file="code_analysis/analysis_output/GAM_5m.rdata")
# GAM.60m<-results[[2]][[]] # specify which gam to save
# save(GAM.60m,file="code_analysis/analysis_output/GAM_60m.rdata")
# GAM.5f<-results[[3]][[]] # specify which gam to save
# save(GAM.5f,file="code_analysis/analysis_output/GAM_5f.rdata")
# GAM.60f<-results[[4]][[]] # specify which gam to save
# save(GAM.60f,file="code_analysis/analysis_output/GAM_60f.rdata")