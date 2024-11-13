############################################################
# Clean up 
rm(list=ls())


source("code_analysis/ggSurface.R")
source("code_analysis/inhull.R")

# Read in the standard LT from Wilmoth et al 2012
standard<-read.csv("data_read/wilmoth_standard.csv")



library(mgcv)
library(arm)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(MortalityLaws)
library(Cairo)
library(geometry)
library(metR)

data.5m<-read.csv("data_cleaned/5mdata.csv", stringsAsFactors=T, header = TRUE)
data.60m<-read.csv("data_cleaned/60mdata.csv", stringsAsFactors=T, header = TRUE)

data.5f<-read.csv("data_cleaned/5fdata.csv", stringsAsFactors=T, header = TRUE)
data.60f<-read.csv("data_cleaned/60fdata.csv", stringsAsFactors=T, header = TRUE)

all.data<-bind_rows(data.5m,data.60m,data.5f,data.60f)

dataset<-list(data.5m,data.60m,data.5f,data.60f)
names(dataset)<-c('5m','60m','5f','60f')

#####################################################
################### Load Models #####################
#####################################################

# load("code_analysis/analysis_output/ASM_doSNOW_allGAMs_results.rdata")
# 
# GAM.5m<-results[[1]][[14]]
# GAM.5f<-results[[3]][[14]]
# GAM.60m<-results[[2]][[14]]
# GAM.60f<-results[[4]][[14]]

#OR

load("code_analysis/analysis_output/GAM_5m.rdata")
load("code_analysis/analysis_output/GAM_5f.rdata")
load("code_analysis/analysis_output/GAM_60m.rdata")
load("code_analysis/analysis_output/GAM_60f.rdata")

# specify year
i=2017
year.plot=i

################################################
############ Male survival @ Age=5 #############
################################################
GAM<-GAM.5m
age.plot<-5
dataset.plot<-dataset[['5m']][which(dataset[['5m']][,"Year"]==2017),]
med.GDP<-round(median(dataset.plot$GDP, na.rm=T))
abp.q1<-quantile(dataset.plot$abp.kcal, c(1/3,  2/3))[1]
med.abp<-round(median(dataset.plot$abp.kcal, na.rm=T))
abp.q3<-quantile(dataset.plot$abp.kcal, c(1/3,  2/3))[2]
med.pbp<-round(median(dataset.plot$pbp.kcal, na.rm=T))
#quantile(dataset.plot$fat.kcal, c(1/3,  2/3))
fat.q1<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[1]
med.fat<-median(dataset.plot$fat.kcal)
fat.q3<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[2]
fat.kcal<-c(fat.q1,med.fat,fat.q3)
predict.val<-data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Males"), Age=age.plot, abp.kcal=med.abp,pbp.kcal=med.pbp,fat.kcal=med.fat)

carb.kcal<-seq(min(all.data$carb.kcal),max(all.data$carb.kcal),1)

predictors<-as.data.frame(cbind(carb.kcal,predict.val))
predictions<-predict(GAM,newdata=predictors, type="response", exclude=c("s(Country)"), newdata.guaranteed=T)

plot.data.5m<-data.frame(carb=carb.kcal,l5=predictions)


################################################
############ Female survival @ Age=5 #############
################################################
GAM<-GAM.5f
age.plot<-5
dataset.plot<-dataset[['5f']][which(dataset[['5f']][,"Year"]==2017),]
med.GDP<-round(median(dataset.plot$GDP, na.rm=T))
abp.q1<-quantile(dataset.plot$abp.kcal, c(1/3,  2/3))[1]
med.abp<-round(median(dataset.plot$abp.kcal, na.rm=T))
abp.q3<-quantile(dataset.plot$abp.kcal, c(1/3,  2/3))[2]
med.pbp<-round(median(dataset.plot$pbp.kcal, na.rm=T))
fat.q1<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[1]
med.fat<-median(dataset.plot$fat.kcal)
fat.q3<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[2]
fat.kcal<-c(fat.q1,med.fat,fat.q3)
predict.val<-data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Females"), Age=age.plot, abp.kcal=med.abp,pbp.kcal=med.pbp,fat.kcal=med.fat)

carb.kcal<-seq(min(all.data$carb.kcal),max(all.data$carb.kcal),1)

predictors<-as.data.frame(cbind(carb.kcal,predict.val))
predictions<-predict(GAM,newdata=predictors, type="response", exclude=c("s(Country)"), newdata.guaranteed=T)

plot.data.5f<-data.frame(carb=carb.kcal,l5=predictions)

# plot.carb.5f.med<-ggplot( plot.data.5f,aes(x=carb,y=l5))+geom_point()+geom_smooth(data=plot.data,method="gam",formula=y~s(x, bs = "cs"))

plot.data.l5<-data.frame(carb=carb.kcal,l5m=plot.data.5m$l5,l5f=plot.data.5f$l5)

# Supplementary Figure S4 (a)
plot.carb.l5.med<-ggplot(plot.data.l5,aes(x=carb)) +geom_smooth(aes(y=l5m),method="gam",formula=y~s(x, bs = "cs"),color="seagreen2",show.legend=TRUE,size=2)+geom_smooth(aes(y=l5f),method="gam",formula=y~s(x, bs = "cs"),color="slateblue2",show.legend=TRUE,size=2)+theme_classic()+labs(x="Carbohydrate Supply (kcal/capita/day)",y="Survivorship at Age 5 (l5)",subtitle="te(Carb,Year,k=5)  & (1/SE)/mean(1/SE)")+theme(legend.position = "top")+ylim(c(0.99,0.99541))

################################################
############ Male survival @ Age=60 #############
################################################
GAM<-GAM.60m
age.plot<-60
dataset.plot<-dataset[['60m']][which(dataset[['60m']][,"Year"]==2017),]
med.GDP<-round(median(dataset.plot$GDP, na.rm=T))
abp.q1<-quantile(dataset.plot$abp.kcal, c(1/3,  2/3))[1]
med.abp<-round(median(dataset.plot$abp.kcal, na.rm=T))
abp.q3<-quantile(dataset.plot$abp.kcal, c(1/3,  2/3))[2]
med.pbp<-round(median(dataset.plot$pbp.kcal, na.rm=T))
fat.q1<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[1]
med.fat<-median(dataset.plot$fat.kcal)
fat.q3<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[2]
fat.kcal<-c(fat.q1,med.fat,fat.q3)
predict.val<-data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Males"), Age=age.plot, abp.kcal=med.abp,pbp.kcal=med.pbp,fat.kcal=med.fat)

carb.kcal<-seq(min(all.data$carb.kcal),max(all.data$carb.kcal),1)

predictors<-as.data.frame(cbind(carb.kcal,predict.val))
predictions<-predict(GAM,newdata=predictors, type="response", exclude=c("s(Country)"), newdata.guaranteed=T)

plot.data.60m<-data.frame(carb=carb.kcal,l60=predictions)

# plot.carb.60m.med<-ggplot( plot.data,aes(x=carb,y=l60))+geom_point()+geom_smooth(data=plot.data,method="gam",formula=y~s(x, bs = "cs"))
# 
################################################
############ Female survival @ Age=60 #############
################################################
GAM<-GAM.60f
age.plot<-60
dataset.plot<-dataset[['60f']][which(dataset[['60f']][,"Year"]==2017),]
med.GDP<-round(median(dataset.plot$GDP, na.rm=T))
abp.q1<-quantile(dataset.plot$abp.kcal, c(1/3,  2/3))[1]
med.abp<-round(median(dataset.plot$abp.kcal, na.rm=T))
abp.q3<-quantile(dataset.plot$abp.kcal, c(1/3,  2/3))[2]
med.pbp<-round(median(dataset.plot$pbp.kcal, na.rm=T))
fat.q1<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[1]
med.fat<-median(dataset.plot$fat.kcal)
fat.q3<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[2]
fat.kcal<-c(fat.q1,med.fat,fat.q3)
predict.val<-data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Females"), Age=age.plot, abp.kcal=med.abp,pbp.kcal=med.pbp,fat.kcal=med.fat)

carb.kcal<-seq(min(all.data$carb.kcal),max(all.data$carb.kcal),1)

predictors<-as.data.frame(cbind(carb.kcal,predict.val))
predictions<-predict(GAM,newdata=predictors, type="response", exclude=c("s(Country)"), newdata.guaranteed=T)

plot.data.60f<-data.frame(carb=carb.kcal,l60=predictions)

max(plot.data.60m$l60)

plot.data.l60<-data.frame(carb=carb.kcal,l60m=plot.data.60m$l60,l60f=plot.data.60f$l60)
max(plot.data.l60[,-1])

min(plot.data.l60[,-1])

# Supplementary Figure S4 (b)
plot.carb.l60.med<-ggplot(plot.data.l60,aes(x=carb))+geom_point(aes(y=l60m),color="seagreen2",show.legend=TRUE,size=2)+geom_point(aes(y=l60f),color="slateblue2",show.legend=TRUE,size=2)+theme_classic()+labs(x="Carbohydrate Supply (kcal/capita/day)",y="Survivorship at Age 60 (l60)")+theme(legend.position = "right")+ylim(c(0.830,0.943))


######################## Life Expectancy at Birth ########################

plot.data.f<-as.data.frame(cbind(plot.data.5f,l60=plot.data.60f$l60,Sex="Females"))
plot.data.m<-as.data.frame(cbind(plot.data.5m,l60=plot.data.60m$l60,Sex="Males"))

predictions<-rbind(plot.data.m,plot.data.f)
glimpse(predictions)
predictions$e0<-NA
d<-dim(predictions)[1]

for(j in 1:d){
  
  # Which sex are we making predictions for
  convert1<-c("Males", "Females")
  sex_used<-match(predictions$Sex, convert1)[j]
  
  # Get the jth lifetable
  tag<-which(standard$Age == 5)
  ls5<-standard[tag, grep("lx", names(standard))[sex_used]]
  tag<-which(standard$Age == 60)
  ls60<-standard[tag, grep("lx", names(standard))[sex_used]]
  lx5<-predictions$l5[j]
  lx60<-predictions$l60[j]
  alpha<-(dot(logit(lx5),logit(ls60),1)-dot(logit(ls5),logit(lx60),1))/(logit(ls60)-logit(ls5))
  beta<-(logit(lx60)-logit(lx5))/(logit(ls60)-logit(ls5))
  
  
  l.x.<-invlogit(alpha + beta * logit(standard[, grep("lx", names(standard))[sex_used]]) + standard[, grep("gamma", names(standard))[sex_used]] * (1 - (logit(lx5)/logit(ls5))) + standard[, grep("theta", names(standard))[sex_used]] * (1 - (logit(lx60)/logit(ls60))))
  
  # Get the full lifetable
  convert2<-c("male", "female")
  LT_j<-LifeTable(x=standard$Age, lx=l.x., sex=convert2[sex_used])$lt
  predictions$e0[j]<-LT_j$ex[which(LT_j$x == 0)]
  
}

min(predictions$e0)
max(predictions$e0)

predictions.f<-predictions[which(predictions$Sex=="Females"),]
predictions.m<-predictions[which(predictions$Sex=="Males"),]
plot.data.e0<-as.data.frame(cbind(carb=predictions.f$carb,e0f=predictions.f$e0,e0m=predictions.m$e0))
glimpse(plot.data.e0)

min(plot.data.e0$e0m,plot.data.e0$e0f)

# Supplementary Figure S5 
plot.carb.e0.med.smooth<-ggplot(plot.data.e0,aes(x=carb))+geom_smooth(aes(y=e0m),color="seagreen2",method="gam",formula=y~s(x, bs = "cs"),show.legend=TRUE,size=2)+geom_smooth(aes(y=e0f),color="slateblue2",method="gam",formula=y~s(x, bs = "cs"),show.legend=TRUE,size=2)+theme_classic()+labs(x="Carbohydrate Supply (kcal/capita/day)",y="Life Expectancy at Birth",subtitle="te(Carb,Year,k=5) (1/SE)/(mean(1/SE)")+theme(legend.position = "right")+ylim(c(65,85))
plot.carb.e0.med.smooth



# Now lets arrange all those plots
CairoPDF(paste0("Carbohydrate_smooth_e0_kcarb5_SE_",year.plot,"_aug23.pdf"), height=10, width=12)

plot.carb.e0.med.smooth

dev.off()

####### calculate e0 and plot using raw data 

match.ID<-match(data.5f$ID,data.60f$ID)
data.f<-data.5f
data.f$l60<-data.60f$l60[match.ID]
data.f$Sex<-"Females"
match.ID<-match(data.5m$ID,data.60m$ID)
data.m<-data.5m
data.m$l60<-data.60m$l60[match.ID]
data.m$Sex<-"Males"
data.mf<-rbind(data.f,data.m)

data.mf$e0<-NA
d<-dim(data.mf)[1]

for(j in 1:d){
  
  # Which sex are we calculalting e0 for
  convert1<-c("Males", "Females")
  sex_used<-match(data.mf$Sex, convert1)[j]
  
  # Get the jth lifetable
  tag<-which(standard$Age == 5)
  ls5<-standard[tag, grep("lx", names(standard))[sex_used]]
  tag<-which(standard$Age == 60)
  ls60<-standard[tag, grep("lx", names(standard))[sex_used]]
  lx5<-data.mf$l5[j]
  lx60<-data.mf$l60[j]
  alpha<-(dot(logit(lx5),logit(ls60),1)-dot(logit(ls5),logit(lx60),1))/(logit(ls60)-logit(ls5))
  beta<-(logit(lx60)-logit(lx5))/(logit(ls60)-logit(ls5))
  
  
  l.x.<-invlogit(alpha + beta * logit(standard[, grep("lx", names(standard))[sex_used]]) + standard[, grep("gamma", names(standard))[sex_used]] * (1 - (logit(lx5)/logit(ls5))) + standard[, grep("theta", names(standard))[sex_used]] * (1 - (logit(lx60)/logit(ls60))))
  
  # Get the full lifetable
  convert2<-c("male", "female")
  LT_j<-LifeTable(x=standard$Age, lx=l.x., sex=convert2[sex_used])$lt
  data.mf$e0[j]<-LT_j$ex[which(LT_j$x == 0)]
  
}


