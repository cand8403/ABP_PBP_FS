library(MortalityLaws)
require(arm)
require(mgcv)
library(ggplot2)
library(dplyr)
library(Cairo)

load("code_analysis/analysis_output/min_data_males_4000.rdata")
head(min_data_males)

load("code_analysis/analysis_output/min_data_females_4000.rdata")
head(min_data_females)

########################################

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

########################################


########### total energy ###########


min_data_males$TE.kcal<-apply(apply(min_data_males[,3:6],1,unlist),2,sum)

min_data_females$TE.kcal<-apply(apply(min_data_females[,3:6],1,unlist),2,sum)


mean(min_data_males$TE.kcal)
mean(min_data_females$TE.kcal)

min_data_males$"animal.ofTE"<-apply(apply(min_data_males[,3:7],1,unlist),2,function(x) (x[1:4]/x[5])*100)[1,]
min_data_males$"nonanimal.ofTE"<-apply(apply(min_data_males[,3:7],1,unlist),2,function(x) (x[1:4]/x[5])*100)[2,]

min_data_males$"carb.ofTE"<-apply(apply(min_data_males[,3:7],1,unlist),2,function(x) (x[1:4]/x[5])*100)[3,]
min_data_males$"fat.ofTE"<-apply(apply(min_data_males[,3:7],1,unlist),2,function(x) (x[1:4]/x[5])*100)[4,]

min_data_females$"animal.ofTE"<-apply(apply(min_data_females[,3:7],1,unlist),2,function(x) (x[1:4]/x[5])*100)[1,]
min_data_females$"nonanimal.ofTE"<-apply(apply(min_data_females[,3:7],1,unlist),2,function(x) (x[1:4]/x[5])*100)[2,]

min_data_females$"carb.ofTE"<-apply(apply(min_data_females[,3:7],1,unlist),2,function(x) (x[1:4]/x[5])*100)[3,]
min_data_females$"fat.ofTE"<-apply(apply(min_data_females[,3:7],1,unlist),2,function(x) (x[1:4]/x[5])*100)[4,]


min_data_males[,2]<-as.numeric(unlist(min_data_males[,2]))
min_data_males[,3]<-as.numeric(unlist(min_data_males[,3]))
min_data_males[,4]<-as.numeric(unlist(min_data_males[,4]))
min_data_males[,5]<-as.numeric(unlist(min_data_males[,5]))
min_data_males[,6]<-as.numeric(unlist(min_data_males[,6]))
min_data_males[,7]<-as.numeric(unlist(min_data_males[,7]))
min_data_males[,9]<-as.numeric(unlist(min_data_males[,9]))
min_data_males[,10]<-as.numeric(unlist(min_data_males[,10]))
min_data_males[,11]<-as.numeric(unlist(min_data_males[,11]))


min_data_females[,2]<-as.numeric(unlist(min_data_females[,2]))
min_data_females[,3]<-as.numeric(unlist(min_data_females[,3]))
min_data_females[,4]<-as.numeric(unlist(min_data_females[,4]))
min_data_females[,5]<-as.numeric(unlist(min_data_females[,5]))
min_data_females[,6]<-as.numeric(unlist(min_data_females[,6]))
min_data_females[,7]<-as.numeric(unlist(min_data_females[,7]))
min_data_females[,9]<-as.numeric(unlist(min_data_females[,9]))
min_data_females[,10]<-as.numeric(unlist(min_data_females[,10]))
min_data_females[,11]<-as.numeric(unlist(min_data_females[,11]))

##################################################################

# Use sex and age specific total energy to find compositions ( early and late life environments @ ages 0 and 75)
################################################################

# Total Energy (for min qx in males) @ age 0
te.early.m<-signif(min_data_males$TE.kcal[1],3)
# Total Energy (for min qx in females) @ age 0
te.early.f<-signif(min_data_females$TE.kcal[1],3)
# Total Energy (for min qx in males) @ age 75
te.late.m<-signif(min_data_males$TE.kcal[17],3)
# Total Energy (for min qx in females) @ age 75
te.late.f<-signif(min_data_females$TE.kcal[17],3)


cols<-c("animal.ofTE","nonanimal.ofTE","carb.ofTE","fat.ofTE")
# percent macros for min qx @ age 0 in males
pc.early.m<-min_data_males[1,cols]
# diet composition for min qx @ age 0 in males
comp.early.m<-(pc.early.m/100)*te.early.m
# 



# percent macros for min qx @ age 0 in females
pc.early.f<-min_data_females[1,cols]
comp.early.f<-(pc.early.f/100)*te.early.f


# percent macros for min qx @ age 75 in males
pc.late.m<-min_data_males[17,cols]
comp.late.m<-(pc.late.m/100)*te.late.m
# 


# percent macros for min qx @ age 75 in females
pc.late.f<-min_data_females[17,cols]
comp.late.f<-(pc.late.f/100)*te.late.f


comp.names<-c("abp.kcal",  "pbp.kcal"  ,"carb.kcal", "fat.kcal")
names(comp.early.m)<-comp.names
names(comp.early.f)<-comp.names

names(comp.late.m)<-comp.names
names(comp.late.f)<-comp.names


########## EARLY COMPOSITION ##########

par(mfrow=c(2,2))
year.plot=2017

dataset.plot<-dataset[['5f']][which(dataset[['5f']][,"Year"]==year.plot),]
med.GDP<-round(median(dataset.plot$GDP, na.rm=T))

predictors.early.m<-cbind(data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Males"), Age=5),comp.early.m)
GAM<-GAM.5m
predictions.early.m<-predict(GAM, newdata=predictors.early.m, type="response", exclude='s(Country)', newdata.guaranteed=T)

predictors.early.m<-cbind(data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Males"), Age=60),comp.early.m)
GAM<-GAM.60m
predictions.early.m<-data.frame(cbind(predictions.early.m,predict(GAM, newdata=predictors.early.m, type="response", exclude='s(Country)', newdata.guaranteed=T)))

names(predictions.early.m)<-c("l5","l60")
predictions.early.m<-cbind(predictors.early.m[-which(names(predictors.early.m)=="Age")], predictions.early.m)

convert1<-c("Males","Females")
sex_used<-match(predictions.early.m$Sex[1],convert1)

# Get the jth lifetable

tag<-which(standard$Age == 5)
ls5<-standard[tag, grep("lx", names(standard))[sex_used]]
tag<-which(standard$Age == 60)
ls60<-standard[tag, grep("lx", names(standard))[sex_used]]
lx5<-predictions.early.m$l5
lx60<-predictions.early.m$l60
alpha<-((logit(lx5)*logit(ls60))-(logit(ls5)*logit(lx60)))/(logit(ls60)-logit(ls5))
beta<-(logit(lx60)-logit(lx5))/(logit(ls60)-logit(ls5))
l.x.<-invlogit(alpha + beta * logit(standard[, grep("lx", names(standard))[sex_used]]) + standard[, grep("gamma", names(standard))[sex_used]] * (1 - (logit(lx5)/logit(ls5))) + standard[, grep("theta", names(standard))[sex_used]] * (1 - (logit(lx60)/logit(ls60))))

# Get the full lifetable
convert2<-c("male", "female")
LT<-LifeTable(x=standard$Age, lx=l.x., sex=convert2[sex_used])$lt
LT.early.males<-LT
# What are we looking for
target.early.m<-LT.early.males[, c('x','qx')]
# plot(target.early.m$x,target.early.m$qx)

############# Females

predictors.early.f<-cbind(data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Females"), Age=5),comp.early.f)
GAM<-GAM.5f
predictions.early.f<-predict(GAM, newdata=predictors.early.f, type="response", exclude='s(Country)', newdata.guaranteed=T)

predictors.early.f<-cbind(data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Females"), Age=60),comp.early.f)
GAM<-GAM.60f
predictions.early.f<-data.frame(cbind(predictions.early.f,predict(GAM, newdata=predictors.early.f, type="response", exclude='s(Country)', newdata.guaranteed=T)))

names(predictions.early.f)<-c("l5","l60")
predictions.early.f<-cbind(predictors.early.f[-which(names(predictors.early.f)=="Age")], predictions.early.f)

convert1<-c("Males","Females")
sex_used<-match(predictions.early.f$Sex[1],convert1)

# Get the jth lifetable

tag<-which(standard$Age == 5)
ls5<-standard[tag, grep("lx", names(standard))[sex_used]]
tag<-which(standard$Age == 60)
ls60<-standard[tag, grep("lx", names(standard))[sex_used]]
lx5<-predictions.early.f$l5
lx60<-predictions.early.f$l60
alpha<-((logit(lx5)*logit(ls60))-(logit(ls5)*logit(lx60)))/(logit(ls60)-logit(ls5))
beta<-(logit(lx60)-logit(lx5))/(logit(ls60)-logit(ls5))
l.x.<-invlogit(alpha + beta * logit(standard[, grep("lx", names(standard))[sex_used]]) + standard[, grep("gamma", names(standard))[sex_used]] * (1 - (logit(lx5)/logit(ls5))) + standard[, grep("theta", names(standard))[sex_used]] * (1 - (logit(lx60)/logit(ls60))))

# Get the full lifetable
convert2<-c("male", "female")
LT<-LifeTable(x=standard$Age, lx=l.x., sex=convert2[sex_used])$lt
LT.early.females<-LT
# What are we looking for
target.early.f<-LT.early.females[, c('x','qx')]
# plot(target.early.f$x,target.early.f$qx)



########## LATE COMPOSITION ##########


predictors.late.m<-cbind(data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Males"), Age=5),comp.late.m)
GAM<-GAM.5m
predictions.late.m<-predict(GAM, newdata=predictors.late.m, type="response", exclude='s(Country)', newdata.guaranteed=T)

predictors.late.m<-cbind(data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Males"), Age=60),comp.late.m)
GAM<-GAM.60m
predictions.late.m<-data.frame(cbind(predictions.late.m,predict(GAM, newdata=predictors.late.m, type="response", exclude='s(Country)', newdata.guaranteed=T)))

names(predictions.late.m)<-c("l5","l60")
predictions.late.m<-cbind(predictors.late.m, predictions.late.m)

convert1<-c("Males","Females")
sex_used<-match(predictions.late.m$Sex[1],convert1)

# Get the jth lifetable

tag<-which(standard$Age == 5)
ls5<-standard[tag, grep("lx", names(standard))[sex_used]]
tag<-which(standard$Age == 60)
ls60<-standard[tag, grep("lx", names(standard))[sex_used]]
lx5<-predictions.late.m$l5
lx60<-predictions.late.m$l60
alpha<-((logit(lx5)*logit(ls60))-(logit(ls5)*logit(lx60)))/(logit(ls60)-logit(ls5))
beta<-(logit(lx60)-logit(lx5))/(logit(ls60)-logit(ls5))
l.x.<-invlogit(alpha + beta * logit(standard[, grep("lx", names(standard))[sex_used]]) + standard[, grep("gamma", names(standard))[sex_used]] * (1 - (logit(lx5)/logit(ls5))) + standard[, grep("theta", names(standard))[sex_used]] * (1 - (logit(lx60)/logit(ls60))))

# Get the full lifetable
convert2<-c("male", "female")
LT<-LifeTable(x=standard$Age, lx=l.x., sex=convert2[sex_used])$lt
LT.late.males<-LT
# What are we looking for
target.late.m<-LT.late.males[, c('x','qx')]
# plot(target.late.m$x,target.late.m$qx)

############# Females ############# 

predictors.late.f<-cbind(data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Females"), Age=5),comp.late.f)
GAM<-GAM.5f
predictions.late.f<-predict(GAM, newdata=predictors.late.f, type="response", exclude='s(Country)', newdata.guaranteed=T)

predictors.late.f<-cbind(data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Females"), Age=60),comp.late.f)
GAM<-GAM.60f
predictions.late.f<-data.frame(cbind(predictions.late.f,predict(GAM, newdata=predictors.late.f, type="response", exclude='s(Country)', newdata.guaranteed=T)))

names(predictions.late.f)<-c("l5","l60")
predictions.late.f<-cbind(predictors.late.f, predictions.late.f)

convert1<-c("Males","Females")
sex_used<-match(predictions.late.f$Sex[1],convert1)

# Get the jth lifetable

tag<-which(standard$Age == 5)
ls5<-standard[tag, grep("lx", names(standard))[sex_used]]
tag<-which(standard$Age == 60)
ls60<-standard[tag, grep("lx", names(standard))[sex_used]]
lx5<-predictions.late.f$l5
lx60<-predictions.late.f$l60
alpha<-((logit(lx5)*logit(ls60))-(logit(ls5)*logit(lx60)))/(logit(ls60)-logit(ls5))
beta<-(logit(lx60)-logit(lx5))/(logit(ls60)-logit(ls5))
l.x.<-invlogit(alpha + beta * logit(standard[, grep("lx", names(standard))[sex_used]]) + standard[, grep("gamma", names(standard))[sex_used]] * (1 - (logit(lx5)/logit(ls5))) + standard[, grep("theta", names(standard))[sex_used]] * (1 - (logit(lx60)/logit(ls60))))

# Get the full lifetable
convert2<-c("male", "female")
LT<-LifeTable(x=standard$Age, lx=l.x., sex=convert2[sex_used])$lt
LT.late.females<-LT
# What are we looking for
target.late.f<-LT.late.females[, c('x','qx')]
# plot(target.late.f$x,target.late.f$qx)


########################

target.early.m$comp<-"early"
target.late.m$comp<-"late"
target.ratio.m<-target.early.m$qx/target.late.m$qx
target.ratio.f<-target.early.f$qx/target.late.f$qx
data.ratio.m<-data.frame(age<-target.early.m$x,ratio=target.ratio.m,sex="Males")
names(data.ratio.m)<-c("Age","qxearly_qxlate","Sex")
data.ratio.f<-data.frame(age<-target.early.f$x,ratio=target.ratio.f,sex="Females")
names(data.ratio.f)<-c("Age","qxearly_qxlate","Sex")

data.ratio<-rbind(data.ratio.m[1:21,],data.ratio.f[1:21,])
data.ratio$log<-log(data.ratio$qxearly_qxlate)
target.m<-rbind(target.early.m,target.late.m)
ggplot(target.m,aes(x=x,y=qx,color=comp))+geom_smooth(se=F)

ggplot(data.ratio.m[1:21,],aes(x=Age,y=qxearly_qxlate))+geom_point()
ggplot(data.ratio.f[1:21,],aes(x=Age,y=qxearly_qxlate))+geom_point()
ggplot(data.ratio,aes(x=Age,y=qxearly_qxlate,color=Sex))+geom_point()+geom_smooth(se=F)+theme_bw()

ylab<-expression(paste("Log Hazard Ratio"))
xlab<-expression(paste("Age (years)"))
ymin<-round(min(data.ratio$log),1)
ymax<-round(max(data.ratio$log),1)


# Figure 5 Main
P<-ggplot(data.ratio,aes(x=Age,y=log))+geom_smooth(data=data.ratio,se=F,linewidth=1.5,aes(colour=Sex))+geom_point(aes(color=Sex,stroke=1),alpha=0.5,size=3)+theme_classic()+
  labs(y=ylab)+
  theme(axis.text.x = element_text(size=20,family="Times",face="bold"),axis.text.y = element_text(size=24,family="Times"),axis.title = element_text(size=24,face="bold",family="Times"),legend.position = c(0.95, .05),
        legend.justification = c("right", "bottom"),legend.text=element_text(size=10),legend.title = element_text(size=12,family="Times",face='bold'),legend.key.size = unit(c(1.5,1.5),"cm"))+
  scale_x_continuous(breaks=seq(0,100,5))+geom_hline(yintercept=0,linetype=2)+scale_y_continuous(limits=c(-0.75,0.55),breaks=c(-0.5,0,0.5))+scale_color_manual(name="Sex",values = c("Males" = "#FF7B25", "Females" = "#9331CC"))+theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'),
    legend.box.margin = margin(0.2,0.2,0.2,r=0.05,"cm")
  )
ggsave("C:/Users/andre/OneDrive - The University of Sydney (Students)/phD Material/ASM_Plant vs Animal Protein/Analysis/Figures/Qx_ratio_plots/log_hazard_ratio_min_qx_dec23_rerun_nov24_nolegend.png", P, bg='transparent',height=10,width=10)




# Table S18 
comps<-rbind(comp.early.m,comp.early.f,comp.late.m,comp.late.f)
comps$TotalEnergy.kcal<-apply(comps,1,sum)


