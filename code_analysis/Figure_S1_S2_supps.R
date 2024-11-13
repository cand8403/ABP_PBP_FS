############################################################
# Clean up 
rm(list=ls())

# read in imputed data
data<-read.csv("data_cleaned/imputed_data.csv",header=TRUE)


data.5m<-read.csv("5mdata_imp.csv", stringsAsFactors=T)
data.60m<-read.csv("60mdata_imp.csv", stringsAsFactors=T)

data.5f<-read.csv("5fdata_imp.csv", stringsAsFactors=T)
data.60f<-read.csv("60fdata_imp.csv", stringsAsFactors=T)

dataset<-list(data.5m,data.60m,data.5f,data.60f)
names(dataset)<-c('5m','60m','5f','60f')


load("code_analysis/analysis_output/GAM_5m_imp.rdata")
load("code_analysis/analysis_output/GAM_5f_imp.rdata")
load("code_analysis/analysis_output/GAM_60m_imp.rdata")
load("code_analysis/analysis_output/GAM_60f_imp.rdata")

############################################################
source("code_analysis/ggSurface.R")
source("code_analysis/inhull.R")
library(Cairo)
library(grid)
library(gridExtra)
library(mgcv)
library(ggplot2)

newSurface<-function(dataset.plot=dataset.plot){
  XYZ_list<-list()
  XYZ_list[[1]]<-c("abp.kcal", "pbp.kcal", "fat.kcal")
  XYZ_list[[2]]<-c("abp.kcal", "pbp.kcal", "fat.kcal")
  XYZ_list[[3]]<-c("abp.kcal", "pbp.kcal", "fat.kcal")
  
  
  y1<-round((min(dataset.plot$pbp.kcal)-10)/10)*10
  y2<-round((min(dataset.plot$fat.kcal)-10)/10)*10
  y3<-round((max(dataset.plot$pbp.kcal)+10)/10)*10
  y4<-round((max(dataset.plot$fat.kcal)+10)/10)*10
  
  
  # Limits for the y.axis
  y_limits<-list()
  y_limits[[1]]<-c(y1, y3)
  y_limits[[2]]<-c(y2, y4)
  y_limits[[3]]<-y_limits[[1]]
  
  output<-list(XYZ_list,y_limits)
  
  return(output)
}

labels_list<-c("Animal-Based Protein (kcal/capita/day)","Plant-Based Protein (kcal/capita/day)", "Fat (kcal/capita/day)")

i=2017


mins<-array(NA, c(3,2))
maxs<-mins
mins60<-mins
maxs60<-mins
year.plot<-i

yaxis<-list()
yaxis[[1]]<-c(100,450)
yaxis[[2]]<-c(100,450)
yaxis[[3]]<-c(100,450)

xaxis<-list()
xaxis[[1]]<-c(100,450)
xaxis[[2]]<-c(100,450)
xaxis[[3]]<-c(100,450)


################################################
############ Male survival @ Age=5 #############
################################################
GAM<-GAM.5m
age.plot<-5
dataset.plot<-dataset[['5m']][which(dataset[['5m']][,"Year"]==2017),]
med.GDP<-round(median(dataset.plot$GDP, na.rm=T))
med.carb<-round(median(dataset.plot$carb.kcal, na.rm=T))
fat.q1<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[1]
med.fat<-median(dataset.plot$fat.kcal)
fat.q3<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[2]
fat.kcal<-c(fat.q1,med.fat,fat.q3)
predict.val<-data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Males"), Age=age.plot, carb.kcal=med.carb)

XYZ_list<-newSurface(dataset.plot=dataset.plot)[[1]]

min_use5<-NA
max_use5<-NA
contour5<-NA

surfaces_list_M5<-list()

for(j in 1:3){
  
  # Set the parameters for XYZ set j
  XYZ<-XYZ_list[[j]]
  labels<-labels_list[match(XYZ, XYZ_list[[1]])]
  z.val<-round(fat.kcal[j])
  
  # Remake the surfaces sacles by the corss-surface min and max
  surface<-ggSurface(GAM=GAM, data=dataset.plot, XYZ=XYZ, labels=labels, exclude=c("s(Country)"), predict_val=predict.val, 
                     col.limit=min_use5,contour_at=contour5, surf_min=min_use5, surf_max=max_use5, 
                     subtitle=paste0(labels[3], " = ", z.val), z.val=z.val, y.limits=yaxis[[j]], x.limits=xaxis[[j]],decimal=3)
  
  
  # Save them				
  surfaces_list_M5[[j]]<-surface
}

################################################
############ Female survival @ Age=5 ###########
################################################
GAM<-GAM.5f
age.plot<-5
dataset.plot<-dataset[['5f']][which(dataset[['5f']][,"Year"]==2017),]
med.GDP<-round(median(dataset.plot$GDP, na.rm=T))
med.carb<-round(median(dataset.plot$carb.kcal, na.rm=T))
fat.q1<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[1]
med.fat<-median(dataset.plot$fat.kcal)
fat.q3<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[2]
fat.kcal<-c(fat.q1,med.fat,fat.q3)
predict.val<-data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Females"), Age=age.plot, carb.kcal=med.carb)

XYZ_list<-newSurface(dataset.plot=dataset.plot)[[1]]

surfaces_list_F5<-list()



for(j in 1:3){
  
  # Set the parameters for XYZ set j
  XYZ<-XYZ_list[[j]]
  labels<-labels_list[match(XYZ, XYZ_list[[1]])]
  z.val<-round(fat.kcal[j])
  
  surface<-ggSurface(GAM=GAM, data=dataset.plot, XYZ=XYZ, labels=labels, exclude=c("s(Country)"), predict_val=predict.val, 
                     col.limit=min_use5,
                     contour_at=contour5,
                     surf_min=min_use5, surf_max=max_use5, 
                     subtitle=paste0(labels[3], " = ", z.val), z.val=z.val, y.limits=yaxis[[j]], x.limits=xaxis[[j]],decimal=3)
  
  
  # Save them				
  surfaces_list_F5[[j]]<-surface
}


for(j in 1:3){
  print(j)
  print(min(surfaces_list_M5[[j]]$data$fit))
  mins[j,1]<-min(surfaces_list_M5[[j]]$data$fit)
  print(max(surfaces_list_M5[[j]]$data$fit))
  maxs[j,1]<-max(surfaces_list_M5[[j]]$data$fit)
}

for(j in 1:3){
  print(j)
  print(min(surfaces_list_F5[[j]]$data$fit))
  mins[j,2]<-min(surfaces_list_F5[[j]]$data$fit)
  print(max(surfaces_list_F5[[j]]$data$fit))
  maxs[j,2]<-max(surfaces_list_F5[[j]]$data$fit)
}

min_use5<-min(mins)
max_use5<-max(maxs)
contour5<-0.001
# Now remake male and females surfaces for l5

################################################
############ Male survival @ Age=5 #############
################################################
GAM<-GAM.5m
age.plot<-5
dataset.plot<-dataset[['5m']][which(dataset[['5m']][,"Year"]==2017),]
med.GDP<-round(median(dataset.plot$GDP, na.rm=T))
med.carb<-round(median(dataset.plot$carb.kcal, na.rm=T))
fat.q1<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[1]
med.fat<-median(dataset.plot$fat.kcal)
fat.q3<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[2]
fat.kcal<-c(fat.q1,med.fat,fat.q3)
predict.val<-data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Males"), Age=age.plot, carb.kcal=med.carb)

XYZ_list<-newSurface(dataset.plot=dataset.plot)[[1]]

surfaces_list_M5<-list()

for(j in 1:3){
  
  # Set the parameters for XYZ set j
  XYZ<-XYZ_list[[j]]
  labels<-labels_list[match(XYZ, XYZ_list[[1]])]
  z.val<-round(fat.kcal[j])
  
  # Remake the surfaces sacles by the corss-surface min and max
  surface<-ggSurface(GAM=GAM, data=dataset.plot, XYZ=XYZ, labels=labels, exclude=c("s(Country)"), predict_val=predict.val, 
                     col.limit=min_use5,contour_at=contour5, surf_min=min_use5, surf_max=max_use5, 
                     subtitle=paste0(labels[3], " = ", z.val), z.val=z.val, y.limits=yaxis[[j]], x.limits=xaxis[[j]],decimal=3)
  
  
  # Save them				
  surfaces_list_M5[[j]]<-surface
}

################################################
############ Female survival @ Age=5 ###########
################################################
GAM<-GAM.5f
age.plot<-5
dataset.plot<-dataset[['5f']][which(dataset[['5f']][,"Year"]==2017),]
med.GDP<-round(median(dataset.plot$GDP, na.rm=T))
med.carb<-round(median(dataset.plot$carb.kcal, na.rm=T))
fat.q1<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[1]
med.fat<-median(dataset.plot$fat.kcal)
fat.q3<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[2]
fat.kcal<-c(fat.q1,med.fat,fat.q3)
predict.val<-data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Females"), Age=age.plot, carb.kcal=med.carb)

XYZ_list<-newSurface(dataset.plot=dataset.plot)[[1]]

min_use<-min_use5
max_use<-max_use5

contour<-contour5


surfaces_list_F5<-list()



for(j in 1:3){
  
  # Set the parameters for XYZ set j
  XYZ<-XYZ_list[[j]]
  labels<-labels_list[match(XYZ, XYZ_list[[1]])]
  z.val<-round(fat.kcal[j])
  
  surface<-ggSurface(GAM=GAM, data=dataset.plot, XYZ=XYZ, labels=labels, exclude=c("s(Country)"), predict_val=predict.val, 
                     col.limit=min_use5,
                     contour_at=contour5,
                     surf_min=min_use5, surf_max=max_use5, 
                     subtitle=paste0(labels[3], " = ", z.val), z.val=z.val, y.limits=yaxis[[j]], x.limits=xaxis[[j]],decimal=3)
  
  
  # Save them				
  surfaces_list_F5[[j]]<-surface
}



bottom<-textGrob(paste(labels[1]), gp = gpar(fontsize = 20))
yleft<-textGrob(paste(labels[2]),rot=90, gp = gpar(fontsize = 20))
top<-textGrob("l5 Males", gp = gpar(fontsize = 20))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
leg<-g_legend( surfaces_list_M5[[3]])


# Now lets arrange all those plots
CairoPDF(paste0("code_analysis/analysis_output/Figures/Surface_1_L5_males_",year.plot,"data_imp.pdf"), height=10, width=32)

grid.arrange(surfaces_list_M5[[1]]+labs(x=NULL,y=NULL,title="A")+ theme(legend.position = "none"), 
             surfaces_list_M5[[2]]+labs(x=NULL,y=NULL,title="B")+ theme(legend.position = "none"), 
             surfaces_list_M5[[3]]+labs(x=NULL,y=NULL,title="C")+ theme(legend.position = "none"),ncol=3,left=yleft,bottom=bottom,top=top,right=leg)
dev.off()

save(surfaces_list_M5, file=paste0("code_analysis/analysis_output/surfaces_list_M5_imputed_",year.plot,"data.rdata"))

top<-textGrob("l5 Females", gp = gpar(fontsize = 20))

leg<-g_legend( surfaces_list_F5[[3]])



# Now lets arrange all those plots
CairoPDF(paste0("code_analysis/analysis_output/Figures/Surface_1_L5_females_",year.plot,"data_imp.pdf"), height=10, width=32)

grid.arrange(surfaces_list_F5[[1]]+labs(x=NULL,y=NULL,title="A")+ theme(legend.position = "none"), 
             surfaces_list_F5[[2]]+labs(x=NULL,y=NULL,title="B")+ theme(legend.position = "none"), 
             surfaces_list_F5[[3]]+labs(x=NULL,y=NULL,title="C")+ theme(legend.position = "none"),ncol=3,left=yleft,bottom=bottom,top=top,right=leg)

dev.off()

save(surfaces_list_F5, file=paste0("code_analysis/analysis_output/surfaces_list_F5_imputed_",year.plot,"data.rdata"))

################################################
############ Male survival @ Age=60 ##########
################################################
GAM<-GAM.60m
age.plot<-60
dataset.plot<-dataset[['60m']][which(dataset[['60m']][,"Year"]==2017),]
med.GDP<-round(median(dataset.plot$GDP, na.rm=T))
med.carb<-round(median(dataset.plot$carb.kcal, na.rm=T))
fat.q1<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[1]
med.fat<-median(dataset.plot$fat.kcal)
fat.q3<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[2]
fat.kcal<-c(fat.q1,med.fat,fat.q3)
predict.val<-data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Males"), Age=age.plot, carb.kcal=med.carb)

XYZ_list<-newSurface(dataset.plot=dataset.plot)[[1]]

min_use60<-NA
max_use60<-NA
contour60<-NA

surfaces_list_M60<-list()

for(j in 1:3){
  
  # Set the parameters for XYZ set j
  XYZ<-XYZ_list[[j]]
  labels<-labels_list[match(XYZ, XYZ_list[[1]])]
  z.val<-round(fat.kcal[j])
  
  # Remake the surfaces scales by the corss-surface min and max
  surface<-ggSurface(GAM=GAM, data=dataset.plot, XYZ=XYZ, labels=labels, exclude=c("s(Country)"), predict_val=predict.val, 
                     col.limit=min_use60,
                     contour_at=contour60,
                     surf_min=min_use60, surf_max=max_use60, 
                     subtitle=paste0(labels[3], " = ", z.val), z.val=z.val, y.limits=yaxis[[j]], x.limits=xaxis[[j]],decimal=2)
  
  
  # Save them				
  surfaces_list_M60[[j]]<-surface
}

################################################
############ Female survival @ Age=60 ##########
################################################
GAM<-GAM.60f
age.plot<-60
dataset.plot<-dataset[['60f']][which(dataset[['60f']][,"Year"]==2017),]
med.GDP<-round(median(dataset.plot$GDP, na.rm=T))
med.carb<-round(median(dataset.plot$carb.kcal, na.rm=T))
fat.q1<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[1]
med.fat<-median(dataset.plot$fat.kcal)
fat.q3<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[2]
fat.kcal<-c(fat.q1,med.fat,fat.q3)
predict.val<-data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Females"), Age=age.plot, carb.kcal=med.carb)

surfaces_list_F60<-list()

for(j in 1:3){
  
  # Set the parameters for XYZ set j
  XYZ<-XYZ_list[[j]]
  labels<-labels_list[match(XYZ, XYZ_list[[1]])]
  z.val<-round(fat.kcal[j])
  
  # Remake the surfaces sacles by the corss-surface min and max
  surface<-ggSurface(GAM=GAM, data=dataset.plot, XYZ=XYZ, labels=labels, exclude=c("s(Country)"), 
                     predict_val=predict.val, surf_min=min_use60, surf_max=max_use60,col.limit=min_use60,contour_at=contour60,
                     subtitle=paste0(labels[3], " = ", z.val), z.val=z.val, y.limits=yaxis[[j]], x.limits=xaxis[[j]],decimal=2)
  
  
  # Save them				
  surfaces_list_F60[[j]]<-surface 
}



for(j in 1:3){
  print(j)
  print(min(surfaces_list_M60[[j]]$data$fit))
  mins60[j,1]<-min(surfaces_list_M60[[j]]$data$fit)
  print(max(surfaces_list_M60[[j]]$data$fit))
  maxs60[j,1]<-max(surfaces_list_M60[[j]]$data$fit)
}

# scroll and run for female l60 surfaces before running the below lines

for(j in 1:3){
  print(j)
  print(min(surfaces_list_F60[[j]]$data$fit))
  mins60[j,2]<-min(surfaces_list_F60[[j]]$data$fit)
  print(max(surfaces_list_F60[[j]]$data$fit))
  maxs60[j,2]<-max(surfaces_list_F60[[j]]$data$fit)
}

min_use60<-min(mins60)
max_use60<-max(maxs60)
contour60<-0.01

# Now remake male and females surfaces for l5

################################################
############ Male survival @ Age=60 ##########
################################################
GAM<-GAM.60m
age.plot<-60
dataset.plot<-dataset[['60m']][which(dataset[['60m']][,"Year"]==2017),]
med.GDP<-round(median(dataset.plot$GDP, na.rm=T))
med.carb<-round(median(dataset.plot$carb.kcal, na.rm=T))
fat.q1<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[1]
med.fat<-median(dataset.plot$fat.kcal)
fat.q3<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[2]
fat.kcal<-c(fat.q1,med.fat,fat.q3)
predict.val<-data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Males"), Age=age.plot, carb.kcal=med.carb)

XYZ_list<-newSurface(dataset.plot=dataset.plot)[[1]]


surfaces_list_M60<-list()

for(j in 1:3){
  
  # Set the parameters for XYZ set j
  XYZ<-XYZ_list[[j]]
  labels<-labels_list[match(XYZ, XYZ_list[[1]])]
  z.val<-round(fat.kcal[j])
  
  # Remake the surfaces scales by the corss-surface min and max
  surface<-ggSurface(GAM=GAM, data=dataset.plot, XYZ=XYZ, labels=labels, exclude=c("s(Country)"), predict_val=predict.val, 
                     col.limit=min_use60,
                     contour_at=contour60,
                     surf_min=min_use60, surf_max=max_use60, 
                     subtitle=paste0(labels[3], " = ", z.val), z.val=z.val, y.limits=yaxis[[j]], x.limits=xaxis[[j]],decimal=2)
  
  
  # Save them				
  surfaces_list_M60[[j]]<-surface
}

################################################
############ Female survival @ Age=60 ##########
################################################
GAM<-GAM.60f
age.plot<-60
dataset.plot<-dataset[['60f']][which(dataset[['60f']][,"Year"]==2017),]
med.GDP<-round(median(dataset.plot$GDP, na.rm=T))
med.carb<-round(median(dataset.plot$carb.kcal, na.rm=T))
fat.q1<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[1]
med.fat<-median(dataset.plot$fat.kcal)
fat.q3<-quantile(dataset.plot$fat.kcal, c(1/3,  2/3))[2]
fat.kcal<-c(fat.q1,med.fat,fat.q3)
predict.val<-data.frame(Year=year.plot, GDP=med.GDP, Sex=as.factor("Females"), Age=age.plot, carb.kcal=med.carb)

surfaces_list_F60<-list()

for(j in 1:3){
  
  # Set the parameters for XYZ set j
  XYZ<-XYZ_list[[j]]
  labels<-labels_list[match(XYZ, XYZ_list[[1]])]
  z.val<-round(fat.kcal[j])
  
  # Remake the surfaces sacles by the corss-surface min and max
  surface<-ggSurface(GAM=GAM, data=dataset.plot, XYZ=XYZ, labels=labels, exclude=c("s(Country)"), 
                     predict_val=predict.val, surf_min=min_use60, surf_max=max_use60,col.limit=min_use60,contour_at=contour60,
                     subtitle=paste0(labels[3], " = ", z.val), z.val=z.val, y.limits=yaxis[[j]], x.limits=xaxis[[j]],decimal=2)
  
  
  # Save them				
  surfaces_list_F60[[j]]<-surface 
}

top<-textGrob("l60 Males", gp = gpar(fontsize = 20))

leg<-g_legend( surfaces_list_M60[[3]])

# Now lets arrange all those plots
CairoPDF(paste0("code_analysis/analysis_output/Figures/Surface_1_l60_males_",year.plot,"data_imp.pdf"), height=10, width=32)

grid.arrange(surfaces_list_M60[[1]]+labs(x=NULL,y=NULL,title="A")+ theme(legend.position = "none"), 
             surfaces_list_M60[[2]]+labs(x=NULL,y=NULL,title="B")+ theme(legend.position = "none"), 
             surfaces_list_M60[[3]]+labs(x=NULL,y=NULL,title="C")+ theme(legend.position = "none"),ncol=3,left=yleft,bottom=bottom,top=top,right=leg)
dev.off()


save(surfaces_list_M60, file=paste0("code_analysis/analysis_output/surfaces_list_M60_imputed_",year.plot,"data.rdata"))


top<-textGrob("l60 Females", gp = gpar(fontsize = 20))

leg<-g_legend( surfaces_list_F60[[3]])


# Now lets arrange all those plots
CairoPDF(paste0("code_analysis/analysis_output/Figures/Surface_1_L60_females_",year.plot,"data_imp.pdf"), height=10, width=32)


grid.arrange(surfaces_list_F60[[1]]+labs(x=NULL,y=NULL,title="A")+ theme(legend.position = "none"), 
             surfaces_list_F60[[2]]+labs(x=NULL,y=NULL,title="B")+ theme(legend.position = "none"), 
             surfaces_list_F60[[3]]+labs(x=NULL,y=NULL,title="C")+ theme(legend.position = "none"),ncol=3,left=yleft,bottom=bottom,top=top,right=leg)

dev.off()

save(surfaces_list_F60, file=paste0("code_analysis/analysis_output/surfaces_list_F60_imputed_",year.plot,"data.rdata"))


#################################################
############### Figures S1 and S2 ###############
#################################################

top<-textGrob("Survivorship (Age 5)", gp = gpar(fontsize = 30))
bottom<-textGrob(paste(labels[1]), gp = gpar(fontsize = 30))
yleft<-textGrob(paste(labels[2]),rot=90, gp = gpar(fontsize = 30))

leg<-g_legend( surfaces_list_F5[[3]])
# Now lets arrange all those plots
CairoPDF(paste0("code_analysis/analysis_output/Figures/Surface_1_l5_imputed_",year.plot,"data.pdf"), height=15, width=23)
grid.arrange(arrangeGrob(surfaces_list_F5[[1]]+labs(x=NULL,y=NULL,title="A")+ theme(legend.position = "none"), 
                         surfaces_list_F5[[2]]+labs(x=NULL,y=NULL,title="B")+ theme(legend.position = "none"), 
                         surfaces_list_F5[[3]]+labs(x=NULL,y=NULL,title="C")+ theme(legend.position = "none"), 
                         surfaces_list_M5[[1]]+labs(x=NULL,y=NULL,title="D")+ theme(legend.position = "none"), 
                         surfaces_list_M5[[2]]+labs(x=NULL,y=NULL,title="E")+ theme(legend.position = "none"), 
                         surfaces_list_M5[[3]]+labs(x=NULL,y=NULL,title="F")+ theme(legend.position = "none"),ncol=3,left=yleft,bottom=bottom,top=top),leg,ncol=2,widths=c(10,0.5),padding=unit(2,"line"))



dev.off()


top<-textGrob("Survivorship (Age 60)", gp = gpar(fontsize = 30))
bottom<-textGrob(paste(labels[1]), gp = gpar(fontsize = 30))
yleft<-textGrob(paste(labels[2]),rot=90, gp = gpar(fontsize = 30))

leg<-g_legend( surfaces_list_F60[[3]])

CairoPDF(paste0("code_analysis/analysis_output/Figures/Surface_1_l60_imputed_",year.plot,"data.pdf"), height=15, width=23)


grid.arrange(arrangeGrob(surfaces_list_F60[[1]]+labs(x=NULL,y=NULL,title="A")+ theme(legend.position = "none"), 
                         surfaces_list_F60[[2]]+labs(x=NULL,y=NULL,title="B")+ theme(legend.position = "none"), 
                         surfaces_list_F60[[3]]+labs(x=NULL,y=NULL,title="C")+ theme(legend.position = "none"), 
                         surfaces_list_M60[[1]]+labs(x=NULL,y=NULL,title="D")+ theme(legend.position = "none"), 
                         surfaces_list_M60[[2]]+labs(x=NULL,y=NULL,title="E")+ theme(legend.position = "none"),
                         surfaces_list_M60[[3]]+labs(x=NULL,y=NULL,title="F")+ theme(legend.position = "none"),
                         ncol=3,left=yleft,bottom=bottom,top=top),leg,ncol=2,widths=c(10,0.5),padding=unit(2,"line"))
dev.off()