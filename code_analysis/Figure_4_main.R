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

############################################################
############ Plot Surfaces for Model 11 (2016) #############
############################################################



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
################################################
############ te(Carb.kcal, Year) ###############
################################################


labels_list<-c("Animal-Based Protein (kcal/capita/day)","Plant-Based Protein (kcal/capita/day)", "Fat (kcal/capita/day)")


i=2017



plotyear<-i
year.plot=i

#load data from lX surfaces
load(paste0("code_analysis/analysis_output/surfaces_list_M5_",year.plot,"data.rdata"))
load(paste0("code_analysis/analysis_output/surfaces_list_M60_",year.plot,"data.rdata"))
load(paste0("code_analysis/analysis_output/surfaces_list_F5_",year.plot,"data.rdata"))
load(paste0("code_analysis/analysis_output/surfaces_list_F60_",year.plot,"data.rdata"))

###################################################
############ Estimate LE @ birth (e0) #############
###################################################

### To begin with do the conversion and find the mins and maxs across all surfaces
predictions_list<-list()
mins<-array(NA, c(3,1))
maxs<-mins

yaxis<-list()
yaxis[[1]]<-c(100,450)
yaxis[[2]]<-c(100,450)
yaxis[[3]]<-c(100,450)

xaxis<-list()
xaxis[[1]]<-c(100,450)
xaxis[[2]]<-c(100,450)
xaxis[[3]]<-c(100,450)

for (k in 1:3){
  
  predictions.M5<-surfaces_list_M5[[k]]$data
  predictions.M5$l5<-predictions.M5$fit
  predictions.M60<-surfaces_list_M60[[k]]$data
  predictions.M5$l60<-predictions.M60$fit
  predictions.males<-predictions.M5
  head(predictions.males)
  predictions.males$Sex<-as.factor(predictions.males$Sex)

  
  
  predictions.F5<-surfaces_list_F5[[k]]$data
  predictions.F5$l5<-predictions.F5$fit
  predictions.F60<-surfaces_list_F60[[k]]$data
  predictions.F5$l60<-predictions.F60$fit
  predictions.females<-predictions.F5
  head(predictions.females)
  

  predictions<-rbind(predictions.males,predictions.females)
  head(predictions)
  
  ### Add in an NA columns to hold data
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
  
  # Save the predictions
  predictions_list[[k]]<-predictions
  mins[k,1]<-min(predictions$e0)
  maxs[k,1]<-max(predictions$e0)
}	

setwd(wd.laptop)
setwd(surfaces)
save(predictions_list,file=paste0("code_analysis/analysis_output/Figure_4_main_predictions_list_",plotyear,"data.rdata"))


#####################################################
all.e0<-rbind(predictions_list[[1]],predictions_list[[2]],predictions_list[[3]])


e0.iqr<-round(IQR(all.e0$e0)/5,3)

min_e0<-min(all.e0$e0)
max_e0<-max(all.e0$e0)


#####################################################

plotyear<-i

convert1<-c("Males", "Females")
convert2<-c("male", "female")

e0_list_M<-list()
e0_list_F<-list()

Mins<-array(NA, c(3,2))
Maxs<-Mins


for(k in 1:3){
  predictions<-predictions_list[[k]]
  predictions.males<-predictions[which(predictions$Sex==convert1[1]),]
  predictions.females<-predictions[which(predictions$Sex==convert1[2]),]
  
  dataset.plot<-predictions.males[which(predictions.males$Year == plotyear),]
  XYZ_list<-newSurface(dataset.plot)[[1]]
  
  labels<-labels_list[match(XYZ_list[[k]], XYZ_list[[1]])]
  # This specifies the color scheme for surface	
  #rgb.palette<-colorRampPalette(c("blue","cyan","yellow","red"), space="Lab", interpolate="linear")
  #warm.palette<-colorRampPalette(c("#FFCC0D","#FF7326","#FF194D","#BF2669","#702A8C"), space="Lab", interpolate="linear")
  rgb2.palette<-colorRampPalette(c("#060AAA","blue","#007fff","cyan",
                                   "#00ffdd","#00ff00","#98ff4d","yellow","#FFE301","#FFA700","#FF8800","#FF5E00","red","#EF3037"), space="Lab", interpolate="linear")
  
  
  
  
  map<-rgb2.palette(256)
  mn<-70
  mx<-84
  
  contour_use<-1
  
  print(paste0("e0 males contour:",contour_use))
  
  e0_list_M[[k]]<-ggplot(predictions.males, aes(x=x, y=y)) +
    geom_raster(aes(fill=e0), show.legend=T, interpolate=F, na.rm=T) +
    scale_fill_gradientn(colors=map,limits=c(mn,mx)) +
    geom_contour(data=predictions.males, aes(x=x, y=y, z=e0), na.rm=T, color="grey10", binwidth=contour_use) +	
    #geom_label_contour(data=predictions.males, aes(x=x, y=y, z=e0), size=8,alpha=0.8,position=position_nudge(x=-5), binwidth=contour_use, skip=1) +
    geom_text_contour(data=predictions.males,aes(x=x, y=y,z=e0,fontface="bold"),size=5,nudge_x = -0.5, binwidth=contour_use, skip=1,rotate=FALSE,color="white",show.legend=FALSE,stroke = 0.3, stroke.colour = "grey10",label.placer = label_placer_flattest(),check_overlap = TRUE)+		
    theme_bw() +
    labs(x = labels[1], y = labels[2], title=paste0(labels[3], " = ", round(quantile(dataset.plot[, XYZ_list[[k]][3]])[3]))) +
    theme(axis.text=element_text(size=25), axis.title=element_text(size=25)) +
    theme(plot.title=element_text(size=25,margin=unit(c(1,1,1,1),"cm"),hjust=0.5,face="bold"),title=element_text(size=25,margin=unit(c(1,1,1,1),"cm")),legend.key.size=unit(2,"cm"),legend.box.margin = margin(l=1,r=1,unit="cm"),legend.text = element_text(size=25),axis.title.x = element_text(margin=unit(c(1,1,1,1),"cm")),axis.title.y = element_text(margin=unit(c(1,1,1,1),"cm"))
    )	+
    ylim(yaxis[[k]])	+
    xlim(xaxis[[k]]) 
  
  
  ############################# FEMALES ###############################
  
  dataset.plot<-predictions.females[which(predictions.females$Year ==plotyear),]
  XYZ_list<-newSurface(dataset.plot)[[1]]
  
  labels<-labels_list[match(XYZ_list[[k]], XYZ_list[[1]])]
  print(paste0("e0 females contour:",contour_use))
  
  e0_list_F[[k]]<-ggplot(predictions.females, aes(x=x, y=y)) +
    geom_raster(aes(fill=e0), show.legend=T, interpolate=F, na.rm=T) +
    #scale_fill_gradientn(colors=map[locs[1]:locs[2]]) +
    scale_fill_gradientn(colors=map,limits=c(mn,mx)) +
    geom_contour(data=predictions.females, aes(x=x, y=y, z=e0), na.rm=T, color="grey10", binwidth=contour_use) +	
    # geom_label_contour(data=predictions.females, aes(x=x, y=y, z=e0), size=8,alpha=0.8,position=position_nudge(x=-5), binwidth=contour_use, skip=1) +
    geom_text_contour(data=predictions.females,aes(x=x, y=y,z=e0,fontface="bold"),size=5,nudge_x = -0.5, binwidth=contour_use, skip=1,rotate=FALSE,color="white",show.legend=FALSE,stroke = 0.3, stroke.colour = "grey10",label.placer = label_placer_flattest(),check_overlap = TRUE)+		
    theme_bw() +
    labs(x = labels[1], y = labels[2], title=paste0(labels[3], " = ", round(quantile(dataset.plot[, XYZ_list[[k]][3]])[3]))) +
    theme(axis.text=element_text(size=25), axis.title=element_text(size=25)) +
    theme(plot.title=element_text(size=25,margin=unit(c(0.5,0.5,0.5,0.5),"cm"),hjust=0.5,face="bold"),title=element_text(size=25),legend.key.size=unit(2,"cm"),legend.box.margin = margin(l=1,r=1,unit="cm"),legend.text = element_text(size=25),axis.title.y = element_text(margin=unit(c(1,1,1,1),"cm")))	+
    ylim(yaxis[[k]])	+
    xlim(xaxis[[k]])
}	
setwd(wd.laptop)
setwd(wd.output)
save(e0_list_M, file=paste0("code_analysis/analysis_output/Figure_4_main_list_males_",year.plot,"data.rdata"))
save(e0_list_F, file=paste0("code_analysis/analysis_output/Figure_4_main_list_females_",year.plot,"data.rdata"))

##################################
library(cowplot)
title <- ggdraw() +
  draw_label("Life Expectancy at Birth",
             x = 0.05, hjust = 0, vjust = 1)
lowfat<-ggdraw(get_title(e0_list_F[[1]]))
medfat<-ggdraw(get_title(e0_list_F[[2]]))
highfat<-ggdraw(get_title(e0_list_F[[3]]))

xlab<-ggdraw()+draw_label(labels[1],x=0.5,size=25)
ylab<-ggdraw()+draw_label(labels[2],vjust=0.5,size=25,angle = 90)

bottom<-textGrob(paste(labels[1]), gp = gpar(fontsize = 25))
yleft<-textGrob(paste(labels[2]),rot=90, gp = gpar(fontsize = 25))
title<-textGrob(paste("e0"), gp = gpar(fontsize = 20))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
leg<-get_legend( e0_list_F[[3]])

plotgrid<-plot_grid(e0_list_F[[1]]+labs(x=NULL,y=NULL,title=NULL)+ theme(legend.position = "none"), 
                    e0_list_F[[2]]+labs(x=NULL,y=NULL,title=NULL)+ theme(legend.position = "none"), 
                    e0_list_F[[3]]+labs(x=NULL,y=NULL,title=NULL)+ theme(legend.position = "none"),
                    e0_list_M[[1]]+labs(x=NULL,y=NULL,title=NULL)+ theme(legend.position = "none"), 
                    e0_list_M[[2]]+labs(x=NULL,y=NULL,title=NULL)+ theme(legend.position = "none"), 
                    e0_list_M[[3]]+labs(x=NULL,y=NULL,title=NULL)+ theme(legend.position = "none"),labels="AUTO",label_size=25,label_x=0.9,vjust=2.2,hjust=-0.1,ncol=3,align='v')


# Now lets arrange all those plots
CairoPDF(paste0("Surface_2_withSEsq_kcarb5_",plotyear,"_2017data_gam18_aug23.pdf"), height=15, width=23)

ggdraw(plot_grid(ylab,plot_grid(plot_grid(plot_grid(lowfat,medfat,highfat,ncol=3),plotgrid,nrow=2,rel_heights=c(0.1,1)),xlab,ncol=1,rel_heights=c(1,0.1)),leg,nrow=1,rel_widths = c(0.1,1,0.1)))

dev.off()


# Now lets arrange all those plots
CairoPDF(paste0("code_analysis/analysis_output/Figures/Figure_4_main_",plotyear,"data.pdf"), height=22, width=30)

# grid.arrange(e0_list_F[[1]]+labs(x=NULL,y=NULL,subtitle="A")+ theme(legend.position = "none"), 
#              e0_list_F[[2]]+labs(x=NULL,y=NULL,subtitle="B")+ theme(legend.position = "none"), 
#              e0_list_F[[3]]+labs(x=NULL,y=NULL,subtitle="C")+ theme(legend.position = "none"),
#              e0_list_M[[1]]+labs(x=NULL,y=NULL,subtitle="D",title=NULL)+ theme(legend.position = "none"), 
#              e0_list_M[[2]]+labs(x=NULL,y=NULL,subtitle="E",title=NULL)+ theme(legend.position = "none"), 
#              e0_list_M[[3]]+labs(x=NULL,y=NULL,subtitle="F",title=NULL)+ theme(legend.position = "none"),ncol=3,top=title,right=leg)

plot_grid(e0_list_F[[1]]+labs(x=NULL,y=NULL)+ theme(legend.position = "none"), 
          e0_list_F[[2]]+labs(x=NULL,y=NULL)+ theme(legend.position = "none"), 
          e0_list_F[[3]]+labs(x=NULL,y=NULL)+ theme(legend.position = "none"),
          e0_list_M[[1]]+labs(x=NULL,y=NULL,title=NULL)+ theme(legend.position = "none"), 
          e0_list_M[[2]]+labs(x=NULL,y=NULL,title=NULL)+ theme(legend.position = "none"), 
          e0_list_M[[3]]+labs(x=NULL,y=NULL,title=NULL)+ theme(legend.position = "none"),labels=c("A","B","C","D","E","F"),ncol=3,top=title,right=leg)


dev.off()


