
# Clean up
rm(list=ls())


source("code_analysis/ggSurface.R")
source("code_analysis/inhull.R")

standard<-read.csv("data_read/wilmoth_standard.csv")


# Load libraries
library(dplyr)
library(arm)
library(ggplot2)
library(countrycode)
library(rnaturalearth)
library(RColorBrewer)
library(mgcv)
library(gridExtra)
library(ggcorrplot)
library(Cairo)
library(geometry)
library(metR)
library(datasets)
library(MortalityLaws)
library(cowplot)
library(data.table)



# Read in the cleaned  data

data.5m<-read.csv("data_cleaned/5mdata.csv", stringsAsFactors=T, header = TRUE)
data.60m<-read.csv("data_cleaned/60mdata.csv", stringsAsFactors=T, header = TRUE)

data.5f<-read.csv("data_cleaned/5fdata.csv", stringsAsFactors=T, header = TRUE)
data.60f<-read.csv("data_cleaned/60fdata.csv", stringsAsFactors=T, header = TRUE)


dataset<-list(data.5m,data.60m,data.5f,data.60f)
names(dataset)<-c('5m','60m','5f','60f')


data.m<-cbind(data.5m[,-which(names(data.5m)=="Sex")],l60=data.60m$l60,Sex="Males")

data.f<-cbind(data.5f[,-which(names(data.5f)=="Sex")],l60=data.60f$l60,Sex="Females")

glimpse(data.m)
glimpse(data.f)
full_data<-rbind(data.m,data.f)



#################################################
#################### FIGURE 1 ###################
#################################################


# Lets start by making two maps, that summarise plant vs animal protein for the periods 1961-1990 and 1991-2017

# World data
world<-ne_countries(scale="medium", returnclass="sf")
world$CC<-countrycode(world$name_sort, origin="country.name", destination="iso3c")


# Get the number of lifetables by country

summary_61to90<-plyr::ddply(full_data_map[which(full_data_map$Year < 1991),], plyr::.(Country,Year), summarise, AoverP=abp.kcal/pbp.kcal)
summary_91to<-plyr::ddply(full_data_map[which(full_data_map$Year >= 1991),], plyr::.(Country,Year), summarise,AoverP=abp.kcal/pbp.kcal)

head(summary_61to90)
head(summary_91to)

summary_61to90<-plyr::ddply(summary_61to90, plyr::.(Country), summarise, meanRatio=log(mean(AoverP)))
summary_91to<-plyr::ddply(summary_91to, plyr::.(Country), summarise,meanRatio=log(mean(AoverP)))


# Match the data in to the world data

world$meanRatio_61<-summary_61to90$meanRatio[match(world$CC,summary_61to90$Country)]
world$meanRatio_91<-summary_91to$meanRatio[match(world$CC,summary_91to$Country)]

color_range<-colorRampPalette(c("green","yellow","red"))
map_cols<-color_range(200)

# Map for 1961-1990
map_61to90<-ggplot(data=world,aes(fill=meanRatio_61)) +
  geom_sf(lwd=0.5) + 
  theme_bw() +
  theme(axis.text.x =element_text (size=15,face="bold", vjust = 1, hjust=0.5),text=element_text(size=20,face="bold"),panel.grid.major=element_blank(), panel.grid.minor=element_blank(), title=element_text(size=20)) + 
  #labs(subtitle="Global Protein Supplies 1961 - 1990") + 
  scale_fill_gradientn(position="bottom",colours=map_cols,limits=c(-2.0452,1.1884),values=scales::rescale(c(-2.0452,0,1.1884)),na.value="lightgrey",name = "ABP:PBP")+
  coord_sf(ylim=c(-57, 80)) + 
  theme(legend.direction = "vertical",legend.position="right", legend.text =element_text(size=25),legend.margin = margin(0.1,0.1,0.1,0.1,"cm"))+
  guides(fill = guide_colourbar(title.position="right",ticks.colour = "black",barwidth = 3,barheight = 40,title.theme=element_text(size=25,hjust=0.5)))
# ,title = "Animal:Plant Based Protein Supply Ratio "))



#values=scales::rescale(c(-2.0452,0,1.1884))

# Map for 1991-2018
# Legend in figure
map_91to<-ggplot(data=world,aes(fill=meanRatio_91)) +
  geom_sf(lwd=0.5) + 
  theme_bw() +
  theme(axis.text.x =element_text (size=15,face="bold", vjust = 1, hjust=0.5),panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.title=element_blank(), title=element_text(size=15)) + 
  #labs(subtitle="Global Animal:Plant Protein Supplies 1991 - 2018") + 
  scale_fill_gradientn(colours=map_cols,limits=c(-2.0452,1.1884),values=scales::rescale(c(-2.0452,0,1.1884)),na.value="lightgrey",name = "ABP:PBP")+
  coord_sf(ylim=c(-57, 80)) + 
  theme(legend.direction = "horizontal",legend.position=c(0.15, 0.1))+
  guides(fill = guide_colourbar(title.position="top",barwidth = 28,barheight = 3,title.theme=element_text(size=20,hjust=0.5,face="bold"),title = "Animal:Plant Based Protein Supply Ratio "))


# Legend Below 
map_91to<-ggplot(data=world,aes(fill=meanRatio_91)) +
  geom_sf(lwd=0.5) + 
  theme_bw() +
  theme(axis.text.x =element_text (size=15,face="bold", vjust = 1, hjust=0.5),panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.title=element_blank(), title=element_text(size=15)) + 
  #labs(subtitle="Global Animal:Plant Protein Supplies 1991 - 2018") + 
  scale_fill_gradientn(colours=map_cols,limits=c(-2.0452,1.1884),values=scales::rescale(c(-2.0452,0,1.1884)),na.value="lightgrey",name = "ABP:PBP")+
  coord_sf(ylim=c(-57, 80)) + 
  theme(legend.direction = "horizontal",legend.position="bottom", legend.text =element_text(size=25),legend.margin = margin(0.1,0.1,0.1,0.1,"cm"))+
  guides(fill = guide_colourbar(title.position="top",barwidth = 100,barheight = 3,title.theme=element_text(size=25,hjust=0.5),title = "Animal:Plant Based Protein Supply Ratio "))


map_61to90_stack<-map_61to90+theme(legend.position = "none")
map_91to_stack<-map_91to+theme(legend.position = "none")
legend<-get_legend(map_61to90)

plot_grid(map_61to90_stack, map_91to_stack, ncol=1,rel_heights = c(1,1), rel_widths =c(1,1))
plot_grid(plot_grid(map_61to90_stack, map_91to_stack, ncol=1,rel_heights = c(1,1), rel_widths =c(1,1)),plot_grid(NULL,legend,NULL,ncol=1,nrow=3,rel_heights=c(0.2,0.5,0.2)),NULL,nrow=1,rel_widths = c(1,0.1,0.1))

# Now lets arrange all those plots
CairoPDF(paste0("code_analysis/analysis_output/Figures/Figure_1_A-B.pdf"), height=20, width=35)

plot_grid(plot_grid(map_61to90_stack, map_91to_stack, NULL, ncol=1,rel_heights = c(1,1,0.05), rel_widths =c(1,1)),plot_grid(NULL,legend,NULL,ncol=1,nrow=3,rel_heights=c(0.1,0.8,0.1)),NULL,nrow=1,rel_widths = c(1,0.1,0.1))

dev.off()


summary_61<-plyr::ddply(full_data_map[which(full_data_map$Year < 1971),], plyr::.(Country), summarise,AoverP=log((sum(abp.kcal)/(sum(pbp.kcal)+sum(abp.kcal)))/(sum(pbp.kcal)/(sum(pbp.kcal)+sum(abp.kcal)))))
summary_71<-plyr::ddply(full_data_map[which(full_data_map$Year < 1981 & full_data_map$Year >=1971),], plyr::.(Country), summarise,AoverP=log((sum(abp.kcal)/(sum(pbp.kcal)+sum(abp.kcal)))/(sum(pbp.kcal)/(sum(pbp.kcal)+sum(abp.kcal)))))
summary_81<-plyr::ddply(full_data_map[which(full_data_map$Year < 1991 & full_data_map$Year >= 1981 ),], plyr::.(Country), summarise,AoverP=log((sum(abp.kcal)/(sum(pbp.kcal)+sum(abp.kcal)))/(sum(pbp.kcal)/(sum(pbp.kcal)+sum(abp.kcal)))))

summary_91<-plyr::ddply(full_data_map[which(full_data_map$Year >= 1991 & full_data_map$Year < 2001),], plyr::.(Country), summarise, AoverP=log((sum(abp.kcal)/(sum(pbp.kcal)+sum(abp.kcal)))/(sum(pbp.kcal)/(sum(pbp.kcal)+sum(abp.kcal)))))

summary_01<-plyr::ddply(full_data_map[which(full_data_map$Year >= 2001),], plyr::.(Country), summarise,AoverP=log((sum(abp.kcal)/(sum(pbp.kcal)+sum(abp.kcal)))/(sum(pbp.kcal)/(sum(pbp.kcal)+sum(abp.kcal)))))


# Match the data in to the world data
world$AoverP_61<-summary_61$AoverP[match(world$CC, summary_61$Country)]
world$AoverP_71<-summary_71$AoverP[match(world$CC, summary_71$Country)]
world$AoverP_81<-summary_81$AoverP[match(world$CC, summary_81$Country)]
world$AoverP_91<-summary_91$AoverP[match(world$CC, summary_91$Country)]
world$AoverP_01<-summary_01$AoverP[match(world$CC, summary_01$Country)]

# Map for 1961-1970
map_61<-ggplot(data=world) +
  geom_sf(aes(fill=AoverP_61), lwd=0) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.title=element_blank(), title=element_text(size=15)) + 
  labs(subtitle="Global Animal:Plant Protein Supplies 1961 - 1970") + 
  scale_fill_gradient2(low="green",mid="yellow", high="red",midpoint=0, limits=c(-2.102, 1.264)) + 
  #scale_fill_gradientn(colors=c("darkgreen","green","yellow","orange","red"),values=c(-2.102,-1.051,0,0.632, 1.264)) + 
  coord_sf(ylim=c(-57, 80)) + 
  theme(legend.position=c(0.075, 0.25), legend.key.size=unit(0.4, "cm"))

# Map for 1971-1980
map_71<-ggplot(data=world) +
  geom_sf(aes(fill=AoverP_71), lwd=0) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.title=element_blank(), title=element_text(size=15)) + 
  labs(subtitle="Global Animal:Plant Protein Supplies 1971 - 1980") + 
  scale_fill_gradient2(low="green",mid="yellow", high="red",midpoint=0, limits=c(-2.102, 1.264)) + 
  #scale_fill_gradientn(colors=c("darkgreen","green","yellow","orange","red"),values=c(-2.102,-1.051,0,0.632, 1.264)) + 
  
  coord_sf(ylim=c(-57, 80)) + 
  theme(legend.position=c(0.075, 0.25), legend.key.size=unit(0.4, "cm"))

# Map for 1981-1990
map_81<-ggplot(data=world) +
  geom_sf(aes(fill=AoverP_81), lwd=0) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.title=element_blank(), title=element_text(size=15)) + 
  labs(subtitle="Global Animal:Plant Protein Supplies 1981 - 1990") + 
  scale_fill_gradient2(low="green",mid="yellow", high="red",midpoint=0, limits=c(-2.102, 1.264)) + 
  coord_sf(ylim=c(-57, 80)) + 
  theme(legend.position=c(0.075, 0.25), legend.key.size=unit(0.4, "cm"))


# Map for 1991-2000
map_91<-ggplot(data=world) +
  geom_sf(aes(fill=AoverP_91), lwd=0) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.title=element_blank(), title=element_text(size=15)) + 
  labs(subtitle="Global Animal:Plant Protein Supplies 1991 - 2000") + 
  scale_fill_gradient2(low="green",mid="yellow", high="red",midpoint=0, limits=c(-2.102, 1.264)) + 
  coord_sf(ylim=c(-57, 80)) + 
  theme(legend.position=c(0.075, 0.25), legend.key.size=unit(0.4, "cm"))

# Map for 2001-2018
map_01<-ggplot(data=world) +
  geom_sf(aes(fill=AoverP_01), lwd=0) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.title=element_blank(), title=element_text(size=15)) + 
  labs(subtitle="Global Animal:Plant Protein Supplies 2001 - 2018") + 
  scale_fill_gradient2(low="green",mid="yellow", high="red",midpoint=0, limits=c(-2.102, 1.264)) + 
  #scale_fill_gradientn(colors=c("darkgreen","green","yellow","orange","red"),values=c(-2.102,-1.051,0,0.632, 1.264)) + 
  
  coord_sf(ylim=c(-57, 80)) + 
  theme(legend.position=c(0.075, 0.25), legend.key.size=unit(0.4, "cm"))


# Now lets arrange all those plots
CairoPDF(paste0("code_analysis/analysis_output/Figures/Map_Figure.pdf"), height=30, width=15)

grid.arrange(map_61+labs(title="A"), 
             map_71+labs(title="B"), 
             map_81+labs(title="C"), 
             map_91+labs(title="D"), 
             map_01+labs(title="E"),
             ncol=2)

dev.off()


keep<-c("Country","Year","GDP","abp.kcal","pbp.kcal","fat.kcal","carb.kcal","protein.kcal")
tag<-which(names(full_data)%in%keep)
data_gam<-full_data[,tag]

melted<-melt(data_gam,id.vars=c("Country","Year","GDP"),measured.vars=c("abp.kcal","pbp.kcal","fat.kcal","carb.kcal","protein.kcal"),na.rm=TRUE)
head(melted)

colnames(melted)[4]<-"Nutrient"
 colnames(melted)[5]<-"Supply"
 glimpse(melted)
 

gamma<-log(dim(data.m)[1])/2

gam_ABP_time<-gam(abp.kcal ~ s(Year, k=50) + s(Country, bs="re"), data=data.m, gamma=gamma)
gam_PBP_time<-gam(pbp.kcal ~ s(Year, k=50) + s(Country, bs="re"), data=data.m, gamma=gamma)
gam_P_time<-gam(protein.kcal ~ s(Year, k=50) + s(Country, bs="re"), data=data.m, gamma=gamma)

gam_ABP_gdp<-gam(abp.kcal ~ s(GDP, k=50) + s(Country, bs="re"), data=data.m, gamma=gamma)
gam_PBP_gdp<-gam(pbp.kcal ~ s(GDP, k=50) + s(Country, bs="re"), data=data.m, gamma=gamma)
gam_P_gdp<-gam(protein.kcal ~ s(GDP, k=50) + s(Country, bs="re"), data=data.m, gamma=gamma)


gam_ABP_carb<-gam(abp.kcal ~ s(carb.kcal, k=50) + s(Country, bs="re"), data=data.m, gamma=gamma)
gam_PBP_carb<-gam(pbp.kcal ~ s(carb.kcal, k=50) + s(Country, bs="re"), data=data.m, gamma=gamma)
gam_P_carb<-gam(protein.kcal ~ s(carb.kcal, k=50) + s(Country, bs="re"), data=data.m, gamma=gamma)

gam_ABP_fat<-gam(abp.kcal ~ s(fat.kcal, k=50) + s(Country, bs="re"), data=data.m, gamma=gamma)
gam_PBP_fat<-gam(pbp.kcal ~ s(fat.kcal, k=50) + s(Country, bs="re"), data=data.m, gamma=gamma)
gam_P_fat<-gam(protein.kcal ~ s(fat.kcal, k=50) + s(Country, bs="re"), data=data.m, gamma=gamma)

# Tables S1-S12
sink(file=paste0("code_analysis/analysis_output/Figure_1_main_gam_summaries.txt"))

gam_ABP_time
gam_PBP_time
gam_P_time

summary(gam_ABP_time)
summary(gam_PBP_time)
summary(gam_P_time)
gam.check(gam_ABP_time)
gam.check(gam_PBP_time)
gam.check(gam_P_time)


gam_ABP_gdp
gam_PBP_gdp
gam_P_gdp

summary(gam_ABP_gdp)
summary(gam_PBP_gdp)
summary(gam_P_gdp)
gam.check(gam_ABP_gdp)
gam.check(gam_PBP_gdp)
gam.check(gam_P_gdp)

gam_ABP_carb
gam_PBP_carb
gam_P_carb

summary(gam_ABP_carb)
summary(gam_PBP_carb)
summary(gam_P_carb)
gam.check(gam_ABP_carb)
gam.check(gam_PBP_carb)
gam.check(gam_P_carb)

gam_ABP_fat
gam_PBP_fat
gam_P_fat

summary(gam_ABP_fat)
summary(gam_PBP_fat)
summary(gam_P_fat)
gam.check(gam_ABP_fat)
gam.check(gam_PBP_fat)
gam.check(gam_P_fat)


sink()

tag<-which(melted$Nutrient%in%c("abp.kcal","pbp.kcal","protein.kcal"))
melted.new<-melted[tag,]
head(melted.new)


#################### Supply ~ Time #####################

ymax<-round(max(data.m$protein.kcal,melted.new$Supply))+5

Predictors.abp.time<-data.frame(Year=c(1961:2018), Nutrient=c(rep(as.character("abp.kcal"),58)))
head(Predictors.abp.time)
prediction.abp.time<-predict(gam_ABP_time, newdata=Predictors.abp.time, exclude="s(Country)", newdata.guaranteed=T, se.fit=T, type="response")
Predictors.abp.time$y<-prediction.abp.time$fit
Predictors.abp.time$se<-prediction.abp.time$se.fit
Predictors.abp.time$l.se<-Predictors.abp.time$y - Predictors.abp.time$se * 1.96
Predictors.abp.time$u.se<-Predictors.abp.time$y + Predictors.abp.time$se * 1.96


plot_abp_time<-ggplot()+geom_point(data=melted.new[which(melted.new$Nutrient=="abp.kcal"),], aes(x=Year, y=Supply),size=0.2, alpha=0.3,color="Red") +
  theme_bw() +
  xlim(1961, 2020) +
  #guides(color=guide_legend(override.aes=list(size=4))) +
  ylim(0, ymax)


plot_abp_time<-plot_abp_time +
  geom_ribbon(data=Predictors.abp.time, aes(x=Year, y=y, ymin=l.se, ymax=u.se),fill='pink', alpha=0.5, linetype=0) +
  geom_line(data=Predictors.abp.time, aes(x=Year, y=y),color='red', size=0.5)


Predictors.pbp.time<-data.frame(Year=c(1961:2018), Nutrient=c(rep(as.character("pbp.kcal"),58)))
head(Predictors.pbp.time)
prediction.pbp.time<-predict(gam_PBP_time, newdata=Predictors.pbp.time, exclude="s(Country)", newdata.guaranteed=T, se.fit=T, type="response")
Predictors.pbp.time$y<-prediction.pbp.time$fit
Predictors.pbp.time$se<-prediction.pbp.time$se.fit
Predictors.pbp.time$l.se<-Predictors.pbp.time$y - Predictors.pbp.time$se * 1.96
Predictors.pbp.time$u.se<-Predictors.pbp.time$y + Predictors.pbp.time$se * 1.96

plot_protein_time<-plot_abp_time+geom_point(data=melted.new[which(melted.new$Nutrient=="pbp.kcal"),], aes(x=Year, y=Supply), 
                                            size=0.2, alpha=0.2,color="darkgreen") +
  labs(y="Protein Supply (kcal/capita/day)") +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=15), legend.background=element_blank()) 


plot_protein_time<-plot_protein_time+geom_line(data=Predictors.pbp.time, aes(x=Year, y=y),color='green', size=0.5)  +
  geom_ribbon(data=Predictors.pbp.time, aes(x=Year, y=y, ymin=l.se, ymax=u.se),fill='Green', alpha=0.3, linetype=0)


Predictors.protein.time<-data.frame(Year=c(1961:2018), Nutrient=c(rep(as.character("protein.kcal"),58)))
head(Predictors.protein.time)
prediction.protein.time<-predict(gam_P_time, newdata=Predictors.protein.time, exclude="s(Country)", newdata.guaranteed=T, se.fit=T, type="response")
Predictors.protein.time$y<-prediction.protein.time$fit
Predictors.protein.time$se<-prediction.protein.time$se.fit
Predictors.protein.time$l.se<-Predictors.protein.time$y - Predictors.protein.time$se * 1.96
Predictors.protein.time$u.se<-Predictors.protein.time$y + Predictors.protein.time$se * 1.96

plot_protein_time<-plot_protein_time+geom_point(data=melted.new[which(melted.new$Nutrient=="protein.kcal"),], aes(x=Year, y=Supply), 
                                                size=0.2, alpha=0.2,color="darkblue") +
  labs(y="Protein Supply (kcal/capita/day)") +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=15), legend.background=element_blank()) 


plot_protein_time<-plot_protein_time+geom_line(data=Predictors.protein.time, aes(x=Year, y=y),color='blue', size=0.5)  +
  geom_ribbon(data=Predictors.protein.time, aes(x=Year, y=y, ymin=l.se, ymax=u.se),fill='blue', alpha=0.3, linetype=0)

#################### Supply ~ CARB #####################

Predictors.abp.carb<-data.frame(carb.kcal=c(floor(min(data.m$carb.kcal)):floor(max(data.m$carb.kcal))), Nutrient=c(rep(as.character("abp.kcal"),1430)))
head(Predictors.abp.carb)
prediction.abp.carb<-predict(gam_ABP_carb, newdata=Predictors.abp.carb, exclude="s(Country)", newdata.guaranteed=T, se.fit=T, type="response")
Predictors.abp.carb$y<-prediction.abp.carb$fit
Predictors.abp.carb$se<-prediction.abp.carb$se.fit
Predictors.abp.carb$l.se<-Predictors.abp.carb$y - Predictors.abp.carb$se * 1.96
Predictors.abp.carb$u.se<-Predictors.abp.carb$y + Predictors.abp.carb$se * 1.96


plot_abp_carb<-ggplot()+geom_point(data=data.m, aes(x=carb.kcal, y=abp.kcal),size=0.2, alpha=0.3,color="red") +
  theme_bw() +
  xlim(1036, 2466) +
  #guides(color=guide_legend(override.aes=list(size=4))) +
  ylim(0, ymax)


plot_abp_carb<-plot_abp_carb+
  geom_ribbon(data=Predictors.abp.carb, aes(x=carb.kcal, y=y, ymin=l.se, ymax=u.se),fill='pink', alpha=0.5, linetype=0) +
  geom_line(data=Predictors.abp.carb, aes(x=carb.kcal, y=y),color='red', size=0.5)


Predictors.pbp.carb<-data.frame(carb.kcal=c(floor(min(data.m$carb.kcal)):floor(max(data.m$carb.kcal))), Nutrient=c(rep(as.character("pbp.kcal"),1430)))
head(Predictors.pbp.carb)
prediction.pbp.carb<-predict(gam_PBP_carb, newdata=Predictors.pbp.carb, exclude="s(Country)", newdata.guaranteed=T, se.fit=T, type="response")
Predictors.pbp.carb$y<-prediction.pbp.carb$fit
Predictors.pbp.carb$se<-prediction.pbp.carb$se.fit
Predictors.pbp.carb$l.se<-Predictors.pbp.carb$y - Predictors.pbp.carb$se * 1.96
Predictors.pbp.carb$u.se<-Predictors.pbp.carb$y + Predictors.pbp.carb$se * 1.96

plot_protein_carb<-plot_abp_carb+geom_point(data=data.m, aes(y=pbp.kcal, x=carb.kcal), 
                                            size=0.2, alpha=0.2,color="darkgreen") +
  labs(y="Protein Supply (kcal/capita/day)", x="Carbohydrate Supply (kcal/capita/day)") +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=15), legend.background=element_blank()) 


plot_protein_carb<-plot_protein_carb+geom_line(data=Predictors.pbp.carb, aes(x=carb.kcal, y=y),color='Darkgreen', size=0.5)  +
  geom_ribbon(data=Predictors.pbp.carb, aes(x=carb.kcal, y=y, ymin=l.se, ymax=u.se),fill='Green', alpha=0.3, linetype=0)

Predictors.protein.carb<-data.frame(carb.kcal=c(floor(min(data.m$carb.kcal)):floor(max(data.m$carb.kcal))), Nutrient=c(rep(as.character("protein.kcal"),1430)))
head(Predictors.protein.carb)
prediction.protein.carb<-predict(gam_P_carb, newdata=Predictors.protein.carb, exclude="s(Country)", newdata.guaranteed=T, se.fit=T, type="response")
Predictors.protein.carb$y<-prediction.protein.carb$fit
Predictors.protein.carb$se<-prediction.protein.carb$se.fit
Predictors.protein.carb$l.se<-Predictors.protein.carb$y - Predictors.protein.carb$se * 1.96
Predictors.protein.carb$u.se<-Predictors.protein.carb$y + Predictors.protein.carb$se * 1.96

plot_protein_carb<-plot_protein_carb+geom_point(data=data.m, aes(y=protein.kcal, x=carb.kcal), 
                                                size=0.2, alpha=0.2,color="darkblue") +
  labs(y="Protein Supply (kcal/capita/day)", x="Carbohydrate Supply (kcal/capita/day)") +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=15), legend.background=element_blank()) 


plot_protein_carb<-plot_protein_carb+geom_line(data=Predictors.protein.carb, aes(x=carb.kcal, y=y),color='Darkblue', size=0.5)  +
  geom_ribbon(data=Predictors.protein.carb, aes(x=carb.kcal, y=y, ymin=l.se, ymax=u.se),fill='blue', alpha=0.3, linetype=0)

#################### Supply ~ FAT #####################
fat.kcal=c(floor(min(data.m$fat.kcal)):floor(max(data.m$fat.kcal)))
Predictors.abp.fat<-data.frame(fat.kcal=c(floor(min(data.m$fat.kcal)):floor(max(data.m$fat.kcal))), Nutrient=c(rep(as.character("abp.kcal"),length(fat.kcal))))
head(Predictors.abp.fat)
prediction.abp.fat<-predict(gam_ABP_fat, newdata=Predictors.abp.fat, exclude="s(Country)", newdata.guaranteed=T, se.fit=T, type="response")
Predictors.abp.fat$y<-prediction.abp.fat$fit
Predictors.abp.fat$se<-prediction.abp.fat$se.fit
Predictors.abp.fat$l.se<-Predictors.abp.fat$y - Predictors.abp.fat$se * 1.96
Predictors.abp.fat$u.se<-Predictors.abp.fat$y + Predictors.abp.fat$se * 1.96


plot_abp_fat<-ggplot()+geom_point(data=data.m, aes(x=fat.kcal, y=abp.kcal),size=0.2, alpha=0.3,color="red") +
  theme_bw() +
  xlim(207, 1610) +
  #guides(color=guide_legend(override.aes=list(size=4))) +
  ylim(0, ymax)


plot_abp_fat<-plot_abp_fat+
  geom_ribbon(data=Predictors.abp.fat, aes(x=fat.kcal, y=y, ymin=l.se, ymax=u.se),fill='pink', alpha=0.5, linetype=0) +
  geom_line(data=Predictors.abp.fat, aes(x=fat.kcal, y=y),color='red', size=0.5)


Predictors.pbp.fat<-data.frame(fat.kcal=c(floor(min(data.m$fat.kcal)):floor(max(data.m$fat.kcal))), Nutrient=c(rep(as.character("pbp.kcal"),length(fat.kcal))))
head(Predictors.pbp.fat)
prediction.pbp.fat<-predict(gam_PBP_fat, newdata=Predictors.pbp.fat, exclude="s(Country)", newdata.guaranteed=T, se.fit=T, type="response")
Predictors.pbp.fat$y<-prediction.pbp.fat$fit
Predictors.pbp.fat$se<-prediction.pbp.fat$se.fit
Predictors.pbp.fat$l.se<-Predictors.pbp.fat$y - Predictors.pbp.fat$se * 1.96
Predictors.pbp.fat$u.se<-Predictors.pbp.fat$y + Predictors.pbp.fat$se * 1.96

plot_protein_fat<-plot_abp_fat+geom_point(data=data.m, aes(y=pbp.kcal, x=fat.kcal), 
                                          size=0.2, alpha=0.2,color="darkgreen") +
  labs(y="Protein Supply (kcal/capita/day)", x="Fat Supply (kcal/capita/day)") +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=15), legend.background=element_blank()) 


plot_protein_fat<-plot_protein_fat+geom_line(data=Predictors.pbp.fat, aes(x=fat.kcal, y=y),color='Darkgreen', size=0.5)  +
  geom_ribbon(data=Predictors.pbp.fat, aes(x=fat.kcal, y=y, ymin=l.se, ymax=u.se),fill='Green', alpha=0.3, linetype=0)

Predictors.protein.fat<-data.frame(fat.kcal=c(floor(min(data.m$fat.kcal)):floor(max(data.m$fat.kcal))), Nutrient=c(rep(as.character("protein.kcal"),length(fat.kcal))))
head(Predictors.protein.fat)
prediction.protein.fat<-predict(gam_P_fat, newdata=Predictors.protein.fat, exclude="s(Country)", newdata.guaranteed=T, se.fit=T, type="response")
Predictors.protein.fat$y<-prediction.protein.fat$fit
Predictors.protein.fat$se<-prediction.protein.fat$se.fit
Predictors.protein.fat$l.se<-Predictors.protein.fat$y - Predictors.protein.fat$se * 1.96
Predictors.protein.fat$u.se<-Predictors.protein.fat$y + Predictors.protein.fat$se * 1.96

plot_protein_fat<-plot_protein_fat+geom_point(data=data.m, aes(y=protein.kcal, x=fat.kcal), 
                                              size=0.2, alpha=0.2,color="darkblue") +
  labs(y="Protein Supply (kcal/capita/day)", x="Fat Supply (kcal/capita/day)") +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=15), legend.background=element_blank()) 


plot_protein_fat<-plot_protein_fat+geom_line(data=Predictors.protein.fat, aes(x=fat.kcal, y=y),color='Darkblue', size=0.5)  +
  geom_ribbon(data=Predictors.protein.fat, aes(x=fat.kcal, y=y, ymin=l.se, ymax=u.se),fill='blue', alpha=0.3, linetype=0)

#################### Supply ~ GDP #####################

Predictors.abp.gdp<-data.frame(GDP=c(min(melted.new$GDP):max(melted.new$GDP)), Nutrient=c(rep(as.character("abp.kcal"), 83478)))

head(Predictors.abp.gdp)
prediction.abp.gdp<-predict(gam_ABP_gdp, newdata=Predictors.abp.gdp, exclude="s(Country)", newdata.guaranteed=T, se.fit=T, type="response")
Predictors.abp.gdp$y<-prediction.abp.gdp$fit
Predictors.abp.gdp$se<-prediction.abp.gdp$se.fit
Predictors.abp.gdp$l.se<-Predictors.abp.gdp$y - Predictors.abp.gdp$se * 1.96
Predictors.abp.gdp$u.se<-Predictors.abp.gdp$y + Predictors.abp.gdp$se * 1.96


plot_abp_gdp<-ggplot(data=melted.new[which(melted.new$Nutrient=="abp.kcal"),], aes(x=GDP, y=Supply)) + 
  geom_point(size=0.2, alpha=0.3,color="red") +
  theme_bw() +
  xlim(1100, 84600) +
  ylim(0, ymax)


plot_abp_gdp<-plot_abp_gdp+
  geom_ribbon(data=Predictors.abp.gdp, aes(x=GDP, y=y, ymin=l.se, ymax=u.se),fill="Pink", alpha=0.5, linetype=0) +
  geom_line(data=Predictors.abp.gdp, aes(x=GDP, y=y),color="Red", size=0.5)


Predictors.pbp.gdp<-data.frame(GDP=c(min(melted.new$GDP):max(melted.new$GDP)), Nutrient=c(rep(as.character("pbp.kcal"), 83478)))

head(Predictors.pbp.gdp)
prediction.pbp.gdp<-predict(gam_PBP_gdp, newdata=Predictors.pbp.gdp, exclude="s(Country)", newdata.guaranteed=T, se.fit=T, type="response")
Predictors.pbp.gdp$y<-prediction.pbp.gdp$fit
Predictors.pbp.gdp$se<-prediction.pbp.gdp$se.fit
Predictors.pbp.gdp$l.se<-Predictors.pbp.gdp$y - Predictors.pbp.gdp$se * 1.96
Predictors.pbp.gdp$u.se<-Predictors.pbp.gdp$y + Predictors.pbp.gdp$se * 1.96

plot_protein_gdp<-plot_abp_gdp+geom_point(data=melted.new[which(melted.new$Nutrient=="pbp.kcal"),], aes(x=GDP, y=Supply),size=0.2, alpha=0.2,color="darkgreen") +
  labs(y="Protein Supply (kcal/capita/day)") +
  geom_ribbon(data=Predictors.pbp.gdp, aes(x=GDP, y=y, ymin=l.se, ymax=u.se),fill="green", alpha=0.3, linetype=0) +
  geom_line(data=Predictors.pbp.gdp, aes(x=GDP, y=y),color='Darkgreen', size=0.5)



plot_pbp_gdp<-ggplot(data=melted.new[which(melted.new$Nutrient=="pbp.kcal"),], aes(x=GDP, y=Supply)) + 
  geom_point(size=0.2, alpha=0.3,color="green") +
  theme_bw() +
  xlim(1100, 84600) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=15), legend.background=element_blank()) +
  theme(legend.position=c(0.5, 0.9), legend.key.size=unit(0.4, "cm"), legend.text=element_text(size=12), legend.title=element_text(size=15), legend.direction="horizontal", title=element_text(size=15)) +
  labs(color="Plant-Based Protein", y="Supply kcal/capita/day") +
  guides(color=guide_legend(override.aes=list(size=4))) +
  ylim(0, round(max(melted.new$Supply)+5))


plot_pbp_gdp<-plot_pbp_gdp+
  geom_ribbon(data=Predictors.pbp.gdp, aes(x=GDP, y=y, ymin=l.se, ymax=u.se),fill='Green', alpha=0.15, linetype=0) +
  geom_line(data=Predictors.pbp.gdp, aes(x=GDP, y=y),color='Darkgreen', size=0.5) + theme(legend.position='none')+labs(color="", title="PBP ~ GDP + re(Country)")


Predictors.protein.gdp<-data.frame(GDP=c(min(melted.new$GDP):max(melted.new$GDP)), Nutrient=c(rep(as.character("protein.kcal"), 83478)))

head(Predictors.protein.gdp)
prediction.protein.gdp<-predict(gam_P_gdp, newdata=Predictors.protein.gdp, exclude="s(Country)", newdata.guaranteed=T, se.fit=T, type="response")
Predictors.protein.gdp$y<-prediction.protein.gdp$fit
Predictors.protein.gdp$se<-prediction.protein.gdp$se.fit
Predictors.protein.gdp$l.se<-Predictors.protein.gdp$y - Predictors.protein.gdp$se * 1.96
Predictors.protein.gdp$u.se<-Predictors.protein.gdp$y + Predictors.protein.gdp$se * 1.96

plot_protein_gdp<-plot_protein_gdp+geom_point(data=melted.new[which(melted.new$Nutrient=="protein.kcal"),], aes(x=GDP, y=Supply),size=0.2, alpha=0.2,color="darkblue") +
  labs(y="Protein Supply (kcal/capita/day)") +
  geom_ribbon(data=Predictors.protein.gdp, aes(x=GDP, y=y, ymin=l.se, ymax=u.se),fill="blue", alpha=0.3, linetype=0) +
  geom_line(data=Predictors.protein.gdp, aes(x=GDP, y=y),color='Darkblue', size=0.5)+
  theme(axis.text=element_text(size=15), axis.title=element_text(size=15), legend.background=element_blank()) 



plot_totalprotein_gdp<-ggplot(data=melted.new[which(melted.new$Nutrient=="protein.kcal"),], aes(x=GDP, y=Supply)) + 
  geom_point(size=0.2, alpha=0.3,color="blue") +
  theme_bw() +
  xlim(1100, 84600) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=15), legend.background=element_blank()) +
  theme(legend.position=c(0.5, 0.9), legend.key.size=unit(0.4, "cm"), legend.text=element_text(size=12), legend.title=element_text(size=15), legend.direction="horizontal", title=element_text(size=15)) +
  labs(color="Total Protein", y="Supply kcal/capita/day") +
  guides(color=guide_legend(override.aes=list(size=4))) +
  ylim(0, round(max(melted.new$Supply)+5))


plot_totalprotein_gdp<-plot_totalprotein_gdp+
  geom_ribbon(data=Predictors.protein.gdp, aes(x=GDP, y=y, ymin=l.se, ymax=u.se),fill='blue', alpha=0.15, linetype=0) +
  geom_line(data=Predictors.protein.gdp, aes(x=GDP, y=y),color='Darkblue', size=0.5) + theme(legend.position='none')+labs(color="", title="protein ~ GDP + re(Country)")




ylab<-ggdraw()+draw_label("Supply (kcal/capita/day)",vjust=0.5,size=20,angle = 90)
leg<-get_legend(plot_protein_gdp)

plot_protein_time<-plot_protein_time+labs(y=NULL,title=NULL,subtitle=NULL)+ theme(legend.position = "none")
plot_protein_gdp<-plot_protein_gdp+labs(y=NULL,title=NULL,subtitle=NULL)+ theme(legend.position = "none")
plot_protein_carb<-plot_protein_carb+labs(y=NULL,title=NULL,subtitle=NULL)+ theme(legend.position = "none")
plot_protein_fat<-plot_protein_fat+labs(y=NULL,title=NULL,subtitle=NULL)+ theme(legend.position = "none")



# Now lets arrange all those plots
CairoPDF(paste0("code_analysis/analysis_output/Figures/Figure_1_supply.pdf"), height=20, width=20)


ggdraw(plot_grid(leg,plot_grid(ylab,
                               plot_grid(plot_grid(plot_protein_time,NULL,plot_protein_gdp,NULL,ncol=4,rel_widths=c(1,0.1,1,0.05)),plot_grid(plot_protein_fat,NULL,plot_protein_carb,NULL,ncol=4,rel_widths=c(1,0.1,1,0.05)),nrow=2,rel_widths = c(1,1)),ncol=2,rel_widths = c(0.1,1.2)),NULL,nrow=3,rel_heights = c(0.2,1,0.05)))

dev.off()




