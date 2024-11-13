# Clean up 
rm(list=ls())

source("code_analysis/ggSurface.R")
source("code_analysis/inhull.R")


# Load libraries
library(arm)
#library(plyr)
library(ggplot2)
library(lmerTest)
library(countrycode)
library(rnaturalearth)
library(mgcv)
library(gridExtra)
library(MortalityLaws)
library(Cairo)
library(doSNOW)
library(readr)
library(dplyr)
library(lme4)
library(mice)
library(miceadds)
library(mitml)
library(GGally)

# read in the datasets 

data.5m<-read.csv("data_cleaned/5mdata.csv", stringsAsFactors=T, header = TRUE)
data.60m<-read.csv("data_cleaned/60mdata.csv", stringsAsFactors=T, header = TRUE)

data.5f<-read.csv("data_cleaned/5fdata.csv", stringsAsFactors=T, header = TRUE)
data.60f<-read.csv("data_cleaned/60fdata.csv", stringsAsFactors=T, header = TRUE)

dataset<-list(data.5m,data.60m,data.5f,data.60f)
names(dataset)<-c('5m','60m','5f','60f')


# Read in the cleaned LT data
LT_data<-read.csv("data_cleaned/Clean_LT_with_POP.csv")
glimpse(LT_data)


# Change sex col to factor from int 
LT_data$Sex<-as.factor(LT_data$Sex)

#create LT_ID col as country_year
LT_data$LT_ID<-paste(LT_data$Country, LT_data$Year, sep="_")

#remove columns with age 0
LT_data<-LT_data[-which(LT_data$Age == 0 ),]
#LT_data<-LT_data[-which(LT_data$Age == 110 ),]

# create sex_ID col and assign as country_year_sex
LT_data$sex_ID<-paste0(LT_data$LT_ID, "_", LT_data$Sex)
glimpse(LT_data)

# NOT SURE WHY WE ARE SUMMING SEX FACTOR??????>>>>
check<-plyr::ddply(LT_data, plyr::.(sex_ID), summarise, age_check=sum(is.na(match(Age, c(5, 60))) == F))
check[which(check$age_check!=2),]

# create data frame with age 5 rows only
LT_5<-LT_data[which(LT_data$Age == 5 ),]
# create data frame with age 60 only
LT_60<-LT_data[which(LT_data$Age == 60 ),]

# create age 5 and age 6 male data frames
LT_males5<-LT_5[which(LT_5$Sex == 1),]
LT_males60<-LT_60[which(LT_60$Sex == 1),]

# create age 5 and age 60 female data frames
LT_females5<-LT_5[which(LT_5$Sex == 2),]
LT_females60<-LT_60[which(LT_60$Sex == 2),]

glimpse(LT_males5)

# read in the cleaned Nutrition data
FB_data<-read.csv("data_cleaned/Clean_FBS.csv")
glimpse(FB_data)

# check there is an ID col called Food_ID as country_year otherwise create and assign
FB_data$Food_ID<-paste0(FB_data$Country, "_", FB_data$Year)


# Lets add in the GDP predictors from Maddison project
SES_mad<-read.table("data_cleaned/Clean_GDP_Mad.csv",sep=",",header=T)
glimpse(SES_mad)


# remove data earlier than 1961 
w<-which(SES_mad$Year < 1961)
SES_mad<-SES_mad[-w,]
glimpse(SES_mad)

# assign ID cols that match iD cols in FBS and LT datasets i.e. country_year
SES_mad$SES_ID<-paste0(SES_mad$Country, "_", SES_mad$Year)


# find all IDs for LT, FBS, MPD data
All_ID_m5<-unique(c(LT_males5$LT_ID, FB_data$Food_ID, SES_mad$SES_ID))
All_ID_f5<-unique(c(LT_females5$LT_ID, FB_data$Food_ID, SES_mad$SES_ID))
All_ID_m60<-unique(c(LT_males60$LT_ID, FB_data$Food_ID, SES_mad$SES_ID))
All_ID_f60<-unique(c(LT_females60$LT_ID, FB_data$Food_ID, SES_mad$SES_ID))

# Create a data frame
all_males5<-data.frame(ID=All_ID_m5)
all_males5$Country<-unlist(lapply(strsplit(as.character(all_males5$ID), "_"), "[[", 1))
all_males5$Year<-unlist(lapply(strsplit(as.character(all_males5$ID), "_"), "[[", 2))
glimpse(all_males5)

# Add in the population data
all_males5<-cbind(all_males5, LT_data[match(all_males5$ID, LT_data$LT_ID), c( "seS_t")])
names(all_males5)[dim(all_males5)[2]]<-"seS_t"
glimpse(all_males5)

# Add in the FB data
all_males5<-cbind(all_males5, FB_data[match(all_males5$ID, FB_data$Food_ID), c("Protein_Total_2901_g","Animal_Protein_2941_kcal", "Carbohydrate_kcal", "Fat_Total_2901_kcal", "Vegetal_Protein_2903_kcal")])
glimpse(all_males5)

# Add in the GDP data from the Maddison Project
all_males5<-cbind(all_males5, SES_mad[match(all_males5$ID, SES_mad$SES_ID), c("GDP")])
names(all_males5)[dim(all_males5)[2]]<-"GDP_perCapita"
glimpse(all_males5)

# MPD data appears to be as value of 10 000 000? # comment written on 18/12/23

rownames(all_males5)<-NULL


# Create a data frame
all_males60<-data.frame(ID=All_ID_m60)
all_males60$Country<-unlist(lapply(strsplit(as.character(all_males60$ID), "_"), "[[", 1))
all_males60$Year<-unlist(lapply(strsplit(as.character(all_males60$ID), "_"), "[[", 2))
glimpse(all_males60)

# Add in the population data
all_males60<-cbind(all_males60, LT_data[match(all_males60$ID, LT_data$LT_ID), c( "seS_t")])
names(all_males60)[dim(all_males60)[2]]<-"seS_t"
glimpse(all_males60)

# Add in the FB data
all_males60<-cbind(all_males60, FB_data[match(all_males60$ID, FB_data$Food_ID),c("Protein_Total_2901_g","Animal_Protein_2941_kcal", "Carbohydrate_kcal", "Fat_Total_2901_kcal", "Vegetal_Protein_2903_kcal")])
glimpse(all_males60)

# Add in the GDP data from the Maddison Project
all_males60<-cbind(all_males60, SES_mad[match(all_males60$ID, SES_mad$SES_ID), c("GDP")])
names(all_males60)[dim(all_males60)[2]]<-"GDP_perCapita"
glimpse(all_males60)

rownames(all_males60)<-NULL

# Create a copy for the females
all_females5<-all_males5
all_females60<-all_males60

# Add in the sex to each data frame
all_males5$Sex<-"Males"
all_females5$Sex<-"Females"
all_males60$Sex<-"Males"
all_females60$Sex<-"Females"

# Now match to the sex/age specific lx
all_males5_match<-cbind(all_males5, LT_males5[match(all_males5$ID, LT_males5$LT_ID), c("lx")])
names(all_males5_match)[dim(all_males5_match)[2]]<-"l5"
glimpse(all_males5_match)
all_females5_match<-cbind(all_females5, LT_females5[match(all_females5$ID, LT_females5$LT_ID), c("lx")])
names(all_females5_match)[dim(all_females5_match)[2]]<-"l5"
glimpse(all_females5_match)

all_males<-cbind(all_males5_match, LT_males60[match(all_males5_match$ID, LT_males60$LT_ID), c("lx")])
names(all_males)[dim(all_males)[2]]<-"l60"
glimpse(all_males)


all_females<-cbind(all_females5_match, LT_females60[match(all_females5_match$ID, LT_females60$LT_ID), c("lx")])
names(all_females)[dim(all_females)[2]]<-"l60"
glimpse(all_females)


# Sort by country and year
all_males<-all_males[order(all_males$Country, all_males$Year),]
all_females<-all_females[order(all_females$Country, all_females$Year),]

glimpse(all_females)
glimpse(all_males)

#remove all rows without l5 or l60 data
tag<-which(is.na(all_males$l5)==TRUE | is.na(all_males$l60)==TRUE)
length(tag)
dim(all_males)
all_males<-all_males[-tag,]
tag<-which(is.na(all_females$l5)==TRUE| is.na(all_females$l60)==TRUE)
all_females<-all_females[-tag,]
dim(all_females)

remove.pop<-which(is.na(all_males$seS_t)==T)
all_males<-all_males[-remove.pop,]
remove.pop<-which(is.na(all_females$seS_t)==T)

length(remove.pop)
all_females<-all_females[-remove.pop,]

remove.row<-which(is.na(all_males$Animal_Protein_2941_kcal)==T & is.na(all_males$Carbohydrate_kcal)==T & is.na(all_males$Fat_Total_2901_kcal)==T & is.na(all_males$GDP_perCapita)==T)
all_males<-all_males[-remove.row,]
remove.row<-which(is.na(all_females$Animal_Protein_2941_kcal)==T & is.na(all_females$Carbohydrate_kcal)==T & is.na(all_females$Fat_Total_2901_kcal)==T & is.na(all_females$GDP_perCapita)==T )

length(remove.row)
all_females<-all_females[-remove.row,]


all_males$Sex<-"Males"
all_females$Sex<-'Females'
all_data<-rbind(all_males,all_females)
glimpse(all_data)

# reformat to wide format
wide_dat<-data.frame(ID = unique(all_data$ID))
wide_dat<-cbind(wide_dat, all_data[match(wide_dat$ID, all_data$ID), c(2:10)])
glimpse(wide_dat)
males<-all_data[which(all_data$Sex == "Males"),]
females<-all_data[which(all_data$Sex == "Females"),]

wide_dat<-cbind(wide_dat, males[match(wide_dat$ID, males$ID), c(12:13)])
glimpse(wide_dat)
names(wide_dat)[c(11:12)]<-c("l5_m", "l60_m")
wide_dat<-cbind(wide_dat, females[match(wide_dat$ID, females$ID), c(12:13)])
names(wide_dat)[c(13:14)]<-c("l5_f", "l60_f")

names(wide_dat)

md.pattern(wide_dat)
summary(wide_dat)
dim(wide_dat)


# Log the GDP
wide_dat[,10]<-log(wide_dat[,10])
glimpse(wide_dat)

# Log the seS_t data column (+ve skew/long right tail)
wide_dat[,4]<-log(wide_dat[,4])
glimpse(wide_dat)

# save the means and sds, which will be needed to backtransform
mus<-apply(wide_dat[,-c(1:3)], 2, mean, na.rm=T)
sds<-apply(wide_dat[,-c(1:3)], 2, sd, na.rm=T)

# and standardise
sdat<-wide_dat
sdat[,-c(1:3)]<-apply(sdat[,-c(1:3)], 2, scale)
sdat$Country<-as.numeric(as.factor(sdat$Country))

dim(sdat)
md.pattern(sdat)

# ggpairs(sdat)

# set up predictor matrix and imputation methods
pred_matrix <- make.predictorMatrix(sdat)
imp_method <- make.method(sdat)

# slope and random slope (2)
# think this is fine for all
pred_matrix[ , "Year"] <- 2

# Setting as 3 will include cluster mean 
pred_matrix[,  c("Protein_Total_2901_g","Animal_Protein_2941_kcal", "Carbohydrate_kcal", "Fat_Total_2901_kcal","Vegetal_Protein_2903_kcal", "GDP_perCapita", "seS_t","l5_m", "l60_m", "l5_f", "l60_f")] <- 3

# # our cluster (-2)
pred_matrix[, "Country"] <- -2

# stting 0 for non-missing data
no_missing <- c("ID", "Country", "Year")
pred_matrix[no_missing, ] <- 0
pred_matrix[, "ID"] <- 0

# also put 0 for diag
diag(pred_matrix) <- 0

# checking
pred_matrix


# setting methods 
imp_method[c("Protein_Total_2901_g","Animal_Protein_2941_kcal", "Carbohydrate_kcal", "Fat_Total_2901_kcal","Vegetal_Protein_2903_kcal", "GDP_perCapita", "seS_t", "l5_m", "l60_m", "l5_f", "l60_f")] <- "2l.pmm" # obs level
imp_method

# checking
# needs fast computer!!!!
m<-50
imp <- mice(sdat, 
            m = m, 
            maxit = 20,
            method = imp_method,
            predictorMatrix = pred_matrix)
xyplot(imp,GDP_perCapita ~ Year+Country+seS_t,pch=18,cex=1)
# look what happens
# looks mostly good
# densityplot(imp, ~ Animal_Protein_2941_kcal) 
# densityplot(imp, ~ Carbohydrate_kcal) 
# densityplot(imp, ~ Fat_Total_2901_kcal)
# densityplot(imp, ~ Vegetal_Protein_2903_kcal)
# densityplot(imp, ~ GDP_perCapita)
# densityplot(imp, ~ seS_t)
# densityplot(imp, ~ l5_m)
# densityplot(imp, ~ l60_m)
# densityplot(imp, ~ l5_f)
# densityplot(imp, ~ l60_f)


# some are good but not others
# plot(imp) 

# get imputed data as a list
# mean of these imputed will be probably match with the best estimate
GDP <- mids2mitml.list(imp)

# Average over the lists
set_i<-GDP[[1]][,-c(1:3)] * (1/m)
for(i in 2:m){
  set_i<-set_i + GDP[[i]][,-c(1:3)] * (1/m)
}

# Unscale
for(i in 1:dim(set_i)[2]){
  set_i[,i]<-set_i[,i] * sds[i] + mus[i]
}

# Unlog the GDP
set_i[,c("GDP_perCapita", "seS_t")]<-exp(set_i[,c("GDP_perCapita", "seS_t")])

glimpse(set_i)
# Add back in
wide_dat[,-c(1:3)]<-set_i
glimpse(wide_dat)
males<-wide_dat[,-c(13,14)]
females<-wide_dat[,-c(11,12)]
names(males)[c(11,12)]<-c("l5", "l60")
names(females)[c(11,12)]<-c("l5", "l60")
males$Sex<-"Males"
females$Sex<-"Females"


# Check that the original complete obs match the imputed
full_imp<-rbind(males, females)
glimpse(full_imp)
plot(full_imp$Animal_Protein_2941_kcal, all_data$Animal_Protein_2941_kcal)
plot(full_imp$GDP_perCapita, all_data$GDP_perCapita)
# Yep


write.table(full_imp, file="data_cleaned/imputed_data.csv", sep=",", row.names=F, col.names=names(full_imp))

