

# Load libraries
library(dplyr)
library(countrycode)
library(prodlim) # for row.match() function

library(mgcv)
library(arm)
library(ggplot2)

# all normalised 1961-2013 data and 2010-2020 data was downloaded from FAOSTAT
# Rows from both datasets with matching values for the below columns were extracted and given a final "Value" that is the mean of the reported values from the new and old datasets
# c( "Area.Code",  "Area" , "Item.Code"  ,  "Item" ,   "Element.Code", "Element" ,  "Year.Code" , "Year", "Unit" ))
# Rows in the 1961-2013 and 2010-2020 datasets with no match in the merged dataset were extracted
# Extracted rows from 1961-2013 and 2010-2020 datasets were then combined with merged dataset using rbind()
# The combined dataset was split into 3 subsets based on "Element" or "Element.Code"
# Food supply subset contains all rows where Element == "Food Supply (kcal/capita/dat)" OR Element.Code == 664
# Protein supply subset contains all rows where Element == "Protein Supply (g/capita/dat)" OR Element.Code == 674
# Fat supply subset contains all rows where Element == "Fat Supply (g/capita/dat)" OR Element.Code == 684

# Read in raw FAOSTAT data subsets

df.food<-read.csv("data_read/Food_supply_(kcal_capita_day)_all_normalized.csv",header=TRUE)
head(df.food)
unique(df.food$Year)
unique(df.food$Year)[order(unique(df.food$Year))]


df.protein<-read.csv("data_read/Protein_supply_(g_capita_day)_all_normalized.csv",header=TRUE)
head(df.protein)
unique(df.protein$Year)
unique(df.protein$Item)


df.fat<-read.csv("data_read/Fat_supply_(g_capita_day)_all_normalized.csv",header=TRUE)
head(df.fat)
dim(df.fat)
unique(df.fat$Year)
unique(df.fat$Item)


df.food$item_code<-paste0(df.food$Item,"_",df.food$Item.Code)

items.food<-unique(df.food$item_code)
df.food$area_year<-paste0(df.food$Area,"_",df.food$Year)

# "Alcoholic Beverages_2924" is an Item Group
# "Beverages, Alcoholic_2658", "Beverages, Fermented_2657", "Wine_2655", "Beer_2656" are items within this group

# We will keep the "Grand Total_2901" and "Alcoholic Beverages_2924" rows from the food supply data subset

tag.total.food<-which(df.food$item_code=="Grand Total_2901")
df.food.total<-df.food[tag.total.food,c("Area","Year","Value")]
names(df.food.total)[3]<-"Grand_Total_2901_kcal"
dim(df.food.total)
head(df.food.total)

tag.alcohol.food<-which(df.food$item_code=="Alcoholic Beverages_2924")
df.food.alcohol<-df.food[tag.alcohol.food,c("Area","Year","Value")]
names(df.food.alcohol)[3]<-"Alcoholic_Beverages_2924_kcal"
dim(df.food.alcohol)
head(df.food.alcohol)

# join values for total and alcohol kcal/capita/day
df.join.food<-full_join(df.food.total,df.food.alcohol,by=c("Area","Year"))
head(df.join.food)
dim(df.join.food)


# Filter Protein Supply data
df.protein$item_code<-paste0(df.protein$Item,"_",df.protein$Item.Code)

items.protein<-unique(df.protein$item_code)

#df.protein$area_year<-paste0(df.protein$Area,"_",df.protein$Year)

# We will keep the "Grand Total_2901", "Vegetal Products_2903", and "Animal Products_2941" rows from the protein supply data subset

tag.total.protein<-which(df.protein$item_code=="Grand Total_2901")
df.protein.total<-df.protein[tag.total.protein,c("Area","Year","Value")]
names(df.protein.total)[3]<-"Protein_Total_2901_g"
dim(df.protein.total)
head(df.protein.total)

tag.vegetal.protein<-which(df.protein$item_code=="Vegetal Products_2903")
df.protein.vegetal<-df.protein[tag.vegetal.protein,c("Area","Year","Value")]
names(df.protein.vegetal)[3]<-"Vegetal_Protein_2903_g"
dim(df.protein.vegetal)
head(df.protein.vegetal)

# join values for total and vegetal g/capita/day
df.join.protein<-full_join(df.protein.total,df.protein.vegetal,by=c("Area","Year"))
head(df.join.protein)
dim(df.join.protein)

tag.animal.protein<-which(df.protein$item_code=="Animal Products_2941")
df.protein.animal<-df.protein[tag.animal.protein,c("Area","Year","Value")]
names(df.protein.animal)[3]<-"Animal_Protein_2941_g"
dim(df.protein.animal)
head(df.protein.animal)

# add values for animal g/capita/day
df.join.protein<-full_join(df.join.protein,df.protein.animal,by=c("Area","Year"))
head(df.join.protein)
dim(df.join.protein)


# Calculate protein kcal
df.protein.kcal<-df.join.protein[,3:5]*4
names(df.protein.kcal)<-c("Protein_Total_2901_kcal", "Vegetal_Protein_2903_kcal", "Animal_Protein_2941_kcal")
head(df.protein.kcal)

df.protein.all<-cbind(df.join.protein,df.protein.kcal)
head(df.protein.all)

# Filter Fat Supply data
df.fat$item_code<-paste0(df.fat$Item,"_",df.fat$Item.Code)

items.fat<-unique(df.fat$item_code)

# We will keep the "Grand Total_2901", "Vegetal Products_2903" and "Animal Products_2941" rows from the fat supply data subset

tag.total.fat<-which(df.fat$item_code=="Grand Total_2901")
df.fat.total<-df.fat[tag.total.fat,c("Area","Year","Value")]
names(df.fat.total)[3]<-"Fat_Total_2901_g"
dim(df.fat.total)
head(df.fat.total)
tail(df.fat.total)

tag.vegetal.fat<-which(df.fat$item_code=="Vegetal Products_2903")
df.fat.vegetal<-df.fat[tag.vegetal.fat,c("Area","Year","Value")]
names(df.fat.vegetal)[3]<-"Vegetal_Fat_2903_g"
dim(df.fat.vegetal)
head(df.fat.vegetal)
tail(df.fat.vegetal)

# join values for total and vegetal g/capita/day
df.join.fat<-full_join(df.fat.total,df.fat.vegetal,by=c("Area","Year"))
head(df.join.fat)
dim(df.join.fat)
tail(df.join.fat)

tag.animal.fat<-which(df.fat$item_code=="Animal Products_2941")
df.fat.animal<-df.fat[tag.animal.fat,c("Area","Year","Value")]
names(df.fat.animal)[3]<-"Animal_Fat_2941_g"
dim(df.fat.animal)
head(df.fat.animal)
tail(df.fat.animal)

# add values for animal g/capita/day
df.join.fat<-full_join(df.join.fat,df.fat.animal,by=c("Area","Year"))
head(df.join.fat)
dim(df.join.fat)
tail(df.join.fat)

df.fat.kcal<-df.join.fat[,3:5]*9
names(df.fat.kcal)<-c("Fat_Total_2901_kcal", "Vegetal_Fat_2903_kcal", "Animal_Fat_2941_kcal")
df.fat.kcal$Prop_Fat_animal<-df.fat.kcal$Animal_Fat_2941_kcal/df.fat.kcal$Fat_Total_2901_kcal
df.fat.kcal$Prop_Fat_vegetal<-df.fat.kcal$Vegetal_Fat_2903_kcal/df.fat.kcal$Fat_Total_2901_kcal
head(df.fat.kcal)

df.fat.all<-cbind(df.join.fat,df.fat.kcal)
tail(df.fat.all)
dim(df.fat.all)

# combine all subsets

df.join.food.protein<-full_join(df.join.food,df.protein.all,by=c("Area","Year"))
dim(df.join.food.protein)
head(df.join.food.protein)
tail(df.join.food.protein)

df.join.all<-full_join(df.join.food.protein,df.fat.all,by=c("Area","Year"))
dim(df.join.all)
head(df.join.all)
tail(df.join.all)

# check for NAs and assign as 0
#tag.na.protein<-which(is.na(df.join.all$Protein_Total_2901_kcal)==TRUE)
#tag.na.protein
#tag.na.fat<-which(is.na(df.join.all$Fat_Total_2901_kcal)==TRUE)
#tag.na.fat
tag.na.alcohol<-which(is.na(df.join.all$Alcoholic_Beverages_2924_kcal)==TRUE)
tag.na.alcohol
df.join.all$Alcoholic_Beverages_2924_kcal[tag.na.alcohol]<-0

# Calculate carbohydrate kcal
df.join.all$Carbohydrate_kcal<-df.join.all$Grand_Total_2901_kcal-(df.join.all$Protein_Total_2901_kcal+df.join.all$Fat_Total_2901_kcal+df.join.all$Alcoholic_Beverages_2924_kcal)
#tag.na.carb<-which(is.na(df.join.all$Carbohydrate_kcal)==TRUE)

head(df.join.all)

# We have to split and duplicate the belgium-luxembourg data (which was pooled until 2000) as we have seperatre lifetables for these
tag<-which(df.join.all$Area == "Belgium-Luxembourg")
BL_data<-df.join.all[tag,]
df.join.all$Area[tag]<-"Belgium"
BL_data$Area<-"Luxembourg"
df.join.all<-rbind(df.join.all, BL_data)

# We have to split and duplicate the serbia-montenegro data (which was pooled in mid 2000s) as we have seperatre lifetables for these
tag<-which(df.join.all$Area == "Serbia and Montenegro")
SM_data<-df.join.all[tag,]
df.join.all$Area[tag]<-"Serbia"
SM_data$Area<-"Montenegro"
df.join.all<-rbind(df.join.all, SM_data)


# We have to split out the data on Yugoslav SFR as we have seperate lifetables for bosnia, serbia, slovenia, croatia, macedonia and montenegro 
tag<-which(df.join.all$Area == "Yugoslav SFR")
YG_data<-df.join.all[tag,]
YG_data$Area<-"Bosnia & Herzegovina"
df.join.all<-rbind(df.join.all, YG_data)
YG_data$Area<-"Serbia"
df.join.all<-rbind(df.join.all, YG_data)
YG_data$Area<-"Slovenia"
df.join.all<-rbind(df.join.all, YG_data)
YG_data$Area<-"Macedonia"
df.join.all<-rbind(df.join.all, YG_data)
YG_data$Area<-"Montenegro"
df.join.all<-rbind(df.join.all, YG_data)
YG_data$Area<-"Croatia"
df.join.all<-rbind(df.join.all, YG_data)


# We have to split out the data on Czechoslovakia as we have seperate lifetables for Czechia, and slovakia 
tag<-which(df.join.all$Area == "Czechoslovakia")
CZ_data<-df.join.all[tag,]
CZ_data$Area<-"Czechia"
df.join.all<-rbind(df.join.all, CZ_data)
CZ_data$Area<-"Slovakia"
df.join.all<-rbind(df.join.all, CZ_data)

# We have to split out the data on USSR as we have seperate lifetables for Russia, Lithuania, Tajikistan and Estonia 
tag<-which(df.join.all$Area == "USSR")
US_data<-df.join.all[tag,]
US_data$Area<-"Lithuania"
df.join.all<-rbind(df.join.all, US_data)
US_data$Area<-"Estonia"
df.join.all<-rbind(df.join.all, US_data)
US_data$Area<-"Russian Federation"
df.join.all<-rbind(df.join.all, US_data)
US_data$Area<-"Tajikistan"
df.join.all<-rbind(df.join.all, US_data)


# Sort by country
df.join.all<-df.join.all[order(df.join.all$Area),]
head(df.join.all)


# Convert to iso3c country names as used in the LT data
df.join.all$Country<-countrycode(df.join.all$Area, origin="country.name", destination="iso3c")


tag<-which(df.join.all$Area == "T\xfcrkiye")
df.join.all$Country[tag]<-"TUR"

tag<-which(df.join.all$Area == "C\xf4te d'Ivoire")
df.join.all$Country[tag]<-"CIV"

# Add a country code for YUG - seems to be missing from the function and we have one or two LTs for Yugoslavia as a whole
tag<-which(df.join.all$Area == "Yugoslav SFR")
df.join.all$Country[tag]<-"YUG"

# Add a country code for CSK - seems to be missing from the function, and we have LTs for Czechoslovakia as a whole
tag<-which(df.join.all$Area == "Czechoslovakia")
df.join.all$Country[tag]<-"CSK"

# Add a country code for SUN - seems to be missing from the function, and we have LTs for USSR as a whole
tag<-which(df.join.all$Area == "USSR")
df.join.all[tag,]
df.join.all$Country[tag]<-"SUN"

head(df.join.all)
df.join.all$ID<-paste0(df.join.all$Country,"_",df.join.all$Year)

# save clean FBS dataset
write.table(df.join.all, file="data_cleaned/Clean_FBS.csv", sep=",", row.names=F, col.names=names(df.join.all))


########### Clean Lifetable Data ####################

# Read in the standard from Wilmoth et al 2012
standard<-read.csv("data_read/wilmoth_standard.csv")

# read in raw LT data
lifetables<-read.csv("data_read/res")
head(lifetables)


# For lifetables with a year-range calculate the mid point
lifetables$YearMid<-floor((lifetables$Year1 + lifetables$Year2) / 2)

# Cull out those ages not in the standard
lifetables<-lifetables[-which(is.na(match(lifetables$Age, standard$Age)) == T),]
head(lifetables)

# Cull out any lifetable data not coming from the whole country
lifetables<-lifetables[-which(lifetables$Region != 0),]
lifetables<-lifetables[-which(lifetables$Residence != 0),]
lifetables<-lifetables[-which(lifetables$Ethnicity != 0),]
lifetables<-lifetables[-which(lifetables$SocDem != 0),]

# Add a lifetable ID for the year, country, sex and reference
lifetables$LT_ID<-paste(lifetables$Country, lifetables$YearMid, lifetables$Sex, lifetables$Ref.ID, sep="_")

# For any one reference for a country x year x sex we want the minimal TypeLT - (1, 2, or 4)
IDS<-unique(lifetables$LT_ID)
for(i in 1:length(IDS)){
  sub<-lifetables[which(lifetables$LT_ID == IDS[i]),]
  sub<-sub[which(sub$TypeLT == min(sub$TypeLT)),]
  if(i == 1){
    lifetables_trim<-sub
  }else{
    lifetables_trim<-rbind(lifetables_trim, sub)
  }
}

# Do we still have several 'versions' for any one ID
check<-plyr::ddply(lifetables_trim, plyr::.(LT_ID), summarise, length(unique(Version)))
check[which(check$"length(unique(Version))" != 1),"LT_ID"]

# Yes there is this one from hongkong in 1973 with both sexes - the data seem identical so lets just use version1
tag<-which(is.na(match(lifetables_trim$LT_ID, check[which(check$"length(unique(Version))" != 1),"LT_ID"])) == F)
hk_1973<-lifetables_trim[tag,]
lifetables_trim<-lifetables_trim[-tag,]
lifetables_trim<-rbind(lifetables_trim, hk_1973[which(hk_1973$Version == 1),])

# Do we still have several 'versions' for anyone reference
check<-plyr::ddply(lifetables_trim, plyr::.(LT_ID), summarise, length(unique(Version)))
check[which(check$"length(unique(Version))" != 1),"LT_ID"]
# No

# Now within each lifetable lets normalise the l.x. to its cohort size
lifetables_trim$l.x.prop<-0
IDS<-unique(lifetables_trim$LT_ID)
for(i in 1:length(IDS)){
  tag<-which(lifetables_trim$LT_ID == IDS[i])
  sub<-lifetables_trim[tag,]
  lifetables_trim$l.x.prop[tag]<-sub$l.x. / sub$l.x.[1]
}

# Lets check that the first entry is 1 for all lifetables
check<-plyr::ddply(lifetables_trim, plyr::.(LT_ID), summarise, l.x.prop[1])
check[which(check$"l.x.prop[1]" != 1),"LT_ID"]
# OK, looking good

# remove data from prior to 1961 since that is when the FBS data starts
tag<-which(lifetables_trim$YearMid<1961)
dim(lifetables_trim)
lifetables_trim<-lifetables_trim[-tag,]

lifetables_trim_new<-lifetables_trim%>%group_by(Country,Sex,Age,YearMid)%>%summarise(lx=median(l.x.prop))
lifetables_trim_new$LT_ID3<-paste(lifetables_trim_new$Country, lifetables_trim_new$YearMid, lifetables_trim_new$Sex, lifetables_trim_new$Age,sep="_")
lifetables_trim_new$LT_ID2<-paste(lifetables_trim_new$Country, lifetables_trim_new$YearMid, lifetables_trim_new$Sex,sep="_")
tag.65<-which(lifetables_trim_new$Age>65)
lifetables_trim_new<-lifetables_trim_new[-tag.65,]
lifetables_trim_count<-lifetables_trim_new%>%group_by(LT_ID3)%>%count()
tag.diffageint<-which(lifetables_trim_count$n>1)

######### Add Popuation Data ################

pop.m<-read.csv("data_read/POPULATION_SINGLE_AGE_MALE.csv")
pop.f<-read.csv("data_read/POPULATION_SINGLE_AGE_FEMALE.csv")
head(pop.m)
head(pop.f)


# Filter population data to give only Countries
tag<-which(pop.m$Type=="Country/Area")
pop.m<-pop.m[tag,]
tag<-which(pop.f$Type=="Country/Area")
pop.f<-pop.f[tag,]

# Rename Cols 
head(pop.m)
names(pop.m)[3]<-"Country"
names(pop.m)[6]<-"Country.code"
head(pop.f)
names(pop.f)[3]<-"Country"
names(pop.f)[6]<-"Country.code"


# Filter out cols for ages 0 to 100
pop.m<-cbind(pop.m[,c("Country","Country.code","Year")],pop.m[,c(12:112)],Sex=rep(1,nrow(pop.m)))
pop.f<-cbind(pop.f[,c("Country","Country.code","Year")],pop.f[,c(12:112)],Sex=rep(2,nrow(pop.f)))

pop<-as.data.frame(rbind(pop.m,pop.f))
pop<-pop%>%mutate(ID=paste0(pop$Country.code, "_",pop$Year,"_", pop$Sex))

lts<-lifetables_trim_new

match<-match(lts$LT_ID2,pop$ID)
length(which(is.na(match)==TRUE))
tag.na<-which(is.na(match)==TRUE)
unique(lts[tag.na,"Country"])
length(which(lts$Country[tag.na]%in%c("CSK","YUG","SUN"))) # All unmatched rows in LifeTable data are Country : CSK, YUG or SUN

# Remove all rows in LifeTable data with Country in c("CSK","YUG", "SUN")

lts<-lts[-tag.na,]
lts$at_risk<-NA

# Add Age Interval, dx and qx columns
lts<-lts%>%group_by(Country,YearMid,Sex)%>%mutate(AgeInt=lead(Age,default=5)-Age,dx=lx-lead(lx,default=0),qx=dx/lx) 

# rename population data col X100. to X100 
names(pop)[which(names(pop)=="X100.")]<-"X100"

# Add population data to lifetable data
# tag Age 0 lifetable data
keep<-which(lts$Age==0)

# create age 0 life table data subset
lts.0<-lts[keep,]
x<-0
# create a variable that matches the population data column name
age.x<-paste0("X",x)

# match age ) data to population data 
match<-match(lts.0$LT_ID2,pop$ID)

length(which(is.na(match)==TRUE)) # Check for unmatched entries \
atrisk<-pop[match,age.x]
atrisk<-as.numeric(gsub(" ","",atrisk))

# add at risk population to lifetable data
lts.0$at_risk<-atrisk

LTs<-lts.0



at_risk<-function(Age){
  
  keep<-which(lts$Age==Age)
  lts.x<-lts[keep,]
  match<-match(lts.x$LT_ID2,pop$ID)
  
  for (i in c(1:nrow(lts.x))){
    if((Age==100)==TRUE){
      atrisk<-pop[match[i],"X100"]
      lts.x[i,]$at_risk<-atrisk
    }else{
      AgeInt<-lts.x$AgeInt[i]
      if ((AgeInt==(-60))==TRUE){
        #print(paste0("Age:",Age," AgeInt:",AgeInt, " i:",i))
        Int<-5
      }else{
        Int<-AgeInt
      }
      x<-seq(Age,Age+(Int-1))
      age.x<-paste0("X",x)
      atrisk<-pop[match[i],age.x]
      total<-sum(as.numeric(gsub(" ","",atrisk)))
      lts.x[i,]$at_risk<-total
    }
  }
  
  return(lts.x)
}


# repeat for Age 1 to 60
for (i in c(1,seq(5,65,5))){
  lt.new<-at_risk(i)
  LTs<-rbind(LTs,lt.new)}

LTs$at_risk<-as.numeric(gsub(" ","",LTs$at_risk))


LTs$atRisk=LTs$at_risk*1000

LTs$n_events<-LTs$atRisk*LTs$qx

LTs$events<-LTs$atRisk*LTs$qx
head(LTs)


LTs %>% mutate(haz=events/atRisk)%>%dplyr::group_by(Country, YearMid, Sex)%>%dplyr::mutate(S_t=cumprod(1-haz)) %>% dplyr::mutate(S_t_new=lag(S_t,default=1))%>%View()
df_seS<-LTs %>% mutate(haz=events/atRisk)%>%group_by(Country, YearMid, Sex)%>%mutate(S_t=cumprod(1-haz)) %>% mutate(S_t_new=lag(S_t,default=1))%>%mutate(seS_t=S_t_new*sqrt(cumsum(haz/(1-haz)/(atRisk))),seS=sqrt((S_t_new^2)*cumsum(events/(atRisk*(atRisk-events)))))
# seS and seS_t are the same - just checking that both formulas are equivalent
df_seS%>%arrange(Country,YearMid,Sex)%>%View()

# rename YearMid column to Year
names(df_seS)[4]<-"Year"

# save clean life and popualtion data 
write.table(df_seS, file="data_cleaned/Clean_LT_with_POP.csv", sep=",", row.names=F, col.names=names(df_seS))

################ Clean GDP data ####################

gdp<-read.csv("data_read/mpd2020.csv")

# Throw out any data with missing GDP - note we will have a strategy for dealing with these IF they have correlates - this just means we will not end up with any data with all missing covariates
gdp<-gdp[-which(is.na(gdp$gdppc) == T),]

gdp<-gdp[-which(gdp$countrycode == "SUN" & gdp$year > 1991),]
gdp<-gdp[-which(gdp$countrycode == "YUG" & gdp$year > 1991),]
gdp<-gdp[-which(gdp$countrycode == "CSK" & gdp$year > 1991),]

glimpse(gdp)

# remove pop column 
gdp<-gdp[,-5]
# rename columns to match other datasets
names(gdp)<-c("Country","Country.Name","Year","GDP")

# remove comma from gdp values
gdp$GDP<-as.numeric(gsub(",","",gdp$GDP))

# save clean maddison project GDP dataset
write.table(gdp, file="data_cleaned/Clean_GDP_Mad.csv", sep=",", row.names=F, col.names=names(gdp))

################# Combine FBS, LT and GDP data ###############

# read in datasets if not already loaded

fbs<-read.csv("data_cleaned/Clean_FBS.csv",header=TRUE)

ltdata<-read.csv("data_cleaned/Clean_LT_with_POP.csv",header=TRUE)

gdp<-read.csv("data_cleaned/Clean_GDP_Mad.csv",header=TRUE)

glimpse(fbs)
glimpse(ltdata)
glimpse(gdp)

ltdata<-ltdata%>%mutate(LT_ID=paste0(Country,"_",Year))
glimpse(ltdata)

gdp<-gdp%>%mutate(ID=paste0(Country,"_",Year))
glimpse(gdp)

match_IDs<-match(ltdata$LT_ID,gdp$ID)

ltdata$GDP<-gdp$GDP[match_IDs]

match.IDs<-match(ltdata$LT_ID,fbs$ID)

# add supply data to lifetable dataset 
ltdata$protein.kcal<-fbs$Protein_Total_2901_kcal[match.IDs]
ltdata$abp.kcal<-fbs$Animal_Protein_2941_kcal[match.IDs]
ltdata$pbp.kcal<-fbs$Vegetal_Protein_2903_kcal[match.IDs]
ltdata$fat.kcal<-fbs$Fat_Total_2901_kcal[match.IDs]
ltdata$carb.kcal<-fbs$Carbohydrate_kcal[match.IDs]


remove.rows<-which(is.na(ltdata$abp.kcal)==TRUE&is.na(ltdata$pbp.kcal)==TRUE&is.na(ltdata$fat.kcal)==TRUE)

df_all_complete<-ltdata[-remove.rows,]

na.rows<-which(is.na(df_all_complete$abp.kcal)==TRUE|(is.na(df_all_complete$fat.kcal)==TRUE)|(is.na(df_all_complete$GDP)==TRUE))

df_all_complete<-df_all_complete[-na.rows,]


glimpse(df_all_complete)


tag.5<-which(df_all_complete$Age==5)
df_5<-df_all_complete[tag.5,]
tag.60<-which(df_all_complete$Age==60)
df_60<-df_all_complete[tag.60,]

df_final<-as.data.frame(rbind(df_5,df_60))

write.table(df_final, file="data_cleaned/Clean_complete.csv", sep=",", row.names=F, col.names=names(df_final))

###########################################################

data<-read.csv("data_cleaned/Clean_complete.csv",header=TRUE)

m<-which(data$Sex==1)
data$Sex[m]<-"Males"
f<-which(data$Sex==2)
data$Sex[f]<-"Females"

data$Country<-as.factor(data$Country)
data$Sex<-as.factor(data$Sex)

glimpse(data)

# split data by Age 
age5.data<-data[which(data$Age==5),]
age60.data<-data[which(data$Age==60),]

glimpse(age5.data)
glimpse(age60.data)

# rename lx column to l5 and l60 
colnames(age5.data)[5]<-"l5"
colnames(age60.data)[5]<-"l60"

# split data further by Sex
age5m.data<-age5.data[(which(age5.data$Sex=="Males")),]
age60m.data<-age60.data[(which(age60.data$Sex=="Males")),]

age5f.data<-age5.data[(which(age5.data$Sex=="Females")),]
age60f.data<-age60.data[(which(age60.data$Sex=="Females")),]

# check the correct data subset has been assigned to each data object 
glimpse(age5m.data)
glimpse(age60m.data)
glimpse(age5f.data)
glimpse(age60f.data)

# save data subsets

write.table(age5m.data,"data_cleaned/5mdata.csv", sep=",", row.names=F, col.names=names(age5m.data))
write.table(age60m.data,"data_cleaned/60mdata.csv", sep=",", row.names=F, col.names=names(age60m.data))
write.table(age5f.data,"data_cleaned/5fdata.csv", sep=",", row.names=F, col.names=names(age5f.data))
write.table(age60f.data,"data_cleaned/60fdata.csv", sep=",", row.names=F, col.names=names(age60f.data))

####################################################################

# see "code_analysis/run_GAMs_and_AIC.R" file to run GAM and model section analysis.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  # formula_list_milk[[2]]<-"~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Prop_AP_milk,Year,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            # formula_list_milk[[3]]<-"~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Prop_VP_infant,Year,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            # 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            # formula_list_milk[[4]]<-"~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Prop_AP_milk,Year,bs=\"cr\")+ s(Prop_VP_infant,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            # formula_list_milk[[5]]<-"~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Prop_fat_veg,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            # formula_list_milk[[6]]<-"~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Prop_fat_animal,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            # 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            # formula_list_milk[[7]]<-"~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Prop_fat_veg,bs=\"cr\")+s(Prop_fat_animal,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            formula_list_milk2<-list()
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            formula_list_milk2[[1]]<-formulas[[18]]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            formula_list_milk2[[2]]<-"~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Prop_AP_milk,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            formula_list_milk2[[3]]<-"~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Prop_fat_animal,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            formula_list_milk2[[4]]<-"~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Prop_AP_milk,bs=\"cr\")+s(Prop_fat_animal,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            formula_list_milk2[[5]]<-"~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Milk_Excluding_Butter_2848_kcal,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            formula_list_milk2[[6]]<-"~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + te(Milk_Excluding_Butter_2848_kcal,Year,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            formula_list_milk2[[7]]<-"~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Animal_Fat_2941_kcal,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            formula_list_milk2[[8]]<-"~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Animal_Fat_2941_kcal,Year,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            formula_list_milk2[[9]]<-"~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Milk_Excluding_Butter_2848_kcal,Year,bs=\"cr\")+ s(Prop_fat_animal,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            formula_list_milk2[[10]]<-"~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Milk_Excluding_Butter_2848_kcal,Year,bs=\"cr\")+ s(Animal_Fat_2941_kcal,Year,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            formula_list_milk2[[11]]<-"~ te(abp.rel, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            formula_list_milk2[[12]]<-"~ te(abp.rel, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Prop_fat_animal,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            formula_list_milk2[[13]]<-"~ te(abp.rel, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k, 7))+ te(carb.kcal,Year, k=10,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Animal_Fat_2941_kcal,Year,bs=\"cr\") + s(Country, bs=\"re\")"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            gam.abp.milk<-gam(abp.kcal~s(Milk_Excluding_Butter_2848_kcal),data=data.5m)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            abp.col<-which(names(data.5m)=="abp.kcal")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            predict.abp.milk<-data.5m[,-abp.col]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            predict.abp.milk$abp.kcal<-NA
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            predictions.abp.milk<-predict(gam.abp.milk,type="response",newdata = predict.abp.milk)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            predict.abp.milk$abp.kcal<-predictions.abp.milk
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            abp.res<-data.5m$abp.kcal-predictions.abp.milk
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            abp.rel<-(abp.res/predictions.abp.milk)*100
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            data.5m.SEsq$abp.rel<-abp.rel
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            library(doSNOW)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            formulas<-formula_list_milk2
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            # I am going to run over clusters (one per sublist)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            cl<-makeCluster(length(formulas), outfile="")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            registerDoSNOW(cl)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            # run function for each part across the eight cores
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            results<-foreach(i = c(5,6,11)) %dopar% {
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              library(mgcv)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              out<-list()
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              out[[2]]<-run.aic.weights("l5",i,50,50,data.5m.SEsq,2017)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              return(out)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            } 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            save(results,file="Milk_GAMs_5_6_11_SEsq.rdata")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            gam.abp.milk<-run.aic.weights("l5",5,50,50,data.5m.SEsq,2017)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            gam.abp.milk.year<-run.aic.weights("l5",6,50,50,data.5m.SEsq,2017)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            gam.abp.rel.milk<-run.aic.weights("l5",11,50,50,data.5m.SEsq,2017)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            load("Milk_GAMs_SE_and_SEsq.rdata")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            summary(results[[1]][[1]])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            summary(results[[1]][[2]])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            load("gam_18_SEsq_5m.rdata")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            summary(GAM.5m)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            GAMs.milk<-list(GAM.5m,gam.abp.milk,gam.abp.milk.year,gam.abp.rel.milk)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            summary(results[[2]][[1]])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            summary(results[[2]][[2]])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            summary(results[[3]][[1]])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            summary(results[[3]][[2]])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            # SE sq weights appear to be better fit so will continue with results[[i]][[2]] models 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            results.SEsq<-list()
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            for (i in 1:length(results)){
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              results.SEsq[[i]]<-results[[i]][[2]]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            summaries.5m<-lapply(GAMs.milk,summary)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            dev_5m<-unlist(lapply(summaries.5m, "[[", 14)) * 100
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            AIC_5m<-unlist(lapply(GAMs.milk, AIC))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            delta_5m<-AIC_5m - min(AIC_5m)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            weights_5m<-exp(-0.5 * delta_5m) / sum(exp(-0.5 * delta_5m))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            sumEDF.5m<-c()
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            for (i in c(1:length(GAMs.milk)))(
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              sumEDF.5m[i]<-sum(summaries.5m[[i]]$edf)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            formula<-unlist(formula_list_milk)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            for(i in 1:length(formula)){
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              formula[i]<-paste0("l5",formula_list_milk[i])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            f<-length(GAMs.milk)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            AIC.df.5m<-data.frame(gam=c(1:f),k_nut=rep(50,f),gamma=rep(gamma,f),AIC_5m,dev_5m,delta_5m, weights_5m,sumEDF.5m,sex=rep("Males",f))#,formula=formula)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            summaries[[7]]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            library(mgcv)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            p<-as.function(alist(a = , b = 2, par(mfrow=c(a,b))))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            p(3,3)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("abp.kcal","pbp.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("abp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("pbp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("abp.kcal","GDP"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("pbp.kcal","GDP"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("abp.kcal","carb.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("pbp.kcal","carb.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("fat.kcal","carb.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("fat.kcal","carb.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            p(3,3)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[1]],view=c("abp.kcal","pbp.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[1]],view=c("abp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[1]],view=c("pbp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[1]],view=c("abp.kcal","GDP"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[1]],view=c("pbp.kcal","GDP"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[1]],view=c("abp.kcal","carb.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[1]],view=c("pbp.kcal","carb.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[1]],view=c("fat.kcal","carb.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[1]],view=c("fat.kcal","carb.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            p(3,3)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("abp.kcal","pbp.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[1]],view=c("abp.kcal","pbp.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[2]],view=c("abp.kcal","pbp.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[3]],view=c("abp.kcal","pbp.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[4]],view=c("abp.kcal","pbp.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[5]],view=c("abp.kcal","pbp.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[6]],view=c("abp.kcal","pbp.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[7]],view=c("abp.kcal","pbp.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            p(3,3)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("abp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("pbp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[2]],view=c("abp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[2]],view=c("pbp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[3]],view=c("abp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[3]],view=c("pbp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[4]],view=c("abp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[4]],view=c("pbp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            p(3,3)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("abp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("pbp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[5]],view=c("abp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[5]],view=c("pbp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[6]],view=c("abp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[6]],view=c("pbp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[7]],view=c("abp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[7]],view=c("pbp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            p(3,3)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("abp.kcal","pbp.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("abp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("pbp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[2]],view=c("abp.kcal","Prop_AP_milk"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[2]],view=c("pbp.kcal","Prop_AP_milk"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[2]],view=c("fat.kcal","Prop_AP_milk"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[4]],view=c("abp.kcal","Prop_AP_milk"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[4]],view=c("pbp.kcal","Prop_AP_milk"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[4]],view=c("fat.kcal","Prop_AP_milk"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            #vis.gam(results.SEsq[[4]],view=c("Prop_VP_infant","Prop_AP_milk"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            p(3,3)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("abp.kcal","pbp.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("abp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("pbp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[5]],view=c("abp.kcal","Prop_fat_veg"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[5]],view=c("pbp.kcal","Prop_fat_veg"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[5]],view=c("fat.kcal","Prop_fat_veg"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[7]],view=c("abp.kcal","Prop_fat_veg"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[7]],view=c("pbp.kcal","Prop_fat_veg"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[7]],view=c("fat.kcal","Prop_fat_veg"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            p(3,3)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("abp.kcal","pbp.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("abp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("pbp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[6]],view=c("abp.kcal","Prop_fat_animal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[6]],view=c("pbp.kcal","Prop_fat_animal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[6]],view=c("fat.kcal","Prop_fat_animal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[7]],view=c("abp.kcal","Prop_fat_animal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[7]],view=c("pbp.kcal","Prop_fat_animal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[7]],view=c("fat.kcal","Prop_fat_animal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[7]],view=c("Prop_fat_veg","Prop_fat_animal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            p(3,3)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("abp.kcal","pbp.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("abp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(GAM.5m,view=c("pbp.kcal","fat.kcal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[5]],view=c("abp.kcal","Prop_fat_veg"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[5]],view=c("pbp.kcal","Prop_fat_veg"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[5]],view=c("fat.kcal","Prop_fat_veg"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[6]],view=c("abp.kcal","Prop_fat_animal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[6]],view=c("pbp.kcal","Prop_fat_animal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            vis.gam(results.SEsq[[6]],view=c("fat.kcal","Prop_fat_animal"),plot.type="contour")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            