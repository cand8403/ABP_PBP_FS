

formulas<-list()

formulas[[1]]<-" ~ 1 + s(Country, bs=\"re\")"

formulas[[2]]<-" ~ s(Year, k=10, bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[3]]<-" ~ s(GDP, k=10, bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[4]]<-" ~ s(Year, k=10, bs=\"cr\")+ s(GDP, k=10, bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[5]]<-" ~ te(Year, GDP, k=10, bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[6]]<-"~ s(abp.kcal, pbp.kcal, fat.kcal, k=k_nut) + s(carb.kcal, k=k_carb,bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[7]]<-"~ s(abp.kcal, pbp.kcal, fat.kcal, k=k_nut)+ s(carb.kcal, k=k_carb,bs=\"cr\") + s(Year, k=10, bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[8]]<-" ~ s(abp.kcal, pbp.kcal, fat.kcal, k=k_nut)+ s(carb.kcal,k=k_carb,bs=\"cr\") + s(GDP, k=10, bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[9]]<-" ~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k_nut, 7))+ te(carb.kcal, Year, k=k_carb,bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[10]]<-" ~ te(abp.kcal, pbp.kcal, fat.kcal, GDP, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k_nut, 7))+ te(carb.kcal, GDP, k=k_carb,bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[11]]<-" ~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k_nut, 7))+ te(carb.kcal, Year, k=k_carb,bs=\"cr\") + s(GDP, k=10, bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[12]]<-" ~ te(abp.kcal, pbp.kcal, fat.kcal, GDP, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k_nut, 7)) + te(carb.kcal, GDP, k=k_carb,bs=\"cr\")+ s(Year, k=10, bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[13]]<-" ~ te(Year, GDP, k=10) + s(abp.kcal, pbp.kcal, fat.kcal, k=k_nut)+ s(carb.kcal,k=k_carb,bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[14]]<-"~ te(abp.kcal, pbp.kcal, fat.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k_nut, 7))+ te(carb.kcal,Year, k=k_carb,bs=\"cr\")+ te(GDP,Year, k=10,bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[15]]<-" ~ te(abp.kcal, pbp.kcal, fat.kcal, GDP, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k_nut, 7)) + te(carb.kcal, GDP, k=k_carb,bs=\"cr\")+ te(GDP, Year, k=10,bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[16]]<-"~ s(abp.kcal, pbp.kcal, carb.kcal,  k=k_nut) + s(fat.kcal,k=k_carb,bs=\"cr\")+ s(Country, bs=\"re\")"

formulas[[17]]<-"~ s(abp.kcal, pbp.kcal, carb.kcal, k=k_nut) + s(fat.kcal,k=k_carb,bs=\"cr\")+ s(Year, k=10, bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[18]]<-" ~ s(abp.kcal, pbp.kcal, carb.kcal, k=k_nut) + s(fat.kcal, k=k_carb,bs=\"cr\")+ s(GDP, k=10, bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[19]]<-" ~ te(abp.kcal, pbp.kcal, carb.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k_nut, 7)) + te(fat.kcal, Year, k=k_carb,bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[20]]<-" ~ te(abp.kcal, pbp.kcal, carb.kcal, GDP, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k_nut, 7))+ te(fat.kcal, GDP, k=k_carb,bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[21]]<-" ~ te(abp.kcal, pbp.kcal, carb.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k_nut, 7))+ te(fat.kcal, Year, k=k_carb,bs=\"cr\") + s(GDP, k=10, bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[22]]<-" ~ te(abp.kcal, pbp.kcal, carb.kcal,GDP, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k_nut, 7))+ te(fat.kcal, GDP, k=k_carb,bs=\"cr\") + s(Year, k=10, bs=\"cr\") + s(Country, bs=\"re\")"

formulas[[23]]<-" ~ te(Year, GDP, k=10) + s(abp.kcal, pbp.kcal, carb.kcal, k=k_nut) + s(fat.kcal,k=k_carb,bs=\"cr\")+ s(Country, bs=\"re\")"

formulas[[24]]<-" ~ te(abp.kcal, pbp.kcal, carb.kcal, Year, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k_nut, 7))+ te(fat.kcal, Year, k=k_carb,bs=\"cr\") + te(GDP, Year, k=10,bs=\"cr\")+ s(Country, bs=\"re\")"

formulas[[25]]<-" ~ te(abp.kcal, pbp.kcal, carb.kcal,GDP, bs=c(\"tp\", \"cr\"), d=c(3,1), k=c(k_nut, 7))+ te(fat.kcal, GDP, k=k_carb,bs=\"cr\") + te(GDP, Year, k=10,bs=\"cr\") + s(Country, bs=\"re\")"


save(formulas, file="code_analysis/GAM_Formulas.rdata")


