#############################################################
################## Optimizer Function #######################
#############################################################

# Function to estimate ASM for a given age, sex, year, and PCF - designed to work with an optimiser so we can get the PCF associated with maximal/minimal ASM

# GAM <- list(list(malesl5,malesl60),list(femalesl5,femalesl60))
# data <- list(l5 data, l60 data)

convert_brass<-function(x, sex, age, GDP, year, data, standard, GAM, stat="qx", aim="minimise"){
		
	# Make sure the required packages are loaded on the cores
	require(arm)
	require(mgcv)
	require(MortalityLaws)
	
	# Create a copy for analysing
	# x is a vector of nutrient values 
	x<-as.vector(x)
	predict_i<-data.frame(abp.kcal=x[1], pbp.kcal=x[2], carb.kcal=x[3], fat.kcal=x[4], Sex=sex, GDP=GDP, Year=year)
	
	# check it is within the observed data
	in.poly1<-as.numeric(inhull(predict_i[,c("abp.kcal", "pbp.kcal", "carb.kcal", "fat.kcal")], data[[1]][,c("abp.kcal", "pbp.kcal", "carb.kcal", "fat.kcal")]) != -1)
	in.poly2<-as.numeric(inhull(predict_i[,c("abp.kcal", "pbp.kcal", "carb.kcal", "fat.kcal")], data[[2]][,c("abp.kcal", "pbp.kcal", "carb.kcal", "fat.kcal")]) != -1)
	
	# If it is observed do the converison, else return 1000 + ED from the mean supply
	if(in.poly1 == 1 & in.poly2 == 1){
		
		# Get the predictions

		predictions<-as.data.frame(predict(GAM[[1]], newdata=predict_i, type="response",exclude=c("s(Country)"), newdata.guaranteed=T))
		predictions<-cbind(predictions,as.data.frame(predict(GAM[[2]], newdata=predict_i, type="response", exclude=c("s(Country)"), newdata.guaranteed=T)))
		names(predictions)<-c("l5","l60")
		predictions<-cbind(predict_i, predictions)
		
			
		# Which sex are we making predictions for
		convert1<-c("Males","Females")
		sex_used<-match(predictions$Sex[1],convert1)
					
		# Get the jth lifetable

		tag<-which(standard$Age == 5)
		ls5<-standard[tag, grep("lx", names(standard))[sex_used]]
		tag<-which(standard$Age == 60)
		ls60<-standard[tag, grep("lx", names(standard))[sex_used]]
		lx5<-predictions$l5
		lx60<-predictions$l60
		alpha<-((logit(lx5)*logit(ls60))-(logit(ls5)*logit(lx60)))/(logit(ls60)-logit(ls5))
		beta<-(logit(lx60)-logit(lx5))/(logit(ls60)-logit(ls5))
		l.x.<-invlogit(alpha + beta * logit(standard[, grep("lx", names(standard))[sex_used]]) + standard[, grep("gamma", names(standard))[sex_used]] * (1 - (logit(lx5)/logit(ls5))) + standard[, grep("theta", names(standard))[sex_used]] * (1 - (logit(lx60)/logit(ls60))))

		# Get the full lifetable
		convert2<-c("male", "female")
		LT<-LifeTable(x=standard$Age, lx=l.x., sex=convert2[sex_used])$lt
		
		# What are we looking for
		target<-LT[which(LT$x == age), stat]
		
		# If we want to maxise the qx multiply by -1
		if(aim == "maximise"){
			target<-target * -1
		}
		
	}else{
		# If we are outside the boundary, return 1000 + ED from the centre - helps guide the optimiser back there
		target<-1000 + sqrt((mean(data[[1]]$abp.kcal) - x[1])^2 + (mean(data[[1]]$pbp.kcal) - x[2])^2 + (mean(data[[1]]$carb.kcal) - x[3])^2 + (mean(data[[1]]$fat.kcal) - x[4])^2)
	}
	
	return(target)
			
}