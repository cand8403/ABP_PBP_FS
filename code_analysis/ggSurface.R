ggSurface<-function(GAM, data, XYZ, labels, predict_val, surf_min=NA, surf_max=NA, x.limits=NA, 
y.limits=NA, z.val=NA, exclude, subtitle="", traits=NA, lab_size=3, nlevels=5, contour_at=NA, skip=1,palette=NA,col.limit=NA,decimal=3){
	
	require(ggplot2)
	require(sp)
	require(geometry)
	require(mgcv)
	require(metR)
	require(scales)

  decimal<-decimal
	# This specifies the color scheme for surface

	rgb.palette<-colorRampPalette(c("#060AAA","blue","#007fff","cyan",
"#00ffdd","#00ff00","#98ff4d","yellow","#FFE301","#FFA700","#FF8800","#FF5E00","red","#EF3037"), space="Lab", interpolate="linear")
	reverse.palette<-colorRampPalette(c("#0C841D","#00A507","#16DD00","#00ff00",
"#00ff77","cyan","#007fff","blue","#060AAA","#3E00B3","#6B00D7","#9400d3","#A230ED","#C364FA","#DE6DF1", "#ED81EE","#FC91D4","#F9429E","#E5358C", "#D1287A","#BF2669","#BE1B69","#AA0E57","#D91858","#FF194D","#EF3037","red","#FE5A1C","#FF5E00","orange","#FFA200","#FFE301","#FFFF00"), space="Lab", interpolate="linear")
	warm.palette<-colorRampPalette(c("#FFCC0D","#FF7326","#FF194D","#BF2669","#702A8C"), space="Lab", interpolate="linear")
	
	rgb.palette.256<-colorRampPalette(c("blue","cyan","yellow","red"), space="Lab", interpolate="linear")
	purple<-colorRampPalette(c("#1e2994","#1C33FF","#5B81FF","#7AA8FF","#99CEFF","#B5B4FF","#C3A7FF","#D199FF","#D184E9","#D16FD2","#D045A6","#D01B79","#db073d", "#AF0430","#820023"), space="Lab", interpolate="linear")

	if(is.na(palette)){
		map<-rgb.palette(1000)
	}else{
		map<-purple(1000)
	}

	#if((palette=="reverse") == T){
		#map<-reverse.palette(5000)
	#}

	#if((palette=="warm") == T){
		#map<-warm.palette(5000)
	#}

	
	# What are the outcomes being modelled, if not specified
	if(is.na(traits)){
		traits<-unlist(lapply(strsplit(as.character(summary(GAM)$formula), " ~ "), "[[", 1))[2]
	}
	
	# List to hold the plots
	plots_list<-list()

	# List for the order of plots
	nutrient.order<-XYZ[c(1,2,3)]
	
	# List for the labels
	labels.order<-labels[c(1,2,3)]
				
	# Values to predict over, if they are unspecified
	if(is.na(x.limits)[1] == T){
		x.limits<-c(floor(min(data[,nutrient.order[1]])), ceiling(max(data[,nutrient.order[1]])))
	}
	if(is.na(y.limits)[1] == T){
		y.limits<-c(floor(min(data[,nutrient.order[2]])), ceiling(max(data[,nutrient.order[2]])))
	}		
			
	# If we do not specify values to slice at, use the 25, 50, and 75 %ile
	if(is.na(z.val) == T){
		z.val<-round(median(data[,nutrient.order[3]]))
	}
			
	# Fitted list to hold some results for later
	x.new<-seq(min(x.limits, na.rm=T), max(x.limits, na.rm=T), len=501)
	y.new<-seq(min(y.limits, na.rm=T), max(y.limits, na.rm=T), len=501)
	z.new<-z.val
	predictors<-as.data.frame(expand.grid(x.new, y.new, z.new))
	names(predictors)<-nutrient.order
	# require "inhull.R" file to be loaded
	# checks if predictors are within realistic combined nutrient boundaries based on dataset
	in.poly<-as.numeric(inhull(predictors[,c(1:3)], data[,names(predictors)]) != -1)
			
	# Add the predictors for the additional 'confounders'
	predictors<-cbind(predictors, predict_val)
	predictors<-predictors[-which(in.poly == 0),]
			
	# Do the predictions
	predictions<-t(t(predict(GAM, newdata=predictors, type="response", exclude=exclude, newdata.guaranteed=T)))


	# Loop for the proteins
	#for(k in 1:length(traits)){

		k=1

		# Get the proedictions for the kth trait					
		predictions_k<-predictions[,k]
								
		# Find the min and max values across all predictions
		mn<-surf_min[k]
		mx<-surf_max[k]
		if(is.na(mn)==T){
			mn<-min(predictions_k, na.rm=T)
		}
		if(is.na(mx)==T){
			mx<-max(predictions_k, na.rm=T)
		}
		

		locs<-(range(predictions_k, na.rm=TRUE) - mn) / (mx-mn) * length(map)
		
		plot_data<-predictors
		plot_data$fit<-predictions_k
		plot_data$x<-plot_data[,nutrient.order[1]]
		plot_data$y<-plot_data[,nutrient.order[2]]
		
		# Set the contour
		if(is.na(contour_at)[1] == T){
			contour_use<-signif((max(predictions_k, na.rm=T)-min(predictions_k, na.rm=T))/nlevels, 1)
		}else{
			contour_use<-contour_at	
		}
		
		if(is.na(col.limit)[1] == T){
			fill_values<-NULL
			fill_colors<-map[locs[1]:locs[2]]
			fill_limits<-c(mn,mx)
		}else{
			col.val1<-col.limit+0.5*(1-col.limit)
			col.val2<-col.val1+0.4*(1-col.val1)
			col.val3<-col.val1+0.9*(1-col.val1)
			fill_values<-c(rescale(seq(col.limit,col.val1,length=1000),to=c(0,0.33)),rescale(seq(col.val2,col.val3,length=1000),to=c(0.34,0.69)),rescale(seq(col.val3,1,length=3500),to=c(0.7,1)))
			fill_colors<-map
			fill_limits<-c(col.limit,surf_max)
		}
		



		# Make the plot
		plot<-ggplot(plot_data, aes(x=x, y=y)) +
				geom_raster(aes(fill=fit), show.legend=T, interpolate=F, na.rm=T) +
				scale_fill_gradientn(name=paste(traits),colors=fill_colors,limits=fill_limits,values=fill_values )+
		  geom_abline(intercept = 400, slope = -1, color = "grey", linetype = "dashed", size=1.5)+
				geom_contour(data=plot_data, aes(x=x, y=y, z=fit), na.rm=T, color="black", binwidth=contour_use) +
				#geom_label_contour(data=plot_data, aes(x=x, y=y, z=fit), size=lab_size, binwidth=contour_use, skip=skip) +
		    geom_text_contour(data=plot_data,aes(x=x, y=y,z=fit,label = round(after_stat(level), decimal),fontface="bold"),size=7, binwidth=contour_use, skip=skip,rotate=FALSE,color="grey10",show.legend=FALSE,stroke = 0.1, stroke.colour = "white",
		                      label.placer = label_placer_flattest(),check_overlap = TRUE)+
				theme_bw() +
				labs(x = labels.order[1], y = labels.order[2], subtitle=subtitle) +
				theme(axis.text=element_text(size=40), axis.title=element_text(size=40)) +
				theme(title=element_text(hjust = 0.5,size=30)) + 
				xlim(x.limits) +
				ylim(y.limits)
		# add thick abline to surface for x + y = 400, x+y = 500, x+y = 300
		# plot<-plot + geom_abline(intercept = 400, slope = -1, color = "grey", linetype = "dashed", size=1.5)
		# # plot<-plot + geom_abline(intercept = 500, slope = -1, color = "grey", linetype = "dashed")
		# plot<-plot + geom_abline(intercept = 300, slope = -1, color = "grey", linetype = "dashed")
		
		
		
		
		
		# Save the plot		
		#plots_list[[k]]<-plot
	
	# }
	#return(plots_list)
	return(plot)
}
