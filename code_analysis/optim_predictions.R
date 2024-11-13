optim_predictions<-function(g,init_list,sexes){
  
  for(j in 1:length(sexes)) {
    
    # Create some variables to hold the max/min starting with the ages that we will search through, note I am dropping 105 and 110 here as preliminary analyses suggested that there are many many PCFs that maximise/minimise these
    ages<-c(0, 1, seq(5, 100, 5))
    max_expect<-list()
    min_expect<-max_expect
    
    
    # Run the optimiser for each age class
    for(i in 1:length(ages)){
      #show(ages[i])
      # Go through the initiating list
      res_temp_min<-data.frame(val=rep(NA, length(init_list)), ABP=NA, PBP=NA, C=NA, F=NA) 
      res_temp_max<-res_temp_min
      
      # Now optimise for each starting value
      for(o in 1:length(init_list)){
        
        # Pull out the oth set of starting values
        pars<-init_list[[o]]
        #show(pars)
        # First get the combination estimated to mimise qx at age i and then repeat to get the max within intial values o
        optim_results<-optim(par=pars, fn=convert_brass, sex=sexes[j], age=ages[i], GDP=med.GDP, year=year.plot, data=dataset_plot, standard=standard, GAM=GAM[[j]])
        res_temp_min[o,]<-c(optim_results$value, optim_results$par)
        
        optim_results<-optim(par=pars, fn=convert_brass, sex=sexes[j], age=ages[i], GDP=med.GDP, year=year.plot, data=dataset_plot, standard=standard, GAM=GAM[[j]], aim="maximise")
        res_temp_max[o,]<-c(optim_results$value, optim_results$par)
      }
      
      # Save the results in to the list
      max_expect[[i]]<-res_temp_max
      min_expect[[i]]<-res_temp_min	
      #show(max_expect[[i]])
      #show(min_expect[[i]])
    }
    
    # Save the list to be returned
    names(max_expect)<-ages
    names(min_expect)<-ages
    predictions<-list(min_expect, max_expect)
    save(predictions, file=sprintf("code_analysis/analysis_output/Predictions_g%s_%s.Rdata",g,sexes[j]))
  }
}