#
# calculate_error_function.R
#
#' measure current model fit
#'
#' error function for fitting process
#'
#' @param model current model configuration
#' @param opt_results model target data
#'
#' @return mean squared errors
#'
#' @export
#
calculate_error_function <- function(model, opt_results) {

	setup		<- elt(model, "setup")
	identifier	<- elt(setup, "model.ident")
	resultsdir	<- elt(setup, "resultsdir")

	# opt_results: built from copy of annualtargetdata which then overwrites 3,4,5,6 and adds 7,8
	#
	# > str(annualtargetdata)
	# 'data.frame':   84 obs. of  6 variables:
 	# $ Annual_measure: num  1522 687.2 600 339.6 44.4 ...
 	# $ SD_of_measure : num  150.94 67.4 100 25.16 2.52 ...
 	# $ Use1_0        : int  1 1 1 1 0 0 0 0 0 0 ...
 	# $ Name          : Factor w/ 84 levels "Obs_amm_io_ratio",..: 83 61 47 62 15 64 34 8 4 11 ...
 	# $ Units         : Factor w/ 6 levels "/y","%N(gN/g_dry_sed)",..: 5 5 4 5 5 5 5 5 5 5 ...
 	# $ Description   : Factor w/ 84 levels "Annual_average_ammonia_conc_in_porewater_of_mud_gs_0.12mm",..: 55 52 56 42 16 53 22 25 14 49 ...
	#
	# > str(opt_results)
	# 'data.frame':   84 obs. of  8 variables:
	# $ Annual_measure: num  1522 687.2 600 339.6 44.4 ...
	# $ SD_of_measure : num  150.94 67.4 100 25.16 2.52 ...
	# $ Model_data    : num  1210.5 749.9 588.7 321.8 46.7 ...
	# $ Use1_0        : int  1 1 1 1 0 0 0 0 0 0 ...
	# $ Chi           : num  2.12961 0.43295 0.00634 0.25111 NA ...
	# $ Name          : Factor w/ 84 levels "Obs_amm_io_ratio",..: 83 61 47 62 15 64 34 8 4 11 ...
	# $ Units         : Factor w/ 6 levels "/y","%N(gN/g_dry_sed)",..: 5 5 4 5 5 5 5 5 5 5 ...
	# $ Description   : Factor w/ 84 levels "Annual_average_ammonia_conc_in_porewater_of_mud_gs_0.12mm",..: 55 52 56 42 16 53 22 25 14 49 ...

	#Calculate the liklihoods

	#Assumes a prior calculated dataframe opt_results which contains the target data, their
	#sd's, the switch to say whether to include or not, and the model results

	#Column 1 = Target value
	#Column 2 = sd of target value
	#Column 3 = model results
	#Column 4 = switch to determine whether to be included in likelihood evaluation (1=yes, 0 = no)

	#Column 5 = name of target value
	#Column 6 = units of target value
	#Column 7 = description of target value


	#First add another column to the opt_results data frame
	#to hold the partial likelihoods - make this columnn 5 of the dataframe
	opt_results$new<-NA
	opt_results[,8]<-opt_results[,7]
	opt_results[,7]<-opt_results[,6]
	opt_results[,6]<-opt_results[,5]
	opt_results[,5]<-NA
	names(opt_results) <- c(names(opt_results[1:4]),"Chi",names(opt_results[5:7]))

	opt_results$Chi <- (((opt_results[,1]-opt_results[,3])^2))/(2*(opt_results[,2]^2))

	opt_results$Chi[which(opt_results[,4]==0)] <- NA


	Objannual<- exp(-1*(sum(opt_results$Chi,na.rm=TRUE))/sum(opt_results[,4]))

	#names(Objannual)<-c("annual_obj")

	#Results<-c(prefstore,ustore,hstore,biogeostore,mortstore,reststore,Objmonthly,Objannual)
	#Results<-c(prefstore,ustore,hstore,biogeostore,mortstore,reststore,Objannual)	ZZ don't do this now


	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Partial_chi is calculated here and saved to a file - split this into separate function?
	Partial_chi<-data.frame("Likelihood"=rep(0,1+nrow(opt_results)))

	rownames(Partial_chi)<-c(as.character(opt_results[,6]),"Overall")

	for(jjjk in 1:nrow(opt_results)){
		Partial_chi[jjjk,1] <- exp(-opt_results$Chi[jjjk])
	}
	Partial_chi[1+nrow(opt_results),1]<-Objannual

	#print(Partial_chi)

	#Print the data to a csv file
	#-----------------------------------------------------------------
	filename = csvname(resultsdir, "model_likelihood_results", identifier)
	writecsv(Partial_chi, filename)

	filename = csvname(resultsdir, "model_target_annualresults_plus_chi", identifier)
	writecsv(opt_results, filename, row.names=FALSE)

	#-------------------------------------------------------------------------------------------------------

	list(
		annual_obj	= Objannual,
		partial_chi	= Partial_chi,
		opt_results	= opt_results
	)
	#Objannual
}

