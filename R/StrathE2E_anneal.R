#
# StrathE2E_anneal.R
#
#' Attempt to fit the model to the given data
#'
#' Uses a simulated annealing process to fit the model data
#'
#' @param model current model configuration
#'
#' @return fitted parameter set
#'
#' @seealso \code{\link{list_models}}, \code{\link{read_model}}, \code{\link{copy_model}}, \code{\link{rebuild_model}}
#'
#' @importFrom deSolve ode
#' @importFrom stats runif
#'
#' @useDynLib StrathE2E2
#'
#' @export
#'
StrathE2E_anneal <- function(model) {

	# check we are setup for annealing:
	if (is.null(model$data$annual.target.data)) {
		stop("Error: don't forget to use annealing=T when loading your model !")
	}

	model.path	<- el(model, "path")
	run		<- el(model, "run")
	nyears		<- el(run, "nyears")		# ZZ should be 100 number of years to run each simulation
	times		<- el(run, "times")
	oudir		<- el(run, "oudir")

	annealing	<- el(run, "annealing")
	n_iter		<- el(annealing, "n_iter")		# ZZ should be 10 Maximum number of iterations of the parameter randomisation process
	identifier	<- el(annealing, "identifier")
	temperature	<- el(annealing, "temperature")
	cooling		<- el(annealing, "cooling")
	axmin		<- el(annealing, "axmin")
	axmax		<- el(annealing, "axmax")

	data		<- el(model, "data")
	initial.state	<- el(data, "initial.state")
	forcings	<- el(data, "forcings")
	physical.parms	<- el(data, "physical.parameters")

	# ---------------------------------------------------------------------
	# | R Programme STRATHE2E_SPATIAL_ANNEALING.R                         |
	# |                                                                   |
	# | Prototype: End-to-end ecosystem model of the North Sea as         |
	# | described in Heath (2012) Progress in Oceanography                |
	# |                                                                   |
	# | The model simulates the nitrogen mass only, of phytoplankton,     |
	# | zooplankton, zoocarnivores, two benthos classes, three fish       |
	# | classes, birds/mammals, plus nitrate and ammonia, and detritus    |
	# |                                                                   |
	# | There are 2 horizontal spatial zones (inshore/offshore, or        |
	# | shallow/deep as you prefer). In the offshore/deep zone there are  |
	# | 2 water column layers. In each horizontal zone there can be up to |
	# | 3 sediment patches each with different properties                 |
	# |                                                                   |
	# | The whole model comprises two separate models - an ecology model  |
	# | and a fishing fleet model                                         |
	# |                                                                   |
	# | The code in this file calls a range of modular sub-routines       |
	# | stored in separate R source files                                 |
	# |                                                                   |
	# | The main model code is written in C and loaded as a Windows dll   |
	# | or a so-file for Linux                                            |
	# |                                                                   |
	# | Requires the R-library deSolve                                    |
	# |                                                                   |
	# | Author: Mike Heath, University of Strathclyde, Glasgow            |
	# | Date of this version: 15 Feb 2018                                 |
	# |                                                                   |
	# | To run, copy and paste the following into the R command line..    |
	# |               source("STRATHE2E_SPATIAL_ANNEALING.R")             |
	# ---------------------------------------------------------------------

	#The output time series of proposed and accepted parameters and likelihoods will be named...
	#   annealing_par-acceptedhistory-[identifier].csv
	#   annealing_par-proposalhistory-[identifier].csv

	#Penalised liklihood settings
	#............................
	perform_zero_fishing_test	<- 1		# Switch for zero fishing test ... 1 = yes, 0 = no	ZZ hard coded - 9 pars
	zero_fishing_biomass_test	<- 0.75		# Penalty test 1 - zero fishing biomass of both PF and DF must be MORE than x* value in the target fitting run. Value in the range 0-1. 
	
	perform_DF_fishing_test		<- 1		# Switch for altered demersal fishing test ... 1 = yes, 0 = no
	df_penalty_test_hr		<- 1.25		# penalty test 2 - scaling applied to DF harvest ratio for DF high-fishing test
	df_penalty_test_landings_scale	<- 1		# penalty test 2 - high fishing DF landings must be LESS than x* value in the target fitting run

	perform_PF_fishing_test		<- 1		# Switch for altered pelagic fishing test ... 1 = yes, 0 = no
	pf_penalty_test_hr		<- 1.5		# penalty test 3 - scaling applied to PF harvest ratio for PF high-fishing test
	pf_penalty_test_landings_scale	<- 0.975	# penalty test 3 - high fishing PF landings must be LESS than x* value in the target fitting run

	penalty_value			<-0.9		# likelihood multiplied by (1-x) for each penalty test failure. Value in the range 0-1. 

	#----------------------------

	#End of model setup

	
	#These are some internal labels and flags - don't edit these
	test1txt<-" All_HR0:NA,"
	test2txt<-" High_HRdf:NA,"
	test3txt<-" High_HRpf:NA,"

	fishing_test1<-(-1)
	fishing_test2<-(-1)
	fishing_test3<-(-1)

	AAA<-identifier

	#Read in and store the initial parameter vector, assign fixed parameters ...
	datastore <- read_fitted_parameters(model.path)		# ZZ lets try it as a list
	datastore$annual_obj <- 1e-60				# ZZ does this have to be part of datastore? - it makes it part of the history record, so it's convenient

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	parhistory<-as.data.frame(datastore)			# parhistory, etc. need to be a DF
	proposalhistory<-parhistory
	proposalstore<-proposalhistory

	BASE_acceptedstore_annual_obj<-datastore$annual_obj
	BASE_proposalstore_annual_obj<-datastore$annual_obj

	#-------------------------------------------------------------------------------------------------------

	n_acceptances<-0   # Counter for the number of parameter acceptances which have occurred

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	fleet.model <- configure_fishing_fleet_model(model.path, physical.parms)

	annealing.parms <- read_annealing_parameters(model.path)

	#ITERATE THE ANNEALING PROCESS.....

	# RUN 1:

	for (kkk in 1:n_iter){

		annealing <- annealing.parms
		if (kkk==1) annealing[] <- 0.0

		#FIRST RUN THROUGH THE MODEL WITH THE CALIBRATION PERIOD HARVEST RATES.....
		#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		perturbed <- perturb_parameters(datastore, annealing)
		model$data$fitted.parameters <- perturbed	# ZZ this is subject to typo mistakes as well!

		######## CALL 1 ###################
		######## CALL 1 ###################
		######## CALL 1 ###################

		results <- StrathE2E(model)

		#Derive the set of annual summary results to compare with target data from the final year of the run and write to a file
		opt_results <- derive_model_target_results(model, results)

		#Store the results for reference to compare with the subsequent HR scenario runs
		opt_results_BASERUN <- opt_results

		err <- calculate_error_function(model, opt_results)
		datastore$annual_obj <- err

		BASE_proposalstore_annual_obj<-c(BASE_proposalstore_annual_obj, err)
		BASE_acceptedstore_annual_obj<-c(BASE_acceptedstore_annual_obj,BASE_acceptedstore_annual_obj[length(BASE_acceptedstore_annual_obj)])

		#-------------------------------------------------------------------------------------------------------


		#END OF FIRST RUN THROUGH THE MODEL
		#So we have got the likelihood fo rthe proposed parameters for the cvalibration period run
		#Next is to run the model again and see if the acceptance criteri are met

		#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		#SECOND RUN THROUGH WITH DF AND PF HR SET TO ZERO
		#WE DEMAND THAT BOTH DF AND PF ARE >0 IN THE RESULTS

		if(perform_zero_fishing_test==1) {

			#.................................................................


			#SET ALL THE ACTIVITIES TO ZERO SO NO PLOUGHIG EFFECTS EITHER - SO WE DON'T READ THE FLEET MODEL INPUTS
			#THIS REPRESENTS A WORLD WITH ABSOLUTELY NO FISHING EFFECTS

			old_gear_activity <- model$data$fleet.model$gear_activity
			model$data$fleet.model$gear_activity <- 0

			#Now simply drop the new fleet vector into the existing parms vector - BE CAREFUL IT HAS TO GO INTO THE CORRECT SPACE

			#parms[53:147] <- fleet_model_output$fleet_vector	# ZZ argh	TODO

			######## CALL 2 ###################
			######## CALL 2 ###################
			######## CALL 2 ###################

			results <- StrathE2E(model)		# will rerun fleet model, rebuild parms and run ode

			#Derive the set of annual summary results to compare with target data from the final year of the run and write to a file
			opt_results <- derive_model_target_results(model, results)

			#TEST THE OUTCOME...
			if(opt_results[5,1] > zero_fishing_biomass_test * opt_results_BASERUN[5,1] & opt_results[6,1] > zero_fishing_biomass_test * opt_results_BASERUN[6,1]) {
				fishing_test1 <- 1
    			} else {
    				fishing_test1 <- 0
    			}
			# 1 = passed the test, 0 = failed the test

			#Penalise the baserun results if the criterin is not met....

          		if(fishing_test1==0) err <- err*(1-penalty_value)

			#END OF SECOND RUN THROUGH THE MODEL
			#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		}

		#THIRD RUN THROUGH WITH DF SET TO xxx*BASE
		#WE DEMAND THAT DF landings must be less than the baserun landings

		if(perform_DF_fishing_test==1) {

			#THIS TIME THROUGH WE NEED TO REREAD THE FLEETMODEL INPUT DATA AND RESTORE ACTIVITY AND PLOUGHING EFFECTS
			#SO IN THIS SCENARIO ITS ONLY THE DF HR WHICH IS CHANGED. ALL NON-HARVESTING EFFECTS ARE AS IN THE BASE RUN

			model$data$fleet.model$gear_activity <- old_gear_activity

			#Tweak the harvest ratios here

			old_HRscale_vector <- model$data$fleet.model$HRscale_vector

			model$data$fleet.model$HRscale_vector[1] <- old_HRscale_vector[1] * 1                     # Pelagic harvesting
			model$data$fleet.model$HRscale_vector[2] <- old_HRscale_vector[2] * df_penalty_test_hr    # Demersal harvesting
			model$data$fleet.model$HRscale_vector[3] <- old_HRscale_vector[3] * 1                     # Migfish harvesting
			model$data$fleet.model$HRscale_vector[4] <- old_HRscale_vector[4] * 1    		       # FD benthos harvesting
			model$data$fleet.model$HRscale_vector[5] <- old_HRscale_vector[5] * 1                     # CS benthos harvesting
			model$data$fleet.model$HRscale_vector[6] <- old_HRscale_vector[6] * 1                     # Canzoo harvesting
			model$data$fleet.model$HRscale_vector[7] <- old_HRscale_vector[7] * 1                     # Birdmammal harvesting
			
			#Run the fleet model to create the vector 'fleet_vector' which gets inserted into the parameter list for the ecology model

			#Now simply drop the new fleet vector into the existing parms vector - BE CAREFUL IT HAS TO GO INTO THE CORRECT SPACE
			#parms[53:147]<-fleet_vector	# ZZ TODO

			######## CALL 3 ###################
			######## CALL 3 ###################
			######## CALL 3 ###################

			results <- StrathE2E(model)		# will rerun fleet model, rebuild parms and run ode

			opt_results <- derive_model_target_results(model, results)

			#TEST THE OUTCOME...
			if(opt_results[21,1] < (df_penalty_test_landings_scale * opt_results_BASERUN[21,1]) ) {
				fishing_test2 <- 1
        		} else {
        			fishing_test2 <- 0
        		}
			# 1 = passed the test, 0 = failed the test

			#Penalise the baserun results if the criterin is not met....

                	if(fishing_test2==0) err <- err*(1-penalty_value)

			#END OF THIRD RUN THROUGH THE MODEL
			#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		}

		#FOURTH RUN THROUGH WITH PF SET TO test_hr*BASE
		#WE DEMAND THAT DF landings must be less than x*baserun landings

		if(perform_PF_fishing_test==1) {

			#THIS TIME THROUGH WE NEED TO REREAD THE FLEETMODEL INPUT DATA AND RESTORE ACTIVITY AND PLOUGHING EFFECTS
			#SO IN THIS SCENARIO ITS ONLY THE DF HR WHICH IS CHANGED. ALL NON-HARVESTING EFFECTS ARE AS IN THE BASE RUN

			model$data$fleet.model$HRscale_vector <- old_HRscale_vector

			#Tweak the harvest ratios here

			model$data$fleet.model$HRscale_vector[1] <- old_HRscale_vector[1] * pf_penalty_test_hr        # Pelagic harvesting
			model$data$fleet.model$HRscale_vector[2] <- old_HRscale_vector[2] * 1                         # Demersal harvesting
			model$data$fleet.model$HRscale_vector[3] <- old_HRscale_vector[3] * 1                         # Migfish harvesting
			model$data$fleet.model$HRscale_vector[4] <- old_HRscale_vector[4] * 1                         # FD benthos harvesting
			model$data$fleet.model$HRscale_vector[5] <- old_HRscale_vector[5] * 1                         # CS benthos harvesting
			model$data$fleet.model$HRscale_vector[6] <- old_HRscale_vector[6] * 1                         # Canzoo harvesting
			model$data$fleet.model$HRscale_vector[7] <- old_HRscale_vector[7] * 1                         # Birdmammal harvesting

			#Now simply drop the new fleet vector into the existing parms vector - BE CAREFUL IT HAS TO GO INTO THE CORRECT SPACE

			#parms[53:147]<-fleet_vector	# ZZ TODO

			######## CALL 4 ###################
			######## CALL 4 ###################
			######## CALL 4 ###################

			results <- StrathE2E(model)		# will rerun fleet model, rebuild parms and run ode

			#Derive the set of annual summary results to compare with target data from the final year of the run and write to a file
			opt_results <- derive_model_target_results(model, results)

			#TEST THE OUTCOME...
			if(opt_results[20,1] < (pf_penalty_test_landings_scale * opt_results_BASERUN[20,1]) ) {
				fishing_test3 <- 1
        		} else {
        			fishing_test3 <- 0
        		}
			# 1 = passed the test, 0 = failed the test

			#Penalise the baserun results if the criterin is not met....

       			if(fishing_test3==0) err <- err*(1-penalty_value)

			#END OF FOURTH RUN THROUGH THE MODEL
			#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		}

		for(ijk in 1:ncol(proposalstore)){
			proposalstore[1,ijk]<-perturbed[ijk]	# ZZ do I need a for loop here?
		}

		temperature<-temperature*cooling

		if (kkk==1){
			lastbest<-datastore$annual_obj	# ZZ no londer a DF [nrow(datastore)]
		}

		#-------------------------------------------------------------------
		#Now the Metropolis algorithm.....

		lik_ratio<-exp(((log(err)) - log(lastbest))/temperature)

		rand<-runif(1,0,1)

		#--------------------------------

		new_accepted<-" Accept_newP:NO"
		
		if(lik_ratio>rand){
			n_acceptances<-n_acceptances+1
			#for(ijk in 1:ncol(datastore)){
				#datastore[1,ijk]<-Results[ijk]	# ZZ no longer a DF
			#}
			datastore<-perturbed

			lastbest<-err

			new_accepted<-" Accept_newP:YES"

			#Keep a record of the base model results
			BASE_acceptedstore_annual_obj[length(BASE_acceptedstore_annual_obj)] <- BASE_proposalstore_annual_obj[length(BASE_proposalstore_annual_obj)]
		}

		#--------------------------------

		proposalhistory<-rbind(proposalhistory,proposalstore)
		parhistory<-rbind(parhistory,datastore)

		write.table(proposalhistory,file=paste(oudir,"annealing_par-proposalhistory-",identifier,".csv",sep=""),sep=",",row.names=FALSE)
		write.table(parhistory,file=paste(oudir,"annealing_par-acceptedhistory-",identifier,".csv",sep=""),sep=",",row.names=FALSE)

		#-------------------------------------------------------------------

		if(fishing_test1==0) test1txt<-" All_HR0:FAIL,"
		if(fishing_test1==1) test1txt<-" All_HR0:PASS,"
    
		if(fishing_test2==0) test2txt<-" High_HRdf:FAIL,"
		if(fishing_test2==1) test2txt<-" High_HRdf:PASS,"

		if(fishing_test3==0) test3txt<-" High_HRpf:FAIL,"
		if(fishing_test3==1) test3txt<-" High_HRpf:PASS,"

		print(paste("Itn:",kkk+1," ",test1txt,test2txt,test3txt,new_accepted,sep=""))

		#-------------------------------------------------------------------

		#Plot or update the time series of proposal and acepted likelihoods so far....
		par(mfrow=c(1,1))

		#Plot the basemodel results 
		plot(seq(1,nrow(proposalhistory)),BASE_proposalstore_annual_obj,ylim=c(axmin,axmax),xlim=c(1,kkk+1),xlab="Iterations",ylab="Parameter liklihood",type="l",col="orange")
		points(seq(1,nrow(parhistory)),BASE_acceptedstore_annual_obj,type="l",col="red",lwd=3)

		#Plot the penalised model results
		points(seq(1,nrow(proposalhistory)),proposalhistory$annual_obj,type="l",col="grey")
		points(seq(1,nrow(parhistory)),parhistory$annual_obj,type="l",col="black",lwd=3)
	}

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	#At the conclusion of the whole process, extract the final row of the parhistory into the format of model fitted parameter files and save to disc
	
	# fitted <- tail(parhistory, 1)		perhaps even just datastore (last thing done)?
	#write_fitted_parameters(model.path, fitted)
	#source(paste("library/2surf/extract_from_datastore_format_back_into_fitted_parameter_files.R",sep=""))	# ZZ TODO

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	#Compile the basemodel results into a dataframe and save
	basemodel_output<-data.frame("proposed"=BASE_proposalstore_annual_obj)
	basemodel_output$accepted<-BASE_acceptedstore_annual_obj
	# ZZ TODO write.table(basemodel_output,file=paste(oudir,"annealing_basemodel_history-",identifier,".csv",sep=""),sep=",",row.names=FALSE)
}

