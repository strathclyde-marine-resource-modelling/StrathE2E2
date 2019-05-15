#
# annealing_fit_ecology_parameters.R
#
#' Finds the best-fit values of the ecology model parameters using an annealing process
#'
#' @param model current model configuration
#'
#' @return fitted parameter set
#'
#' @export
#'
# ---------------------------------------------------------------------
# | R Script STRATHE2E_SPATIAL_ANNEALING_ECOLOGY_MODEL_PARAMETERS.R   |
# |                                                                   |
# | Prototype: End-to-end ecosystem model of the North Sea as         |
# | described in Heath (2012) Progress in Oceanography                |
# |                                                                   |
# | The model simulates the nitrogen mass only, of phytoplankton,     |
# | macrophytes, zooplankton, zoocarnivores, two benthos classes,     |
# | three fish classes, birds, pinnipeds and cetaceans, plus nitrate  |
# | and ammonia, and detritus                                         |
# |                                                                   |
# | There are 2 horizontal spatial zones (inshore/offshore, or        |
# | shallow/deep as you prefer). In the offshore/deep zone there are  |
# | 2 water column layers. In each horizontal zone there can be up to |
# | 3 sediment patches each with different properties, plus bare rock |
# | habitats                                                          |
# |                                                                   |
# | The whole system comprises two separate models - an ecology model |
# | and a fishing fleet model                                         |
# |                                                                   |
# | The code in this file calls a range of modular sub-routines       |
# | stored in separate R source files                                 |
# |                                                                   |
# | The main model code is written in C and loaded as a Windows dll   |
# | or a so-file for Linux                                            |
# |                                                                   |
# | Requires the R-library deSolve, and RTools installation           |
# |                                                                   |
# | ***************************************************************** |
# | This script uses annealing to find the best-fit values of the     |
# | ecology model parameters, given known values for the scaling      |
# | parameters linking effort to harvest ratios, and known gear       |
# | activity rates.                                                   |
# |                                                                   |
# | At the end of the run the code creates new versions of the three  |
# | "fitted_parameter... " files in the "/Parameters" folder of the   |
# | model version.                                                    |
# |                                                                   |
# | You will need to perform a single run of the model to a           |
# | with these new parameter sets and then save the end-state of the  |
# | model to act as new hot-start initial conditions to avoid any     |
# | burn-in period for future runs. The end state file is saved in    |
# | the designated results folder, so you may want to edit its        |
# | filename before copying into the model version "/Parameters"      |
# | folder where it will be found by future runs.                     |
# | ***************************************************************** |
# |                                                                   |
# | Author: Mike Heath, University of Strathclyde, Glasgow            |
# | Date of this version: 4 March 2019                                |
# |                                                                   |
# | To run this script, from a Windows or linux R-commander launch,   |
# | use File/Change_dir.. to set the home directory to the folder     |
# | holding this file, and then copy and paste the following into the |
# | R command line..                                                  |
# |               source("script_name.R")                             |
# ---------------------------------------------------------------------

annealing_fit_ecology_parameters <- function(model) {

	model.path			<- el(model, "path")
	data				<- el(model, "data")
	fitter.parameters		<- el(data, "fitted.parameters")
	fleet.model			<- el(data, "fleet.model")
	HRscale_vector_multiplier	<- el(fleet.model, "HRscale_vector_multiplier")

	run				<- el(model, "run")
        resultsdir			<- el(run, "resultsdir")

	print(date())

	# LOCK THE BIRD SEAL AND CETACEAN UPTAKE PARAMETERES OR NOT.....
	toppredlock <- 0		# 0 = LOCKED; 1 = ALLOW TO BE FITTED

	nyears <- 40		#Number of years to run each simulation

	# Set the annealing parameters:
	identifier	<- "TESTING_RUN_NEWTRIAL"
	n_iter		<- 1000		# Maximum number of iterations of the parameter randomisation process
	temperature	<- 0.00005	# values 0.00005 to 5. Higher values make the annealing process accept more bad jumps in th early stages 
	cooling		<- 0.975	# values < 1. Suggest do not change this

	if (length(which(HRscale_vector_multiplier<1 | HRscale_vector_multiplier>1)) > 0) {
		print("**************************************************************************")
		print("WARNING - one or more baseline Harvest Ratio scaling values differs from 1")
		print("**************************************************************************")
	}

	# this is simply the read_fitted_parameters
	#source("library/ecology/load_fitted_parameter_values_into_datastore_format_CLEAN_tp_kelp.R")

	datastore <- model$data$fitted.parameters
	datastore <- read_fitted_parameters(model.path)         # ZZ lets try it as a list
        datastore$annual_obj <- 1e-60                           # ZZ does this have to be part of datastore? - it makes it part of the history record, so it's convenient

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	parhistory	<- as.data.frame(datastore)
	proposalhistory	<- parhistory
	proposalstore	<- proposalhistory

	#-------------------------------------------------------------------------------------------------------

	n_acceptances<-0   # Counter for the number of parameter acceptances which have occurred

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	#ITERATE THE ANNEALING PROCESS.....

	for (kkk in 1:n_iter){

		#FIRST RUN THROUGH THE MODEL WITH THE CALIBRATION PERIOD HARVEST RATES.....
		#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		SD_control <- read_SD_control_ecology(model.path)	# read every loop so control is available during a run

		#At the start of each cycle need to run the fleet model setup 
		#source("library/PREPARE_THE_FISHING_FLEET_MODEL_tp_KELP.R")


		#source("library/RUN_THE_FISHING_FLEET_MODEL_tp_KELP.R")
		#_________________________________________________________________
		#Jiggle the parameter values and load them into the vector which is passed to the model function

		perturbed <- perturb_parameters(datastore, toppredlock)

        	#if(toppredlock==0){
			# LOCKED - some params are not perturbed
			#source("library/ecology/annealing_set_parameter_values_and_make_parms_vector_SD_fromfile_REVISED-V4_tp_kelp_T.R")
        	#}
        	#if(toppredlock==1){
			#source("library/ecology/annealing_set_parameter_values_and_make_parms_vector_SD_fromfile_REVISED-V4_tp_kelp.R")
        	#}
		#_________________________________________________________________
		#	source("library/RUN_THE_ECOLOGY_MODEL_tp_KELP.R")
		#source("library/RUN_THE_ECOLOGY_MODEL_tp_KELP-VERSION11.R")
		results <- StrathE2E(model)

		#_________________________________________________________________
		#Derive the set of annual summary results to compare with target data from the final year of the run and write to a file
		#source("library/tools/derive_model_target_results_MERP_2SURF_V10_extra3_tp_kelp.R")
		#_________________________________________________________________
		#source("library/tools/calculate_error_function_MERP_2SURF_V10_CLEAN_tp_kelp.R")
		#_________________________________________________________________

		for(ijk in 1:ncol(proposalstore)){	# ZZ can this be done without a loop?
			#proposalstore[1,ijk]<-Results[ijk]
			proposalstore[1,ijk]<-perturbed[ijk]
		}

		temperature<-temperature*cooling

		if (kkk==1){
			lastbest<-datastore$annual_obj		# ZZ not a data.frame now [nrow(datastore)]
		}

		#-------------------------------------------------------------------
		#Now the Metropolis algorithm.....

		lik_ratio<-exp(((log(Results["annual_obj"])) - log(lastbest))/temperature)

		rand<-runif(1,0,1)

		#--------------------------------

		new_accepted<-" Accepted: NO"

		if(lik_ratio>rand){
			n_acceptances<-n_acceptances+1
			for(ijk in 1:ncol(datastore)){
				datastore[1,ijk]<-Results[ijk]
			}

			lastbest<-Results["annual_obj"]

			new_accepted<-" Accepted: YES"


		}

		#--------------------------------

		proposalhistory<-rbind(proposalhistory,proposalstore)
		filename <- csvname(resultsdir, "annealing_par-proposalhistory", identifier)
		writecsv(proposalhistory, filename, row.names=FALSE)

		parhistory<-rbind(parhistory,datastore)
		filename <- csvname(resultsdir, "annealing_par-acceptedhistory", identifier)
		writecsv(parhistory, filename, row.names=FALSE)

		#-------------------------------------------------------------------

		#print(paste("Itn:",kkk+1," ",lastbest,"   ",new_accepted,sep=""))
		print(paste("Itn: ",kkk+1," ","Proposal: ",Results["annual_obj"],"   ",new_accepted,sep=""))

		#-------------------------------------------------------------------

		#Plot or update the time series of proposal and acepted likelihoods so far....
		par(mfrow=c(1,1))

		#Plot the basemodel results 
		plot(seq(1,nrow(proposalhistory)),proposalhistory$annual_obj,ylim=c(axmin,axmax),xlim=c(1,kkk+1),xlab="Iterations",ylab="Likelihood",type="l",col="grey")
		points(seq(1,nrow(parhistory)),parhistory$annual_obj,type="l",col="black",lwd=3)
	}

	#At the conclusion of the whole process, extract the final row of the parhistory into the format of model fitted parameter files and save to disc

	source(paste("library/tools/extract_from_datastore_format_back_into_fitted_parameter_files_tp_kelp.R",sep=""))

}

