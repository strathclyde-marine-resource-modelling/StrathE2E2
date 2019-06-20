#
# annealing_fit_ecology_parameters.R
#
#' Finds the best-fit values of the ecology model parameters using an annealing process
#'
#' @param model current model configuration
#' @param nyears length of annealing run
#'
#' @return fitted parameter set
#'
#' @importFrom stats runif
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
# ---------------------------------------------------------------------

annealing_fit_ecology_parameters <- function(model, nyears=40) {

	setup				<- elt(model, "setup")
	model.path			<- elt(setup, "model.path")
        resultsdir			<- elt(setup, "resultsdir")
	identifier			<- elt(setup, "model.ident")

	data				<- elt(model, "data")
	fitted.parameters		<- elt(data, "fitted.parameters")
	HRscale_vector_multiplier	<- elt(data, "fleet.model", "HRscale_vector_multiplier")


	print(date())

	toppredlock <- TRUE		# LOCK THE BIRD SEAL AND CETACEAN UPTAKE PARAMETERES OR NOT.....

	# Set the annealing parameters:
	n_iter		<- 1000		# Maximum number of iterations of the parameter randomisation process
n_iter=5 # ZZ temp
	temperature	<- 0.00005	# values 0.00005 to 5. Higher values make the annealing process accept more bad jumps in th early stages 
	cooling		<- 0.975	# values < 1. Suggest do not change this

	if (length(which(HRscale_vector_multiplier<1 | HRscale_vector_multiplier>1)) > 0) {
		print("**************************************************************************")
		print("WARNING - one or more baseline Harvest Ratio scaling values differs from 1")
		print("**************************************************************************")
	}

	datastore <- c(
		fitted.parameters,
        	annual_obj = 1e-60
	)

	parhistory	<- as.data.frame(datastore)		# these are all data frames
	proposalhistory	<- parhistory

	n_acceptances<-0   # Counter for the number of parameter acceptances which have occurred

	#ITERATE THE ANNEALING PROCESS.....
	#
	for (kkk in 1:n_iter){

		#FIRST RUN THROUGH THE MODEL WITH THE CALIBRATION PERIOD HARVEST RATES.....
		#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		annealing.parms <- read_SD_control_ecology(model.path, "annealing_SD_ecology.csv")	# read every loop so control is available during a run
                if (kkk==1) annealing.parms[] <- 0.0

		# Jiggle the parameter values and load them into the vector which is passed to the model function
		perturbed <- perturb_parameters(datastore, annealing.parms, toppredlock)

		# perturbed parameters will be built into model.parameters for run:
		model$data$fitted.parameters <- perturbed
		results <- StrathE2E(model, nyears)

		err <- elt(results, "final.year.outputs", "annual_obj")
		perturbed$annual_obj <- err

		temperature<-temperature*cooling

		if (kkk==1) lastbest <- err

		#Now the Metropolis algorithm.....
		#
		lik_ratio<-exp(((log(err)) - log(lastbest))/temperature)
		rand<-runif(1,0,1)
		new_accepted<-" Accepted: NO"
		if(lik_ratio>rand){
			n_acceptances<-n_acceptances+1
			datastore <- perturbed
			lastbest <- err
			new_accepted <- " Accepted: YES"
		}

		#--------------------------------

		proposalhistory<-rbind(proposalhistory, perturbed)
		filename <- csvname(resultsdir, "annealing_par-proposalhistory", identifier)
		writecsv(proposalhistory, filename, row.names=FALSE)

		parhistory<-rbind(parhistory, datastore)
		filename <- csvname(resultsdir, "annealing_par-acceptedhistory", identifier)
		writecsv(parhistory, filename, row.names=FALSE)

		print(paste("Itn: ",kkk+1," ","Proposal: ", err,"   ",new_accepted,sep=""))

		#Plot or update the time series of proposal and acepted likelihoods so far....
		par(mfrow=c(1,1))
		axmin <- elt(annealing.parms, "axmin")
		axmax <- elt(annealing.parms, "axmax")
		plot(seq(1,nrow(proposalhistory)),proposalhistory$annual_obj,ylim=c(axmin,axmax),xlim=c(1,kkk+1),xlab="Iterations",ylab="Likelihood",type="l",col="grey")
		points(seq(1,nrow(parhistory)),parhistory$annual_obj,type="l",col="black",lwd=3)
	}

	#At the conclusion of the whole process, extract the final row of the parhistory into the format of model fitted parameter files and save to disc
	write_fitted_parameters(model, parhistory)
}

