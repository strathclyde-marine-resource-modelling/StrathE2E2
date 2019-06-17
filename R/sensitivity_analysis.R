#
# sensitivity_analysis.R
#
#' performs a one-at-a-time sensitivity analysis using
#' the Morris Method for factorial sampling of the ecology model
#' parameters, the fishing fleet model parameters, and the
#' environmental forcings
#'
#' @param model full model object
#'
#' @return list of default model run settings
#'
#' @importFrom stats runif 
#'
#' @export
#
sensitivity_analysis <- function(model) {

# ---------------------------------------------------------------------
# | R Script STRATHE2E_OAT_SENSITIVITY_ANALYSIS.R                     |
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
# | This script performs a oine-at-a-time sensitivity analysis using  |
# | the Morris Method for factorial sampling of the ecology model     |
# | parameters, the fishing fleet model parameters, and the           |
# | environmental forcings                                            |
# |                                                                   |
# | The results are saved to a nominated folder and then processed    |
# | to generate a diagram and outputs files                           |
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	model.path      <- elt(model, "path")


#The output time series of proposed and accepted parameters and likelihoods will be named...
identifier<-"MERP_North_Sea_OAT"  

#RUN CONFIGURATION DATA
#----------------------


#Number of years to run each simulation
nyears<-60


outf_label<-"_VERSION11-12_OAT(FFEset3)"  # Identification label incorporated into the output files

#This an internal label  - don't edit this
    AAA<-outf_label
identifier <- AAA


# CONFIGURATION OF THE SENSITIVITY ANALYSIS
#-------------------------------------------------------------------------------

#THIS VERSION RUNS (N+1)*J  RUNS OF THE MODEL
#N = NUMBER OF PARAMETERS
#J = NUMBER OF TRAJECTORIES

#This version picks a single P-LEVEL for each trajectory to be applied to each parameter in turn from a vectror of uniformly
#distributed discrete value

#set the sd for the random normal from which each parametyer will be jiggled for each trajectory
trajsd<-0.0075     #########   Best not to change this


#Set the cv level scaling factor to be applied to each parameter
n_levels<-1      # DONT CHANGE THIS
n_setoflevels<-4 # this must be an even number //////////////////////////
v_setoflevels<-0.1  #levels are set a +/- this cv arouund zero

pcvscaleL<-NULL
for(jj in 1:(n_setoflevels/2)){
	pcvscaleL<-c(pcvscaleL,(1-v_setoflevels)+((jj-1)*v_setoflevels/(n_setoflevels/2)))
}
pcvscaleU<-NULL
for(jj in 1:(n_setoflevels/2)){
	pcvscaleU<-c(pcvscaleU,(1+v_setoflevels)-((jj-1)*v_setoflevels/(n_setoflevels/2)))
}
pcvscaleU<-pcvscaleU[seq((length(pcvscaleU)),1,by=-1)]

pcvscale<-c(pcvscaleL,pcvscaleU)


#........................
n_iter<-4  # number of randomised sets of baseline parameters (trajectories)
#........................

#PRODUCES (PARAMETERS+1)*n_iter  RUNS
#ABOUT 367 PARAMETERS - SO BE CAREFUL !



#End of model setup

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#source("library/CONFIGURE_PATHS.R")
#source(paste(modeldef,"MODEL_SETUP_SCRIPT.R",sep=""))

#source("library/tools/load_ANNUAL_target_data_MERP_2SURF_V10_tp_kelp.R")

#source("library/PREPARE_THE_ECOLOGY_MODEL_tp_KELP-VERSION11.R") 
#source("library/PREPARE_THE_FISHING_FLEET_MODEL_tp_KELP.R")

	#source("library/RUN_THE_FISHING_FLEET_MODEL_tp_KELP.R")

#_________________________________________________________________





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#SO NOW WE NEED TO LOAD THE BEST FIT PARAMETER VECTOR, AND THEN
#LOOP THROUGH A SET OF RUNS WHERE THE BASELINE VECTOR IS GIVEN A RANDOM JIGGLE EACH TIME
#SO THIS IS A BIT LIKE THE ANNEALING CODE VERSION WHERE THE ACCEPTED PARAMETERS NEVER CHANGE


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Vector of the locations in parms which are to be tweaked
# ZZ don't like this parameter numbering...can we not use names?
p_to_tweak<-c(seq(1,4,by=1),seq(28,56,by=1),                         # physical config parameters excl the area proportions of sediments
              seq(57,123,by=1),seq(286,306,by=1),seq(307,585,by=1))  # FISHING PARAMETERS - omitting the undersize and discard switch parameters loc 124 and 125
                                                                     #                     and the proportional spatial distributions of discards and offal as these can easily be jiggled
                                                                     # ECOLOGY PARAMETERS - start from end of fleet vector slot through to end of parms

Nruns<-length(p_to_tweak)+1

#Set up a vector of parms locations which are constrained to lie within 0 and 1
p_zero_one<-c(83:123,  # discard and gutting rates
              286:293, # swept area ratios
              294:297, # benthois damage rates
              298,     # offal as prop live weight
              299:306, # plough depths as prop sed layer thickness
#--------------------------
              480:493, # assimilation efficiencies
              494:507, # excretion rates
              508    , # mt
              509    , # nst
              510    , # dst
              511    , # ndt
              512    , # ddt
              514    , # qs_p2
              515    , # qs_p3
              516    , # msedt
              518    , # nsedt
              520    , # dsedt
              539    , # kelpdebris_det
              540    , # corp_det
              541    , # disc_corp
              542    , # dsink_s
              543    , # dsink_d
              545:550, # migcoef rates
              570:579, # maximum exloitable fractions
              580:583) #fecundities


#Set up a vector of parms locations where the parameter is expected to be negative
p_negative<-c(517,519)   # msens,  nsens


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#This is the single run parms vector build - this gets done once only for the baseline parameter set
#source("library/ecology/load_fitted_parameter_values_and_make_parms_vector_MERP_2SURF_V12_fittedfromfile_tp_KELP.R")

def_parms<-parms

#set the defailt forcings set
def_forc<-forc
Nforcs<-length(names(forc))
forcid<-seq(1001,1000+Nforcs,by=1)
forcname<-(names(forc))
whichforclogs<-c(2,3)

#add Nforcs to Nruns
Nruns<-Nruns+Nforcs


#ITERATE THE TRAJECTORY PROCESS.....

for (kkk in 1:n_iter){

	traj_parms<-def_parms

	traj_forc<-def_forc


	#===========================================
	if(kkk==1){

		#Set up an array to hold the results which will be the liklihood values of the the modle runs given the parameters and the observed data
		OAT_results<-array(NA,dim=c(n_levels*Nruns*n_iter,6))
		colnames(OAT_results)<-c("parameterid","trajectoryid","levelid","delta_p","liklihood","EE")
		rownames(OAT_results)<-rep(rep(c("baseline",forcname,names(def_parms[p_to_tweak])),n_levels),n_iter)

		OAT_results[,1]<-rep(c(0,forcid,p_to_tweak),n_iter*n_levels)

		traji<-rep(1,Nruns*n_levels)
		traj<-NULL
		for(iiz in 1:(n_iter)) {
			traj<-c(traj,(traji+iiz-1))
		}

		OAT_results[,2]<-traj

		levid<-NULL
		lev<-NULL
		for(iiz in 1:(n_iter)) {
			level_to_use<-floor(runif(1,1,(n_setoflevels + 1 -(1e-10))))
			levid<-c(levid,rep(level_to_use,Nruns))
			lev<-c(lev,rep((pcvscale[level_to_use]-1),Nruns))
		}       # NOTE (p-1) here

		OAT_results[,3]<-levid
		OAT_results[,4]<-lev
		OAT_results[which(OAT_results[,1]==0),3]<-0
		OAT_results[which(OAT_results[,1]==0),4]<-0

		#Set up an array to hold the vectors of parameter values
		#OAT_parmvalues <- array(NA,dim=c((Nruns-Nforcs)*n_levels*n_iter,(Nruns-Nforcs)-1)) # This line has a mistake (...-Nforcs) not needed
		OAT_parmvalues <- array(NA,dim=c((Nruns)*n_levels*n_iter,(Nruns-Nforcs)-1))
		colnames(OAT_parmvalues)<-names(def_parms[p_to_tweak])

	}

	#===========================================


	if(kkk>1){    # <<<<<<<<<<<##############################>>>>>>>>>>>>>>
              # FIRST RUN FROM A COLD START USES THE MAX LIKELIHOOD AS THE BASELINE SO SET kkk>1
              # IF THIS IS A HOT START THEN CHANGE THIS TO kkk>0

		#Jiggle all the forcings to create a new trajectory

		for(ffff in 1:Nforcs){

			if(length( which(whichforclogs==ffff)  ) == 0){
				deltaforc<-rnorm(1,1,trajsd)
				traj_forc[[ffff]][,2]<-def_forc[[ffff]][,2]*deltaforc
			}

			if(length( which(whichforclogs==ffff)  ) > 0){
				deltaforc<-rnorm(1,1,trajsd)
				traj_forc[[ffff]][,2]<-(def_forc[[ffff]][,2]) + log(deltaforc)
			}

		}

		#Now we have to be a bit careful and redo the volume conservation calculations
		#First, these are the flows we actually know
		#		   fdriverso_inflow    (8)
		#		   fdriverd_inflow<-   (9)
		#		   fdriversi_inflow<-  (10)
		#		   fdriversi_outflow<- (13)
		#		   fdrivers_upwell<-   (16)
		#		   fdriverso_si_flow<- (14)

		#Derived outflow from si to so (15)
		#fdriversi_so_flow[,2]<-fdriverso_si_flow[,2] + fdriversi_inflow[,2] - fdriversi_outflow[,2]
		traj_forc[[15]][,2]  <- traj_forc[[14]][,2] + traj_forc[[10]][,2]  - traj_forc[[13]][,2]

		#Derived outflow from so (11)
		#fdriverso_outflow[,2]<-fdriverso_inflow[,2] + fdrivers_upwell[,2] + fdriversi_so_flow[,2]  -  fdriverso_si_flow[,2]
		traj_forc[[11]][,2]  <- traj_forc[[8]][,2] + traj_forc[[16]][,2] + traj_forc[[15]][,2] - traj_forc[[14]][,2]
	
		#Derived outflow from d (12)
		#fdriverd_outflow[,2]<-fdriverd_inflow[,2] - fdrivers_upwell[,2] 
		traj_forc[[12]][,2]  <- traj_forc[[9]][,2] - traj_forc[[16]][,2] 


		#Jiggle all the parameters (inlcuding the fixed ones) to create a a new trajectory

		for(aaaaa in p_to_tweak){

			if(length( which(p_negative==aaaaa)  ) == 0){
				traj_parms[aaaaa]<-max(0,rnorm(1,def_parms[aaaaa],def_parms[aaaaa]*trajsd))
			}

			if(length( which(p_negative==aaaaa)  ) > 0){
				traj_parms[aaaaa]<-max(0,rnorm(1,(-1*def_parms[aaaaa]),(-1*def_parms[aaaaa])*trajsd))
				traj_parms[aaaaa]<-(-1*traj_parms[aaaaa])
			}

			#Then any that are not allowed to be greater than 1 are trimmed back to 1
			if(length(  which(p_zero_one==aaaaa)  ) >0){
				if(traj_parms[aaaaa]>1) traj_parms[aaaaa]<-1
			}

		}

	}

	#===========================================

	#So, here we go....


	for(qqqq in 1:n_levels){        # for each level

		for(jjjj in 1:Nruns){           # for each parameter

			rtof<-(Nruns*n_levels*(kkk-1)) + (Nruns*(qqqq-1)) + jjjj
			rtst<-(Nruns*n_levels*(kkk-1)) + (Nruns*(qqqq-1)) + 1
			rten<-(Nruns*n_levels*(kkk-1)) + (Nruns*(qqqq-1)) + Nruns

			forc<-traj_forc

			parms<-traj_parms

			if(jjjj>1) {

				if(jjjj<(Nforcs+2)){

   					if(length( which(whichforclogs==(jjjj-1))  ) == 0){
   						forc[[(jjjj-1)]][,2]<-traj_forc[[(jjjj-1)]][,2]*pcvscale[OAT_results[rtof,3]]
   					}

   					if(length( which(whichforclogs==(jjjj-1))  ) > 0){
   						forc[[(jjjj-1)]][,2]<-(traj_forc[[(jjjj-1)]][,2]) + log(pcvscale[OAT_results[rtof,3]])
   					}


					#Now we have to be a bit careful and redo the volume conservation calculations
					#if jjjj is in the range 8 to 16 then we need to redo the volume conservation calculation

					#First, these are the flows we actually know
					#		   fdriverso_inflow    (8)
					#		   fdriverd_inflow<-   (9)
					#		   fdriversi_inflow<-  (10)
					#		   fdriversi_outflow<- (13)
					#		   fdrivers_upwell<-   (16)
					#		   fdriverso_si_flow<- (14)


					if(length(which( (c(10,13,14))==(jjjj-1))  ) > 0){
						#Derived outflow from si to so (15)
						#fdriversi_so_flow[,2]<-fdriverso_si_flow[,2] + fdriversi_inflow[,2] - fdriversi_outflow[,2]
						forc[[15]][,2]  <- forc[[14]][,2] + forc[[10]][,2]  - forc[[13]][,2]
					}
					if((jjjj-1)==15) forc[[15]][,2]<-traj_forc[[15]][,2]   # we can't jiggle this independently of the others

					if(length(which( (c(8,10,13,14,16))==(jjjj-1))  ) > 0){
						#Derived outflow from so (11)
						#fdriverso_outflow[,2]<-fdriverso_inflow[,2] + fdrivers_upwell[,2] + fdriversi_so_flow[,2]  -  fdriverso_si_flow[,2]
						forc[[11]][,2]  <- forc[[8]][,2] + forc[[16]][,2] + forc[[15]][,2] - forc[[14]][,2]
					}
					if((jjjj-1)==11) forc[[11]][,2]<-traj_forc[[11]][,2]   # we can't jiggle this independently of the others

					if(length(which( (c(9,16))==(jjjj-1))  ) > 0){
						#Derived outflow from d (12)
						#fdriverd_outflow[,2]<-fdriverd_inflow[,2] - fdrivers_upwell[,2] 
						forc[[12]][,2]  <- forc[[9]][,2] - forc[[16]][,2] 
					}
					if((jjjj-1)==12) forc[[12]][,2]<-traj_forc[[12]][,2]   # we can't jiggle this independently of the others


   				}

   				if(jjjj>(Nforcs+1)) {
   					parms[p_to_tweak[jjjj-Nforcs-1]] <- traj_parms[p_to_tweak[jjjj-Nforcs-1]] * pcvscale[OAT_results[rtof,3]]
   					if(length(     which(p_zero_one==parms[p_to_tweak[jjjj-Nforcs-1]])) >0 ) {
      						if(parms[p_to_tweak[jjjj-Nforcs-1]] > 1) parms[p_to_tweak[jjjj-Nforcs-1]]<-1
   					}
   				}

			}



			needtorun<-1
			if(jjjj>(Nforcs+1)){
				if(def_parms[p_to_tweak[jjjj-Nforcs-1]]==0){
     					needtorun<-0  # if the default parameter value is zero no need to run the model
				}
			}


			if(needtorun==1){

				results <- StrathE2E(model)

				annual.target.data <- read_annual_target_data(model$path)       # ZZ surely already loaded ? or has this been changed?
				opt_results <- derive_model_target_results(model, results, annual.target.data)

				err <- calculate_error_function(model, opt_results)

				#source("library/RUN_THE_ECOLOGY_MODEL_tp_KELP-VERSION11.R")
				#_________________________________________________________________
				#Derive the set of annual summary results to compare with target data from the final year of the run and write to a file
				#source("library/tools/derive_model_target_results_MERP_2SURF_V10_extra3_tp_kelp.R")

				source("library/tools/offline_calculate_error_function_MERP_2SURF_V10_CLEAN_tp_kelp.R")

				#--------------------------------------------------------------------------------

			}

			OAT_parmvalues[rtof,]<-parms[p_to_tweak]

			if(needtorun==1) OAT_results[rtof,5] <- Partial_chi[nrow(Partial_chi),]
			if(needtorun==0) OAT_results[rtof,5] <- OAT_results[rtst,5]               # if default parm value is zero implant the baseline likelihood


			if(jjjj==1) OAT_results[rtof,6]<-0
			if(jjjj>1) OAT_results[rtof,6]<-  ( (OAT_results[rtof,5]-OAT_results[rtst,5])/   sqrt((OAT_results[rtof,4]^2))    )


			if(jjjj>1) {
				annealing_control_data<-readcsv(model.path, SD_CONTROL_DIR, annealingSDcontrolfile)
				axmin<-annealing_control_data[1,1]
				axmax<-annealing_control_data[2,1]
				par(mfrow=c(1,1))
				pltxset<-c(  (forcid-max(forcid)) , p_to_tweak )
				plot(pltxset[1:(jjjj-1)],OAT_results[(rtst+1):rtof,5],type="p",col="black",xlab="parameter id",ylab="model liklihood",main=paste("Trajectory ",kkk,sep=""),ylim=c(axmin,axmax),xlim=c(pltxset[1],pltxset[Nruns-1]))
				lines(pltxset[c(1,(Nruns-1))],c(OAT_results[1,5],OAT_results[1,5]),col="red")
				lines(pltxset[c(1,(Nruns-1))],c(OAT_results[rtst,5],OAT_results[rtst,5]),col="grey")
			}


			print(OAT_results[rtof,])

			#-------------------
			#Transfer the results into a dataframe for output (so number of elements in the csv header row = number of columns)
			# - makes it easier to read the data in again
	
			results_df_out <- as.data.frame(rep(0,nrow(OAT_results)))
			names(results_df_out) <-"parametername"
			results_df_out$parameterid<-OAT_results[,1]
			results_df_out$trajectoryid<-OAT_results[,2]
			results_df_out$levelid<-OAT_results[,3]
			results_df_out$delta_p<-OAT_results[,4]
			results_df_out$liklihood<-OAT_results[,5]
			results_df_out$EE<-OAT_results[,6]
			results_df_out$parametername <- rownames(OAT_results)
			#-------------------


			filename <- csvname(resultsdir, "OAT_results", identifier)
			writecsv(results_df_out, filename, row.names=FALSE)

			filename <- csvname(resultsdir, "OAT_parameter_values", identifier)
			writecsv(OAT_parmvalues, filename, row.names=FALSE)

		}  # end inner Runs jjjj loop
	}  # end mid levels qqqq loop
}  # end outer trajectories kkk loop


#print(OAT_results)


#-------------------------------------------------------------------------------------------------------


#PROCESS THE FINAL RESULTS TO GET THE GRAPH AND THE SORTED PARAMETER SENSITIVITY FILE

source("library/tools/process_OAT_results_V3_NOREAD_tp_kelp_withdrivers.R")


#-------------------------------------------------------------------------------------------------------


#End of the OAT analysis
}

