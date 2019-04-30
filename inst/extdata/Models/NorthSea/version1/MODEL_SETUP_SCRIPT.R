cat("---------------------------------------\n")
cat("Model setup for MERP_NorthSea_2003-2013\n")
cat("---------------------------------------\n")

#LIST THE FILES CONTAINING ESSENTIAL EXTERNALLY SET PARAMETER VALUES AND DRIVING DATA
#------------------------------------------------------------------------------------
# Driving_Data/
physicsdrivingfile			<- "physics_drivers.csv"
chemistrydrivingfile			<- "boundary_data.csv"

# Parameters/
physicalconfigfile			<- "physical_parameters.csv"

#ECOLOGY MODEL PARAMETER FILES
#-----------------------------
# Parameters/
initialstatefile			<- "model_endstate_export.csv"
eventtimingparameterfile		<- "biological_event_timing_parameters.csv"
fixedparameterfile_consumers		<- "fixed_parameters_for_consumer_groups.csv"
fixedparameterfile_miscellaneous	<- "fixed_parameters_miscellaneous.csv"
fittedparameterfile_preferences		<- "fitted_parameters_preference_matrix.csv"
fittedparameterfile_uptake_mort		<- "fitted_parameters_uptake_and_mortality_rates.csv"
fittedparameterfile_microbiology	<- "fitted_parameters_microbiology.csv"

#FLEET MODEL PARAMETER FILES
#---------------------------
# Parameters/
fishingparametersfile			<- "fishing_fleet_parameters.csv"
fishingactivityfile			<- "fishing_activity_parameters.csv"
fishingpowerfile			<- "fishing_power_parameters.csv"
fishingdiscardfile			<- "fishing_discard_parameters.csv"
fishingguttingfile			<- "fishing_processing_at_sea_parameters.csv"
fishingdistributionfile			<- "fishing_distribution_parameters.csv"
gearmultfile				<- "fishing_activity_scaling_values.csv"

foodwebflowmatrixfile			<- "food_web_flow_matrix_template.csv"

HRmultfile				<- "harvest_ratio_scaling_values.csv"

#FILES OF OBSERVATIONAL (TARGET) DATA FOR COMPARING WITH MODEL OUTPUT AND FITTING
#--------------------------------------------------------------------------------
annualtargetfile			<- "annual_target_data.csv"
monthlytargetfile			<- "monthly_target_data.csv"


