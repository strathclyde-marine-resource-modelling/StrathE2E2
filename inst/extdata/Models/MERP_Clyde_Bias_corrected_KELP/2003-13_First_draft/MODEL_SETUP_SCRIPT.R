print("-------------------------------------------")
print("Model setup for MERP_Clyde_2003-2013")
print("-------------------------------------------")


#LIST THE FILES CONTAINING ESSENTIAL EXTERNALLY SET PARAMETER VALUES AND DRIVING DATA
#------------------------------------------------------------------------------------
physicalconfigfile   <-"physical_parameters_CLYDE-v13.csv"


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
physicsdrivingfile   <-"2003-2013_physics_drivers_2surf_pdistreordered-CLYDE.csv"
chemistrydrivingfile <-"2003-2013_boundary_data_2surf_SInitadjusted-CLYDE_WOAtlas.csv"
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#ECOLOGY MODEL PARAMETER FILES
#-----------------------------

initialstatefile                 <- "model_endstate_export-MERP_CLYDE3.csv"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
eventtimingparameterfile         <- "2003-2013_biological_event_timing_parameters_clyde.csv"
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

fixedparameterfile_consumers     <-"fixed_parameters_for_consumer_groups-NEWYEAR18_tp_kelp_e30_2003-2013.csv" #MERP WEST SCOT, CELTIC SEA, ENGLISH CHANNEL

fixedparameterfile_miscellaneous <-"fixed_parameters_miscellaneous_tp_kelp_lowQ10.csv" # ALL

fittedparameterfile_preferences  <- "fitted_parameters_preference_matrix-MERP_North_Sea_tp_kelp_VERSION11-12.csv"              #1970-1999 MERP NORTH SEA
fittedparameterfile_uptake_mort  <- "fitted_parameters_uptake_and_mortality_rates-MERP_North_Sea_tp_kelp_VERSION11-12.csv"     #1970-1999 MERP NORTH SEA
fittedparameterfile_microbiology <- "fitted_parameters_microbiology_and_others-MERP_North_Sea_tp_kelp_VERSION11-12.csv"        #1970-1999 MERP NORTH SEA

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#FLEET MODEL PARAMETER FILES
#---------------------------

fishingparametersfile      <-"fishing_fleet_parameters_CLYDE-V11-12(4)EXTdisc.csv"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
fishingactivityfile        <-"fishing_activity_parameters_2003-2013_CLYDE.csv"
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

fishingpowerfile           <-"fishing_power_parameters_BMbycatch_tp_kelp_incNOK-CLYDE.csv"

fishingdiscardfile         <-"fishing_discard_parameters_BMbycatch_tp_kelp-CLYDE.csv"

fishingguttingfile         <-"fishing_processing_at_sea_parameters_BMbycatch_tp_kelp-WESTSCOT.csv"

fishingdistributionfile    <-"fishing_distribution_parameters_tp_kelp_CLYDE.csv"

# Multipliers (values>=0) to be applied to the base-level activity rate of each gear
gearmultfile               <-"fishing_activity_scaling_values.csv"

# Multipliers (values>=0) to be applied to the harvest ratios derived by the fleet model with the given levels of activity
# This is a means of scaling fishing rates up and down without affecting all the other collateral effects of fishing
# such as seabed abrasion
HRmultfile                 <-"harvest_ratio_scaling_values_tp_kelp.csv"




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#FILES OF OBSERVATIONAL (TARGET) DATA FOR COMPARING WITH MODEL OUTPUT AND FITTING
#--------------------------------------------------------------------------------

annualtargetfile     <- "annual_target_data_2SURF_V15tp_kelp-2_2003-2013_Clyde.csv"
monthlytargetfile    <- "monthly_target_data.csv"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

foodwebflowmatrixfile <- "food_web_flow_matrix_template_tp_kelp.csv"

