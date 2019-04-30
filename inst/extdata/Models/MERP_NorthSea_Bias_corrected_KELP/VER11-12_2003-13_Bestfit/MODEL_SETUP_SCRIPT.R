print("-----------------------------------------")
print("Model setup for MERP_NorthSea_2003-2013")
print("-----------------------------------------")


#LIST THE FILES CONTAINING ESSENTIAL EXTERNALLY SET PARAMETER VALUES AND DRIVING DATA
#------------------------------------------------------------------------------------
physicalconfigfile   <- "physical_parameters_2SURF_REV-TON-v13.csv"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
physicsdrivingfile   <- "2003-2013_physics_drivers_NORTH_SEA_ERSEM_bias_corrected.csv"
chemistrydrivingfile <- "2003-2013_boundary_data_NORTH_SEA_ERSEM_bias_corrected_WOAtlas.csv"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#ECOLOGY MODEL PARAMETER FILES
#-----------------------------

initialstatefile                 <-  "model_endstate_export-MERP_North_Sea_2003-2013_VERSION11-12.csv" # MERP NS 2003-2013 - after HR optimistaion

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
eventtimingparameterfile         <-  "2003-2013_biological_event_timing_parameters_e_2.csv" # 2003-2013
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

fixedparameterfile_consumers     <- "fixed_parameters_for_consumer_groups-NEWYEAR18_tp_kelp_e30_2003-2013.csv" # OLD AND MERP NORTH SEA 70-99, IRISH SEA


fixedparameterfile_miscellaneous <- "fixed_parameters_miscellaneous_tp_kelp_lowQ10.csv" # OLD AND MERP NORTH SEA 70-99


fittedparameterfile_preferences  <-  "fitted_parameters_preference_matrix-MERP_North_Sea_tp_kelp_VERSION11-12.csv"              #1970-1999 MERP NORTH SEA
fittedparameterfile_uptake_mort  <-  "fitted_parameters_uptake_and_mortality_rates-MERP_North_Sea_tp_kelp_VERSION11-12.csv" #1970-1999 MERP NORTH SEA
fittedparameterfile_microbiology <-  "fitted_parameters_microbiology_and_others-MERP_North_Sea_tp_kelp_VERSION11-12.csv"          #1970-1999 MERP NORTH SEA

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#FLEET MODEL PARAMETER FILES
#---------------------------

fishingparametersfile      <- "fishing_fleet_parameters_REVISED_MF_DPupscaled_selSW_tp_kelp_incNOK-VER11-12-EXTdisc.csv"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
fishingactivityfile        <- "fishing_activity_parameters_2003-2013_incNOK-NEW-3.csv"
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

fishingpowerfile           <- "fishing_power_parameters_BMbycatch_tp_kelp_incNOK-NEW-4.csv"

fishingdiscardfile         <- "fishing_discard_parameters_BMbycatch_tp_kelp-NEW.csv"

fishingguttingfile         <- "fishing_processing_at_sea_parameters_BMbycatch_tp_kelp.csv"

fishingdistributionfile    <- "fishing_distribution_parameters_tp_kelp_incNOK-NEW.csv"

# Multipliers (values>=0) to be applied to the base-level activity rate of each gear
gearmultfile               <- "fishing_activity_scaling_values.csv"

# Multipliers (values>=0) to be applied to the harvest ratios derived by the fleet model with the given levels of activity
# This is a means of scaling fishing rates up and down without affecting all the other collateral effects of fishing
# such as seabed abrasion
HRmultfile                 <- "harvest_ratio_scaling_values_tp_kelp.csv"




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#FILES OF OBSERVATIONAL (TARGET) DATA FOR COMPARING WITH MODEL OUTPUT AND FITTING
#--------------------------------------------------------------------------------

annualtargetfile     <-  "annual_target_data_2SURF_V15tp_kelp-2_2003-2013_NEW-3.csv"  # 2003-2013 fitting data North Sea
monthlytargetfile    <-  "monthly_target_data.csv"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

