print("-----------------------------------------")
print("Model setup for MERP_NorthSea_1970-1999")
print("-----------------------------------------")


#LIST THE FILES CONTAINING ESSENTIAL EXTERNALLY SET PARAMETER VALUES AND DRIVING DATA
#------------------------------------------------------------------------------------
physicalconfigfile   <-"physical_parameters_2SURF_REV-TON-v13.csv"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
physicsdrivingfile   <-"1970-1999_physics_drivers_NORTH_SEA_ERSEM_bias_corrected.csv"
chemistrydrivingfile <-"1970-1999_boundary_data_NORTH_SEA_ERSEM_bias_corrected.csv"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#ECOLOGY MODEL PARAMETER FILES
#-----------------------------

initialstatefile                 <- "model_endstate_export-MERP_North_Sea_1970-1999_VERSION11-12(2).csv" # MERP NS 1970-99

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
eventtimingparameterfile         <- "1970-1999_biological_event_timing_parameters_e.csv"
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

fixedparameterfile_consumers     <-"fixed_parameters_for_consumer_groups-NEWYEAR18_tp_kelp_e40_1970-1999.csv" # OLD AND MERP NORTH SEA 70-99, IRISH SEA


fixedparameterfile_miscellaneous <-"fixed_parameters_miscellaneous_tp_kelp_lowQ10.csv" # OLD AND MERP NORTH SEA 70-99


fittedparameterfile_preferences  <- "fitted_parameters_preference_matrix-MERP_North_Sea_tp_kelp_VERSION11-12.csv"              #1970-1999 MERP NORTH SEA
fittedparameterfile_uptake_mort  <- "fitted_parameters_uptake_and_mortality_rates-MERP_North_Sea_tp_kelp_VERSION11-12.csv" #1970-1999 MERP NORTH SEA
fittedparameterfile_microbiology <- "fitted_parameters_microbiology_and_others-MERP_North_Sea_tp_kelp_VERSION11-12.csv"          #1970-1999 MERP NORTH SEA

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#FLEET MODEL PARAMETER FILES
#---------------------------

fishingparametersfile      <-"fishing_fleet_parameters_REVISED_MF_DPupscaled_selSW_tp_kelp_incNOK-VER11-12-INTdisc.csv"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
fishingactivityfile        <-"fishing_activity_parameters_1970-1999_incNOK-NEW-7-VERSION11-12(2).csv"
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

fishingpowerfile           <-"fishing_power_parameters_BMbycatch_tp_kelp_incNOK-NEW-4.csv"

fishingdiscardfile         <-"fishing_discard_parameters_BMbycatch_tp_kelp-NEW.csv"

fishingguttingfile         <-"fishing_processing_at_sea_parameters_BMbycatch_tp_kelp.csv"

fishingdistributionfile    <-"fishing_distribution_parameters_tp_kelp_incNOK-NEW.csv"

# Multipliers (values>=0) to be applied to the base-level activity rate of each gear
#gearmultfile               <-"fishing_activity_scaling_values_70-99_REVISED_ESTIMATE_1.csv"
gearmultfile               <-"fishing_activity_scaling_values.csv"

# Multipliers (values>=0) to be applied to the harvest ratios derived by the fleet model with the given levels of activity
# This is a means of scaling fishing rates up and down without affecting all the other collateral effects of fishing
# such as seabed abrasion
HRmultfile                 <-"harvest_ratio_scaling_values_tp_kelp.csv"




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#FILES OF OBSERVATIONAL (TARGET) DATA FOR COMPARING WITH MODEL OUTPUT AND FITTING
#--------------------------------------------------------------------------------

annualtargetfile     <- "annual_target_data_2SURF_V15tp_kelp-2_1970-1999_NEW-3.csv"  # 1970-1999 fitting data North Sea
monthlytargetfile    <- "monthly_target_data.csv"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

annealingSDcontrolfile <- "OATsensitivity_SD.csv"

