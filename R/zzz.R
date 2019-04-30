.onLoad <- function(libname = find.package("StrathE2E2"), pkgname = "StrathE2E2") {

	# CRAN Note avoidance
	utils::globalVariables(c(
		"physicalconfigfile",
		"physicsdrivingfile",
		"chemistrydrivingfile",
		"initialstatefile",
		"eventtimingparameterfile",
		"fixedparameterfile_consumers",
		"fixedparameterfile_miscellaneous",
		"fittedparameterfile_preferences",
		"fittedparameterfile_uptake_mort",
		"fittedparameterfile_microbiology",
		"fishingparametersfile",
		"fishingactivityfile",
		"fishingpowerfile",
		"fishingdiscardfile",
		"fishingguttingfile",
		"fishingdistributionfile",
		"gearmultfile",
		"HRmultfile",
		"annualtargetfile",
		"monthlytargetfile",
		"annealingSDcontrolfile",
		"foodwebflowmatrixfile"
	))

	invisible(NULL)
}

