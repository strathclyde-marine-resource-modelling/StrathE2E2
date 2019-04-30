#
# set_default_run.R
#
#' return settings for a default run
#'
#' returns named list of time settings, output directories, plus optional annealing parameters if required
#'
#' @param model.name name of model being loaded
#' @param model.variant which model variant being loaded
#' @param model.tag appended to output filenames
#' @param model.path full path to model configuration files
#' @param nyears length of model run in years
#' @param annealing boolean stating whether annealing settings are to be returned
#'
#' @return list of default model run settings
#'
#' @export
#
set_default_run <- function(model.name, model.variant, model.tag, model.path, nyears=20, annealing=F) {		# ZZ make internal?

	outp_time_interval <- 1				# interval for outputting data (days)

	default_run <- list(
		# main model settings:
		"nyears"	= nyears,
		"ndays"		= nyears*360 + 1,
		"drndays"	= nyears*12,
		"times"		= seq(0, nyears*360, by=outp_time_interval),
		"drtimes"	= seq(15, nyears*360-15, by=30),
		"sprectimes"	= seq(0, nyears*360, by=1),
		"daynum"	= seq(0, 360, by=1),
		"AAA"		= "MERP_North_Sea_TESTING",				# ZZ parameterise?
		"oudir"		= makepath(MODEL_RESULTS_DIR, model.name, model.variant)
	)

	if (annealing) {
		# annealing settings requested:
		annealing.parameters <- read_annealing_parameters(model.path)	# ZZ make separate function, and make it internal? don't need model.path as par to set_default_run() 

		annealing.parameters$identifier		<- "StrathE2E-testbed"	# ZZ needs parameterising?
		annealing.parameters$n_iter		<- 1			# ZZ 10
		annealing.parameters$temperature	<- 0.00005		# parameter of the annealing scheme - sets the probability of accepting a retrograde step
		annealing.parameters$cooling		<- 0.975		# rate at which the annualing scheme focusses in on a fit.

		default_run$annealing			<- annealing.parameters
	}

	default_run
}

