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
#' @param nyears length of model run in years
#'
#' @return list of default model run settings
#'
#' @export
#
set_default_run <- function(model.name, model.variant, model.tag="", nyears=20) {

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
		"AAA"		= model.tag,
		"oudir"		= makepath(MODEL_RESULTS_DIR, model.name, model.variant)
	)

	default_run
}

