#
# read_biological_event_timings.R
#
#' Read the biological event timing parameters
#'
#' returns event timings
#'
#' @param model.path path to model
#'
#' @return events
#'
#' @export
#
read_biological_event_timings <- function(model.path) {

	events <- readcsv(model.path, PARAMETERS_DIR, eventtimingparameterfile)

	events
}

