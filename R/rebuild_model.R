#
# rebuild_model.R
#
#' rebuild model object
#'
#' restore consistency to model data after modifying base parameter set
#'
#' @param model existing model object
#'
#' @return rebuilt model object
#'
#' @export
#
rebuild_model <- function(model) {

	model <- rebuild_model_run(model)
	model <- rebuild_model_data(model)

	model
}

