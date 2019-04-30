#
# rebuild_model_run.R
#
#' rebuild model run
#'
#' restore consistency to model run if nyears has been modified
#'
#' @param model existing model
#'
#' @return model with rebuilt run
#'
#' @export
#
rebuild_model_run <- function(model) {

	nyears <- model$run$nyears

	outp_time_interval <- 1

	model$run$ndays		<- nyears*360+1
	model$run$drndays	<- nyears*12
	model$run$times		<- seq(0, nyears*360, by=outp_time_interval)
	model$run$drtimes	<- seq(15, nyears*360-15, by=30)
	model$run$sprectimes	<- seq(0, nyears*360, by=1)
	model$run$daynum	<- seq(0, 360, by=1)

	model
}

