#
# rebuild_model_data.R
#
#' rebuild model data object
#'
#' restore consistency to model data after modifying base parameter set
#'
#' @param model existing model object
#'
#' @return model with rebuilt data object
#'
#' @export
#
rebuild_model_data <- function(model) {

	run <- model$run

	# Unpack:
	data			<- elt(model, "data")

	state.vars		<- elt(data, "state.vars")
	physical.parms		<- elt(data, "physical.parms")
	bio.events		<- elt(data, "bio.events")
	fixed.parms		<- elt(data, "fixed.parms")
	physics			<- elt(data, "physics")
	boundconc		<- elt(data, "boundconc")
	fishing.parms		<- elt(data, "fishing.parms")
	rate.multipliers	<- elt(data, "rate.multipliers")
	fitted.parms		<- elt(data, "fitted.parms")

	# rebuild data for model:
	#initial.state	<- build_initial_state(state.vars, physical.parms)
	#drivers		<- build_annual_drivers(run, bio.events, fixed.parms, physical.parms, physics, boundconc)
	#harvest.rates	<- build_harvesting_rates(run, fishing.parms, rate.multipliers)
	#model.parms	<- build_model_parameters(fitted.parms, fixed.parms, physical.parms, harvest.rates)
	#forcings	<- build_forcings(run, harvest.rates, drivers)

	# store back in model object:
	#model$data$initial.state	<- initial.state
	#model$data$drivers		<- drivers
	#model$data$harvest.rates	<- harvest.rates
	#model$data$model.parms		<- model.parms
	#model$data$forcings		<- forcings

	model
}

