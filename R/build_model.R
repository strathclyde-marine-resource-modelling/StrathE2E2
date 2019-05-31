#
# build_model.R
#
#' build model for invocation of StrathE2E
#'
#' calculates derived variables such as annual drivers, forcings, fleet model, uptakes, etc.
#'
#' @param model model object
#'
#' @return build object
#'
#' @export
#
build_model <- function(model) {

	setup			<- elt(model, "setup")
	data			<- elt(model, "data")

	nyears			<- elt(setup, "nyears")

	physical.parameters	<- elt(data, "physical.parameters")
	fixed.parameters	<- elt(data, "fixed.parameters")
	fitted.parameters	<- elt(data, "fitted.parameters")
	physics.drivers		<- elt(data, "physics.drivers")
	chemistry.drivers	<- elt(data, "chemistry.drivers")
	biological.events	<- elt(data, "biological.events")
	initial.state		<- elt(data, "initial.state")
	fleet.model		<- elt(data, "fleet.model")

	# generate or make adjustments to any variables down the line:
	run			<- build_model_run(nyears)
	initial.state		<- build_initial_state(initial.state, physical.parameters)
	drivers			<- build_annual_drivers(run, fixed.parameters, physical.parameters, physics.drivers, chemistry.drivers, biological.events)
	forcings		<- interpolate_drivers(run, drivers)
	fleet.model		<- build_fishing_fleet_model(fleet.model, physical.parameters)
	uptakes			<- calculate_uptakes(fitted.parameters)

	# model$build <- list( ZZ maybe eventually )
	model$run		<- run
	model$data$initial.state<- initial.state
	model$data$drivers	<- drivers
	model$data$forcings	<- forcings
	model$data$fleet.model	<- fleet.model
	model$data$uptakes	<- uptakes

	model
}

