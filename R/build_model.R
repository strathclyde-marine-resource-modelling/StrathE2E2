#
# build_model.R
#
#' build model for invocation of StrathE2E
#'
#' calculates derived variables such as annual drivers, forcings, fleet model, uptakes, etc.
#'
#' @param model model object
#' @param nyears number of years the model will run
#'
#' @return build object
#'
#' @export
#
build_model <- function(model, nyears) {

	setup			<- elt(model, "setup")
	data			<- elt(model, "data")

	physical.parameters	<- elt(data, "physical.parameters")
	fixed.parameters	<- elt(data, "fixed.parameters")
	fitted.parameters	<- elt(data, "fitted.parameters")
	physics.drivers		<- elt(data, "physics.drivers")
	chemistry.drivers	<- elt(data, "chemistry.drivers")
	biological.events	<- elt(data, "biological.events")
	initial.state		<- elt(data, "initial.state")

	# generate or make adjustments to any variables down the line:
	run			<- build_model_run(nyears)
	initial.state		<- build_initial_state(initial.state, physical.parameters)
	drivers			<- build_annual_drivers(run, fixed.parameters, physical.parameters, physics.drivers, chemistry.drivers, biological.events)
	forcings		<- interpolate_drivers(run, drivers)
	uptakes			<- calculate_uptakes(fitted.parameters)

	build <- list(
		run		= run,
		initial.state	= initial.state,
		drivers		= drivers,
		forcings	= forcings,
		uptakes		= uptakes
	)

	build
}

