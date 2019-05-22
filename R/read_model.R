#
# read_model.R
#
#' read designated model
#'
#' loads the named model and returns a model object with run and data slots. By default, the model is loaded from the list of package supplied models.
#' If user.path is set then the model is loaded from this location instead. 
#'
#' @param model.name name of model to read
#' @param model.variant read the designated model variant (no default)
#' @param model.ident appended to output files (e.g. OFFSHORE_model_annualresults-TAG.csv instead of just OFFSHORE_model_annualresults.csv)
#' @param model.subdir store results in this sub directory of the main results folder
#' @param user.path path to users top level model folder
#' @param nyears number of years that model will run
#'
#' @return model object
#'
#' @export
#
read_model <- function(model.name, model.variant, model.ident="base", model.subdir="", user.path="", nyears=20) {

	read.only <- (user.path == "")					# read only unless user path is specified - i.e. it's not just based on write permissions!

	# full path to either the system model or the user specified one:
	model.path <- get.model.path(model.name, model.variant, user.path)

	cat(" Loading model   : ", model.path, "\n", sep="")

	# read the setup file containing all the filenames:
	read.model.setup(model.path)				# Models/Model/Variant/MODEL_SETUP.csv

	# run slot:
	run <- set_default_run(model.name, model.variant, model.subdir, model.ident, nyears)

	# read model inputs:
	physical.parameters	<- read_physical_parameters(model.path)
	fixed.parameters	<- read_fixed_parameters(model.path)
	physics.drivers		<- read_physics_drivers(model.path)
	chemistry.drivers	<- read_chemistry_drivers(model.path)
	biological.events	<- read_biological_event_timings(model.path)

	drivers			<- build_annual_drivers(run, fixed.parameters, physical.parameters, physics.drivers, chemistry.drivers, biological.events)

	forcings		<- interpolate_drivers(run, drivers)

	initial.state		<- read_initial_state(model.path, physical.parameters)	# ZZ combines read_initial_state() and build_initial_state()
											# ZZ maybe rebuild_model() requires this separation?
	fitted.parameters	<- read_fitted_parameters(model.path)

	fleet.model		<- configure_fishing_fleet_model(model.path, physical.parameters)

	uptakes			<- calculate_uptakes(fitted.parameters)	# ZZ nope! must be re-calculated in annealing loop!

	# data slot:
	data <- list(
		# read:
		fixed.parameters	= fixed.parameters,
		fitted.parameters	= fitted.parameters,
		physical.parameters	= physical.parameters,
		physics.drivers		= physics.drivers,
		chemistry.drivers	= chemistry.drivers,
		biological.events	= biological.events,

		# built:
		initial.state		= initial.state,
		drivers			= drivers,
		forcings		= forcings,
		fleet.model		= fleet.model,
		uptakes			= uptakes
	)

	model <- list(
		read.only	= read.only,
		path		= model.path,
		run		= run,
		data		= data
	)

	model
}

