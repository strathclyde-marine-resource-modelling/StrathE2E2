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
#' @param model.tag appended to output files (e.g. OFFSHORE_model_annualresults-TAG.csv instead of just OFFSHORE_model_annualresults.csv)
#' @param user.path path to users top level model folder
#' @param nyears number of years that model will run
#' @param annealing boolean stating whether annealing settings are to be returned
#'
#' @return model object
#'
#' @export
#
read_model <- function(model.name, model.variant, model.tag="", user.path="", nyears=20, annealing=FALSE) {

	read.only <- (user.path == "")					# read only unless user path is specified - i.e. it's not just based on write permissions!

	if (read.only && annealing) {
		cat(" Error: cannot set annealing=TRUE when loading a system model!\n")
		stop("Read-only model")
	}

	# full path to either the system model or the user specified one:
	model.path <- get.model.path(model.name, model.variant, user.path)

	cat(" Loading model: ", model.path, "\n", sep="")

	# source the setup file containing all the file paths:
	sourcefile(makepath(model.path, MODEL_SETUP_SCRIPT))		# NorthSea/MODEL_SETUP.R

	# run slot:
	run <- set_default_run(model.name, model.variant, model.tag, nyears=nyears)

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
		fleet.model		= fleet.model,		## maybe somewhere else ZZ
		uptakes			= uptakes
	)

	model <- list(
		read.only	= read.only,
		path		= model.path,
		run		= run,
		data		= data
	)

	if (annealing) {
		# annealing settings requested:
		model$anneal <- read_annealing_parameters(model.path)

		model$anneal$identifier		<- "StrathE2E-testbed"	# ZZ needs parameterising?
		model$anneal$n_iter		<- 1			# ZZ 10
		model$anneal$temperature	<- 0.00005		# parameter of the annealing scheme - sets the probability of accepting a retrograde step
		model$anneal$cooling		<- 0.975		# rate at which the annualing scheme focusses in on a fit.
		model$anneal$annual.target.data	<- read_annual_target_data(model.path)
	}

	model
}

