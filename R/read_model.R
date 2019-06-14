#
# read_model.R
#
#' read designated model
#'
#' loads the named model and returns a model object with model setup and data slots. By default, the model is loaded from the list of package supplied models.
#' If user.path is set then the model is loaded from this location instead. 
#'
#' @param model.name name of model to read
#' @param model.variant read the designated model variant (no default)
#' @param model.ident appended to output files (e.g. OFFSHORE_model_annualresults-TAG.csv instead of just OFFSHORE_model_annualresults.csv)
#' @param model.subdir store results in this sub directory of the main results folder
#' @param user.path path to users top level model folder
#'
#' @return model object
#'
#' @export
#
read_model <- function(model.name, model.variant, model.ident="base", model.subdir="", user.path="") {

	read.only	<- (user.path == "")							# read only unless user path is specified

	model.path	<- get.variant.path(model.name, model.variant, user.path)		# full path to either the system model or the user specified one:
	resultsdir	<- makepath(MODEL_RESULTS_DIR, model.name, model.variant, model.subdir)	# results/<model>/<variant>/<subdir>

	setup <- list(
		read.only	= read.only,
		model.name	= model.name,		# "NorthSea"
		model.variant	= model.variant,	# "2000-2013"
		model.ident	= model.ident,		# "speculative"
		model.subdir	= model.subdir,		# "test"
		model.path	= model.path,		# "../Models/NorthSea/2000-2013"
		resultsdir	= resultsdir		# "results/NorthSea/2000-2013/test" ...
	)

	cat(" Loading model   : ", model.path, "\n", sep="")

	read.model.setup(model.path)			# Models/Model/Variant/MODEL_SETUP.csv

	# read model inputs:
	physical.parameters	<- read_physical_parameters(model.path)
	fixed.parameters	<- read_fixed_parameters(model.path)
	physics.drivers		<- read_physics_drivers(model.path)
	chemistry.drivers	<- read_chemistry_drivers(model.path)
	biological.events	<- read_biological_event_timings(model.path)
	fitted.parameters	<- read_fitted_parameters(model.path)
	initial.state		<- read_initial_state(model.path)
	fleet.model		<- read_fishing_fleet_model(model.path, physical.parameters)

	# data slot:
	data <- list(
		# read:
		fixed.parameters	= fixed.parameters,
		fitted.parameters	= fitted.parameters,
		physical.parameters	= physical.parameters,
		physics.drivers		= physics.drivers,
		chemistry.drivers	= chemistry.drivers,
		biological.events	= biological.events,
		fleet.model		= fleet.model,
		initial.state		= initial.state
	)

	model <- list(
		setup		= setup,
		data		= data
	)

	model
}

