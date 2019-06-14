
# StrathE2E.R
#
#' run the StrathE2E model
#'
#' perform a single run of the StrathE2E model
#'
#' @param model current model configuration
#' @param nyears number of years to run model
#'
#' @return model output
#'
#' @seealso \code{\link{list_models}}, \code{\link{read_model}}, \code{\link{copy_model}}
#'
#' @importFrom deSolve ode
#'
#' @useDynLib StrathE2E2
#'
#' @export
#'
#' @examples
#' # after running the example below, look at the various objects to see how you can modify them:
#' #
#' #	str(NorthSea)
#' #
#' NorthSea <- read_model("North_Sea", "2003-2013")
#'
#' results <- StrathE2E(NorthSea)
#'
#' plot_full_length_timeseries(results)
#
StrathE2E <- function(model, nyears=20) {

	build			<- build_model(model, nyears)

	setup			<- elt(model, "setup")
	model.path		<- elt(setup, "model.path")

	#data			<- elt(model, "data")	# ZZ delete

	run			<- elt(build, "run")
	initial.state		<- elt(build, "initial.state")
	forcings		<- elt(build, "forcings")
	times			<- elt(run, "times")

	fleet.output		<- fishing_fleet_model(model, build)			# run the fishing fleet model
	model.parameters	<- build_model_parameters(model, build, fleet.output)	# all parameters going through to C model

	StrathE2E.load()
	cat("Running model for", nyears, "years\n")
	output <- as.data.frame(
		ode(
			y		= unlist(initial.state),		# flatten list to vector, names used for output if present
			times		= times,
			func		= "derivsc",
			parms		= model.parameters,
			dllname		= "StrathE2E2",
			initforc	= "forcc",
			forcings	= forcings,
			initfunc	= "odec",
			fcontrol	= list(method="linear", rule=2, ties="ordered"),
			method		= "lsoda"
		)
	)
	StrathE2E.unload()

	# main processed output:
	aggregates			<- aggregate_model_output(model, output)
	total.annual.catch		<- extract_timeseries_annual_landings(model, build, output)
	annual.catch.by.gear		<- disaggregate_landings_discards_by_gear(fleet.output, total.annual.catch)
	catch.land.disc			<- extract_simulated_catch_land_disc_by_gear_for_given_year(model, annual.catch.by.gear)

	# additional processed output:
	monthly.averages		<- monthly_averages_of_final_year(model, build, output, aggregates)
	annual.results.wholedomain	<- derive_annual_results_wholedomain(model, build, output, aggregates)
	annual.results.offshore		<- derive_annual_results_offshore(model, build, output, aggregates)
	annual.results.inshore		<- derive_annual_results_inshore(model, build, output, aggregates)
	flow.matrices			<- assemble_flow_matrix_from_model_annual_output(model, build, output, aggregates)

	annual.target.data		<- read_annual_target_data(model.path)
	monthly.target.data		<- read_monthly_target_data(model.path)

	model.target.results		<- derive_model_target_results(model, build, output, aggregates, annual.target.data)
	fit.to.target.data		<- calculate_error_function(model, model.target.results)

	results <- list(
		build			= list(
			model.parameters		= model.parameters,
			run				= elt(build, "run"),
			drivers				= elt(build, "drivers"),
			forcings			= elt(build, "forcings")
		),
		output			= output,
		aggregates		= aggregates,
		fleet.output		= fleet.output,
		total.annual.catch	= total.annual.catch,
		annual.catch.by.gear	= annual.catch.by.gear,
		final.year.outputs	= list(
			# catch/landings/discards:
			inshore_catchmat		= elt(catch.land.disc, "inshore_catchmat"),
			inshore_discmat			= elt(catch.land.disc, "inshore_discmat"),
			inshore_landmat			= elt(catch.land.disc, "inshore_landmat"),
			offshore_catchmat		= elt(catch.land.disc, "offshore_catchmat"),
			offshore_landmat		= elt(catch.land.disc, "offshore_landmat"),
			offshore_discmat		= elt(catch.land.disc, "offshore_discmat"),

			monthly.averages		= monthly.averages,

			mass_results_inshore		= elt(annual.results.inshore, "mass_results"),
			maxmass_results_inshore		= elt(annual.results.inshore, "maxmass_results"),
			minmass_results_inshore		= elt(annual.results.inshore, "minmass_results"),
			annual_flux_results_inshore	= elt(annual.results.inshore, "annual_flux_results"),

			mass_results_offshore		= elt(annual.results.offshore, "mass_results"),
			maxmass_results_offshore	= elt(annual.results.offshore, "maxmass_results"),
			minmass_results_offshore	= elt(annual.results.offshore, "minmass_results"),
			annual_flux_results_offshore	= elt(annual.results.offshore, "annual_flux_results"),

			mass_results_wholedomain	= elt(annual.results.wholedomain, "mass_results"),
			maxmass_results_wholedomain	= elt(annual.results.wholedomain, "maxmass_results"),
			minmass_results_wholedomain	= elt(annual.results.wholedomain, "minmass_results"),
			annual_flux_results_wholedomain	= elt(annual.results.wholedomain, "annual_flux_results"),

			# Network fluxes:
			flow_matrix_all_fluxes		= elt(flow.matrices, "flow_matrix_all_fluxes"),
			flow_matrix_excl_spawn_recruit	= elt(flow.matrices, "flow_matrix_excl_spawn_recruit"),
			NetworkIndexResults		= elt(flow.matrices, "NetworkIndexResults"),

			annual.target.data		= annual.target.data,
			monthly.target.data		= monthly.target.data,

			# error function/likelihoods:
			annual_obj			= elt(fit.to.target.data, "annual_obj"),
			partial_chi			= elt(fit.to.target.data, "partial_chi"),
			opt_results			= elt(fit.to.target.data, "opt_results")
		)
	)

	results
}

