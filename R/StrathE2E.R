
# StrathE2E.R
#
#' run the StrathE2E model
#'
#' perform a single run of the StrathE2E model
#'
#' @param model current model configuration
#'
#' @return model output
#'
#' @seealso \code{\link{list_models}}, \code{\link{read_model}}, \code{\link{copy_model}}, \code{\link{rebuild_model}}
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
#' NorthSea <- read_model("NorthSea", "version1")
#'
#' results <- StrathE2E(NorthSea)
#'
#' plot_full_length_timeseries(results)
#
StrathE2E <- function(model) {

	model.path		<- elt(model, "path")

	run			<- elt(model, "run")
	times			<- elt(run, "times")

	data			<- elt(model, "data")
	initial.state		<- elt(data, "initial.state")
	fitted.parameters	<- elt(data, "fitted.parameters")
	forcings		<- elt(data, "forcings")

	fleet.output		<- fishing_fleet_model(model)

	model$data$uptakes	<- calculate_uptakes(fitted.parameters)

	model.parameters	<- build_model_parameters(model, fleet.output)

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

	# main processed output:
	aggregates			<- aggregate_model_output(model, output)
	total.annual.catch		<- extract_timeseries_annual_landings(model, output)
	annual.catch.by.gear		<- disaggregate_landings_discards_by_gear(fleet.output, total.annual.catch)
	catch.land.disc			<- extract_simulated_catch_land_disc_by_gear_for_given_year(model, annual.catch.by.gear)

	# additional processed output:
	monthly.averages		<- monthly_averages_of_final_year(model, output, aggregates)
	annual.results.wholedomain	<- derive_annual_results_wholedomain(model, output, aggregates)
	annual.results.offshore		<- derive_annual_results_offshore(model, output, aggregates)
	annual.results.inshore		<- derive_annual_results_inshore(model, output, aggregates)
	flow.matrices			<- assemble_flow_matrix_from_model_annual_output(model, output, aggregates)

	annual.target.data		<- read_annual_target_data(model.path)
	model.target.results		<- derive_model_target_results(model, output, aggregates, annual.target.data)
	fit.to.target.data		<- calculate_error_function(model, model.target.results)

	results <- list(
		model.parameters	= model.parameters,
		output			= output,
		aggregates		= aggregates,
		fleet.output		= fleet.output,
		total.annual.catch	= total.annual.catch,
		annual.catch.by.gear	= annual.catch.by.gear,	# ZZ mix n match var_names and var.names?
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

			# error function/likelihoods:
			annual_obj			= elt(fit.to.target.data, "annual_obj"),
			partial_chi			= elt(fit.to.target.data, "partial_chi"),
			opt_results			= elt(fit.to.target.data, "opt_results")
		)
	)

	results
}

