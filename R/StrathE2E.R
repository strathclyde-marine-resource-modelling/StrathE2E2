#
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

	model.path		<- el(model, "path")

	run			<- el(model, "run")
	times			<- el(run, "times")

	data			<- el(model, "data")
	initial.state		<- el(data, "initial.state")
	fitted.parameters	<- el(data, "fitted.parameters")
	forcings		<- el(data, "forcings")

	fleet.output		<- fishing_fleet_model(model)

	model$data$uptakes	<- calculate_uptakes(fitted.parameters)

	model.parameters	<- build_model_parameters(model, fleet.output)

	#showall("initial.state", initial.state)
	#showall("times", times)
	#showall("model parms", model.parameters)
	#showall("forcings", forcings)
	#stop("Halt")

	output <- as.data.frame(
		ode(
			y		= unlist(initial.state),		# flatten to vector, names used for output if present
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


if (CHECKRESULTS==1) {
		aggregates	<- aggregate_model_output(model, output)
output$totalN <- aggregates$totalN
output$totalN_o <- aggregates$totalN_o
output$totalN_i <- aggregates$totalN_i
output$x_detritus <- aggregates$x_detritus
output$x_detritus_o <- aggregates$x_detritus_o
output$x_detritus_i <- aggregates$x_detritus_i
output$corpse <- aggregates$corpse
output$corpse_o <- aggregates$corpse_o
output$corpse_i <- aggregates$corpse_i
output$x_ammonia <- aggregates$x_ammonia
output$x_ammonia_o <- aggregates$x_ammonia_o
output$x_ammonia_i <- aggregates$x_ammonia_i
output$x_nitrate <- aggregates$x_nitrate
output$x_nitrate_o <- aggregates$x_nitrate_o
output$x_nitrate_i <- aggregates$x_nitrate_i
output$s_detritus <- aggregates$s_detritus
output$s_ammonia <- aggregates$s_ammonia
output$s_nitrate <- aggregates$s_nitrate
output$s_phyt <- aggregates$s_phyt
output$benthslar <- aggregates$benthslar
output$benthclar <- aggregates$benthclar
output$benths <- aggregates$benths
output$benthc <- aggregates$benthc
output$discard <- aggregates$discard
output$herb <- aggregates$herb
output$carn <- aggregates$carn
output$fishp <- aggregates$fishp
output$fishd <- aggregates$fishd
output$fishm <- aggregates$fishm
output$bird <- aggregates$bird
output$seal <- aggregates$seal
output$ceta <- aggregates$ceta
output$fishplar <- aggregates$fishplar
output$fishdlar <- aggregates$fishdlar
output$PNP <- aggregates$PNP
output$netpprod <- aggregates$netpprod
output$fluxwcamm_phyt <- aggregates$fluxwcamm_phyt
output$fluxwcnit_phyt <- aggregates$fluxwcnit_phyt
output$phytgrossprod <- aggregates$phytgrossprod
output$herbgrossprod <- aggregates$herbgrossprod
output$carngrossprod <- aggregates$carngrossprod
output$pfishlargrossprod <- aggregates$pfishlargrossprod
output$dfishlargrossprod <- aggregates$dfishlargrossprod
output$pfishgrossprod <- aggregates$pfishgrossprod
output$mfishgrossprod <- aggregates$mfishgrossprod
output$dfishgrossprod <- aggregates$dfishgrossprod
output$benthslargrossprod <- aggregates$benthslargrossprod
output$benthclargrossprod <- aggregates$benthclargrossprod
output$benthsgrossprod <- aggregates$benthsgrossprod
output$benthcgrossprod <- aggregates$benthcgrossprod
output$birdgrossprod <- aggregates$birdgrossprod
output$sealgrossprod <- aggregates$sealgrossprod
output$cetagrossprod <- aggregates$cetagrossprod
output$herbnetprod <- aggregates$herbnetprod
output$carnnetprod <- aggregates$carnnetprod
output$pfishlarnetprod <- aggregates$pfishlarnetprod
output$dfishlarnetprod <- aggregates$dfishlarnetprod
output$pfishnetprod <- aggregates$pfishnetprod
output$mfishnetprod <- aggregates$mfishnetprod
output$dfishnetprod <- aggregates$dfishnetprod
output$benthslarnetprod <- aggregates$benthslarnetprod
output$benthclarnetprod <- aggregates$benthclarnetprod
output$benthsnetprod <- aggregates$benthsnetprod
output$benthcnetprod <- aggregates$benthcnetprod
output$birdnetprod <- aggregates$birdnetprod
output$sealnetprod <- aggregates$sealnetprod
output$cetanetprod <- aggregates$cetanetprod
output$wcdenitrif <- aggregates$wcdenitrif
output$seddenitrif <- aggregates$seddenitrif
output$fluxsedboundary <- aggregates$fluxsedboundary
output$DIN_NET_flux_o_i <- aggregates$DIN_NET_flux_o_i
output$PART_NET_flux_o_i <- aggregates$PART_NET_flux_o_i
output$NET_activemigpelfish_o_i <- aggregates$NET_activemigpelfish_o_i
output$NET_activemigmigfish_o_i <- aggregates$NET_activemigmigfish_o_i
output$NET_activemigdemfish_o_i <- aggregates$NET_activemigdemfish_o_i
output$NET_activemigbird_o_i <- aggregates$NET_activemigbird_o_i
output$NET_activemigseal_o_i <- aggregates$NET_activemigseal_o_i
output$NET_activemigceta_o_i <- aggregates$NET_activemigceta_o_i
output$NET_mfish_ext_o <- aggregates$NET_mfish_ext_o
output$fluxDINinflow <- aggregates$fluxDINinflow
output$fluxDINoutflow <- aggregates$fluxDINoutflow
output$fluxPARTinflow <- aggregates$fluxPARTinflow
output$fluxPARToutflow <- aggregates$fluxPARToutflow
output$atmosDINinput <- aggregates$atmosDINinput
output$rivDINinflow <- aggregates$rivDINinflow
output$landp <- aggregates$landp
output$landd <- aggregates$landd
output$landd_o <- aggregates$landd_o
output$landd_i <- aggregates$landd_i
output$landd_quota <- aggregates$landd_quota
output$landd_nonquota <- aggregates$landd_nonquota
output$landm <- aggregates$landm
output$landsb <- aggregates$landsb
output$landcb <- aggregates$landcb
output$landcz <- aggregates$landcz
output$landbd <- aggregates$landbd
output$landsl <- aggregates$landsl
output$landct <- aggregates$landct
output$discpel <- aggregates$discpel
output$discdem <- aggregates$discdem
output$discdem_o <- aggregates$discdem_o
output$discdem_i <- aggregates$discdem_i
output$discdem_quota <- aggregates$discdem_quota
output$discdem_nonquota <- aggregates$discdem_nonquota
output$discmig <- aggregates$discmig
output$discsb <- aggregates$discsb
output$disccb <- aggregates$disccb
output$disccz <- aggregates$disccz
output$discbd <- aggregates$discbd
output$discsl <- aggregates$discsl
output$discct <- aggregates$discct
output$offalpel <- aggregates$offalpel
output$offaldem <- aggregates$offaldem
output$offaldem_o <- aggregates$offaldem_o
output$offaldem_i <- aggregates$offaldem_i
output$offaldem_quota <- aggregates$offaldem_quota
output$offaldem_nonquota <- aggregates$offaldem_nonquota
output$offalmig <- aggregates$offalmig
output$offalsb <- aggregates$offalsb
output$offalcb <- aggregates$offalcb
output$offalcz <- aggregates$offalcz
output$offalbd <- aggregates$offalbd
output$offalsl <- aggregates$offalsl
output$offalct <- aggregates$offalct
#output$x_poros <- aggregates$x_poros
#output$x_depth <- aggregates$x_depth
#output$x_poros_o <- aggregates$x_poros_o
#output$x_poros_i <- aggregates$x_poros_i
#output$x_depth_o <- aggregates$x_depth_o
#output$x_depth_i <- aggregates$x_depth_i

	showall("results", output)
	stop("Halt")
}
	# main processed output:
	aggregates		<- aggregate_model_output(model, output)
	total.annual.catch	<- extract_timeseries_annual_landings(model, output)
	annual.catch.by.gear	<- disaggregate_landings_discards_by_gear(fleet.output, total.annual.catch)
	catch.land.disc		<- extract_simulated_catch_land_disc_by_gear_for_given_year(model, annual.catch.by.gear)

	# additional processed output:
	monthly.averages	<- monthly_averages_of_final_year(model, output, aggregates)
	annual.flux.wholedomain	<- derive_annual_results_wholedomain(model, output, aggregates)
	annual.flux.offshore	<- derive_annual_results_offshore(model, output, aggregates)
	annual.flux.inshore	<- derive_annual_results_inshore(model, output, aggregates)
	network.index.results	<- assemble_flow_matrix_from_model_annual_output(model, output, aggregates)

	annual.target.data	<- read_annual_target_data(model.path)
	model.target.results	<- derive_model_target_results(model, output, aggregates, annual.target.data)
	fit.to.target.data	<- calculate_error_function(model, model.target.results)

	results <- list(
		model.parameters	= model.parameters,
		output			= output,
		aggregates		= aggregates,
		fleet.output		= fleet.output,
		total.annual.catch	= total.annual.catch,
		annual.catch.by.gear	= annual.catch.by.gear,
		catch.land.disc		= catch.land.disc,
		final.year.outputs	= list(
			monthly.averages	= monthly.averages,
			annual.flux.wholedomain	= annual.flux.wholedomain,
			annual.flux.offshore	= annual.flux.offshore,
			annual.flux.inshore	= annual.flux.inshore,
			network.index.results	= network.index.results,
			annual.target.data	= annual.target.data,
			fit.to.target.data	= fit.to.target.data
		)
	)

	results
}

