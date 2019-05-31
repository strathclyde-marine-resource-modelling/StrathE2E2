#
# assemble_flow_matrix_from_model_annual_output.R
#
#' process the whole domain annual flux results held in the dataframe annual_flux_results
#' to produce a flow-matrix defining all of the annual integrated mass fluxes been every pair of
#' prey and predator guilds in the food web, alf the geochemical flows, and all of the boundary flows
#'
#' The code then uses the NetIndices R package which relies on the flow-matrix data as input
#' to derive a suite of network indices as outputs These are saved as a standard csv file
#'
#' @param model model object
#' @param output model output
#' @param aggegates aggregated model output
#'
#' @importFrom NetIndices TrophInd AscInd PathInd GenInd EffInd
#'
#' @export
#
assemble_flow_matrix_from_model_annual_output <- function(model, output, aggregates) {

	# Unpack:

	run		<- elt(model, "run")
	setup		<- elt(model, "setup")
	data		<- elt(model, "data")

	ndays		<- elt(run, "ndays")

	nyears		<- elt(setup, "nyears")
	model.path	<- elt(setup, "model.path")
	identifier	<- elt(setup, "model.ident")
	resultsdir	<- elt(setup, "resultsdir")

	physical.parms	<- elt(data, "physical.parameters")

	# extract physical parameters:
	so_depth		<- elt(physical.parms, "so_depth")
	d_depth			<- elt(physical.parms, "d_depth")
	si_depth		<- elt(physical.parms, "si_depth")
	bx_depth		<- elt(physical.parms, "bx_depth")
	x_depth_s1		<- elt(physical.parms, "x_depth_s1")
	x_depth_s2		<- elt(physical.parms, "x_depth_s2")
	x_depth_s3		<- elt(physical.parms, "x_depth_s3")
	x_depth_d1		<- elt(physical.parms, "x_depth_d1")
	x_depth_d2		<- elt(physical.parms, "x_depth_d2")
	x_depth_d3		<- elt(physical.parms, "x_depth_d3")
	x_area_s0		<- elt(physical.parms, "x_area_s0")
	x_area_s1		<- elt(physical.parms, "x_area_s1")
	x_area_s2		<- elt(physical.parms, "x_area_s2")
	x_area_s3		<- elt(physical.parms, "x_area_s3")
	x_area_d0		<- elt(physical.parms, "x_area_d0")
	x_area_d1		<- elt(physical.parms, "x_area_d1")
	x_area_d2		<- elt(physical.parms, "x_area_d2")
	x_area_d3		<- elt(physical.parms, "x_area_d3")
	x_rock_s1		<- elt(physical.parms, "x_rock_s1")
	x_rock_s2		<- elt(physical.parms, "x_rock_s2")
	x_rock_s3		<- elt(physical.parms, "x_rock_s3")
	x_rock_d1		<- elt(physical.parms, "x_rock_d1")
	x_rock_d2		<- elt(physical.parms, "x_rock_d2")
	x_rock_d3		<- elt(physical.parms, "x_rock_d3")
	x_nonrock_s		<- elt(physical.parms, "x_nonrock_s")
	x_nonrock_d		<- elt(physical.parms, "x_nonrock_d")
	x_poros_s1		<- elt(physical.parms, "x_poros_s1")
	x_poros_s2		<- elt(physical.parms, "x_poros_s2")
	x_poros_s3		<- elt(physical.parms, "x_poros_s3")
	x_poros_d1		<- elt(physical.parms, "x_poros_d1")
	x_poros_d2		<- elt(physical.parms, "x_poros_d2")
	x_poros_d3		<- elt(physical.parms, "x_poros_d3")
	Kxw_s1			<- elt(physical.parms, "Kxw_s1")
	Kxw_s2			<- elt(physical.parms, "Kxw_s2")
	Kxw_s3			<- elt(physical.parms, "Kxw_s3")
	Kxw_d1			<- elt(physical.parms, "Kxw_d1")
	Kxw_d2			<- elt(physical.parms, "Kxw_d2")
	Kxw_d3			<- elt(physical.parms, "Kxw_d3")
	xbioturb_depth_s1	<- elt(physical.parms, "xbioturb_depth_s1")
	xbioturb_depth_s2	<- elt(physical.parms, "xbioturb_depth_s2")
	xbioturb_depth_s3	<- elt(physical.parms, "xbioturb_depth_s3")
	xbioturb_depth_d1	<- elt(physical.parms, "xbioturb_depth_d1")
	xbioturb_depth_d2	<- elt(physical.parms, "xbioturb_depth_d2")
	xbioturb_depth_d3	<- elt(physical.parms, "xbioturb_depth_d3")
	xerosion_depth_s1	<- elt(physical.parms, "xerosion_depth_s1")
	xerosion_depth_s2	<- elt(physical.parms, "xerosion_depth_s2")
	xerosion_depth_s3	<- elt(physical.parms, "xerosion_depth_s3")
	xerosion_depth_d1	<- elt(physical.parms, "xerosion_depth_d1")
	xerosion_depth_d2	<- elt(physical.parms, "xerosion_depth_d2")
	xerosion_depth_d3	<- elt(physical.parms, "xerosion_depth_d3")
	xlightSPM_intercept	<- elt(physical.parms, "xlightSPM_intercept")
	xlightSPM_slope		<- elt(physical.parms, "xlightSPM_slope")
	xinshore_phyt_prop_depth<- elt(physical.parms, "xinshore_phyt_prop_depth")
	xinshore_kelp_prop_depth<- elt(physical.parms, "xinshore_kelp_prop_depth")
	x_xR_detritus_s1	<- elt(physical.parms, "x_xR_detritus_s1")
	x_xR_detritus_s2	<- elt(physical.parms, "x_xR_detritus_s2")
	x_xR_detritus_s3	<- elt(physical.parms, "x_xR_detritus_s3")
	x_xR_detritus_d1	<- elt(physical.parms, "x_xR_detritus_d1")
	x_xR_detritus_d2	<- elt(physical.parms, "x_xR_detritus_d2")
	x_xR_detritus_d3	<- elt(physical.parms, "x_xR_detritus_d3")
	ref_Kxw			<- elt(physical.parms, "ref_Kxw")
	x_shallowprop		<- elt(physical.parms, "x_shallowprop")
	habitat_areas		<- elt(physical.parms, "habitat_areas")

	# extract output:
	time			<- elt(output, "time")
	detritus_so		<- elt(output, "detritus_so")
	detritus_d		<- elt(output, "detritus_d")
	x_detritus_s1		<- elt(output, "x_detritus_s1")
	x_detritus_s2		<- elt(output, "x_detritus_s2")
	x_detritus_s3		<- elt(output, "x_detritus_s3")
	x_detritus_d1		<- elt(output, "x_detritus_d1")
	x_detritus_d2		<- elt(output, "x_detritus_d2")
	x_detritus_d3		<- elt(output, "x_detritus_d3")
	xR_detritus_s1		<- elt(output, "xR_detritus_s1")
	xR_detritus_s2		<- elt(output, "xR_detritus_s2")
	xR_detritus_s3		<- elt(output, "xR_detritus_s3")
	xR_detritus_d1		<- elt(output, "xR_detritus_d1")
	xR_detritus_d2		<- elt(output, "xR_detritus_d2")
	xR_detritus_d3		<- elt(output, "xR_detritus_d3")
	discard_o		<- elt(output, "discard_o")
	corpse_s1		<- elt(output, "corpse_s1")
	corpse_s2		<- elt(output, "corpse_s2")
	corpse_s3		<- elt(output, "corpse_s3")
	corpse_d1		<- elt(output, "corpse_d1")
	corpse_d2		<- elt(output, "corpse_d2")
	corpse_d3		<- elt(output, "corpse_d3")
	ammonia_so		<- elt(output, "ammonia_so")
	ammonia_d		<- elt(output, "ammonia_d")
	x_ammonia_s1		<- elt(output, "x_ammonia_s1")
	x_ammonia_s2		<- elt(output, "x_ammonia_s2")
	x_ammonia_s3		<- elt(output, "x_ammonia_s3")
	x_ammonia_d1		<- elt(output, "x_ammonia_d1")
	x_ammonia_d2		<- elt(output, "x_ammonia_d2")
	x_ammonia_d3		<- elt(output, "x_ammonia_d3")
	nitrate_so		<- elt(output, "nitrate_so")
	nitrate_d		<- elt(output, "nitrate_d")
	x_nitrate_s1		<- elt(output, "x_nitrate_s1")
	x_nitrate_s2		<- elt(output, "x_nitrate_s2")
	x_nitrate_s3		<- elt(output, "x_nitrate_s3")
	x_nitrate_d1		<- elt(output, "x_nitrate_d1")
	x_nitrate_d2		<- elt(output, "x_nitrate_d2")
	x_nitrate_d3		<- elt(output, "x_nitrate_d3")
	phyt_so			<- elt(output, "phyt_so")
	phyt_d			<- elt(output, "phyt_d")
	herb_o			<- elt(output, "herb_o")
	carn_o			<- elt(output, "carn_o")
	benthslar_o		<- elt(output, "benthslar_o")
	benths_o		<- elt(output, "benths_o")
	benthclar_o		<- elt(output, "benthclar_o")
	benthc_o		<- elt(output, "benthc_o")
	fishp_o			<- elt(output, "fishp_o")
	fishplar_o		<- elt(output, "fishplar_o")
	fishd_o			<- elt(output, "fishd_o")
	fishdlar_o		<- elt(output, "fishdlar_o")
	fishm_o			<- elt(output, "fishm_o")
	bird_o			<- elt(output, "bird_o")
	detritus_si		<- elt(output, "detritus_si")
	ammonia_si		<- elt(output, "ammonia_si")
	nitrate_si		<- elt(output, "nitrate_si")
	phyt_si			<- elt(output, "phyt_si")
	benthslar_i		<- elt(output, "benthslar_i")
	benthclar_i		<- elt(output, "benthclar_i")
	benths_i		<- elt(output, "benths_i")
	benthc_i		<- elt(output, "benthc_i")
	discard_i		<- elt(output, "discard_i")
	herb_i			<- elt(output, "herb_i")
	carn_i			<- elt(output, "carn_i")
	fishplar_i		<- elt(output, "fishplar_i")
	fishdlar_i		<- elt(output, "fishdlar_i")
	fishp_i			<- elt(output, "fishp_i")
	fishm_i			<- elt(output, "fishm_i")
	fishd_i			<- elt(output, "fishd_i")
	bird_i			<- elt(output, "bird_i")
	seal_o			<- elt(output, "seal_o")
	seal_i			<- elt(output, "seal_i")
	ceta_o			<- elt(output, "ceta_o")
	ceta_i			<- elt(output, "ceta_i")
	corpse_s0		<- elt(output, "corpse_s0")
	corpse_d0		<- elt(output, "corpse_d0")
	kelpC			<- elt(output, "kelpC")
	kelpN			<- elt(output, "kelpN")
	kelpdebris		<- elt(output, "kelpdebris")
	netpprod_o		<- elt(output, "netpprod_o")
	netpprod_i		<- elt(output, "netpprod_i")
	PNP_o			<- elt(output, "PNP_o")
	PNP_i			<- elt(output, "PNP_i")
	phytgrossprod_o		<- elt(output, "phytgrossprod_o")
	phytgrossprod_i		<- elt(output, "phytgrossprod_i")
	kelpCprod_i		<- elt(output, "kelpCprod_i")
	kelpCexud_i		<- elt(output, "kelpCexud_i")
	kelpNprod_i		<- elt(output, "kelpNprod_i")
	herbgrossprod_o		<- elt(output, "herbgrossprod_o")
	herbgrossprod_i		<- elt(output, "herbgrossprod_i")
	carngrossprod_o		<- elt(output, "carngrossprod_o")
	carngrossprod_i		<- elt(output, "carngrossprod_i")
	pfishlargrossprod_o	<- elt(output, "pfishlargrossprod_o")
	pfishlargrossprod_i	<- elt(output, "pfishlargrossprod_i")
	dfishlargrossprod_o	<- elt(output, "dfishlargrossprod_o")
	dfishlargrossprod_i	<- elt(output, "dfishlargrossprod_i")
	pfishgrossprod_o	<- elt(output, "pfishgrossprod_o")
	pfishgrossprod_i	<- elt(output, "pfishgrossprod_i")
	mfishgrossprod_o	<- elt(output, "mfishgrossprod_o")
	mfishgrossprod_i	<- elt(output, "mfishgrossprod_i")
	dfishgrossprod_o	<- elt(output, "dfishgrossprod_o")
	dfishgrossprod_i	<- elt(output, "dfishgrossprod_i")
	benthslargrossprod_o	<- elt(output, "benthslargrossprod_o")
	benthslargrossprod_i	<- elt(output, "benthslargrossprod_i")
	benthclargrossprod_o	<- elt(output, "benthclargrossprod_o")
	benthclargrossprod_i	<- elt(output, "benthclargrossprod_i")
	benthsgrossprod_o	<- elt(output, "benthsgrossprod_o")
	benthsgrossprod_i	<- elt(output, "benthsgrossprod_i")
	benthcgrossprod_o	<- elt(output, "benthcgrossprod_o")
	benthcgrossprod_i	<- elt(output, "benthcgrossprod_i")
	birdgrossprod_o		<- elt(output, "birdgrossprod_o")
	birdgrossprod_i		<- elt(output, "birdgrossprod_i")
	sealgrossprod_o		<- elt(output, "sealgrossprod_o")
	sealgrossprod_i		<- elt(output, "sealgrossprod_i")
	cetagrossprod_o		<- elt(output, "cetagrossprod_o")
	cetagrossprod_i		<- elt(output, "cetagrossprod_i")
	wcdenitrif_o		<- elt(output, "wcdenitrif_o")
	wcdenitrif_i		<- elt(output, "wcdenitrif_i")
	seddenitrif_o		<- elt(output, "seddenitrif_o")
	seddenitrif_i		<- elt(output, "seddenitrif_i")
	fluxsedamm_wcamm	<- elt(output, "fluxsedamm_wcamm")
	fluxwcdet_wcamm		<- elt(output, "fluxwcdet_wcamm")
	fluxherb_wcamm		<- elt(output, "fluxherb_wcamm")
	fluxcarn_wcamm		<- elt(output, "fluxcarn_wcamm")
	fluxpfishlar_wcamm	<- elt(output, "fluxpfishlar_wcamm")
	fluxdfishlar_wcamm	<- elt(output, "fluxdfishlar_wcamm")
	fluxpfish_wcamm		<- elt(output, "fluxpfish_wcamm")
	fluxmfish_wcamm		<- elt(output, "fluxmfish_wcamm")
	fluxdfish_wcamm		<- elt(output, "fluxdfish_wcamm")
	fluxbenthslar_wcamm	<- elt(output, "fluxbenthslar_wcamm")
	fluxbenthclar_wcamm	<- elt(output, "fluxbenthclar_wcamm")
	fluxbenths_wcamm	<- elt(output, "fluxbenths_wcamm")
	fluxbenthc_wcamm	<- elt(output, "fluxbenthc_wcamm")
	fluxbird_wcamm		<- elt(output, "fluxbird_wcamm")
	fluxseal_wcamm		<- elt(output, "fluxseal_wcamm")
	fluxceta_wcamm		<- elt(output, "fluxceta_wcamm")
	fluxxdet_sedamm		<- elt(output, "fluxxdet_sedamm")
	fluxxRdet_sedamm	<- elt(output, "fluxxRdet_sedamm")
	fluxwcamm_wcnit		<- elt(output, "fluxwcamm_wcnit")
	fluxsednit_wcnit	<- elt(output, "fluxsednit_wcnit")
	fluxsedamm_sednit	<- elt(output, "fluxsedamm_sednit")
	fluxxdet_wcdet		<- elt(output, "fluxxdet_wcdet")
	fluxkelpdebris_wcdet	<- elt(output, "fluxkelpdebris_wcdet")
	fluxcorp_wcdet		<- elt(output, "fluxcorp_wcdet")
	fluxphyt_wcdet		<- elt(output, "fluxphyt_wcdet")
	fluxherb_wcdet		<- elt(output, "fluxherb_wcdet")
	fluxcarn_wcdet		<- elt(output, "fluxcarn_wcdet")
	fluxpfishlar_wcdet	<- elt(output, "fluxpfishlar_wcdet")
	fluxdfishlar_wcdet	<- elt(output, "fluxdfishlar_wcdet")
	fluxpfish_wcdet		<- elt(output, "fluxpfish_wcdet")
	fluxmfish_wcdet		<- elt(output, "fluxmfish_wcdet")
	fluxdfish_wcdet		<- elt(output, "fluxdfish_wcdet")
	fluxbenthslar_wcdet	<- elt(output, "fluxbenthslar_wcdet")
	fluxbenthclar_wcdet	<- elt(output, "fluxbenthclar_wcdet")
	fluxbenths_wcdet	<- elt(output, "fluxbenths_wcdet")
	fluxbenthc_wcdet	<- elt(output, "fluxbenthc_wcdet")
	fluxbird_wcdet		<- elt(output, "fluxbird_wcdet")
	fluxseal_wcdet		<- elt(output, "fluxseal_wcdet")
	fluxceta_wcdet		<- elt(output, "fluxceta_wcdet")
	fluxwcdet_xdet		<- elt(output, "fluxwcdet_xdet")
	fluxcorp_xdet		<- elt(output, "fluxcorp_xdet")
	fluxbenths_xdet		<- elt(output, "fluxbenths_xdet")
	fluxbenthc_xdet		<- elt(output, "fluxbenthc_xdet")
	fluxxdet_xRdet		<- elt(output, "fluxxdet_xRdet")
	fluxkelpdebris_xRdet	<- elt(output, "fluxkelpdebris_xRdet")
	fluxcorp_xRdet		<- elt(output, "fluxcorp_xRdet")
	fluxkelp_kelpdebris	<- elt(output, "fluxkelp_kelpdebris")
	fluxdisc_corp		<- elt(output, "fluxdisc_corp")
	fluxpfish_corp		<- elt(output, "fluxpfish_corp")
	fluxmfish_corp		<- elt(output, "fluxmfish_corp")
	fluxdfish_corp		<- elt(output, "fluxdfish_corp")
	fluxbenths_corp		<- elt(output, "fluxbenths_corp")
	fluxbenthc_corp		<- elt(output, "fluxbenthc_corp")
	fluxbird_corp		<- elt(output, "fluxbird_corp")
	fluxseal_corp		<- elt(output, "fluxseal_corp")
	fluxceta_corp		<- elt(output, "fluxceta_corp")
	fluxwcamm_kelp		<- elt(output, "fluxwcamm_kelp")
	fluxwcnit_kelp		<- elt(output, "fluxwcnit_kelp")
	fluxwcamm_phyt_o	<- elt(output, "fluxwcamm_phyt_o")
	fluxwcamm_phyt_i	<- elt(output, "fluxwcamm_phyt_i")
	fluxwcnit_phyt_o	<- elt(output, "fluxwcnit_phyt_o")
	fluxwcnit_phyt_i	<- elt(output, "fluxwcnit_phyt_i")
	fluxwcdet_herb		<- elt(output, "fluxwcdet_herb")
	fluxphyt_herb		<- elt(output, "fluxphyt_herb")
	fluxbenthslar_herb	<- elt(output, "fluxbenthslar_herb")
	fluxbenthclar_herb	<- elt(output, "fluxbenthclar_herb")
	fluxherb_carn		<- elt(output, "fluxherb_carn")
	fluxpfishlar_carn	<- elt(output, "fluxpfishlar_carn")
	fluxdfishlar_carn	<- elt(output, "fluxdfishlar_carn")
	fluxbenthslar_carn	<- elt(output, "fluxbenthslar_carn")
	fluxbenthclar_carn	<- elt(output, "fluxbenthclar_carn")
	fluxherb_pfishlar	<- elt(output, "fluxherb_pfishlar")
	fluxbenthslar_pfishlar	<- elt(output, "fluxbenthslar_pfishlar")
	fluxbenthclar_pfishlar	<- elt(output, "fluxbenthclar_pfishlar")
	fluxherb_dfishlar	<- elt(output, "fluxherb_dfishlar")
	fluxbenthslar_dfishlar	<- elt(output, "fluxbenthslar_dfishlar")
	fluxbenthclar_dfishlar	<- elt(output, "fluxbenthclar_dfishlar")
	fluxherb_pfish		<- elt(output, "fluxherb_pfish")
	fluxcarn_pfish		<- elt(output, "fluxcarn_pfish")
	fluxpfishlar_pfish	<- elt(output, "fluxpfishlar_pfish")
	fluxdfishlar_pfish	<- elt(output, "fluxdfishlar_pfish")
	fluxbenthslar_pfish	<- elt(output, "fluxbenthslar_pfish")
	fluxbenthclar_pfish	<- elt(output, "fluxbenthclar_pfish")
	fluxherb_mfish		<- elt(output, "fluxherb_mfish")
	fluxcarn_mfish		<- elt(output, "fluxcarn_mfish")
	fluxpfishlar_mfish	<- elt(output, "fluxpfishlar_mfish")
	fluxdfishlar_mfish	<- elt(output, "fluxdfishlar_mfish")
	fluxbenthslar_mfish	<- elt(output, "fluxbenthslar_mfish")
	fluxbenthclar_mfish	<- elt(output, "fluxbenthclar_mfish")
	fluxcorp_dfish		<- elt(output, "fluxcorp_dfish")
	fluxdisc_dfish		<- elt(output, "fluxdisc_dfish")
	fluxcarn_dfish		<- elt(output, "fluxcarn_dfish")
	fluxpfishlar_dfish	<- elt(output, "fluxpfishlar_dfish")
	fluxdfishlar_dfish	<- elt(output, "fluxdfishlar_dfish")
	fluxpfish_dfish		<- elt(output, "fluxpfish_dfish")
	fluxmfish_dfish		<- elt(output, "fluxmfish_dfish")
	fluxdfish_dfish		<- elt(output, "fluxdfish_dfish")
	fluxbenths_dfish	<- elt(output, "fluxbenths_dfish")
	fluxbenthc_dfish	<- elt(output, "fluxbenthc_dfish")
	fluxwcdet_benthslar	<- elt(output, "fluxwcdet_benthslar")
	fluxphyt_benthslar	<- elt(output, "fluxphyt_benthslar")
	fluxwcdet_benthclar	<- elt(output, "fluxwcdet_benthclar")
	fluxphyt_benthclar	<- elt(output, "fluxphyt_benthclar")
	fluxwcdet_benths	<- elt(output, "fluxwcdet_benths")
	fluxxdet_benths		<- elt(output, "fluxxdet_benths")
	fluxxRdet_benths	<- elt(output, "fluxxRdet_benths")
	fluxphyt_benths		<- elt(output, "fluxphyt_benths")
	fluxkelp_benthc		<- elt(output, "fluxkelp_benthc")
	fluxkelpdebris_benthc	<- elt(output, "fluxkelpdebris_benthc")
	fluxcorp_benthc		<- elt(output, "fluxcorp_benthc")
	fluxbenths_benthc	<- elt(output, "fluxbenths_benthc")
	fluxcorp_bird		<- elt(output, "fluxcorp_bird")
	fluxdisc_bird		<- elt(output, "fluxdisc_bird")
	fluxcarn_bird		<- elt(output, "fluxcarn_bird")
	fluxpfish_bird		<- elt(output, "fluxpfish_bird")
	fluxmfish_bird		<- elt(output, "fluxmfish_bird")
	fluxdfish_bird		<- elt(output, "fluxdfish_bird")
	fluxbenths_bird		<- elt(output, "fluxbenths_bird")
	fluxbenthc_bird		<- elt(output, "fluxbenthc_bird")
	fluxcorp_seal		<- elt(output, "fluxcorp_seal")
	fluxdisc_seal		<- elt(output, "fluxdisc_seal")
	fluxcarn_seal		<- elt(output, "fluxcarn_seal")
	fluxpfish_seal		<- elt(output, "fluxpfish_seal")
	fluxmfish_seal		<- elt(output, "fluxmfish_seal")
	fluxdfish_seal		<- elt(output, "fluxdfish_seal")
	fluxbenths_seal		<- elt(output, "fluxbenths_seal")
	fluxbenthc_seal		<- elt(output, "fluxbenthc_seal")
	fluxbird_seal		<- elt(output, "fluxbird_seal")
	fluxdisc_ceta		<- elt(output, "fluxdisc_ceta")
	fluxherb_ceta		<- elt(output, "fluxherb_ceta")
	fluxcarn_ceta		<- elt(output, "fluxcarn_ceta")
	fluxpfish_ceta		<- elt(output, "fluxpfish_ceta")
	fluxmfish_ceta		<- elt(output, "fluxmfish_ceta")
	fluxdfish_ceta		<- elt(output, "fluxdfish_ceta")
	fluxbenths_ceta		<- elt(output, "fluxbenths_ceta")
	fluxbenthc_ceta		<- elt(output, "fluxbenthc_ceta")
	fluxbird_ceta		<- elt(output, "fluxbird_ceta")
	fluxseal_ceta		<- elt(output, "fluxseal_ceta")
	Bs_spawn		<- elt(output, "Bs_spawn")
	Bs_recruit		<- elt(output, "Bs_recruit")
	Bc_spawn		<- elt(output, "Bc_spawn")
	Bc_recruit		<- elt(output, "Bc_recruit")
	Pfish_spawn		<- elt(output, "Pfish_spawn")
	Pfish_recruit		<- elt(output, "Pfish_recruit")
	Dfish_spawn		<- elt(output, "Dfish_spawn")
	Dfish_recruit		<- elt(output, "Dfish_recruit")
	fluxwcnit_Ngas		<- elt(output, "fluxwcnit_Ngas")
	fluxsednit_Ngas		<- elt(output, "fluxsednit_Ngas")
	fluxkelpdebris_beachexport<- elt(output, "fluxkelpdebris_beachexport")
	fluxAMMoutflow_o	<- elt(output, "fluxAMMoutflow_o")
	fluxNIToutflow_o	<- elt(output, "fluxNIToutflow_o")
	fluxAMMoutflow_i	<- elt(output, "fluxAMMoutflow_i")
	fluxNIToutflow_i	<- elt(output, "fluxNIToutflow_i")
	fluxPHYToutflow_o	<- elt(output, "fluxPHYToutflow_o")
	fluxDEToutflow_o	<- elt(output, "fluxDEToutflow_o")
	fluxPHYToutflow_i	<- elt(output, "fluxPHYToutflow_i")
	fluxDEToutflow_i	<- elt(output, "fluxDEToutflow_i")
	mfish_emigration	<- elt(output, "mfish_emigration")
	fluxsedboundary_o	<- elt(output, "fluxsedboundary_o")
	fluxsedboundary_i	<- elt(output, "fluxsedboundary_i")
	fluxAMMinflow_o		<- elt(output, "fluxAMMinflow_o")
	fluxNITinflow_o		<- elt(output, "fluxNITinflow_o")
	fluxAMMinflow_i		<- elt(output, "fluxAMMinflow_i")
	fluxNITinflow_i		<- elt(output, "fluxNITinflow_i")
	fluxPHYTinflow_o	<- elt(output, "fluxPHYTinflow_o")
	fluxDETinflow_o		<- elt(output, "fluxDETinflow_o")
	fluxPHYTinflow_i	<- elt(output, "fluxPHYTinflow_i")
	fluxDETinflow_i		<- elt(output, "fluxDETinflow_i")
	mfish_imigration	<- elt(output, "mfish_imigration")
	atmosAMMinput_o		<- elt(output, "atmosAMMinput_o")
	atmosNITinput_o		<- elt(output, "atmosNITinput_o")
	atmosAMMinput_i		<- elt(output, "atmosAMMinput_i")
	atmosNITinput_i		<- elt(output, "atmosNITinput_i")
	rivAMMinflow		<- elt(output, "rivAMMinflow")
	rivNITinflow		<- elt(output, "rivNITinflow")
	rivPARTinflow		<- elt(output, "rivPARTinflow")
	DINflux_i_o		<- elt(output, "DINflux_i_o")
	DINflux_o_i		<- elt(output, "DINflux_o_i")
	PARTflux_i_o		<- elt(output, "PARTflux_i_o")
	PARTflux_o_i		<- elt(output, "PARTflux_o_i")
	activemigpelfish_i_o	<- elt(output, "activemigpelfish_i_o")
	activemigmigfish_i_o	<- elt(output, "activemigmigfish_i_o")
	activemigdemfish_i_o	<- elt(output, "activemigdemfish_i_o")
	activemigbird_i_o	<- elt(output, "activemigbird_i_o")
	activemigseal_i_o	<- elt(output, "activemigseal_i_o")
	activemigceta_i_o	<- elt(output, "activemigceta_i_o")
	activemigpelfish_o_i	<- elt(output, "activemigpelfish_o_i")
	activemigmigfish_o_i	<- elt(output, "activemigmigfish_o_i")
	activemigdemfish_o_i	<- elt(output, "activemigdemfish_o_i")
	activemigbird_o_i	<- elt(output, "activemigbird_o_i")
	activemigseal_o_i	<- elt(output, "activemigseal_o_i")
	activemigceta_o_i	<- elt(output, "activemigceta_o_i")
	vertnitflux		<- elt(output, "vertnitflux")
	horiznitflux		<- elt(output, "horiznitflux")
	landp_o			<- elt(output, "landp_o")
	landd_quota_o		<- elt(output, "landd_quota_o")
	landd_nonquota_o	<- elt(output, "landd_nonquota_o")
	landm_o			<- elt(output, "landm_o")
	landsb_o		<- elt(output, "landsb_o")
	landcb_o		<- elt(output, "landcb_o")
	landcz_o		<- elt(output, "landcz_o")
	landbd_o		<- elt(output, "landbd_o")
	landsl_o		<- elt(output, "landsl_o")
	landct_o		<- elt(output, "landct_o")
	discpel_o		<- elt(output, "discpel_o")
	discdem_quota_o		<- elt(output, "discdem_quota_o")
	discdem_nonquota_o	<- elt(output, "discdem_nonquota_o")
	discmig_o		<- elt(output, "discmig_o")
	discsb_o		<- elt(output, "discsb_o")
	disccb_o		<- elt(output, "disccb_o")
	disccz_o		<- elt(output, "disccz_o")
	discbd_o		<- elt(output, "discbd_o")
	discsl_o		<- elt(output, "discsl_o")
	discct_o		<- elt(output, "discct_o")
	landp_i			<- elt(output, "landp_i")
	landd_quota_i		<- elt(output, "landd_quota_i")
	landd_nonquota_i	<- elt(output, "landd_nonquota_i")
	landm_i			<- elt(output, "landm_i")
	landsb_i		<- elt(output, "landsb_i")
	landcb_i		<- elt(output, "landcb_i")
	landcz_i		<- elt(output, "landcz_i")
	landbd_i		<- elt(output, "landbd_i")
	landsl_i		<- elt(output, "landsl_i")
	landct_i		<- elt(output, "landct_i")
	landkp_i		<- elt(output, "landkp_i")
	discpel_i		<- elt(output, "discpel_i")
	discdem_quota_i		<- elt(output, "discdem_quota_i")
	discdem_nonquota_i	<- elt(output, "discdem_nonquota_i")
	discmig_i		<- elt(output, "discmig_i")
	discsb_i		<- elt(output, "discsb_i")
	disccb_i		<- elt(output, "disccb_i")
	disccz_i		<- elt(output, "disccz_i")
	discbd_i		<- elt(output, "discbd_i")
	discsl_i		<- elt(output, "discsl_i")
	discct_i		<- elt(output, "discct_i")
	disckp_i		<- elt(output, "disckp_i")
	offalpel_o		<- elt(output, "offalpel_o")
	offaldem_quota_o	<- elt(output, "offaldem_quota_o")
	offaldem_nonquota_o	<- elt(output, "offaldem_nonquota_o")
	offalmig_o		<- elt(output, "offalmig_o")
	offalsb_o		<- elt(output, "offalsb_o")
	offalcb_o		<- elt(output, "offalcb_o")
	offalcz_o		<- elt(output, "offalcz_o")
	offalbd_o		<- elt(output, "offalbd_o")
	offalsl_o		<- elt(output, "offalsl_o")
	offalct_o		<- elt(output, "offalct_o")
	offalpel_i		<- elt(output, "offalpel_i")
	offaldem_quota_i	<- elt(output, "offaldem_quota_i")
	offaldem_nonquota_i	<- elt(output, "offaldem_nonquota_i")
	offalmig_i		<- elt(output, "offalmig_i")
	offalsb_i		<- elt(output, "offalsb_i")
	offalcb_i		<- elt(output, "offalcb_i")
	offalcz_i		<- elt(output, "offalcz_i")
	offalbd_i		<- elt(output, "offalbd_i")
	offalsl_i		<- elt(output, "offalsl_i")
	offalct_i		<- elt(output, "offalct_i")
	offalkp_i		<- elt(output, "offalkp_i")
	herbnetprod_o		<- elt(output, "herbnetprod_o")
	herbnetprod_i		<- elt(output, "herbnetprod_i")
	carnnetprod_o		<- elt(output, "carnnetprod_o")
	carnnetprod_i		<- elt(output, "carnnetprod_i")
	pfishlarnetprod_o	<- elt(output, "pfishlarnetprod_o")
	pfishlarnetprod_i	<- elt(output, "pfishlarnetprod_i")
	dfishlarnetprod_o	<- elt(output, "dfishlarnetprod_o")
	dfishlarnetprod_i	<- elt(output, "dfishlarnetprod_i")
	pfishnetprod_o		<- elt(output, "pfishnetprod_o")
	pfishnetprod_i		<- elt(output, "pfishnetprod_i")
	mfishnetprod_o		<- elt(output, "mfishnetprod_o")
	mfishnetprod_i		<- elt(output, "mfishnetprod_i")
	dfishnetprod_o		<- elt(output, "dfishnetprod_o")
	dfishnetprod_i		<- elt(output, "dfishnetprod_i")
	benthslarnetprod_o	<- elt(output, "benthslarnetprod_o")
	benthslarnetprod_i	<- elt(output, "benthslarnetprod_i")
	benthclarnetprod_o	<- elt(output, "benthclarnetprod_o")
	benthclarnetprod_i	<- elt(output, "benthclarnetprod_i")
	benthsnetprod_o		<- elt(output, "benthsnetprod_o")
	benthsnetprod_i		<- elt(output, "benthsnetprod_i")
	benthcnetprod_o		<- elt(output, "benthcnetprod_o")
	benthcnetprod_i		<- elt(output, "benthcnetprod_i")
	birdnetprod_o		<- elt(output, "birdnetprod_o")
	birdnetprod_i		<- elt(output, "birdnetprod_i")
	sealnetprod_o		<- elt(output, "sealnetprod_o")
	sealnetprod_i		<- elt(output, "sealnetprod_i")
	cetanetprod_o		<- elt(output, "cetanetprod_o")
	cetanetprod_i		<- elt(output, "cetanetprod_i")

	# extract aggregates:
	totalN			<- elt(aggregates, "totalN")
	totalN_o		<- elt(aggregates, "totalN_o")
	totalN_i		<- elt(aggregates, "totalN_i")
	x_detritus		<- elt(aggregates, "x_detritus")
	x_detritus_o		<- elt(aggregates, "x_detritus_o")
	x_detritus_i		<- elt(aggregates, "x_detritus_i")
	corpse			<- elt(aggregates, "corpse")
	corpse_o		<- elt(aggregates, "corpse_o")
	corpse_i		<- elt(aggregates, "corpse_i")
	x_ammonia		<- elt(aggregates, "x_ammonia")
	x_ammonia_o		<- elt(aggregates, "x_ammonia_o")
	x_ammonia_i		<- elt(aggregates, "x_ammonia_i")
	x_nitrate		<- elt(aggregates, "x_nitrate")
	x_nitrate_o		<- elt(aggregates, "x_nitrate_o")
	x_nitrate_i		<- elt(aggregates, "x_nitrate_i")
	s_detritus		<- elt(aggregates, "s_detritus")
	s_ammonia		<- elt(aggregates, "s_ammonia")
	s_nitrate		<- elt(aggregates, "s_nitrate")
	s_phyt			<- elt(aggregates, "s_phyt")
	benthslar		<- elt(aggregates, "benthslar")
	benthclar		<- elt(aggregates, "benthclar")
	benths			<- elt(aggregates, "benths")
	benthc			<- elt(aggregates, "benthc")
	discard			<- elt(aggregates, "discard")
	herb			<- elt(aggregates, "herb")
	carn			<- elt(aggregates, "carn")
	fishp			<- elt(aggregates, "fishp")
	fishd			<- elt(aggregates, "fishd")
	fishm			<- elt(aggregates, "fishm")
	bird			<- elt(aggregates, "bird")
	seal			<- elt(aggregates, "seal")
	ceta			<- elt(aggregates, "ceta")
	fishplar		<- elt(aggregates, "fishplar")
	fishdlar		<- elt(aggregates, "fishdlar")
	PNP			<- elt(aggregates, "PNP")
	netpprod		<- elt(aggregates, "netpprod")
	fluxwcamm_phyt		<- elt(aggregates, "fluxwcamm_phyt")
	fluxwcnit_phyt		<- elt(aggregates, "fluxwcnit_phyt")
	phytgrossprod		<- elt(aggregates, "phytgrossprod")
	herbgrossprod		<- elt(aggregates, "herbgrossprod")
	carngrossprod		<- elt(aggregates, "carngrossprod")
	pfishlargrossprod	<- elt(aggregates, "pfishlargrossprod")
	dfishlargrossprod	<- elt(aggregates, "dfishlargrossprod")
	pfishgrossprod		<- elt(aggregates, "pfishgrossprod")
	mfishgrossprod		<- elt(aggregates, "mfishgrossprod")
	dfishgrossprod		<- elt(aggregates, "dfishgrossprod")
	benthslargrossprod	<- elt(aggregates, "benthslargrossprod")
	benthclargrossprod	<- elt(aggregates, "benthclargrossprod")
	benthsgrossprod		<- elt(aggregates, "benthsgrossprod")
	benthcgrossprod		<- elt(aggregates, "benthcgrossprod")
	birdgrossprod		<- elt(aggregates, "birdgrossprod")
	sealgrossprod		<- elt(aggregates, "sealgrossprod")
	cetagrossprod		<- elt(aggregates, "cetagrossprod")
	herbnetprod		<- elt(aggregates, "herbnetprod")
	carnnetprod		<- elt(aggregates, "carnnetprod")
	pfishlarnetprod		<- elt(aggregates, "pfishlarnetprod")
	dfishlarnetprod		<- elt(aggregates, "dfishlarnetprod")
	pfishnetprod		<- elt(aggregates, "pfishnetprod")
	mfishnetprod		<- elt(aggregates, "mfishnetprod")
	dfishnetprod		<- elt(aggregates, "dfishnetprod")
	benthslarnetprod	<- elt(aggregates, "benthslarnetprod")
	benthclarnetprod	<- elt(aggregates, "benthclarnetprod")
	benthsnetprod		<- elt(aggregates, "benthsnetprod")
	benthcnetprod		<- elt(aggregates, "benthcnetprod")
	birdnetprod		<- elt(aggregates, "birdnetprod")
	sealnetprod		<- elt(aggregates, "sealnetprod")
	cetanetprod		<- elt(aggregates, "cetanetprod")
	wcdenitrif		<- elt(aggregates, "wcdenitrif")
	seddenitrif		<- elt(aggregates, "seddenitrif")
	fluxsedboundary		<- elt(aggregates, "fluxsedboundary")
	DIN_NET_flux_o_i	<- elt(aggregates, "DIN_NET_flux_o_i")
	PART_NET_flux_o_i	<- elt(aggregates, "PART_NET_flux_o_i")
	NET_activemigpelfish_o_i<- elt(aggregates, "NET_activemigpelfish_o_i")
	NET_activemigmigfish_o_i<- elt(aggregates, "NET_activemigmigfish_o_i")
	NET_activemigdemfish_o_i<- elt(aggregates, "NET_activemigdemfish_o_i")
	NET_activemigbird_o_i	<- elt(aggregates, "NET_activemigbird_o_i")
	NET_activemigseal_o_i	<- elt(aggregates, "NET_activemigseal_o_i")
	NET_activemigceta_o_i	<- elt(aggregates, "NET_activemigceta_o_i")
	NET_mfish_ext_o		<- elt(aggregates, "NET_mfish_ext_o")
	fluxDINinflow		<- elt(aggregates, "fluxDINinflow")
	fluxDINoutflow		<- elt(aggregates, "fluxDINoutflow")
	fluxPARTinflow		<- elt(aggregates, "fluxPARTinflow")
	fluxPARToutflow		<- elt(aggregates, "fluxPARToutflow")
	atmosDINinput		<- elt(aggregates, "atmosDINinput")
	rivDINinflow		<- elt(aggregates, "rivDINinflow")
	landp			<- elt(aggregates, "landp")
	landd			<- elt(aggregates, "landd")
	landd_o			<- elt(aggregates, "landd_o")
	landd_i			<- elt(aggregates, "landd_i")
	landd_quota		<- elt(aggregates, "landd_quota")
	landd_nonquota		<- elt(aggregates, "landd_nonquota")
	landm			<- elt(aggregates, "landm")
	landsb			<- elt(aggregates, "landsb")
	landcb			<- elt(aggregates, "landcb")
	landcz			<- elt(aggregates, "landcz")
	landbd			<- elt(aggregates, "landbd")
	landsl			<- elt(aggregates, "landsl")
	landct			<- elt(aggregates, "landct")
	discpel			<- elt(aggregates, "discpel")
	discdem			<- elt(aggregates, "discdem")
	discdem_o		<- elt(aggregates, "discdem_o")
	discdem_i		<- elt(aggregates, "discdem_i")
	discdem_quota		<- elt(aggregates, "discdem_quota")
	discdem_nonquota	<- elt(aggregates, "discdem_nonquota")
	discmig			<- elt(aggregates, "discmig")
	discsb			<- elt(aggregates, "discsb")
	disccb			<- elt(aggregates, "disccb")
	disccz			<- elt(aggregates, "disccz")
	discbd			<- elt(aggregates, "discbd")
	discsl			<- elt(aggregates, "discsl")
	discct			<- elt(aggregates, "discct")
	offalpel		<- elt(aggregates, "offalpel")
	offaldem		<- elt(aggregates, "offaldem")
	offaldem_o		<- elt(aggregates, "offaldem_o")
	offaldem_i		<- elt(aggregates, "offaldem_i")
	offaldem_quota		<- elt(aggregates, "offaldem_quota")
	offaldem_nonquota	<- elt(aggregates, "offaldem_nonquota")
	offalmig		<- elt(aggregates, "offalmig")
	offalsb			<- elt(aggregates, "offalsb")
	offalcb			<- elt(aggregates, "offalcb")
	offalcz			<- elt(aggregates, "offalcz")
	offalbd			<- elt(aggregates, "offalbd")
	offalsl			<- elt(aggregates, "offalsl")
	offalct			<- elt(aggregates, "offalct")
	x_poros			<- elt(aggregates, "x_poros")
	x_depth			<- elt(aggregates, "x_depth")
	x_poros_o		<- elt(aggregates, "x_poros_o")
	x_poros_i		<- elt(aggregates, "x_poros_i")
	x_depth_o		<- elt(aggregates, "x_depth_o")
	x_depth_i		<- elt(aggregates, "x_depth_i")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Parameters which have been established by the simulated annealing scheme are
# loaded from a csv file which is made from the last line of the 'accepted parameters'
# file produced by the annealing programme.
#
# A variety of other parameters which are not optimised by simulated annealing are
# hard-wired in this subroutine
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Read in the template for the flowmatrix

#resultsdir         <- "results/final fitting runs culminating in run 31/"  # not needed if run as part of a model sequence
#identifier           <- "test_model"                                         # not needed if run as part of a model sequence

	flowmatrix_template <- get.model.file(model.path, PARAMETERS_DIR, file.pattern=FOOD_WEB_FLOW_MATRIX)

#Read in the wholedomain annual flux data file, if not alreday in memory
#If already in memory the dataframe is called "annual_flux_results"
#  annual_results_file  <- "WHOLEDOMAIN_model_annualresults-NEWYEAR18-AD-again-31.csv"
#  annual_flux_results  <- read.csv(paste(resultsdir,annual_results_file,sep=""),header=TRUE)


#Now we need to trawl through the model output dataframe and integrate each o fthe flux terms over the final year and drop
#into the flow matrix template. For some elements of the matrix we have to aggregate a few individual fluxes, for example 
#feeding and recruitment to adult fish groups, so it all has to be done carefully.

#names(flowmatrix_template)
# [1] "wcammonia"       "sedammonia"      "wcnitrate"       "sednitrate"     
# [5] "wcdetritus"           "seddetritus"          "seddetritusR"         "corpses"        
# [9] "discards"        "phyt"            "omnivzoo"        "carnzoo"        
#[13] "pfishlar"        "dfishlar"        "pfish"           "mfish"          
#[17] "dfish"           "benthslar"       "benthclar"       "benths"         
#[21] "benthc"          "birdmam"         "landings"        "denitrification"
#[25] "emigration"      "exports"         "imigration"      "imports"        
#Its very importnat that the rownames = colnames
#The fluxes in matrix are from row elements to column elements


outrowstart<-((nyears-1)*360+1)
outrowend  <-ndays

#-------------------------------
#Fluxes to water column ammonia

#From sediment ammonia
fluxvec<-fluxsedamm_wcamm
sourcename<-"sedammonia"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
   #SPECIAL CASE
   #If this flux is negative then it needs to go into a different cell
      if(flux<0) {
        destname<-"sedammonia"
        sourcename  <-"wcammonia"
      }
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From wc detritus
fluxvec<-fluxwcdet_wcamm
sourcename<-"wcdetritus"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From omnivorous zooplankton
fluxvec<-fluxherb_wcamm
sourcename<-"omnivzoo"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From carnivorous zooplankton
fluxvec<-fluxcarn_wcamm
sourcename<-"carnzoo"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From pelagic fish larvae
fluxvec<-fluxpfishlar_wcamm
sourcename<-"pfishlar"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From demersal fish larvae
fluxvec<-fluxdfishlar_wcamm
sourcename<-"dfishlar"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From pelagic fish
fluxvec<-fluxpfish_wcamm
sourcename<-"pfish"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From migratory fish
fluxvec<-fluxmfish_wcamm
sourcename<-"mfish"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From demersal fish
fluxvec<-fluxdfish_wcamm
sourcename<-"dfish"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos larvae
fluxvec<-fluxbenthslar_wcamm
sourcename<-"benthslar"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From c/s benthos larvae
fluxvec<-fluxbenthclar_wcamm
sourcename<-"benthclar"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos
fluxvec<-fluxbenths_wcamm
sourcename<-"benths"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux
## NOTE this correct - excretion by settled benthos gos direct to the water column not to porewaters

#From c/s benthos
fluxvec<-fluxbenthc_wcamm
sourcename<-"benthc"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux
## NOTE this correct - excretion by settled benthos gos direct to the water column not to porewaters

#From birds 
fluxvec<-fluxbird_wcamm
sourcename<-"bird"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From seals 
fluxvec<-fluxseal_wcamm
sourcename<-"seal"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From cetaceans 
fluxvec<-fluxceta_wcamm
sourcename<-"ceta"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#-------------------------------
#Fluxes to sediment porewater ammonia

#From sediment detritus
fluxvec<-fluxxdet_sedamm
sourcename<-"seddetritus"
destname  <-"sedammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From sediment refratory detritus
fluxvec<-fluxxRdet_sedamm
sourcename<-"seddetritusR"
destname  <-"sedammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#-------------------------------
#Fluxes to water column nitrate

#From water column ammonia
fluxvec<-fluxwcamm_wcnit
sourcename<-"wcammonia"
destname  <-"wcnitrate"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From sediment porewater nitrate
fluxvec<-fluxsednit_wcnit
sourcename<-"sednitrate"
destname  <-"wcnitrate"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
   #SPECIAL CASE
   #If this flux is negative then it needs to go into a different cell
      if(flux<0) {
        flux <- (-1*flux)
        destname<-"sednitrate"
        sourcename  <-"wcnitrate"
      }
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#-------------------------------
#Fluxes to sediment porewater nitrate

#From sediment porewater ammonia
fluxvec<-fluxsedamm_sednit
sourcename<-"sedammonia"
destname  <-"sednitrate"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#-------------------------------
#Fluxes to water column detritus

#From sediment detritus (resuspension)
fluxvec<-fluxxdet_wcdet
sourcename<-"seddetritus"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From corpses (only over rocky habitats)
fluxvec<-fluxcorp_wcdet
sourcename<-"corpses"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From kelp debris (only over inshore rocky habitats)
fluxvec<-fluxkelpdebris_wcdet
sourcename<-"kelpdebris"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#From phytoplankton
fluxvec<-fluxphyt_wcdet
sourcename<-"phyt"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From omnivorous zooplankton
fluxvec<-fluxherb_wcdet
sourcename<-"omnivzoo"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From carnivorous zooplankton
fluxvec<-fluxcarn_wcdet
sourcename<-"carnzoo"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From pelagic fish larvae
fluxvec<-fluxpfishlar_wcdet
sourcename<-"pfishlar"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From demersal fish larvae
fluxvec<-fluxdfishlar_wcdet
sourcename<-"dfishlar"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From pelagic fish
fluxvec<-fluxpfish_wcdet
sourcename<-"pfish"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From migratory fish
fluxvec<-fluxmfish_wcdet
sourcename<-"mfish"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From demersal fish
fluxvec<-fluxdfish_wcdet
sourcename<-"dfish"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos larvae
fluxvec<-fluxbenthslar_wcdet
sourcename<-"benthslar"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From c/s benthos larvae
fluxvec<-fluxbenthclar_wcdet
sourcename<-"benthclar"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos (only over rocky habitats)
fluxvec<-fluxbenths_wcdet
sourcename<-"benths"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From c/s benthos (only over rocky habitats)
fluxvec<-fluxbenthc_wcdet
sourcename<-"benthc"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From birds 
fluxvec<-fluxbird_wcdet
sourcename<-"bird"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From seals 
fluxvec<-fluxseal_wcdet
sourcename<-"seal"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From cetaceans 
fluxvec<-fluxceta_wcdet
sourcename<-"ceta"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#-------------------------------
#Fluxes to sediment detritus

#From water column detritus (sedimentation)
fluxvec<-fluxwcdet_xdet
sourcename<-"wcdetritus"
destname  <-"seddetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From corpses (over sand and mud)
fluxvec<-fluxcorp_xdet
sourcename<-"corpses"
destname  <-"seddetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos (over sand and mud)
fluxvec<-fluxbenths_xdet
sourcename<-"benths"
destname  <-"seddetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From c/s benthos (over sand and mud)
fluxvec<-fluxbenthc_xdet
sourcename<-"benthc"
destname  <-"seddetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#-------------------------------
#Fluxes to sediment refractory detritus

#From sediment detritus
fluxvec<-fluxxdet_xRdet
sourcename<-"seddetritus"
destname  <-"seddetritusR"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From corpses
fluxvec<-fluxcorp_xRdet
sourcename<-"corpses"
destname  <-"seddetritusR"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From kelpdebris
fluxvec<-fluxkelpdebris_xRdet
sourcename<-"kelpdebris"
destname  <-"seddetritusR"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#-------------------------------
#Fluxes to corpses

#From discards
fluxvec<-fluxdisc_corp
sourcename<-"discards"
destname  <-"corpses"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From pelagic fish
fluxvec<-fluxpfish_corp
sourcename<-"pfish"
destname  <-"corpses"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From migratory fish
fluxvec<-fluxmfish_corp
sourcename<-"mfish"
destname  <-"corpses"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From demersal fish
fluxvec<-fluxdfish_corp
sourcename<-"dfish"
destname  <-"corpses"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos
fluxvec<-fluxbenths_corp
sourcename<-"benths"
destname  <-"corpses"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From c/s benthos
fluxvec<-fluxbenthc_corp
sourcename<-"benthc"
destname  <-"corpses"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From birds (includes discards of by-catch)
fluxvec<-fluxbird_corp
sourcename<-"bird"
destname  <-"corpses"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From seals (includes discards of by-catch)
fluxvec<-fluxseal_corp
sourcename<-"seal"
destname  <-"corpses"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From cetaceans (includes discards of by-catch)
fluxvec<-fluxseal_corp
sourcename<-"ceta"
destname  <-"corpses"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#-------------------------------
#Fluxes to discards (includes fluxes of offal from processing at sea)

#From carnivorous zooplankton
fluxvec<-disccz + offalcz
sourcename<-"carnzoo"
destname  <-"discards"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From pelagic fish
fluxvec<-discpel + offalpel
sourcename<-"pfish"
destname  <-"discards"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From migratory fish
fluxvec<-discmig + offalmig
sourcename<-"mfish"
destname  <-"discards"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From demersal fish
fluxvec<-discdem + offaldem
sourcename<-"dfish"
destname  <-"discards"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos
fluxvec<-discsb + offalsb
sourcename<-"benths"
destname  <-"discards"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From c/s benthos
fluxvec<-disccb + offalcb
sourcename<-"benthc"
destname  <-"discards"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From birds & mammals - discards included within the model as a flux to corpses, but offal from processing at sea goes to discards

#From birds
fluxvec<-offalbd
sourcename<-"bird"
destname  <-"discards"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From seals
fluxvec<-offalsl
sourcename<-"seal"
destname  <-"discards"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From cetaceans
fluxvec<-offalct
sourcename<-"ceta"
destname  <-"discards"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#-------------------------------
#Fluxes to kelp debris (includes destruction y waves plus discards and fluxes of kelp offal from processing at sea)

#From kelp
fluxvec<-fluxkelp_kelpdebris + disckp_i + offalkp_i
sourcename<-"kelp"
destname  <-"kelpdebris"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#-------------------------------
#Fluxes to phytoplankton

#From water column ammonia
fluxvec<-fluxwcamm_phyt
sourcename<-"wcammonia"
destname  <-"phyt"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From water column nitrate
fluxvec<-fluxwcnit_phyt
sourcename<-"wcnitrate"
destname  <-"phyt"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#-------------------------------
#Fluxes to kelp

#From water column ammonia
fluxvec<-fluxwcamm_kelp
sourcename<-"wcammonia"
destname  <-"kelp"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From water column nitrate
fluxvec<-fluxwcnit_kelp
sourcename<-"wcnitrate"
destname  <-"kelp"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#-------------------------------
#Fluxes to omnivorous zooplankton

#From water column detritus
fluxvec<-fluxwcdet_herb
sourcename<-"wcdetritus"
destname  <-"omnivzoo"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From phytoplankton
fluxvec<-fluxphyt_herb
sourcename<-"phyt"
destname  <-"omnivzoo"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos larvae
fluxvec<-fluxbenthslar_herb
sourcename<-"benthslar"
destname  <-"omnivzoo"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From c/s benthos larvae
fluxvec<-fluxbenthclar_herb
sourcename<-"benthclar"
destname  <-"omnivzoo"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#-------------------------------
#Fluxes to carnivorous zooplankton

#From omnivorous zooplankton
fluxvec<-fluxherb_carn
sourcename<-"omnivzoo"
destname  <-"carnzoo"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From pelagic fish larvae
fluxvec<-fluxpfishlar_carn
sourcename<-"pfishlar"
destname  <-"carnzoo"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From demersal fish larvae
fluxvec<-fluxdfishlar_carn
sourcename<-"dfishlar"
destname  <-"carnzoo"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos larvae
fluxvec<-fluxbenthslar_carn
sourcename<-"benthslar"
destname  <-"carnzoo"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From c/s benthos larvae
fluxvec<-fluxbenthclar_carn
sourcename<-"benthclar"
destname  <-"carnzoo"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#-------------------------------
#Fluxes to pelagic fish larvae

#From omnivorous zooplankton
fluxvec<-fluxherb_pfishlar
sourcename<-"omnivzoo"
destname  <-"pfishlar"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos larvae
fluxvec<-fluxbenthslar_pfishlar
sourcename<-"benthslar"
destname  <-"pfishlar"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From c/s benthos larvae
fluxvec<-fluxbenthclar_pfishlar
sourcename<-"benthclar"
destname  <-"pfishlar"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#SPECIAL CASE - spawning flux from adult pelagic fish
fluxvec<-Pfish_spawn
sourcename<-"pfish"
destname  <-"pfishlar"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#-------------------------------
#Fluxes to demersal fish larvae

#From omnivorous zooplankton
fluxvec<-fluxherb_dfishlar
sourcename<-"omnivzoo"
destname  <-"dfishlar"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos larvae
fluxvec<-fluxbenthslar_dfishlar
sourcename<-"benthslar"
destname  <-"dfishlar"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From c/s benthos larvae
fluxvec<-fluxbenthclar_dfishlar
sourcename<-"benthclar"
destname  <-"dfishlar"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#SPECIAL CASE - spawning flux from adult demersal fish
fluxvec<-Dfish_spawn
sourcename<-"dfish"
destname  <-"dfishlar"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#-------------------------------
#Fluxes to pelagic fish

#From omnivorous zooplankton
fluxvec<-fluxherb_pfish
sourcename<-"omnivzoo"
destname  <-"pfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From carnivorous zooplankton
fluxvec<-fluxcarn_pfish
sourcename<-"carnzoo"
destname  <-"pfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From pelagic fish larvae
fluxvec<-fluxpfishlar_pfish
   #SPECIAL CASE
   #Here we have to add on the recruitment flux from larvae to settled stage
   fluxvec2<-Pfish_recruit
sourcename<-"pfishlar"
destname  <-"pfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flux2<-fluxvec2[outrowend]-fluxvec2[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux+flux2

#From demersal fish larvae
fluxvec<-fluxdfishlar_pfish
sourcename<-"dfishlar"
destname  <-"pfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos larvae
fluxvec<-fluxbenthslar_pfish
sourcename<-"benthslar"
destname  <-"pfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From c/s benthos larvae
fluxvec<-fluxbenthclar_pfish
sourcename<-"benthclar"
destname  <-"pfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux



#-------------------------------
#Fluxes to migratory fish

#From omnivorous zooplankton
fluxvec<-fluxherb_mfish
sourcename<-"omnivzoo"
destname  <-"mfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From carnivorous zooplankton
fluxvec<-fluxcarn_mfish
sourcename<-"carnzoo"
destname  <-"mfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From pelagic fish larvae
fluxvec<-fluxpfishlar_mfish
sourcename<-"pfishlar"
destname  <-"mfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From demersal fish larvae
fluxvec<-fluxdfishlar_mfish
sourcename<-"dfishlar"
destname  <-"mfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos larvae
fluxvec<-fluxbenthslar_mfish
sourcename<-"benthslar"
destname  <-"mfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From c/s benthos larvae
fluxvec<-fluxbenthclar_mfish
sourcename<-"benthclar"
destname  <-"mfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#-------------------------------
#Fluxes to demersal fish

#From corpses
fluxvec<-fluxcorp_dfish
sourcename<-"corpses"
destname  <-"dfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From discards
fluxvec<-fluxdisc_dfish
sourcename<-"discards"
destname  <-"dfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From carnivorous zooplankton
fluxvec<-fluxcarn_dfish
sourcename<-"carnzoo"
destname  <-"dfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From pelagic fish larvae
fluxvec<-fluxpfishlar_dfish
sourcename<-"pfishlar"
destname  <-"dfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From demersal fish larvae
fluxvec<-fluxdfishlar_dfish
   #SPECIAL CASE
   #Here we have to add on the recruitment flux from larvae to settled stage
   fluxvec2<-Dfish_recruit
sourcename<-"dfishlar"
destname  <-"dfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flux2<-fluxvec2[outrowend]-fluxvec2[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux+flux2

#From pelagic fish
fluxvec<-fluxpfish_dfish
sourcename<-"pfish"
destname  <-"dfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From migratory fish
fluxvec<-fluxmfish_dfish
sourcename<-"mfish"
destname  <-"dfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From demersal fish
fluxvec<-fluxdfish_dfish
sourcename<-"dfish"
destname  <-"dfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos
fluxvec<-fluxbenths_dfish
sourcename<-"benths"
destname  <-"dfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From c/s benthos
fluxvec<-fluxbenthc_dfish
sourcename<-"benthc"
destname  <-"dfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#-------------------------------
#Fluxes to f/d benthos larvae

#From water column detritus
fluxvec<-fluxwcdet_benthslar
sourcename<-"wcdetritus"
destname  <-"benthslar"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From phytoplankton
fluxvec<-fluxphyt_benthslar
sourcename<-"phyt"
destname  <-"benthslar"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#SPECIAL CASE - spawning flux from settled benthos
fluxvec<-Bs_spawn
sourcename<-"benths"
destname  <-"benthslar"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#-------------------------------
#Fluxes to c/s benthos larvae

#From water column detritus
fluxvec<-fluxwcdet_benthclar
sourcename<-"wcdetritus"
destname  <-"benthclar"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From phytoplankton
fluxvec<-fluxphyt_benthclar
sourcename<-"phyt"
destname  <-"benthclar"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#SPECIAL CASE - spawning flux from settled benthos
fluxvec<-Bc_spawn
sourcename<-"benthc"
destname  <-"benthclar"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#-------------------------------
#Fluxes to f/d benthos

#From water column detritus
fluxvec<-fluxwcdet_benths
sourcename<-"wcdetritus"
destname  <-"benths"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From sediment detritus
fluxvec<-fluxxdet_benths
sourcename<-"seddetritus"
destname  <-"benths"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From sediment refractory detritus
fluxvec<-fluxxRdet_benths
sourcename<-"seddetritusR"
destname  <-"benths"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From phytoplankton
fluxvec<-fluxphyt_benths
sourcename<-"phyt"
destname  <-"benths"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#SPECIAL CASE - recruitment flux from benthos larvae
fluxvec<-Bs_recruit
sourcename<-"benthslar"
destname  <-"benths"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#-------------------------------
#Fluxes to c/s benthos

#From corpses
fluxvec<-fluxcorp_benthc
sourcename<-"corpses"
destname  <-"benthc"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From kelp debris
fluxvec<-fluxkelpdebris_benthc
sourcename<-"kelpdebris"
destname  <-"benthc"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From kelp 
fluxvec<-fluxkelp_benthc
sourcename<-"kelp"
destname  <-"benthc"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos
fluxvec<-fluxbenths_benthc
sourcename<-"benths"
destname  <-"benthc"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#SPECIAL CASE - recruitment flux from benthos larvae
fluxvec<-Bc_recruit
sourcename<-"benthclar"
destname  <-"benthc"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#-------------------------------
#Fluxes to birds

#From corpses
fluxvec<-fluxcorp_bird
sourcename<-"corpses"
destname  <-"bird"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From discards
fluxvec<-fluxdisc_bird
sourcename<-"discards"
destname  <-"bird"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

# #From omnivorous zooplankton
# fluxvec<-fluxherb_bird
# sourcename<-"omnivzoo"
# destname  <-"bird"
# flux<-fluxvec[outrowend]-fluxvec[outrowstart]
# flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From carnivorous zooplankton
fluxvec<-fluxcarn_bird
sourcename<-"carnzoo"
destname  <-"bird"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From pelagic fish
fluxvec<-fluxpfish_bird
sourcename<-"pfish"
destname  <-"bird"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From migratory fish
fluxvec<-fluxmfish_bird
sourcename<-"mfish"
destname  <-"bird"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From demersal fish
fluxvec<-fluxdfish_bird
sourcename<-"dfish"
destname  <-"bird"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos
fluxvec<-fluxbenths_bird
sourcename<-"benths"
destname  <-"bird"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From c/s benthos
fluxvec<-fluxbenthc_bird
sourcename<-"benthc"
destname  <-"bird"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux




#-------------------------------
#Fluxes to seals

#From corpses
fluxvec<-fluxcorp_seal
sourcename<-"corpses"
destname  <-"seal"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From discards
fluxvec<-fluxdisc_seal
sourcename<-"discards"
destname  <-"seal"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

# #From omnivorous zooplankton
# fluxvec<-fluxherb_seal
# sourcename<-"omnivzoo"
# destname  <-"seal"
# flux<-fluxvec[outrowend]-fluxvec[outrowstart]
# flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From carnivorous zooplankton
fluxvec<-fluxcarn_seal
sourcename<-"carnzoo"
destname  <-"seal"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From pelagic fish
fluxvec<-fluxpfish_seal
sourcename<-"pfish"
destname  <-"seal"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From migratory fish
fluxvec<-fluxmfish_seal
sourcename<-"mfish"
destname  <-"seal"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From demersal fish
fluxvec<-fluxdfish_seal
sourcename<-"dfish"
destname  <-"seal"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos
fluxvec<-fluxbenths_seal
sourcename<-"benths"
destname  <-"seal"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From c/s benthos
fluxvec<-fluxbenthc_seal
sourcename<-"benthc"
destname  <-"seal"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From birds
fluxvec<-fluxbird_seal
sourcename<-"bird"
destname  <-"seal"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux



#-------------------------------
#Fluxes to cetaceans

# #From corpses
# fluxvec<-fluxcorp_ceta
# sourcename<-"corpses"
# destname  <-"ceta"
# flux<-fluxvec[outrowend]-fluxvec[outrowstart]
# flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From discards
fluxvec<-fluxdisc_ceta
sourcename<-"discards"
destname  <-"ceta"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From omnivorous zooplankton
fluxvec<-fluxherb_ceta
sourcename<-"omnivzoo"
destname  <-"ceta"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From carnivorous zooplankton
fluxvec<-fluxcarn_ceta
sourcename<-"carnzoo"
destname  <-"ceta"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From pelagic fish
fluxvec<-fluxpfish_ceta
sourcename<-"pfish"
destname  <-"ceta"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From migratory fish
fluxvec<-fluxmfish_ceta
sourcename<-"mfish"
destname  <-"ceta"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From demersal fish
fluxvec<-fluxdfish_ceta
sourcename<-"dfish"
destname  <-"ceta"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos
fluxvec<-fluxbenths_ceta
sourcename<-"benths"
destname  <-"ceta"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From c/s benthos
fluxvec<-fluxbenthc_ceta
sourcename<-"benthc"
destname  <-"ceta"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From birds
fluxvec<-fluxbird_ceta
sourcename<-"bird"
destname  <-"ceta"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From seals
fluxvec<-fluxseal_ceta
sourcename<-"bird"
destname  <-"ceta"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux




#-------------------------------
#Fluxes to landings - THIS SHOULD BE THE PROCESSED LANDED WEIGHT ie LIVE WEIGHT MINUS OFFAL WEIGHT

#From carnivorous zooplankton
fluxvec<-landcz - offalcz
sourcename<-"carnzoo"
destname  <-"landings"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From pelagic fish
fluxvec<-landp - offalpel
sourcename<-"pfish"
destname  <-"landings"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From migratory fish
fluxvec<-landm - offalmig
sourcename<-"mfish"
destname  <-"landings"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From demersal fish
fluxvec<-landd - offaldem
sourcename<-"dfish"
destname  <-"landings"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From f/d benthos
fluxvec<-landsb - offalsb
sourcename<-"benths"
destname  <-"landings"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From c/s benthos
fluxvec<-landcb - offalcb
sourcename<-"benthc"
destname  <-"landings"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From birds
fluxvec<-landbd - offalbd
sourcename<-"bird"
destname  <-"landings"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From seals
fluxvec<-landsl - offalsl
sourcename<-"seal"
destname  <-"landings"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From cetaceans
fluxvec<-landct - offalct
sourcename<-"ceta"
destname  <-"landings"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From kelp
fluxvec<-landkp_i - offalkp_i
sourcename<-"kelp"
destname  <-"landings"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#----------------------------
#Fluxes to beachcast

#From kelp debris
fluxvec<-fluxkelpdebris_beachexport
sourcename<-"kelpdebris"
destname  <-"beachcast"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#----------------------------
#Fluxes to nitrogen gas

#From water column nitrate
fluxvec<-wcdenitrif
sourcename<-"wcnitrate"
destname  <-"atmosphere"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#From sediment porewater nitrate
fluxvec<-seddenitrif
sourcename<-"sednitrate"
destname  <-"atmosphere"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#----------------------------
#Fluxes to emigration

#From migratory fish
fluxvec<-mfish_emigration
sourcename<-"mfish"
destname  <-"ocean"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux


#----------------------------
#Fluxes to hydrodynamic exports and burial

#From water column ammonia
fluxvec<-fluxAMMoutflow_o
fluxvec2<-fluxAMMoutflow_i
sourcename<-"wcammonia"
destname  <-"ocean"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flux2<-fluxvec2[outrowend]-fluxvec2[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux+flux2

#From water column nitrate
fluxvec<-fluxNIToutflow_o
fluxvec2<-fluxNIToutflow_i
sourcename<-"wcnitrate"
destname  <-"ocean"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flux2<-fluxvec2[outrowend]-fluxvec2[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux+flux2

#From water column detritus
fluxvec<-fluxDEToutflow_o
fluxvec2<-fluxDEToutflow_i
sourcename<-"wcdetritus"
destname  <-"ocean"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flux2<-fluxvec2[outrowend]-fluxvec2[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux+flux2

#From sediment refractory detritus (burial)
fluxvec<-fluxsedboundary_o
fluxvec2<-fluxsedboundary_i
sourcename<-"seddetritusR"
destname  <-"seabed"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flux2<-fluxvec2[outrowend]-fluxvec2[outrowstart]
cflux<-flux+flux2
   #SPECIAL CASE
   #If the sum of these fluxes is -ve then it indicated burial so place it as an export
   #If the sum of these fluxes is +ve then it indicated exhumation so place it as an import
     if(cflux>0) {
     destname  <-"seddetritusR"
     sourcename<-"seabed"
     }
     if(cflux<0) cflux <- (-1*cflux)
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- cflux

#From phytoplankton
fluxvec<-fluxPHYToutflow_o
fluxvec2<-fluxPHYToutflow_i
sourcename<-"phyt"
destname  <-"ocean"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flux2<-fluxvec2[outrowend]-fluxvec2[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux+flux2


#----------------------------
#Imigration fluxes

#To migratory fish
fluxvec<-mfish_imigration
sourcename<-"ocean"
destname  <-"mfish"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#-----------------------------
#Hydrodynamic, river and atmosphere imports

#Ocean to water column ammonia
fluxvec <-fluxAMMinflow_i
fluxvec2<-fluxAMMinflow_o
sourcename<-"ocean"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flux2<-fluxvec2[outrowend]-fluxvec2[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux+flux2

#Rivers to water column ammonia
fluxvec<-rivAMMinflow
sourcename<-"rivers"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#Atmosphere to water column ammonia
fluxvec<-atmosAMMinput_i
fluxvec2<-atmosAMMinput_o
sourcename<-"atmosphere"
destname  <-"wcammonia"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flux2<-fluxvec2[outrowend]-fluxvec2[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux+flux2



#Ocean to water column nitrate
fluxvec <-fluxNITinflow_i
fluxvec2<-fluxNITinflow_o
sourcename<-"ocean"
destname  <-"wcnitrate"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flux2<-fluxvec2[outrowend]-fluxvec2[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux+flux2

#Rivers to water column nitrate
fluxvec<-rivNITinflow
sourcename<-"rivers"
destname  <-"wcnitrate"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#Atmosphere to water column nitrate
fluxvec<-atmosNITinput_i
fluxvec2<-atmosNITinput_o
sourcename<-"atmosphere"
destname  <-"wcnitrate"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flux2<-fluxvec2[outrowend]-fluxvec2[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux+flux2


#Ocean to water column detritus
fluxvec <-fluxDETinflow_i
fluxvec2<-fluxDETinflow_o
sourcename<-"ocean"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flux2<-fluxvec2[outrowend]-fluxvec2[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux+flux2


#Rivers to water column detritus
fluxvec<-rivPARTinflow
sourcename<-"rivers"
destname  <-"wcdetritus"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux

#To phytoplankton
fluxvec <-fluxPHYTinflow_i
fluxvec2<-fluxPHYTinflow_o
sourcename<-"ocean"
destname  <-"phyt"
flux<-fluxvec[outrowend]-fluxvec[outrowstart]
flux2<-fluxvec2[outrowend]-fluxvec2[outrowstart]
flowmatrix_template[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- flux+flux2



flow_matrix_all_fluxes <- flowmatrix_template
#Save the new flow matrix to a file.
filename = csvname(resultsdir, "flow_matrix_all_fluxes", identifier)
writecsv(flowmatrix_template, filename, row.names=TRUE)

#--------------------------------------------------------------------------------------------------------

#FOR SOME INDICES WE NEED TO REMOVE FROM THE FLOW MATRIX THE FLUXES DUE TO SPAWNING AND RECRUITMENT

#MAKE A COPY OF THE FLOWMATRIX
flowmatrix_no_sp_rec<-flowmatrix_template
outrowstart<-((nyears-1)*360+1)
outrowend  <-ndays

#Remove recruitment of pelagic fish
sourcename<-"pfishlar"
destname  <-"pfish"
fluxvec<-fluxpfishlar_pfish
rflux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_no_sp_rec[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- 
       flowmatrix_no_sp_rec[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] - rflux 

#Remove recruitment of demersal fish
sourcename<-"dfishlar"
destname  <-"dfish"
fluxvec<-fluxdfishlar_dfish
rflux<-fluxvec[outrowend]-fluxvec[outrowstart]
flowmatrix_no_sp_rec[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- 
       flowmatrix_no_sp_rec[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] - rflux 

#Remove recruitment of f/d benthos
sourcename<-"benthslar"
destname  <-"benths"
flowmatrix_no_sp_rec[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- 0

#Remove recruitment of c/s benthos
sourcename<-"benthclar"
destname  <-"benthc"
flowmatrix_no_sp_rec[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- 0


#Remove spawning of pelagic fish
sourcename<-"pfish"
destname  <-"pfishlar"
flowmatrix_no_sp_rec[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- 0

#Remove spawning  of demersal fish
sourcename<-"dfish"
destname  <-"dfishlar"
flowmatrix_no_sp_rec[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- 0

#Remove spawninmg of f/d benthos
sourcename<-"benths"
destname  <-"benthslar"
flowmatrix_no_sp_rec[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- 0

#Remove spawning of c/s benthos
sourcename<-"benthc"
destname  <-"benthclar"
flowmatrix_no_sp_rec[(which(colnames(flowmatrix_template)==sourcename)),(which(colnames(flowmatrix_template)==destname))] <- 0


#Save the new flow matrix to a file.

filename = csvname(resultsdir, "flow_matrix_excl_spawn_recruit", identifier)
writecsv(flowmatrix_no_sp_rec, filename, row.names=TRUE)

#--------------------------------------------------------------------------------------------------------




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Derive a load of network indices


#library(NetIndices)

#............................................................................

#Check if discards are present or not - if not we have to treat these as an inport and export
Disc_sum<-sum(flowmatrix_template$discards)

zerocheck<-which(colSums(flowmatrix_template)[1:26]==0)
zerolist<-colnames(flowmatrix_template)[zerocheck]

#............................................................................


#Trophic index calculation


TrophicIndexResults<-TrophInd(flowmatrix_no_sp_rec, Import = c("ocean","rivers","atmosphere","seabed"),
                              Export = c("ocean","rivers","atmosphere","seabed","landings","beachcast"),
                              Dead=c("wcammonia","sedammonia","wcnitrate","sednitrate","wcdetritus","seddetritus","seddetritusR","corpses","kelpdebris","discards"))
#Output -- TL = Trophic level, OI = Omnivory index


#............................................................................


if(Disc_sum>0) AscendencyIndexResults<-AscInd(flowmatrix_template, Import = c("ocean","rivers","atmosphere","seabed"),Export = c("ocean","rivers","atmosphere","seabed","landings","beachcast"))
if(Disc_sum==0) AscendencyIndexResults<-AscInd(flowmatrix_template, Import = c("ocean","rivers","atmosphere","seabed","discards"),Export = c("ocean","rivers","atmosphere","seabed","landings","beachcast","discards"))
#Outputs:
#asc the ascendency of the network, a measure of growth and development.
#overh the overhead of the network.
#cap the development capacity of the network, an upper bound on ascendency.
#ACratio the ratio of ascendency and capacity.


#............................................................................

#For dependency analysis we need to reduce the flowmatrix to the living components

   #flowmatrix_for_dependency<-flowmatrix_template[8:26,8:27]
   #Dependency(flowmatrix_for_dependency, Import = c("ocean","rivers","atmosphere","seabed"),Export = c("ocean","rivers","atmosphere","seabed","landings","beachcast"))
#Produces a matrix of the dependency of component i (row) on component j (column)

#............................................................................


if(length(zerolist)>0) PathwayIndexResults<-PathInd(flowmatrix_template, Import = c(zerolist,"ocean","rivers","atmosphere","seabed"),Export = c(zerolist,"ocean","rivers","atmosphere","seabed","landings","beachcast"))
if(length(zerolist)==0) PathwayIndexResults<-PathInd(flowmatrix_template, Import = c("ocean","rivers","atmosphere","seabed"),Export = c("ocean","rivers","atmosphere","seabed","landings","beachcast"))
#if(Disc_sum>0) PathwayIndexResults<-PathInd(flowmatrix_template, Import = c("ocean","rivers","atmosphere","seabed"),Export = c("ocean","rivers","atmosphere","seabed","landings","beachcast"))
#if(Disc_sum==0) PathwayIndexResults<-PathInd(flowmatrix_template, Import = c("ocean","rivers","atmosphere","seabed","discards"),Export = c("ocean","rivers","atmosphere","seabed","landings","beachcast","discards"))
#Outputs:
#TSTC total system cycled throughflow.
#TSTS non-cycled throughflow.
#FCI Finn's cycling index (1980).
#FCIb revised Finn's cycling index, sensu Allesina and Ulanowicz, 2004.
#APL average pathlength, also known as Network Aggradation (Sum of APLc and APLs in Latham 2006).

#............................................................................

if(Disc_sum>0) GeneralIndexResults<-GenInd(flowmatrix_template, Import = c("ocean","rivers","atmosphere","seabed"),Export = c("ocean","rivers","atmosphere","seabed","landings","beachcast"))
if(Disc_sum==0) GeneralIndexResults<-GenInd(flowmatrix_template, Import = c("ocean","rivers","atmosphere","seabed","discards"),Export = c("ocean","rivers","atmosphere","seabed","landings","beachcast","discards"))
#Outputs:
#N number of compartments, excluding the externals.
#T.. total System Throughput.
#TST total System Throughflow.
#Lint number of Internal links.
#Ltot total number of links.
#LD link Density
#C connectance (internal).
#Tijbar average Link Weight.
#TSTbar average Compartment Throughflow .
#Cbar compartmentalization, [0,1], the degree of connectedness of subsystems within a network.

#............................................................................

#   if(length(zerolist)>0) EnvironmentIndexResults<-EnvInd(flowmatrix_template, Import = c(zerolist,"ocean","rivers","atmosphere","seabed"),Export = c(zerolist,"ocean","rivers","atmosphere","seabed","landings","beachcast"))
#   if(length(zerolist)==0) EnvironmentIndexResults<-EnvInd(flowmatrix_template, Import = c("ocean","rivers","atmosphere","seabed"),Export = c("ocean","rivers","atmosphere","seabed","landings","beachcast"))
# TEMPORARILY DISABLED
#if(Disc_sum>0) EnvironmentIndexResults<-EnvInd(flowmatrix_template, Import = c("ocean","rivers","atmosphere","seabed"),Export = c("ocean","rivers","atmosphere","seabed","landings","beachcast"))
#if(Disc_sum==0) EnvironmentIndexResults<-EnvInd(flowmatrix_template, Import = c("ocean","rivers","atmosphere","seabed","discards"),Export = c("ocean","rivers","atmosphere","seabed","landings","beachcast","discards"))
#Outputs:
#NAG Network aggradation = average path length.
#HP Homogenization index.
#BC Synergism.
#ID Dominance of Indirect effects.
#MN Mean of non-dimensional flow-matrix (N).
#MG Mean of direct flow-matrix (G).
#CVN Coefficient of variation of non-dimensional flow-matrix (N).
#CVG Coefficient of variation of direct flow-matrix (G).
#U Only if Full == TRUE: The Utility non-dimensional matrix.
#N1 Only if Full == TRUE: The Integral non-dimensional Flow Matrix.
#G Only if Full == TRUE: The Normalized direct flow (or transitive closure) matrix.


#............................................................................

if(Disc_sum>0) EffectivenessIndexResults<-EffInd(flowmatrix_template, Import = c("ocean","rivers","atmosphere","seabed"),Export = c("ocean","rivers","atmosphere","seabed","landings","beachcast"))
if(Disc_sum==0) EffectivenessIndexResults<-EffInd(flowmatrix_template, Import = c("ocean","rivers","atmosphere","seabed","discards"),Export = c("ocean","rivers","atmosphere","seabed","landings","beachcast","discards"))
#Outputs:
#CZ Effective connectance
#FZ Effective Flows
#NZ Effective nodes
#RZ Effective roles

#............................................................................

#TrophicIndexResults


#AscendencyIndexResults


#PathwayIndexResults


#GeneralIndexResults


#EnvironmentIndexResults


#EffectivenessIndexResults


#Build all these indices together into a structure
#Need to store 47+number of rows in the trophic index results rows of data

for(ii in 1:(nrow(TrophicIndexResults))){
if(ii==1) trophicnames<-paste(rownames(TrophicIndexResults)[ii],"_trophiclevel",sep="")
if(ii>1) trophicnames<-c(trophicnames,paste(rownames(TrophicIndexResults)[ii],"_trophiclevel",sep=""))
}
for(ii in 1:(nrow(TrophicIndexResults))){
trophicnames<-c(trophicnames,paste(rownames(TrophicIndexResults)[ii],"_omnivoryindex",sep=""))
}

NetworkIndexResults<-data.frame(rep(NA,(47+(2*(nrow(TrophicIndexResults))))))
names(NetworkIndexResults)<-"NetworkData"

row.names(NetworkIndexResults)<-c(
trophicnames,

#Ascendency Indices
"Ascendency_total",
"Ascendency_internal",
"Ascendency_import",
"Ascendency_external",
"Ascendency_dissipation",
"Overhead_total",
"Overhead_internal",
"Overhead_import",
"Overhead_external",
"Overhead_dissipation",
"Capacity_total",
"Capacity_internal",
"Capacity_import",
"Capacity_external",
"Capacity_dissipation",
"ACratio_total",
"ACratio_internal",
"ACratio_import",
"ACratio_external",
"ACratio_dissipation",

#Pathway Indices
"Total_system_cycled_thoughflow_TSTC",
"Non_cycled_throughflow_TSTS",
"Finns_cycling_index_FCI",
"Revised_Finns_cycling_index_FCIb",
"Average_path_length",

#Generaal Indices
"Number_of_compartments_N",
"Total_system_throughput_T",
"Total_system_throughflow_TST",
"Number_of_internal_links_Lint",
"Total_number_of_links_Ltot",
"Link_density_LD",
"Connectance_C",
"Average_link_weight_Tijbar",
"Average_compartment_thoughflow_TSTbar",
"Compartmentalization_Cbar",

#Environment Indices
"Network_aggradation_NAG",
"Homogenization_index_HP",
"Synergism_BC",
"Dominance_of_indirect_effects_ID",
"Mean_of_non_dimension_flowmatrix_MN",
"Mean_of_direct_flowmatrix_MG",
"CV_of_non_dimension_flowmatrix_MN",
"CV_of_direct_flowmatrix_MG",

#Effectiveness Indices
"Effective_connectance_CZ",
"Effective_flows_FZ",
"Effective_nodes_NZ",
"Effective_roles_RZ")


NetworkIndexResults[1:(nrow(TrophicIndexResults)),1] <- TrophicIndexResults[,1]
NetworkIndexResults[(1+(nrow(TrophicIndexResults))):(2*(nrow(TrophicIndexResults))),1] <- TrophicIndexResults[,2]
lastfilled<-(2*(nrow(TrophicIndexResults)))

for(jj in 1:4){
NetworkIndexResults[(lastfilled+1+((jj-1)*5)):(lastfilled+5+((jj-1)*5)),1] <- AscendencyIndexResults[,jj]
}
lastfilled<-lastfilled+5+((4-1)*5)

NetworkIndexResults[(lastfilled+1):(lastfilled+5),1] <- t(as.data.frame(PathwayIndexResults))
lastfilled<-lastfilled+5

NetworkIndexResults[(lastfilled+1):(lastfilled+10),1] <- t(as.data.frame(GeneralIndexResults))
lastfilled<-lastfilled+10


#NetworkIndexResults[(lastfilled+1):(lastfilled+8),1] <- t(as.data.frame(EnvironmentIndexResults))
NetworkIndexResults[(lastfilled+1):(lastfilled+8),1] <- NA   # OPTION WHERE THE ENVIND ROUTINE IS DISABLED
lastfilled<-lastfilled+8



NetworkIndexResults[(lastfilled+1):(lastfilled+4),1] <- t(as.data.frame(EffectivenessIndexResults))
lastfilled<-lastfilled+4


#Save the new flow matrix to a file.

filename = csvname(resultsdir, "Network_indices_output", identifier)
writecsv(NetworkIndexResults, filename, row.names=TRUE)

	list(
		flow_matrix_all_fluxes		= flow_matrix_all_fluxes,
		flow_matrix_excl_spawn_recruit	= flowmatrix_no_sp_rec,
		NetworkIndexResults		= NetworkIndexResults
	)
}

