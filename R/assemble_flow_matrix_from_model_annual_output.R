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
#' @importFrom utils write.table
#' @importFrom NetIndices TrophInd AscInd PathInd GenInd EffInd
#'
#' @export
#
assemble_flow_matrix_from_model_annual_output <- function(model, output, aggregates) {

	# Unpack:
	model.path	<- el(model, "path")

	run		<- el(model, "run")
	ndays		<- el(run, "ndays")
	nyears		<- el(run, "nyears")
	identifier	<- el(run, "identifier")
	resultsdir	<- el(run, "resultsdir")

	data		<- el(model, "data")
	physical.parms	<- el(data, "physical.parameters")

	# extract physical parameters:
	so_depth		<- el(physical.parms, "so_depth")
	d_depth			<- el(physical.parms, "d_depth")
	si_depth		<- el(physical.parms, "si_depth")
	bx_depth		<- el(physical.parms, "bx_depth")
	x_depth_s1		<- el(physical.parms, "x_depth_s1")
	x_depth_s2		<- el(physical.parms, "x_depth_s2")
	x_depth_s3		<- el(physical.parms, "x_depth_s3")
	x_depth_d1		<- el(physical.parms, "x_depth_d1")
	x_depth_d2		<- el(physical.parms, "x_depth_d2")
	x_depth_d3		<- el(physical.parms, "x_depth_d3")
	x_area_s0		<- el(physical.parms, "x_area_s0")
	x_area_s1		<- el(physical.parms, "x_area_s1")
	x_area_s2		<- el(physical.parms, "x_area_s2")
	x_area_s3		<- el(physical.parms, "x_area_s3")
	x_area_d0		<- el(physical.parms, "x_area_d0")
	x_area_d1		<- el(physical.parms, "x_area_d1")
	x_area_d2		<- el(physical.parms, "x_area_d2")
	x_area_d3		<- el(physical.parms, "x_area_d3")
	x_rock_s1		<- el(physical.parms, "x_rock_s1")
	x_rock_s2		<- el(physical.parms, "x_rock_s2")
	x_rock_s3		<- el(physical.parms, "x_rock_s3")
	x_rock_d1		<- el(physical.parms, "x_rock_d1")
	x_rock_d2		<- el(physical.parms, "x_rock_d2")
	x_rock_d3		<- el(physical.parms, "x_rock_d3")
	x_nonrock_s		<- el(physical.parms, "x_nonrock_s")
	x_nonrock_d		<- el(physical.parms, "x_nonrock_d")
	x_poros_s1		<- el(physical.parms, "x_poros_s1")
	x_poros_s2		<- el(physical.parms, "x_poros_s2")
	x_poros_s3		<- el(physical.parms, "x_poros_s3")
	x_poros_d1		<- el(physical.parms, "x_poros_d1")
	x_poros_d2		<- el(physical.parms, "x_poros_d2")
	x_poros_d3		<- el(physical.parms, "x_poros_d3")
	Kxw_s1			<- el(physical.parms, "Kxw_s1")
	Kxw_s2			<- el(physical.parms, "Kxw_s2")
	Kxw_s3			<- el(physical.parms, "Kxw_s3")
	Kxw_d1			<- el(physical.parms, "Kxw_d1")
	Kxw_d2			<- el(physical.parms, "Kxw_d2")
	Kxw_d3			<- el(physical.parms, "Kxw_d3")
	xbioturb_depth_s1	<- el(physical.parms, "xbioturb_depth_s1")
	xbioturb_depth_s2	<- el(physical.parms, "xbioturb_depth_s2")
	xbioturb_depth_s3	<- el(physical.parms, "xbioturb_depth_s3")
	xbioturb_depth_d1	<- el(physical.parms, "xbioturb_depth_d1")
	xbioturb_depth_d2	<- el(physical.parms, "xbioturb_depth_d2")
	xbioturb_depth_d3	<- el(physical.parms, "xbioturb_depth_d3")
	xerosion_depth_s1	<- el(physical.parms, "xerosion_depth_s1")
	xerosion_depth_s2	<- el(physical.parms, "xerosion_depth_s2")
	xerosion_depth_s3	<- el(physical.parms, "xerosion_depth_s3")
	xerosion_depth_d1	<- el(physical.parms, "xerosion_depth_d1")
	xerosion_depth_d2	<- el(physical.parms, "xerosion_depth_d2")
	xerosion_depth_d3	<- el(physical.parms, "xerosion_depth_d3")
	xlightSPM_intercept	<- el(physical.parms, "xlightSPM_intercept")
	xlightSPM_slope		<- el(physical.parms, "xlightSPM_slope")
	xinshore_phyt_prop_depth<- el(physical.parms, "xinshore_phyt_prop_depth")
	xinshore_kelp_prop_depth<- el(physical.parms, "xinshore_kelp_prop_depth")
	x_xR_detritus_s1	<- el(physical.parms, "x_xR_detritus_s1")
	x_xR_detritus_s2	<- el(physical.parms, "x_xR_detritus_s2")
	x_xR_detritus_s3	<- el(physical.parms, "x_xR_detritus_s3")
	x_xR_detritus_d1	<- el(physical.parms, "x_xR_detritus_d1")
	x_xR_detritus_d2	<- el(physical.parms, "x_xR_detritus_d2")
	x_xR_detritus_d3	<- el(physical.parms, "x_xR_detritus_d3")
	ref_Kxw			<- el(physical.parms, "ref_Kxw")
	x_shallowprop		<- el(physical.parms, "x_shallowprop")
	habitat_areas		<- el(physical.parms, "habitat_areas")

	# extract output:
	time			<- el(output, "time")
	detritus_so		<- el(output, "detritus_so")
	detritus_d		<- el(output, "detritus_d")
	x_detritus_s1		<- el(output, "x_detritus_s1")
	x_detritus_s2		<- el(output, "x_detritus_s2")
	x_detritus_s3		<- el(output, "x_detritus_s3")
	x_detritus_d1		<- el(output, "x_detritus_d1")
	x_detritus_d2		<- el(output, "x_detritus_d2")
	x_detritus_d3		<- el(output, "x_detritus_d3")
	xR_detritus_s1		<- el(output, "xR_detritus_s1")
	xR_detritus_s2		<- el(output, "xR_detritus_s2")
	xR_detritus_s3		<- el(output, "xR_detritus_s3")
	xR_detritus_d1		<- el(output, "xR_detritus_d1")
	xR_detritus_d2		<- el(output, "xR_detritus_d2")
	xR_detritus_d3		<- el(output, "xR_detritus_d3")
	discard_o		<- el(output, "discard_o")
	corpse_s1		<- el(output, "corpse_s1")
	corpse_s2		<- el(output, "corpse_s2")
	corpse_s3		<- el(output, "corpse_s3")
	corpse_d1		<- el(output, "corpse_d1")
	corpse_d2		<- el(output, "corpse_d2")
	corpse_d3		<- el(output, "corpse_d3")
	ammonia_so		<- el(output, "ammonia_so")
	ammonia_d		<- el(output, "ammonia_d")
	x_ammonia_s1		<- el(output, "x_ammonia_s1")
	x_ammonia_s2		<- el(output, "x_ammonia_s2")
	x_ammonia_s3		<- el(output, "x_ammonia_s3")
	x_ammonia_d1		<- el(output, "x_ammonia_d1")
	x_ammonia_d2		<- el(output, "x_ammonia_d2")
	x_ammonia_d3		<- el(output, "x_ammonia_d3")
	nitrate_so		<- el(output, "nitrate_so")
	nitrate_d		<- el(output, "nitrate_d")
	x_nitrate_s1		<- el(output, "x_nitrate_s1")
	x_nitrate_s2		<- el(output, "x_nitrate_s2")
	x_nitrate_s3		<- el(output, "x_nitrate_s3")
	x_nitrate_d1		<- el(output, "x_nitrate_d1")
	x_nitrate_d2		<- el(output, "x_nitrate_d2")
	x_nitrate_d3		<- el(output, "x_nitrate_d3")
	phyt_so			<- el(output, "phyt_so")
	phyt_d			<- el(output, "phyt_d")
	herb_o			<- el(output, "herb_o")
	carn_o			<- el(output, "carn_o")
	benthslar_o		<- el(output, "benthslar_o")
	benths_o		<- el(output, "benths_o")
	benthclar_o		<- el(output, "benthclar_o")
	benthc_o		<- el(output, "benthc_o")
	fishp_o			<- el(output, "fishp_o")
	fishplar_o		<- el(output, "fishplar_o")
	fishd_o			<- el(output, "fishd_o")
	fishdlar_o		<- el(output, "fishdlar_o")
	fishm_o			<- el(output, "fishm_o")
	bird_o			<- el(output, "bird_o")
	detritus_si		<- el(output, "detritus_si")
	ammonia_si		<- el(output, "ammonia_si")
	nitrate_si		<- el(output, "nitrate_si")
	phyt_si			<- el(output, "phyt_si")
	benthslar_i		<- el(output, "benthslar_i")
	benthclar_i		<- el(output, "benthclar_i")
	benths_i		<- el(output, "benths_i")
	benthc_i		<- el(output, "benthc_i")
	discard_i		<- el(output, "discard_i")
	herb_i			<- el(output, "herb_i")
	carn_i			<- el(output, "carn_i")
	fishplar_i		<- el(output, "fishplar_i")
	fishdlar_i		<- el(output, "fishdlar_i")
	fishp_i			<- el(output, "fishp_i")
	fishm_i			<- el(output, "fishm_i")
	fishd_i			<- el(output, "fishd_i")
	bird_i			<- el(output, "bird_i")
	seal_o			<- el(output, "seal_o")
	seal_i			<- el(output, "seal_i")
	ceta_o			<- el(output, "ceta_o")
	ceta_i			<- el(output, "ceta_i")
	corpse_s0		<- el(output, "corpse_s0")
	corpse_d0		<- el(output, "corpse_d0")
	kelpC			<- el(output, "kelpC")
	kelpN			<- el(output, "kelpN")
	kelpdebris		<- el(output, "kelpdebris")
	netpprod_o		<- el(output, "netpprod_o")
	netpprod_i		<- el(output, "netpprod_i")
	PNP_o			<- el(output, "PNP_o")
	PNP_i			<- el(output, "PNP_i")
	phytgrossprod_o		<- el(output, "phytgrossprod_o")
	phytgrossprod_i		<- el(output, "phytgrossprod_i")
	kelpCprod_i		<- el(output, "kelpCprod_i")
	kelpCexud_i		<- el(output, "kelpCexud_i")
	kelpNprod_i		<- el(output, "kelpNprod_i")
	herbgrossprod_o		<- el(output, "herbgrossprod_o")
	herbgrossprod_i		<- el(output, "herbgrossprod_i")
	carngrossprod_o		<- el(output, "carngrossprod_o")
	carngrossprod_i		<- el(output, "carngrossprod_i")
	pfishlargrossprod_o	<- el(output, "pfishlargrossprod_o")
	pfishlargrossprod_i	<- el(output, "pfishlargrossprod_i")
	dfishlargrossprod_o	<- el(output, "dfishlargrossprod_o")
	dfishlargrossprod_i	<- el(output, "dfishlargrossprod_i")
	pfishgrossprod_o	<- el(output, "pfishgrossprod_o")
	pfishgrossprod_i	<- el(output, "pfishgrossprod_i")
	mfishgrossprod_o	<- el(output, "mfishgrossprod_o")
	mfishgrossprod_i	<- el(output, "mfishgrossprod_i")
	dfishgrossprod_o	<- el(output, "dfishgrossprod_o")
	dfishgrossprod_i	<- el(output, "dfishgrossprod_i")
	benthslargrossprod_o	<- el(output, "benthslargrossprod_o")
	benthslargrossprod_i	<- el(output, "benthslargrossprod_i")
	benthclargrossprod_o	<- el(output, "benthclargrossprod_o")
	benthclargrossprod_i	<- el(output, "benthclargrossprod_i")
	benthsgrossprod_o	<- el(output, "benthsgrossprod_o")
	benthsgrossprod_i	<- el(output, "benthsgrossprod_i")
	benthcgrossprod_o	<- el(output, "benthcgrossprod_o")
	benthcgrossprod_i	<- el(output, "benthcgrossprod_i")
	birdgrossprod_o		<- el(output, "birdgrossprod_o")
	birdgrossprod_i		<- el(output, "birdgrossprod_i")
	sealgrossprod_o		<- el(output, "sealgrossprod_o")
	sealgrossprod_i		<- el(output, "sealgrossprod_i")
	cetagrossprod_o		<- el(output, "cetagrossprod_o")
	cetagrossprod_i		<- el(output, "cetagrossprod_i")
	wcdenitrif_o		<- el(output, "wcdenitrif_o")
	wcdenitrif_i		<- el(output, "wcdenitrif_i")
	seddenitrif_o		<- el(output, "seddenitrif_o")
	seddenitrif_i		<- el(output, "seddenitrif_i")
	fluxsedamm_wcamm	<- el(output, "fluxsedamm_wcamm")
	fluxwcdet_wcamm		<- el(output, "fluxwcdet_wcamm")
	fluxherb_wcamm		<- el(output, "fluxherb_wcamm")
	fluxcarn_wcamm		<- el(output, "fluxcarn_wcamm")
	fluxpfishlar_wcamm	<- el(output, "fluxpfishlar_wcamm")
	fluxdfishlar_wcamm	<- el(output, "fluxdfishlar_wcamm")
	fluxpfish_wcamm		<- el(output, "fluxpfish_wcamm")
	fluxmfish_wcamm		<- el(output, "fluxmfish_wcamm")
	fluxdfish_wcamm		<- el(output, "fluxdfish_wcamm")
	fluxbenthslar_wcamm	<- el(output, "fluxbenthslar_wcamm")
	fluxbenthclar_wcamm	<- el(output, "fluxbenthclar_wcamm")
	fluxbenths_wcamm	<- el(output, "fluxbenths_wcamm")
	fluxbenthc_wcamm	<- el(output, "fluxbenthc_wcamm")
	fluxbird_wcamm		<- el(output, "fluxbird_wcamm")
	fluxseal_wcamm		<- el(output, "fluxseal_wcamm")
	fluxceta_wcamm		<- el(output, "fluxceta_wcamm")
	fluxxdet_sedamm		<- el(output, "fluxxdet_sedamm")
	fluxxRdet_sedamm	<- el(output, "fluxxRdet_sedamm")
	fluxwcamm_wcnit		<- el(output, "fluxwcamm_wcnit")
	fluxsednit_wcnit	<- el(output, "fluxsednit_wcnit")
	fluxsedamm_sednit	<- el(output, "fluxsedamm_sednit")
	fluxxdet_wcdet		<- el(output, "fluxxdet_wcdet")
	fluxkelpdebris_wcdet	<- el(output, "fluxkelpdebris_wcdet")
	fluxcorp_wcdet		<- el(output, "fluxcorp_wcdet")
	fluxphyt_wcdet		<- el(output, "fluxphyt_wcdet")
	fluxherb_wcdet		<- el(output, "fluxherb_wcdet")
	fluxcarn_wcdet		<- el(output, "fluxcarn_wcdet")
	fluxpfishlar_wcdet	<- el(output, "fluxpfishlar_wcdet")
	fluxdfishlar_wcdet	<- el(output, "fluxdfishlar_wcdet")
	fluxpfish_wcdet		<- el(output, "fluxpfish_wcdet")
	fluxmfish_wcdet		<- el(output, "fluxmfish_wcdet")
	fluxdfish_wcdet		<- el(output, "fluxdfish_wcdet")
	fluxbenthslar_wcdet	<- el(output, "fluxbenthslar_wcdet")
	fluxbenthclar_wcdet	<- el(output, "fluxbenthclar_wcdet")
	fluxbenths_wcdet	<- el(output, "fluxbenths_wcdet")
	fluxbenthc_wcdet	<- el(output, "fluxbenthc_wcdet")
	fluxbird_wcdet		<- el(output, "fluxbird_wcdet")
	fluxseal_wcdet		<- el(output, "fluxseal_wcdet")
	fluxceta_wcdet		<- el(output, "fluxceta_wcdet")
	fluxwcdet_xdet		<- el(output, "fluxwcdet_xdet")
	fluxcorp_xdet		<- el(output, "fluxcorp_xdet")
	fluxbenths_xdet		<- el(output, "fluxbenths_xdet")
	fluxbenthc_xdet		<- el(output, "fluxbenthc_xdet")
	fluxxdet_xRdet		<- el(output, "fluxxdet_xRdet")
	fluxkelpdebris_xRdet	<- el(output, "fluxkelpdebris_xRdet")
	fluxcorp_xRdet		<- el(output, "fluxcorp_xRdet")
	fluxkelp_kelpdebris	<- el(output, "fluxkelp_kelpdebris")
	fluxdisc_corp		<- el(output, "fluxdisc_corp")
	fluxpfish_corp		<- el(output, "fluxpfish_corp")
	fluxmfish_corp		<- el(output, "fluxmfish_corp")
	fluxdfish_corp		<- el(output, "fluxdfish_corp")
	fluxbenths_corp		<- el(output, "fluxbenths_corp")
	fluxbenthc_corp		<- el(output, "fluxbenthc_corp")
	fluxbird_corp		<- el(output, "fluxbird_corp")
	fluxseal_corp		<- el(output, "fluxseal_corp")
	fluxceta_corp		<- el(output, "fluxceta_corp")
	fluxwcamm_kelp		<- el(output, "fluxwcamm_kelp")
	fluxwcnit_kelp		<- el(output, "fluxwcnit_kelp")
	fluxwcamm_phyt_o	<- el(output, "fluxwcamm_phyt_o")
	fluxwcamm_phyt_i	<- el(output, "fluxwcamm_phyt_i")
	fluxwcnit_phyt_o	<- el(output, "fluxwcnit_phyt_o")
	fluxwcnit_phyt_i	<- el(output, "fluxwcnit_phyt_i")
	fluxwcdet_herb		<- el(output, "fluxwcdet_herb")
	fluxphyt_herb		<- el(output, "fluxphyt_herb")
	fluxbenthslar_herb	<- el(output, "fluxbenthslar_herb")
	fluxbenthclar_herb	<- el(output, "fluxbenthclar_herb")
	fluxherb_carn		<- el(output, "fluxherb_carn")
	fluxpfishlar_carn	<- el(output, "fluxpfishlar_carn")
	fluxdfishlar_carn	<- el(output, "fluxdfishlar_carn")
	fluxbenthslar_carn	<- el(output, "fluxbenthslar_carn")
	fluxbenthclar_carn	<- el(output, "fluxbenthclar_carn")
	fluxherb_pfishlar	<- el(output, "fluxherb_pfishlar")
	fluxbenthslar_pfishlar	<- el(output, "fluxbenthslar_pfishlar")
	fluxbenthclar_pfishlar	<- el(output, "fluxbenthclar_pfishlar")
	fluxherb_dfishlar	<- el(output, "fluxherb_dfishlar")
	fluxbenthslar_dfishlar	<- el(output, "fluxbenthslar_dfishlar")
	fluxbenthclar_dfishlar	<- el(output, "fluxbenthclar_dfishlar")
	fluxherb_pfish		<- el(output, "fluxherb_pfish")
	fluxcarn_pfish		<- el(output, "fluxcarn_pfish")
	fluxpfishlar_pfish	<- el(output, "fluxpfishlar_pfish")
	fluxdfishlar_pfish	<- el(output, "fluxdfishlar_pfish")
	fluxbenthslar_pfish	<- el(output, "fluxbenthslar_pfish")
	fluxbenthclar_pfish	<- el(output, "fluxbenthclar_pfish")
	fluxherb_mfish		<- el(output, "fluxherb_mfish")
	fluxcarn_mfish		<- el(output, "fluxcarn_mfish")
	fluxpfishlar_mfish	<- el(output, "fluxpfishlar_mfish")
	fluxdfishlar_mfish	<- el(output, "fluxdfishlar_mfish")
	fluxbenthslar_mfish	<- el(output, "fluxbenthslar_mfish")
	fluxbenthclar_mfish	<- el(output, "fluxbenthclar_mfish")
	fluxcorp_dfish		<- el(output, "fluxcorp_dfish")
	fluxdisc_dfish		<- el(output, "fluxdisc_dfish")
	fluxcarn_dfish		<- el(output, "fluxcarn_dfish")
	fluxpfishlar_dfish	<- el(output, "fluxpfishlar_dfish")
	fluxdfishlar_dfish	<- el(output, "fluxdfishlar_dfish")
	fluxpfish_dfish		<- el(output, "fluxpfish_dfish")
	fluxmfish_dfish		<- el(output, "fluxmfish_dfish")
	fluxdfish_dfish		<- el(output, "fluxdfish_dfish")
	fluxbenths_dfish	<- el(output, "fluxbenths_dfish")
	fluxbenthc_dfish	<- el(output, "fluxbenthc_dfish")
	fluxwcdet_benthslar	<- el(output, "fluxwcdet_benthslar")
	fluxphyt_benthslar	<- el(output, "fluxphyt_benthslar")
	fluxwcdet_benthclar	<- el(output, "fluxwcdet_benthclar")
	fluxphyt_benthclar	<- el(output, "fluxphyt_benthclar")
	fluxwcdet_benths	<- el(output, "fluxwcdet_benths")
	fluxxdet_benths		<- el(output, "fluxxdet_benths")
	fluxxRdet_benths	<- el(output, "fluxxRdet_benths")
	fluxphyt_benths		<- el(output, "fluxphyt_benths")
	fluxkelp_benthc		<- el(output, "fluxkelp_benthc")
	fluxkelpdebris_benthc	<- el(output, "fluxkelpdebris_benthc")
	fluxcorp_benthc		<- el(output, "fluxcorp_benthc")
	fluxbenths_benthc	<- el(output, "fluxbenths_benthc")
	fluxcorp_bird		<- el(output, "fluxcorp_bird")
	fluxdisc_bird		<- el(output, "fluxdisc_bird")
	fluxcarn_bird		<- el(output, "fluxcarn_bird")
	fluxpfish_bird		<- el(output, "fluxpfish_bird")
	fluxmfish_bird		<- el(output, "fluxmfish_bird")
	fluxdfish_bird		<- el(output, "fluxdfish_bird")
	fluxbenths_bird		<- el(output, "fluxbenths_bird")
	fluxbenthc_bird		<- el(output, "fluxbenthc_bird")
	fluxcorp_seal		<- el(output, "fluxcorp_seal")
	fluxdisc_seal		<- el(output, "fluxdisc_seal")
	fluxcarn_seal		<- el(output, "fluxcarn_seal")
	fluxpfish_seal		<- el(output, "fluxpfish_seal")
	fluxmfish_seal		<- el(output, "fluxmfish_seal")
	fluxdfish_seal		<- el(output, "fluxdfish_seal")
	fluxbenths_seal		<- el(output, "fluxbenths_seal")
	fluxbenthc_seal		<- el(output, "fluxbenthc_seal")
	fluxbird_seal		<- el(output, "fluxbird_seal")
	fluxdisc_ceta		<- el(output, "fluxdisc_ceta")
	fluxherb_ceta		<- el(output, "fluxherb_ceta")
	fluxcarn_ceta		<- el(output, "fluxcarn_ceta")
	fluxpfish_ceta		<- el(output, "fluxpfish_ceta")
	fluxmfish_ceta		<- el(output, "fluxmfish_ceta")
	fluxdfish_ceta		<- el(output, "fluxdfish_ceta")
	fluxbenths_ceta		<- el(output, "fluxbenths_ceta")
	fluxbenthc_ceta		<- el(output, "fluxbenthc_ceta")
	fluxbird_ceta		<- el(output, "fluxbird_ceta")
	fluxseal_ceta		<- el(output, "fluxseal_ceta")
	Bs_spawn		<- el(output, "Bs_spawn")
	Bs_recruit		<- el(output, "Bs_recruit")
	Bc_spawn		<- el(output, "Bc_spawn")
	Bc_recruit		<- el(output, "Bc_recruit")
	Pfish_spawn		<- el(output, "Pfish_spawn")
	Pfish_recruit		<- el(output, "Pfish_recruit")
	Dfish_spawn		<- el(output, "Dfish_spawn")
	Dfish_recruit		<- el(output, "Dfish_recruit")
	fluxwcnit_Ngas		<- el(output, "fluxwcnit_Ngas")
	fluxsednit_Ngas		<- el(output, "fluxsednit_Ngas")
	fluxkelpdebris_beachexport<- el(output, "fluxkelpdebris_beachexport")
	fluxAMMoutflow_o	<- el(output, "fluxAMMoutflow_o")
	fluxNIToutflow_o	<- el(output, "fluxNIToutflow_o")
	fluxAMMoutflow_i	<- el(output, "fluxAMMoutflow_i")
	fluxNIToutflow_i	<- el(output, "fluxNIToutflow_i")
	fluxPHYToutflow_o	<- el(output, "fluxPHYToutflow_o")
	fluxDEToutflow_o	<- el(output, "fluxDEToutflow_o")
	fluxPHYToutflow_i	<- el(output, "fluxPHYToutflow_i")
	fluxDEToutflow_i	<- el(output, "fluxDEToutflow_i")
	mfish_emigration	<- el(output, "mfish_emigration")
	fluxsedboundary_o	<- el(output, "fluxsedboundary_o")
	fluxsedboundary_i	<- el(output, "fluxsedboundary_i")
	fluxAMMinflow_o		<- el(output, "fluxAMMinflow_o")
	fluxNITinflow_o		<- el(output, "fluxNITinflow_o")
	fluxAMMinflow_i		<- el(output, "fluxAMMinflow_i")
	fluxNITinflow_i		<- el(output, "fluxNITinflow_i")
	fluxPHYTinflow_o	<- el(output, "fluxPHYTinflow_o")
	fluxDETinflow_o		<- el(output, "fluxDETinflow_o")
	fluxPHYTinflow_i	<- el(output, "fluxPHYTinflow_i")
	fluxDETinflow_i		<- el(output, "fluxDETinflow_i")
	mfish_imigration	<- el(output, "mfish_imigration")
	atmosAMMinput_o		<- el(output, "atmosAMMinput_o")
	atmosNITinput_o		<- el(output, "atmosNITinput_o")
	atmosAMMinput_i		<- el(output, "atmosAMMinput_i")
	atmosNITinput_i		<- el(output, "atmosNITinput_i")
	rivAMMinflow		<- el(output, "rivAMMinflow")
	rivNITinflow		<- el(output, "rivNITinflow")
	rivPARTinflow		<- el(output, "rivPARTinflow")
	DINflux_i_o		<- el(output, "DINflux_i_o")
	DINflux_o_i		<- el(output, "DINflux_o_i")
	PARTflux_i_o		<- el(output, "PARTflux_i_o")
	PARTflux_o_i		<- el(output, "PARTflux_o_i")
	activemigpelfish_i_o	<- el(output, "activemigpelfish_i_o")
	activemigmigfish_i_o	<- el(output, "activemigmigfish_i_o")
	activemigdemfish_i_o	<- el(output, "activemigdemfish_i_o")
	activemigbird_i_o	<- el(output, "activemigbird_i_o")
	activemigseal_i_o	<- el(output, "activemigseal_i_o")
	activemigceta_i_o	<- el(output, "activemigceta_i_o")
	activemigpelfish_o_i	<- el(output, "activemigpelfish_o_i")
	activemigmigfish_o_i	<- el(output, "activemigmigfish_o_i")
	activemigdemfish_o_i	<- el(output, "activemigdemfish_o_i")
	activemigbird_o_i	<- el(output, "activemigbird_o_i")
	activemigseal_o_i	<- el(output, "activemigseal_o_i")
	activemigceta_o_i	<- el(output, "activemigceta_o_i")
	vertnitflux		<- el(output, "vertnitflux")
	horiznitflux		<- el(output, "horiznitflux")
	landp_o			<- el(output, "landp_o")
	landd_quota_o		<- el(output, "landd_quota_o")
	landd_nonquota_o	<- el(output, "landd_nonquota_o")
	landm_o			<- el(output, "landm_o")
	landsb_o		<- el(output, "landsb_o")
	landcb_o		<- el(output, "landcb_o")
	landcz_o		<- el(output, "landcz_o")
	landbd_o		<- el(output, "landbd_o")
	landsl_o		<- el(output, "landsl_o")
	landct_o		<- el(output, "landct_o")
	discpel_o		<- el(output, "discpel_o")
	discdem_quota_o		<- el(output, "discdem_quota_o")
	discdem_nonquota_o	<- el(output, "discdem_nonquota_o")
	discmig_o		<- el(output, "discmig_o")
	discsb_o		<- el(output, "discsb_o")
	disccb_o		<- el(output, "disccb_o")
	disccz_o		<- el(output, "disccz_o")
	discbd_o		<- el(output, "discbd_o")
	discsl_o		<- el(output, "discsl_o")
	discct_o		<- el(output, "discct_o")
	landp_i			<- el(output, "landp_i")
	landd_quota_i		<- el(output, "landd_quota_i")
	landd_nonquota_i	<- el(output, "landd_nonquota_i")
	landm_i			<- el(output, "landm_i")
	landsb_i		<- el(output, "landsb_i")
	landcb_i		<- el(output, "landcb_i")
	landcz_i		<- el(output, "landcz_i")
	landbd_i		<- el(output, "landbd_i")
	landsl_i		<- el(output, "landsl_i")
	landct_i		<- el(output, "landct_i")
	landkp_i		<- el(output, "landkp_i")
	discpel_i		<- el(output, "discpel_i")
	discdem_quota_i		<- el(output, "discdem_quota_i")
	discdem_nonquota_i	<- el(output, "discdem_nonquota_i")
	discmig_i		<- el(output, "discmig_i")
	discsb_i		<- el(output, "discsb_i")
	disccb_i		<- el(output, "disccb_i")
	disccz_i		<- el(output, "disccz_i")
	discbd_i		<- el(output, "discbd_i")
	discsl_i		<- el(output, "discsl_i")
	discct_i		<- el(output, "discct_i")
	disckp_i		<- el(output, "disckp_i")
	offalpel_o		<- el(output, "offalpel_o")
	offaldem_quota_o	<- el(output, "offaldem_quota_o")
	offaldem_nonquota_o	<- el(output, "offaldem_nonquota_o")
	offalmig_o		<- el(output, "offalmig_o")
	offalsb_o		<- el(output, "offalsb_o")
	offalcb_o		<- el(output, "offalcb_o")
	offalcz_o		<- el(output, "offalcz_o")
	offalbd_o		<- el(output, "offalbd_o")
	offalsl_o		<- el(output, "offalsl_o")
	offalct_o		<- el(output, "offalct_o")
	offalpel_i		<- el(output, "offalpel_i")
	offaldem_quota_i	<- el(output, "offaldem_quota_i")
	offaldem_nonquota_i	<- el(output, "offaldem_nonquota_i")
	offalmig_i		<- el(output, "offalmig_i")
	offalsb_i		<- el(output, "offalsb_i")
	offalcb_i		<- el(output, "offalcb_i")
	offalcz_i		<- el(output, "offalcz_i")
	offalbd_i		<- el(output, "offalbd_i")
	offalsl_i		<- el(output, "offalsl_i")
	offalct_i		<- el(output, "offalct_i")
	offalkp_i		<- el(output, "offalkp_i")
	herbnetprod_o		<- el(output, "herbnetprod_o")
	herbnetprod_i		<- el(output, "herbnetprod_i")
	carnnetprod_o		<- el(output, "carnnetprod_o")
	carnnetprod_i		<- el(output, "carnnetprod_i")
	pfishlarnetprod_o	<- el(output, "pfishlarnetprod_o")
	pfishlarnetprod_i	<- el(output, "pfishlarnetprod_i")
	dfishlarnetprod_o	<- el(output, "dfishlarnetprod_o")
	dfishlarnetprod_i	<- el(output, "dfishlarnetprod_i")
	pfishnetprod_o		<- el(output, "pfishnetprod_o")
	pfishnetprod_i		<- el(output, "pfishnetprod_i")
	mfishnetprod_o		<- el(output, "mfishnetprod_o")
	mfishnetprod_i		<- el(output, "mfishnetprod_i")
	dfishnetprod_o		<- el(output, "dfishnetprod_o")
	dfishnetprod_i		<- el(output, "dfishnetprod_i")
	benthslarnetprod_o	<- el(output, "benthslarnetprod_o")
	benthslarnetprod_i	<- el(output, "benthslarnetprod_i")
	benthclarnetprod_o	<- el(output, "benthclarnetprod_o")
	benthclarnetprod_i	<- el(output, "benthclarnetprod_i")
	benthsnetprod_o		<- el(output, "benthsnetprod_o")
	benthsnetprod_i		<- el(output, "benthsnetprod_i")
	benthcnetprod_o		<- el(output, "benthcnetprod_o")
	benthcnetprod_i		<- el(output, "benthcnetprod_i")
	birdnetprod_o		<- el(output, "birdnetprod_o")
	birdnetprod_i		<- el(output, "birdnetprod_i")
	sealnetprod_o		<- el(output, "sealnetprod_o")
	sealnetprod_i		<- el(output, "sealnetprod_i")
	cetanetprod_o		<- el(output, "cetanetprod_o")
	cetanetprod_i		<- el(output, "cetanetprod_i")

	# extract aggregates:
	totalN			<- el(aggregates, "totalN")
	totalN_o		<- el(aggregates, "totalN_o")
	totalN_i		<- el(aggregates, "totalN_i")
	x_detritus		<- el(aggregates, "x_detritus")
	x_detritus_o		<- el(aggregates, "x_detritus_o")
	x_detritus_i		<- el(aggregates, "x_detritus_i")
	corpse			<- el(aggregates, "corpse")
	corpse_o		<- el(aggregates, "corpse_o")
	corpse_i		<- el(aggregates, "corpse_i")
	x_ammonia		<- el(aggregates, "x_ammonia")
	x_ammonia_o		<- el(aggregates, "x_ammonia_o")
	x_ammonia_i		<- el(aggregates, "x_ammonia_i")
	x_nitrate		<- el(aggregates, "x_nitrate")
	x_nitrate_o		<- el(aggregates, "x_nitrate_o")
	x_nitrate_i		<- el(aggregates, "x_nitrate_i")
	s_detritus		<- el(aggregates, "s_detritus")
	s_ammonia		<- el(aggregates, "s_ammonia")
	s_nitrate		<- el(aggregates, "s_nitrate")
	s_phyt			<- el(aggregates, "s_phyt")
	benthslar		<- el(aggregates, "benthslar")
	benthclar		<- el(aggregates, "benthclar")
	benths			<- el(aggregates, "benths")
	benthc			<- el(aggregates, "benthc")
	discard			<- el(aggregates, "discard")
	herb			<- el(aggregates, "herb")
	carn			<- el(aggregates, "carn")
	fishp			<- el(aggregates, "fishp")
	fishd			<- el(aggregates, "fishd")
	fishm			<- el(aggregates, "fishm")
	bird			<- el(aggregates, "bird")
	seal			<- el(aggregates, "seal")
	ceta			<- el(aggregates, "ceta")
	fishplar		<- el(aggregates, "fishplar")
	fishdlar		<- el(aggregates, "fishdlar")
	PNP			<- el(aggregates, "PNP")
	netpprod		<- el(aggregates, "netpprod")
	fluxwcamm_phyt		<- el(aggregates, "fluxwcamm_phyt")
	fluxwcnit_phyt		<- el(aggregates, "fluxwcnit_phyt")
	phytgrossprod		<- el(aggregates, "phytgrossprod")
	herbgrossprod		<- el(aggregates, "herbgrossprod")
	carngrossprod		<- el(aggregates, "carngrossprod")
	pfishlargrossprod	<- el(aggregates, "pfishlargrossprod")
	dfishlargrossprod	<- el(aggregates, "dfishlargrossprod")
	pfishgrossprod		<- el(aggregates, "pfishgrossprod")
	mfishgrossprod		<- el(aggregates, "mfishgrossprod")
	dfishgrossprod		<- el(aggregates, "dfishgrossprod")
	benthslargrossprod	<- el(aggregates, "benthslargrossprod")
	benthclargrossprod	<- el(aggregates, "benthclargrossprod")
	benthsgrossprod		<- el(aggregates, "benthsgrossprod")
	benthcgrossprod		<- el(aggregates, "benthcgrossprod")
	birdgrossprod		<- el(aggregates, "birdgrossprod")
	sealgrossprod		<- el(aggregates, "sealgrossprod")
	cetagrossprod		<- el(aggregates, "cetagrossprod")
	herbnetprod		<- el(aggregates, "herbnetprod")
	carnnetprod		<- el(aggregates, "carnnetprod")
	pfishlarnetprod		<- el(aggregates, "pfishlarnetprod")
	dfishlarnetprod		<- el(aggregates, "dfishlarnetprod")
	pfishnetprod		<- el(aggregates, "pfishnetprod")
	mfishnetprod		<- el(aggregates, "mfishnetprod")
	dfishnetprod		<- el(aggregates, "dfishnetprod")
	benthslarnetprod	<- el(aggregates, "benthslarnetprod")
	benthclarnetprod	<- el(aggregates, "benthclarnetprod")
	benthsnetprod		<- el(aggregates, "benthsnetprod")
	benthcnetprod		<- el(aggregates, "benthcnetprod")
	birdnetprod		<- el(aggregates, "birdnetprod")
	sealnetprod		<- el(aggregates, "sealnetprod")
	cetanetprod		<- el(aggregates, "cetanetprod")
	wcdenitrif		<- el(aggregates, "wcdenitrif")
	seddenitrif		<- el(aggregates, "seddenitrif")
	fluxsedboundary		<- el(aggregates, "fluxsedboundary")
	DIN_NET_flux_o_i	<- el(aggregates, "DIN_NET_flux_o_i")
	PART_NET_flux_o_i	<- el(aggregates, "PART_NET_flux_o_i")
	NET_activemigpelfish_o_i<- el(aggregates, "NET_activemigpelfish_o_i")
	NET_activemigmigfish_o_i<- el(aggregates, "NET_activemigmigfish_o_i")
	NET_activemigdemfish_o_i<- el(aggregates, "NET_activemigdemfish_o_i")
	NET_activemigbird_o_i	<- el(aggregates, "NET_activemigbird_o_i")
	NET_activemigseal_o_i	<- el(aggregates, "NET_activemigseal_o_i")
	NET_activemigceta_o_i	<- el(aggregates, "NET_activemigceta_o_i")
	NET_mfish_ext_o		<- el(aggregates, "NET_mfish_ext_o")
	fluxDINinflow		<- el(aggregates, "fluxDINinflow")
	fluxDINoutflow		<- el(aggregates, "fluxDINoutflow")
	fluxPARTinflow		<- el(aggregates, "fluxPARTinflow")
	fluxPARToutflow		<- el(aggregates, "fluxPARToutflow")
	atmosDINinput		<- el(aggregates, "atmosDINinput")
	rivDINinflow		<- el(aggregates, "rivDINinflow")
	landp			<- el(aggregates, "landp")
	landd			<- el(aggregates, "landd")
	landd_o			<- el(aggregates, "landd_o")
	landd_i			<- el(aggregates, "landd_i")
	landd_quota		<- el(aggregates, "landd_quota")
	landd_nonquota		<- el(aggregates, "landd_nonquota")
	landm			<- el(aggregates, "landm")
	landsb			<- el(aggregates, "landsb")
	landcb			<- el(aggregates, "landcb")
	landcz			<- el(aggregates, "landcz")
	landbd			<- el(aggregates, "landbd")
	landsl			<- el(aggregates, "landsl")
	landct			<- el(aggregates, "landct")
	discpel			<- el(aggregates, "discpel")
	discdem			<- el(aggregates, "discdem")
	discdem_o		<- el(aggregates, "discdem_o")
	discdem_i		<- el(aggregates, "discdem_i")
	discdem_quota		<- el(aggregates, "discdem_quota")
	discdem_nonquota	<- el(aggregates, "discdem_nonquota")
	discmig			<- el(aggregates, "discmig")
	discsb			<- el(aggregates, "discsb")
	disccb			<- el(aggregates, "disccb")
	disccz			<- el(aggregates, "disccz")
	discbd			<- el(aggregates, "discbd")
	discsl			<- el(aggregates, "discsl")
	discct			<- el(aggregates, "discct")
	offalpel		<- el(aggregates, "offalpel")
	offaldem		<- el(aggregates, "offaldem")
	offaldem_o		<- el(aggregates, "offaldem_o")
	offaldem_i		<- el(aggregates, "offaldem_i")
	offaldem_quota		<- el(aggregates, "offaldem_quota")
	offaldem_nonquota	<- el(aggregates, "offaldem_nonquota")
	offalmig		<- el(aggregates, "offalmig")
	offalsb			<- el(aggregates, "offalsb")
	offalcb			<- el(aggregates, "offalcb")
	offalcz			<- el(aggregates, "offalcz")
	offalbd			<- el(aggregates, "offalbd")
	offalsl			<- el(aggregates, "offalsl")
	offalct			<- el(aggregates, "offalct")
	x_poros			<- el(aggregates, "x_poros")
	x_depth			<- el(aggregates, "x_depth")
	x_poros_o		<- el(aggregates, "x_poros_o")
	x_poros_i		<- el(aggregates, "x_poros_i")
	x_depth_o		<- el(aggregates, "x_depth_o")
	x_depth_i		<- el(aggregates, "x_depth_i")



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

	flowmatrix_template <- readcsv(model.path, PARAMETERS_DIR, foodwebflowmatrixfile)

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



#Save the new flow matrix to a file.
filename = csvname(resultsdir, "flow_matrix_all_fluxes", identifier)
writecsv(flowmatrix_template, filename, row.names=TRUE)
write.table(flowmatrix_template,file=paste(resultsdir,"flow_matrix_all_fluxes",identifier,".csv",sep=""),sep=",",row.names=TRUE)

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

filename = csvname(resultsdir, "flow_matrix_excl_spawn_recuit", identifier)
writecsv(flowmatrix_no_sp_rec, filename, row.names=TRUE)
write.table(flowmatrix_no_sp_rec,file=paste(resultsdir,"flow_matrix_excl_spawn_recuit",identifier,".csv",sep=""),sep=",",row.names=TRUE)

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
#FCI Finns cycling index (1980).
#FCIb revised Finns cycling index, sensu Allesina and Ulanowicz, 2004.
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
write.table(NetworkIndexResults,file=paste(resultsdir,"Network_indices_output",identifier,".csv",sep=""),sep=",",row.names=TRUE)

	NetworkIndexResults
}

