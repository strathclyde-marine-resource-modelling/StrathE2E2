#
# derive_model_target_results.R
#
#' derive annual target results and write to file
#'
#' returns annual target results
#'
#' @param model model object
#' @param output model output
#' @param aggregates aggregated model output
#' @param annualtargetdata annual target data
#'
#' @return target results
#'
#' @export
#
derive_model_target_results <- function(model, output, aggregates, annualtargetdata) {

	# Unpack:
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


#Derive the subset of annual flux measures used for optimision and print to a file

# includes birdmammal bycatch

#======================================================================

#Requires the prior loading of the dataframe annualtargetdata which should contain the following columns 

#Add a column to the input target dataframe to hold to corresponding derived model values
#Column 1 = Target value
#Column 2 = sd of target value
#Column 3 = switch to determine whether to be included in likelihood evaluation (1=yes, 0 = no)
#Column 4 = name of target value
#Column 5 = units of target value
#Column 6 = description of target value
#Column 7 = Region
#Column 8 = Time period
#Column 9 = source

#======================================================================

#Make a new dataframe and copy into it the contents of annualtargetdata but with a gap in
#the column sequence at index 3 to hold the model results which will be generated in this programme

opt_results<-annualtargetdata[,1:6]
opt_results[,3]<-NA
opt_results[,4]<-annualtargetdata[,3]
opt_results[,5]<-annualtargetdata[,4]
opt_results[,6]<-annualtargetdata[,5]
opt_results[,7]<-annualtargetdata[,6]

names(opt_results) <- c(names(annualtargetdata[1:2]),
                                     "Model_data",
                                     names(annualtargetdata[3:6]))

#The dataframe opt_results will be needed by the routine that derives the likelihood
#of the model output given the observed data

#The routine in this file takes some of the information in opt-results and outputs a
#csv file of just the model results with suitably descritive rownames and the units for
#each variable

#======================================================================




xvolume_si<-si_depth*x_shallowprop
xvolume_so<-so_depth*(1-x_shallowprop)
xd_volume<-d_depth*(1-x_shallowprop)

xs_volume <- xvolume_si + xvolume_so



#First some proportions that are needed for deriving results

total_porewater_vol <- (x_area_s1*x_depth_s1*x_poros_s1 + x_area_s2*x_depth_s2*x_poros_s2 +x_area_s3*x_depth_s3*x_poros_s3 +
                      + x_area_d1*x_depth_d1*x_poros_d1 + x_area_d2*x_depth_d2*x_poros_d2 +x_area_d3*x_depth_d3*x_poros_d3)
x_porevol_s1<-(x_area_s1*x_depth_s1*x_poros_s1)/total_porewater_vol
x_porevol_s2<-(x_area_s2*x_depth_s2*x_poros_s2)/total_porewater_vol
x_porevol_s3<-(x_area_s3*x_depth_s3*x_poros_s3)/total_porewater_vol
x_porevol_d1<-(x_area_d1*x_depth_d1*x_poros_d1)/total_porewater_vol
x_porevol_d2<-(x_area_d2*x_depth_d2*x_poros_d2)/total_porewater_vol
x_porevol_d3<-(x_area_d3*x_depth_d3*x_poros_d3)/total_porewater_vol
x_porevol_s <- x_porevol_s1 +x_porevol_s2 + x_porevol_s3
x_porevol_d <- (1-x_porevol_s)


total_sedsolid_vol <- (x_area_s1*x_depth_s1*(1-x_poros_s1) + x_area_s2*x_depth_s2*(1-x_poros_s2) +x_area_s3*x_depth_s3*(1-x_poros_s3) +
                      + x_area_d1*x_depth_d1*(1-x_poros_d1) + x_area_d2*x_depth_d2*(1-x_poros_d2) +x_area_d3*x_depth_d3*(1-x_poros_d3))
x_sedsolidvol_s1<-(x_area_s1*x_depth_s1*(1-x_poros_s1))/total_sedsolid_vol
x_sedsolidvol_s2<-(x_area_s2*x_depth_s2*(1-x_poros_s2))/total_sedsolid_vol
x_sedsolidvol_s3<-(x_area_s3*x_depth_s3*(1-x_poros_s3))/total_sedsolid_vol
x_sedsolidvol_d1<-(x_area_d1*x_depth_d1*(1-x_poros_d1))/total_sedsolid_vol
x_sedsolidvol_d2<-(x_area_d2*x_depth_d2*(1-x_poros_d2))/total_sedsolid_vol
x_sedsolidvol_d3<-(x_area_d3*x_depth_d3*(1-x_poros_d3))/total_sedsolid_vol
x_sedsolidvol_s <- x_sedsolidvol_s1 +x_sedsolidvol_s2 + x_sedsolidvol_s3
x_sedsolidvol_d <- (1-x_sedsolidvol_s)

#----------------------------------

#Some transpport and geochemical fluxes

sumriverDINinflow<-rivDINinflow[ndays-90]-rivDINinflow[((nyears-1)*360+1+89)]
sumatmosDINinput<-(atmosDINinput[ndays-90]-atmosDINinput[((nyears-1)*360+1+89)])
sumDINinflow<-(fluxDINinflow[ndays-90]-fluxDINinflow[((nyears-1)*360+1+89)]) 
sumDINoutflow<-(fluxDINoutflow[ndays-90]-fluxDINoutflow[((nyears-1)*360+1+89)])



#Calculate whole domain annual average mass of a range of state variables
#Except fo rkelp, these variables ar the aggregares of the inshore and offshore mass data

aamass_herb<-mean(herb[((nyears-1)*360+1):ndays])
aamass_carn<-mean(carn[((nyears-1)*360+1):ndays])
aamass_benthslar<-mean(benthslar[((nyears-1)*360+1):ndays])
aamass_benthclar<-mean(benthclar[((nyears-1)*360+1):ndays])
aamass_benths<-mean(benths[((nyears-1)*360+1):ndays])
aamass_benthc<-mean(benthc[((nyears-1)*360+1):ndays])
aamass_fishplar<-mean(fishplar[((nyears-1)*360+1):ndays])
aamass_fishp<-mean(fishp[((nyears-1)*360+1):ndays])
aamass_fishdlar<-mean(fishdlar[((nyears-1)*360+1):ndays])
aamass_fishd<-mean(fishd[((nyears-1)*360+1):ndays])
aamass_fishm<-mean(fishm[((nyears-1)*360+1):ndays])
aamass_bird<-mean(bird[((nyears-1)*360+1):ndays])
aamass_seal<-mean(seal[((nyears-1)*360+1):ndays])
aamass_ceta<-mean(ceta[((nyears-1)*360+1):ndays])


#And concentrations and area densities for inshore and offshore zones respectively...

aamassCmolar_kelp<-((mean(kelpC[((nyears-1)*360+1):ndays]))/x_area_s0)            # mMC/m2 in forest<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
aamassCgram_kelp<- ((mean(kelpC[((nyears-1)*360+1):ndays]))/x_area_s0)*12/1000    # gC/m2 in-forest  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
aamassN_kelp<-     ((mean(kelpN[((nyears-1)*360+1):ndays]))/x_area_s0)            # mMN/m2 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

aaconc_fishp_i<-(mean(fishp_i[((nyears-1)*360+1):ndays]))/x_shallowprop
aaconc_fishp_o<-(mean(fishp_o[((nyears-1)*360+1):ndays]))/(1-x_shallowprop)

aaconc_fishd_i<-(mean(fishd_i[((nyears-1)*360+1):ndays]))/x_shallowprop
aaconc_fishd_o<-(mean(fishd_o[((nyears-1)*360+1):ndays]))/(1-x_shallowprop)

aaconc_herb_i<-(mean(herb_i[((nyears-1)*360+1):ndays]))/xvolume_si
aaconc_herb_o<-(mean(herb_o[((nyears-1)*360+1):ndays]))/(xvolume_so+xd_volume)

aaconc_carn_i<-(mean(carn_i[((nyears-1)*360+1):ndays]))/xvolume_si
aaconc_carn_o<-(mean(carn_o[((nyears-1)*360+1):ndays]))/(xvolume_so+xd_volume)

aaconc_phyt_si<-(mean(phyt_si[((nyears-1)*360+1):ndays]))/xvolume_si
aaconc_phyt_so<-(mean(phyt_so[((nyears-1)*360+1):ndays]))/xvolume_so

aaconc_nitrate_si<-(mean(nitrate_si[((nyears-1)*360+1):ndays]))/xvolume_si
aaconc_nitrate_so<-(mean(nitrate_so[((nyears-1)*360+1):ndays]))/xvolume_so

aaconc_ammonia_si<-(mean(ammonia_si[((nyears-1)*360+1):ndays]))/xvolume_si
aaconc_ammonia_so<-(mean(ammonia_so[((nyears-1)*360+1):ndays]))/xvolume_so


#And annual production rates.....

TAPP <- netpprod[ndays]-netpprod[((nyears-1)*360+1)]

MMP<-(max(s_nitrate[((nyears-1)*360+1):ndays]+nitrate_d[((nyears-1)*360+1):ndays])) - (min(s_nitrate[((nyears-1)*360+1):ndays]+nitrate_d[((nyears-1)*360+1):ndays]))
NP <- MMP+sumriverDINinflow+sumatmosDINinput+sumDINinflow-sumDINoutflow

KelpNP <- ((kelpNprod_i[ndays]-kelpNprod_i[((nyears-1)*360+1)])/x_area_s0)          # Nitrogen production - Units mMN/m2 in forest/y
KelpCP <- ((kelpCprod_i[ndays]-kelpCprod_i[((nyears-1)*360+1)])/x_area_s0)*12/1000  # Carbon gross production - Units gC/m2 in forest/y
KelpCE <- ((kelpCexud_i[ndays]-kelpCexud_i[((nyears-1)*360+1)])/x_area_s0)*12/1000  # Carbon exudation - Units gC/m2 in forest/y
prop_exud_C_kelp  <- KelpCE/KelpCP
kelp_NC           <- aamassN_kelp/aamassCmolar_kelp

Kelp_netCP <- KelpCP-KelpCE                                                             # Carbon net prodtion -  - Units gC/m2 in forest/y

kelp_beachcast<-((fluxkelpdebris_beachexport[ndays]-fluxkelpdebris_beachexport[((nyears-1)*360+1)])/x_area_s0)          # Units mMN/m2 of forest/y

kelp_beachcast_per_annProd <- kelp_beachcast/KelpNP

OmnizoogrossP <- herbgrossprod[ndays]-herbgrossprod[((nyears-1)*360+1)]

CarnzoogrossP <- carngrossprod[ndays]-carngrossprod[((nyears-1)*360+1)]

PFishgrossP<-pfishgrossprod[ndays]-pfishgrossprod[((nyears-1)*360+1)]
PFishlargrossP<-pfishlargrossprod[ndays]-pfishlargrossprod[((nyears-1)*360+1)]

DFishgrossP<-dfishgrossprod[ndays]-dfishgrossprod[((nyears-1)*360+1)]
DFishlargrossP<-dfishlargrossprod[ndays]-dfishlargrossprod[((nyears-1)*360+1)]

MFishgrossP<-mfishgrossprod[ndays]-mfishgrossprod[((nyears-1)*360+1)]

BenthsgrossP<-benthsgrossprod[ndays]-benthsgrossprod[((nyears-1)*360+1)]
BenthslargrossP<-benthslargrossprod[ndays]-benthslargrossprod[((nyears-1)*360+1)]

BenthcgrossP<-benthcgrossprod[ndays]-benthcgrossprod[((nyears-1)*360+1)]
BenthclargrossP<-benthclargrossprod[ndays]-benthclargrossprod[((nyears-1)*360+1)]

BirdnetP<-birdnetprod[ndays]-birdnetprod[((nyears-1)*360+1)]

SealnetP<-sealnetprod[ndays]-sealnetprod[((nyears-1)*360+1)]

CetanetP<-cetanetprod[ndays]-cetanetprod[((nyears-1)*360+1)]


kelpC_pb <- KelpCP/aamassCgram_kelp

benslar_pb<-BenthslargrossP/aamass_benthslar
benclar_pb<-BenthclargrossP/aamass_benthclar

benc_pb<-BenthcgrossP/aamass_benthc
bens_pb<-BenthsgrossP/aamass_benths
herb_pb<-OmnizoogrossP/aamass_herb
carn_pb<-CarnzoogrossP/aamass_carn
pfishlar_pb<-PFishlargrossP/aamass_fishplar
dfishlar_pb<-DFishlargrossP/aamass_fishdlar
pfish_pb<-PFishgrossP/aamass_fishp
dfish_pb<-DFishgrossP/aamass_fishd

mfish_pb<-MFishgrossP/aamass_fishm

bird_pb<-BirdnetP/aamass_bird
seal_pb<-SealnetP/aamass_seal
ceta_pb<-CetanetP/aamass_ceta





aaconc_x_ammonia_s1<-0
aaconc_x_ammonia_s2<-0
aaconc_x_ammonia_s3<-0
if(x_area_s1>0) {aaconc_x_ammonia_s1  <-  (mean(x_ammonia_s1[((nyears-1)*360+1):ndays]))/(x_area_s1*x_depth_s1*(x_poros_s1))} # mMN/m3 porewater
if(x_area_s2>0) {aaconc_x_ammonia_s2  <-  (mean(x_ammonia_s2[((nyears-1)*360+1):ndays]))/(x_area_s2*x_depth_s2*(x_poros_s2))} # mMN/m3 porewater
if(x_area_s3>0) {aaconc_x_ammonia_s3  <-  (mean(x_ammonia_s3[((nyears-1)*360+1):ndays]))/(x_area_s3*x_depth_s3*(x_poros_s3))} # mMN/m3 porewater

aaconc_x_ammonia_d1<-0
aaconc_x_ammonia_d2<-0
aaconc_x_ammonia_d3<-0
if(x_area_d1>0) {aaconc_x_ammonia_d1  <-  (mean(x_ammonia_d1[((nyears-1)*360+1):ndays]))/(x_area_d1*x_depth_d1*(x_poros_d1))} # mMN/m3 porewater
if(x_area_d2>0) {aaconc_x_ammonia_d2  <-  (mean(x_ammonia_d2[((nyears-1)*360+1):ndays]))/(x_area_d2*x_depth_d2*(x_poros_d2))} # mMN/m3 porewater
if(x_area_d3>0) {aaconc_x_ammonia_d3  <-  (mean(x_ammonia_d3[((nyears-1)*360+1):ndays]))/(x_area_d3*x_depth_d3*(x_poros_d3))} # mMN/m3 porewater


aaconc_x_nitrate_s1<-0
aaconc_x_nitrate_s2<-0
aaconc_x_nitrate_s3<-0
if(x_area_s1>0) {aaconc_x_nitrate_s1  <-  (mean(x_nitrate_s1[((nyears-1)*360+1):ndays]))/(x_area_s1*x_depth_s1*(x_poros_s1))} # mMN/m3 porewater
if(x_area_s2>0) {aaconc_x_nitrate_s2  <-  (mean(x_nitrate_s2[((nyears-1)*360+1):ndays]))/(x_area_s2*x_depth_s2*(x_poros_s2))} # mMN/m3 porewater
if(x_area_s3>0) {aaconc_x_nitrate_s3  <-  (mean(x_nitrate_s3[((nyears-1)*360+1):ndays]))/(x_area_s3*x_depth_s3*(x_poros_s3))} # mMN/m3 porewater

aaconc_x_nitrate_d1<-0
aaconc_x_nitrate_d2<-0
aaconc_x_nitrate_d3<-0
if(x_area_d1>0) {aaconc_x_nitrate_d1  <-  (mean(x_nitrate_d1[((nyears-1)*360+1):ndays]))/(x_area_d1*x_depth_d1*(x_poros_d1))} # mMN/m3 porewater
if(x_area_d2>0) {aaconc_x_nitrate_d2  <-  (mean(x_nitrate_d2[((nyears-1)*360+1):ndays]))/(x_area_d2*x_depth_d2*(x_poros_d2))} # mMN/m3 porewater
if(x_area_d3>0) {aaconc_x_nitrate_d3  <-  (mean(x_nitrate_d3[((nyears-1)*360+1):ndays]))/(x_area_d3*x_depth_d3*(x_poros_d3))} # mMN/m3 porewater




#This calculates the model target results assuming target of the average over all habitats in shallow and deep
#aaconc_x_ammonia_s<-0
#if(x_shallowprop>0) {aaconc_x_ammonia_s <- (x_porevol_s1*aaconc_x_ammonia_s1 + x_porevol_s2*aaconc_x_ammonia_s2 + x_porevol_s3*aaconc_x_ammonia_s3)/x_porevol_s }
#aaconc_x_ammonia_d <- (x_porevol_d1*aaconc_x_ammonia_d1 + x_porevol_d2*aaconc_x_ammonia_d2 + x_porevol_d3*aaconc_x_ammonia_d3)/(1-x_porevol_s)


#This calculates the model target results assuming _s = sandy sediment and _d = muddy sediment
aaconc_x_ammonia_s <- (x_porevol_s2*aaconc_x_ammonia_s2 + x_porevol_d2*aaconc_x_ammonia_d2)/(x_porevol_s2+x_porevol_d2)
aaconc_x_ammonia_d <- (x_porevol_s1*aaconc_x_ammonia_s1 + x_porevol_d1*aaconc_x_ammonia_d1)/(x_porevol_s1+x_porevol_d1)

aaconc_x_nitrate_s <- (x_porevol_s2*aaconc_x_nitrate_s2 + x_porevol_d2*aaconc_x_nitrate_d2)/(x_porevol_s2+x_porevol_d2)
aaconc_x_nitrate_d <- (x_porevol_s1*aaconc_x_nitrate_s1 + x_porevol_d1*aaconc_x_nitrate_d1)/(x_porevol_s1+x_porevol_d1)

aaconc_x_TON_s1<-0
aaconc_x_TON_s2<-0
aaconc_x_TON_s3<-0
if(x_poros_s1>0 && x_area_s1>0) {aaconc_x_TON_s1  <-  100*(14/1000)*(mean(x_detritus_s1[((nyears-1)*360+1):ndays]+xR_detritus_s1[((nyears-1)*360+1):ndays]))/(x_area_s1*x_depth_s1*(1-x_poros_s1)*(2650*1000)) } # N-mg/mg-sed dry wt as a percentage
if(x_poros_s2>0 && x_area_s2>0) {aaconc_x_TON_s2  <-  100*(14/1000)*(mean(x_detritus_s2[((nyears-1)*360+1):ndays]+xR_detritus_s2[((nyears-1)*360+1):ndays]))/(x_area_s2*x_depth_s2*(1-x_poros_s2)*(2650*1000)) } # N-mg/mg-sed dry wt as a percentage
if(x_poros_s3>0 && x_area_s3>0) {aaconc_x_TON_s3  <-  100*(14/1000)*(mean(x_detritus_s3[((nyears-1)*360+1):ndays]+xR_detritus_s3[((nyears-1)*360+1):ndays]))/(x_area_s3*x_depth_s3*(1-x_poros_s3)*(2650*1000)) } # N-mg/mg-sed dry wt as a percentage
#This converts the sediment detritus into units of %N by dry wt (100*gN/g-drysediment) (density of dry solid matter = 2.65g/cm3)

aaconc_x_TON_d1<-0
aaconc_x_TON_d2<-0
aaconc_x_TON_d3<-0
if(x_poros_d1>0 && x_area_d1>0) {aaconc_x_TON_d1  <-  100*(14/1000)*(mean(x_detritus_d1[((nyears-1)*360+1):ndays]+xR_detritus_d1[((nyears-1)*360+1):ndays]))/(x_area_d1*x_depth_d1*(1-x_poros_d1)*(2650*1000)) } # N-mg/mg-sed dry wt as a percentage
if(x_poros_d2>0 && x_area_d2>0) {aaconc_x_TON_d2  <-  100*(14/1000)*(mean(x_detritus_d2[((nyears-1)*360+1):ndays]+xR_detritus_d2[((nyears-1)*360+1):ndays]))/(x_area_d2*x_depth_d2*(1-x_poros_d2)*(2650*1000)) } # N-mg/mg-sed dry wt as a percentage
if(x_poros_d3>0 && x_area_d3>0) {aaconc_x_TON_d3  <-  100*(14/1000)*(mean(x_detritus_d3[((nyears-1)*360+1):ndays]+xR_detritus_d3[((nyears-1)*360+1):ndays]))/(x_area_d3*x_depth_d3*(1-x_poros_d3)*(2650*1000)) } # N-mg/mg-sed dry wt as a percentage
#This converts the sediment detritus into units of %N by dry wt (100*gN/g-drysediment) (density of dry solid matter = 2.65g/cm3)

#This calculates the model target results assuming target of the average over all habitats in shallow and deep
#aaconc_x_TON_s<-0
#if(x_shallowprop>0) {aaconc_x_TON_s <- (x_sedsolidvol_s1*aaconc_x_TON_s1 + x_sedsolidvol_s2*aaconc_x_TON_s2 + x_sedsolidvol_s3*aaconc_x_ammonia_s3)/x_sedsolidvol_s }
#aaconc_x_TON_d <- (x_sedsolidvol_d1*aaconc_x_TON_d1 + x_sedsolidvol_d2*aaconc_x_TON_d2 + x_sedsolidvol_d3*aaconc_x_TON_d3)/(1-x_sedsolidvol_s)

#This calculates the model target results assuming _s = sandy sediment and _d = muddy sediment
aaconc_x_TON_s <- (x_sedsolidvol_s2*aaconc_x_TON_s2 + x_sedsolidvol_d2*aaconc_x_TON_d2)/(x_sedsolidvol_s2+x_sedsolidvol_d2)
aaconc_x_TON_d <- (x_sedsolidvol_s1*aaconc_x_TON_s1 + x_sedsolidvol_d1*aaconc_x_TON_d1)/(x_sedsolidvol_s1+x_sedsolidvol_d1)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Need to find the max monthly average value of the benthic larvae
finaly_benthslar<-benthslar[((nyears-1)*360+1):ndays]
finaly_benthclar<-benthclar[((nyears-1)*360+1):ndays]
ma_benthslar<-rep(0,12)
ma_benthclar<-rep(0,12)
for(jj in 1:12){
ma_benthslar[jj]<-mean(finaly_benthslar[(((jj-1)*30)+1) : (jj*30)])
ma_benthclar[jj]<-mean(finaly_benthclar[(((jj-1)*30)+1) : (jj*30)])
}
maconc_benthslar<-max(ma_benthslar)/(xs_volume+xd_volume)
maconc_benthclar<-max(ma_benthclar)/(xs_volume+xd_volume)
#Here the units are mMN/m3 to correspond with the CPR data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Winter and summer average nutrient concentartions in the water column
NDJF_start <- ((nyears-1)*360+1)-60
NDJF_end   <- ((nyears-1)*360+1)+60

MJJA_start <- ((nyears-1)*360+1)+120
MJJA_end   <- ndays-120


waconc_nitrate_s <- (mean(s_nitrate[NDJF_start : NDJF_end]))/xs_volume
waconc_ammonia_s <- (mean(s_ammonia[NDJF_start : NDJF_end]))/xs_volume

saconc_nitrate_s <- (mean(s_nitrate[MJJA_start : MJJA_end]))/xs_volume
saconc_ammonia_s <- (mean(s_ammonia[MJJA_start : MJJA_end]))/xs_volume

waconc_nitrate_d <- (mean(nitrate_d[NDJF_start : NDJF_end]))/xd_volume
waconc_ammonia_d <- (mean(ammonia_d[NDJF_start : NDJF_end]))/xd_volume

saconc_nitrate_d <- (mean(nitrate_d[MJJA_start : MJJA_end]))/xd_volume
saconc_ammonia_d <- (mean(ammonia_d[MJJA_start : MJJA_end]))/xd_volume

#~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Various feeding fluxes...

herb_pfishlarflux<-fluxherb_pfishlar[ndays]-fluxherb_pfishlar[((nyears-1)*360+1)]
herb_dfishlarflux<-fluxherb_dfishlar[ndays]-fluxherb_dfishlar[((nyears-1)*360+1)]
herb_pfishflux<-fluxherb_pfish[ndays]-fluxherb_pfish[((nyears-1)*360+1)]

mzoofishflux<-herb_pfishlarflux+herb_dfishlarflux+herb_pfishflux

herb_carnflux<-fluxherb_carn[ndays]-fluxherb_carn[((nyears-1)*360+1)]

pfishlar_pfishflux<-fluxpfishlar_pfish[ndays]-fluxpfishlar_pfish[((nyears-1)*360+1)]
pfishlar_dfishflux<-fluxpfishlar_dfish[ndays]-fluxpfishlar_dfish[((nyears-1)*360+1)]
dfishlar_pfishflux<-fluxdfishlar_pfish[ndays]-fluxdfishlar_pfish[((nyears-1)*360+1)]
dfishlar_dfishflux<-fluxdfishlar_dfish[ndays]-fluxdfishlar_dfish[((nyears-1)*360+1)]
pfish_dfishflux<-fluxpfish_dfish[ndays]-fluxpfish_dfish[((nyears-1)*360+1)]
dfish_dfishflux<-fluxdfish_dfish[ndays]-fluxdfish_dfish[((nyears-1)*360+1)]
disc_dfishflux<-fluxdisc_dfish[ndays]-fluxdisc_dfish[((nyears-1)*360+1)]

pfishfishflux<-pfishlar_pfishflux+pfishlar_dfishflux+pfish_dfishflux
dfishfishflux<-dfishlar_pfishflux+dfishlar_dfishflux+dfish_dfishflux

benths_dfishflux<-fluxbenths_dfish[ndays]-fluxbenths_dfish[((nyears-1)*360+1)]
benthc_dfishflux<-fluxbenthc_dfish[ndays]-fluxbenthc_dfish[((nyears-1)*360+1)]
benfishflux<-benths_dfishflux+benthc_dfishflux


corp_birdflux<-fluxcorp_bird[ndays]-fluxcorp_bird[((nyears-1)*360+1)]
carn_birdflux<-fluxcarn_bird[ndays]-fluxcarn_bird[((nyears-1)*360+1)]
benths_birdflux<-fluxbenths_bird[ndays]-fluxbenths_bird[((nyears-1)*360+1)]
benthc_birdflux<-fluxbenthc_bird[ndays]-fluxbenthc_bird[((nyears-1)*360+1)]
pfish_birdflux<-fluxpfish_bird[ndays]-fluxpfish_bird[((nyears-1)*360+1)]
dfish_birdflux<-fluxdfish_bird[ndays]-fluxdfish_bird[((nyears-1)*360+1)]
mfish_birdflux<-fluxmfish_bird[ndays]-fluxmfish_bird[((nyears-1)*360+1)]
disc_birdflux<-fluxdisc_bird[ndays]-fluxdisc_bird[((nyears-1)*360+1)]
total_birdflux<-  pfish_birdflux + dfish_birdflux + mfish_birdflux + disc_birdflux +
                + corp_birdflux + carn_birdflux + benths_birdflux + benthc_birdflux


corp_sealflux <- fluxcorp_seal[ndays]-fluxcorp_seal[((nyears-1)*360+1)]
disc_sealflux <-fluxdisc_seal[ndays]-fluxdisc_seal[((nyears-1)*360+1)]
carn_sealflux <- fluxcarn_seal[ndays]-fluxcarn_seal[((nyears-1)*360+1)]
benths_sealflux <- fluxbenths_seal[ndays]-fluxbenths_seal[((nyears-1)*360+1)]
benthc_sealflux <- fluxbenthc_seal[ndays]-fluxbenthc_seal[((nyears-1)*360+1)]
bird_sealflux <- fluxbird_seal[ndays]-fluxbird_seal[((nyears-1)*360+1)]
pfish_sealflux<-fluxpfish_seal[ndays]-fluxpfish_seal[((nyears-1)*360+1)]
dfish_sealflux<-fluxdfish_seal[ndays]-fluxdfish_seal[((nyears-1)*360+1)]
mfish_sealflux<-fluxmfish_seal[ndays]-fluxmfish_seal[((nyears-1)*360+1)]
total_sealflux<-  pfish_sealflux + dfish_sealflux + mfish_sealflux +
                + corp_sealflux + disc_sealflux + carn_sealflux + bird_sealflux +
                + benths_sealflux + benthc_sealflux


disc_cetaflux<-fluxherb_ceta[ndays]-fluxherb_ceta[((nyears-1)*360+1)]
herb_cetaflux<-fluxcarn_ceta[ndays]-fluxcarn_ceta[((nyears-1)*360+1)]
carn_cetaflux<-fluxdisc_ceta[ndays]-fluxdisc_ceta[((nyears-1)*360+1)]
pfish_cetaflux<-fluxpfish_ceta[ndays]-fluxpfish_ceta[((nyears-1)*360+1)]
dfish_cetaflux<-fluxdfish_ceta[ndays]-fluxdfish_ceta[((nyears-1)*360+1)]
mfish_cetaflux<-fluxmfish_ceta[ndays]-fluxmfish_ceta[((nyears-1)*360+1)]
benths_cetaflux<-fluxbenths_ceta[ndays]-fluxbenths_ceta[((nyears-1)*360+1)]
benthc_cetaflux<-fluxbenthc_ceta[ndays]-fluxbenthc_ceta[((nyears-1)*360+1)]
bird_cetaflux<-fluxbird_ceta[ndays]-fluxbird_ceta[((nyears-1)*360+1)]
seal_cetaflux<-fluxseal_ceta[ndays]-fluxseal_ceta[((nyears-1)*360+1)]
total_cetaflux<-   pfish_cetaflux + dfish_cetaflux + mfish_cetaflux +
                 + herb_cetaflux + carn_cetaflux + disc_cetaflux +
                 + benths_cetaflux + benthc_cetaflux + bird_cetaflux + seal_cetaflux 


#Landings, discards and bycatch

FishpLand<-landp[ndays]-landp[((nyears-1)*360+1)]
FishdLand<-landd[ndays]-landd[((nyears-1)*360+1)]
FishmLand<-landm[ndays]-landm[((nyears-1)*360+1)]

FishbsLand<-(landsb[ndays]-landsb[((nyears-1)*360+1)])
FishbcLand<-(landcb[ndays]-landcb[((nyears-1)*360+1)])

FishzcLand<-(landcz[ndays]-landcz[((nyears-1)*360+1)])

FishkpLand<-(landkp_i[ndays]-landkp_i[((nyears-1)*360+1)])

FishdDiscard<-discdem[ndays]-discdem[((nyears-1)*360+1)]

BDDiscard<-discbd[ndays]-discbd[((nyears-1)*360+1)]

SLDiscard<-discsl[ndays]-discsl[((nyears-1)*360+1)]

CTDiscard<-discct[ndays]-discct[((nyears-1)*360+1)]

FishctLand<-landct[ndays]-landct[((nyears-1)*360+1)]


#Some other geochemical fluxes

WCDenitrif<-(wcdenitrif[ndays]-wcdenitrif[((nyears-1)*360+1)])
SedDenitrif<-(seddenitrif[ndays]-seddenitrif[((nyears-1)*360+1)])
Denitrif<-WCDenitrif+SedDenitrif


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Now drop all the derived values into the correct rows in the target dataframe
#-----------------------------------------------------------------------------

opt_results$Model_data[which(opt_results$Name=="Obs_TAPP")] <- TAPP

opt_results$Model_data[which(opt_results$Name=="Obs_NP")]  <- NP

opt_results$Model_data[which(opt_results$Name=="Obs_KelpP")] <- Kelp_netCP      # NET production

opt_results$Model_data[which(opt_results$Name=="Obs_OmnizooP")] <- OmnizoogrossP 

opt_results$Model_data[which(opt_results$Name=="Obs_CarnzooP")] <- CarnzoogrossP

opt_results$Model_data[which(opt_results$Name=="Obs_PFishP")]  <- PFishgrossP+PFishlargrossP

opt_results$Model_data[which(opt_results$Name=="Obs_DFishP")]  <- DFishgrossP+DFishlargrossP

opt_results$Model_data[which(opt_results$Name=="Obs_BensuspP")] <- BenthsgrossP

opt_results$Model_data[which(opt_results$Name=="Obs_BencarnP")] <- BenthcgrossP

opt_results$Model_data[which(opt_results$Name=="Obs_birdP")] <- BirdnetP

opt_results$Model_data[which(opt_results$Name=="Obs_sealP")] <- SealnetP

opt_results$Model_data[which(opt_results$Name=="Obs_cetaP")] <- CetanetP

opt_results$Model_data[which(opt_results$Name=="Obs_maxbenthslar")] <- maconc_benthslar

opt_results$Model_data[which(opt_results$Name=="Obs_maxbenthclar")] <- maconc_benthclar

opt_results$Model_data[which(opt_results$Name=="Obs_Conpfishfish")] <- pfishfishflux

opt_results$Model_data[which(opt_results$Name=="Obs_Condfishfish")] <- dfishfishflux

opt_results$Model_data[which(opt_results$Name=="Obs_Conzoofish")] <- mzoofishflux

opt_results$Model_data[which(opt_results$Name=="Obs_Conzoocarnz")] <- herb_carnflux

opt_results$Model_data[which(opt_results$Name=="Obs_Conbenfish")] <- benfishflux


opt_results$Model_data[which(opt_results$Name=="Obs_Contotal_bird")] <- total_birdflux

opt_results$Model_data[which(opt_results$Name=="Obs_Proppfishbird")] <- pfish_birdflux/total_birdflux

opt_results$Model_data[which(opt_results$Name=="Obs_Propdfishbird")] <- dfish_birdflux/total_birdflux

opt_results$Model_data[which(opt_results$Name=="Obs_Propmfishbird")] <- mfish_birdflux/total_birdflux

opt_results$Model_data[which(opt_results$Name=="Obs_Propdiscbird")]  <- disc_birdflux/total_birdflux




opt_results$Model_data[which(opt_results$Name=="Obs_Contotal_seal")] <- total_sealflux

opt_results$Model_data[which(opt_results$Name=="Obs_Proppfishseal")] <- pfish_sealflux/total_sealflux

opt_results$Model_data[which(opt_results$Name=="Obs_Propdfishseal")] <- dfish_sealflux/total_sealflux

opt_results$Model_data[which(opt_results$Name=="Obs_Propmfishseal")] <- mfish_sealflux/total_sealflux




opt_results$Model_data[which(opt_results$Name=="Obs_Contotal_ceta")] <-  total_cetaflux

opt_results$Model_data[which(opt_results$Name=="Obs_Proppfishceta")] <- pfish_cetaflux/total_cetaflux

opt_results$Model_data[which(opt_results$Name=="Obs_Propdfishceta")] <- dfish_cetaflux/total_cetaflux

opt_results$Model_data[which(opt_results$Name=="Obs_Propmfishceta")] <- mfish_cetaflux/total_cetaflux


opt_results$Model_data[which(opt_results$Name=="Obs_Pland_livewt")] <- FishpLand

opt_results$Model_data[which(opt_results$Name=="Obs_Dland_livewt")] <- FishdLand

opt_results$Model_data[which(opt_results$Name=="Obs_Mland_livewt")] <- FishmLand

opt_results$Model_data[which(opt_results$Name=="Obs_Bsland_livewt")] <- FishbsLand

opt_results$Model_data[which(opt_results$Name=="Obs_Bcland_livewt")] <- FishbcLand

opt_results$Model_data[which(opt_results$Name=="Obs_Zcland_livewt")] <- FishzcLand

opt_results$Model_data[which(opt_results$Name=="Obs_Kland_livewt")] <- FishkpLand


opt_results$Model_data[which(opt_results$Name=="Obs_Ctland_livewt")] <-FishctLand

opt_results$Model_data[which(opt_results$Name=="Obs_kelp_pb")] <- kelpC_pb

opt_results$Model_data[which(opt_results$Name=="Obs_benslar_pb")]<-benslar_pb
opt_results$Model_data[which(opt_results$Name=="Obs_benclar_pb")]<-benclar_pb
opt_results$Model_data[which(opt_results$Name=="Obs_bens_pb")]<-bens_pb
opt_results$Model_data[which(opt_results$Name=="Obs_benc_pb")]<-benc_pb
opt_results$Model_data[which(opt_results$Name=="Obs_herb_pb")]<-herb_pb
opt_results$Model_data[which(opt_results$Name=="Obs_carn_pb")]<-carn_pb
opt_results$Model_data[which(opt_results$Name=="Obs_fishplar_pb")]<-pfishlar_pb
opt_results$Model_data[which(opt_results$Name=="Obs_fishdlar_pb")]<-dfishlar_pb
opt_results$Model_data[which(opt_results$Name=="Obs_fishp_pb")]<-pfish_pb
opt_results$Model_data[which(opt_results$Name=="Obs_fishd_pb")]<-dfish_pb
opt_results$Model_data[which(opt_results$Name=="Obs_fishm_pb")]<-mfish_pb
opt_results$Model_data[which(opt_results$Name=="Obs_bird_pb")]<-bird_pb
opt_results$Model_data[which(opt_results$Name=="Obs_seal_pb")]<-seal_pb
opt_results$Model_data[which(opt_results$Name=="Obs_ceta_pb")]<-ceta_pb

opt_results$Model_data[which(opt_results$Name=="Obs_exud_C_kelp")]<-prop_exud_C_kelp
opt_results$Model_data[which(opt_results$Name=="Obs_kelp_NC")]<-kelp_NC
opt_results$Model_data[which(opt_results$Name=="Obs_Denitrif")]<-Denitrif
opt_results$Model_data[which(opt_results$Name=="Obs_Dfdiscardp")]<-FishdDiscard/(FishdDiscard+FishdLand)





opt_results$Model_data[which(opt_results$Name=="Obs_s_x_ammonia")]<-aaconc_x_ammonia_s
opt_results$Model_data[which(opt_results$Name=="Obs_d_x_ammonia")]<-aaconc_x_ammonia_d
opt_results$Model_data[which(opt_results$Name=="Obs_s_x_nitrate")]<-aaconc_x_nitrate_s
opt_results$Model_data[which(opt_results$Name=="Obs_d_x_nitrate")]<-aaconc_x_nitrate_d
opt_results$Model_data[which(opt_results$Name=="Obs_s_x_TON")]<-aaconc_x_TON_s
opt_results$Model_data[which(opt_results$Name=="Obs_d_x_TON")]<-aaconc_x_TON_d
opt_results$Model_data[which(opt_results$Name=="Obs_NDJF_s_nitrate")]<-waconc_nitrate_s
opt_results$Model_data[which(opt_results$Name=="Obs_MJJA_s_nitrate")]<-saconc_nitrate_s
opt_results$Model_data[which(opt_results$Name=="Obs_NDJF_d_nitrate")]<-waconc_nitrate_d
opt_results$Model_data[which(opt_results$Name=="Obs_MJJA_d_nitrate")]<-saconc_nitrate_d
opt_results$Model_data[which(opt_results$Name=="Obs_NDJF_s_ammonia")]<-waconc_ammonia_s
opt_results$Model_data[which(opt_results$Name=="Obs_MJJA_s_ammonia")]<-saconc_ammonia_s
opt_results$Model_data[which(opt_results$Name=="Obs_NDJF_d_ammonia")]<-waconc_ammonia_d
opt_results$Model_data[which(opt_results$Name=="Obs_MJJA_d_ammonia")]<-saconc_ammonia_d





opt_results$Model_data[which(opt_results$Name=="Obs_carn_io_ratio")]<-aaconc_carn_i/aaconc_carn_o

opt_results$Model_data[which(opt_results$Name=="Obs_herb_io_ratio")]<-aaconc_herb_i/aaconc_herb_o

opt_results$Model_data[which(opt_results$Name=="Obs_phyt_io_ratio")]<-aaconc_phyt_si/aaconc_phyt_so

opt_results$Model_data[which(opt_results$Name=="Obs_nit_io_ratio")]<-aaconc_nitrate_si/aaconc_nitrate_so

opt_results$Model_data[which(opt_results$Name=="Obs_amm_io_ratio")]<-aaconc_ammonia_si/aaconc_ammonia_so

opt_results$Model_data[which(opt_results$Name=="Obs_pfish_io_ratio")]<-aaconc_fishp_i/aaconc_fishp_o

opt_results$Model_data[which(opt_results$Name=="Obs_dfish_io_ratio")]<-aaconc_fishd_i/aaconc_fishd_o




opt_results$Model_data[which(opt_results$Name=="Obs_birddisc")]<-BDDiscard
opt_results$Model_data[which(opt_results$Name=="Obs_sealdisc")]<-SLDiscard
opt_results$Model_data[which(opt_results$Name=="Obs_cetadisc")]<-CTDiscard

opt_results$Model_data[which(opt_results$Name=="Obs_kelp_beachcast")]<-kelp_beachcast_per_annProd

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#opt_results remains in memory and is available if this rioutine is followed by the 
#routine which calculates the likelihood
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Next, we need to prepare a simpler dataframe contining just the model results
#which is output as a csv file

target_results_output <- data.frame(opt_results[,3])
target_results_output[,2] <- opt_results[,6]
target_results_output[,3] <- opt_results[,7]

names(target_results_output)<-names(opt_results[,c(3,6,7)])


#Print the data to a csv file
#-----------------------------------------------------------------
filename = csvname(resultsdir, "model_target_annualresults", identifier)
writecsv(target_results_output, filename, row.names=FALSE)

#-------------------------------------------------------------------------------------------------------

	opt_results
}

