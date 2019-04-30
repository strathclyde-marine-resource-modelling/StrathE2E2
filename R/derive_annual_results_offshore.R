#
# derive_annual_results_offshore.R
#
#' derive set of results for offshore only and write to files
#'
#' @param model model object
#' @param results  model results
#'
#' @importFrom utils write.table
#'
#' @export
#
derive_annual_results_offshore <- function(model, results) {

	# Unpack:
	run		<- el(model, "run")
	ndays		<- el(run, "ndays")
	nyears		<- el(run, "nyears")
	AAA		<- el(run, "AAA")
	oudir		<- el(run, "oudir")

	data		<- el(model, "data")

	# extract physical parameters:
	physical.parms  <- el(data, "physical.parameters")

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
	output			<- el(results, "output")

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
	aggregates		<- el(results, "aggregates")

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


#Derive a load of  stuff from the output and write to a file



#Extract annual average biomass in the final year for the OFFSHORE model domain

aamass_s_detritus<-mean(detritus_so[((nyears-1)*360+1):ndays])
aamass_d_detritus<-mean(detritus_d[((nyears-1)*360+1):ndays])
aamass_x_detritus<-mean(x_detritus_o[((nyears-1)*360+1):ndays]) #Includes both labile and refractory detritus
aamass_xR_detritus<-mean(xR_detritus_d1[((nyears-1)*360+1):ndays]
                       + xR_detritus_d2[((nyears-1)*360+1):ndays]
                       + xR_detritus_d3[((nyears-1)*360+1):ndays])
aamass_discard<-mean(discard_o[((nyears-1)*360+1):ndays])
aamass_corpse<-mean(corpse_o[((nyears-1)*360+1):ndays])

#aamass_kelpdebris<-mean(kelpdebris[((nyears-1)*360+1):ndays])
aamass_kelpdebris<-NA

aamass_s_ammonia<-mean(ammonia_so[((nyears-1)*360+1):ndays])
aamass_d_ammonia<-mean(ammonia_d[((nyears-1)*360+1):ndays])
aamass_x_ammonia<-mean(x_ammonia_o[((nyears-1)*360+1):ndays])
aamass_s_nitrate<-mean(nitrate_so[((nyears-1)*360+1):ndays])
aamass_d_nitrate<-mean(nitrate_d[((nyears-1)*360+1):ndays])
aamass_x_nitrate<-mean(x_nitrate_o[((nyears-1)*360+1):ndays])

#aamass_kelpN<-mean(kelpN[((nyears-1)*360+1):ndays])
aamass_kelpN<-NA

aamass_s_phyt<-mean(phyt_so[((nyears-1)*360+1):ndays])
aamass_d_phyt<-mean(phyt_d[((nyears-1)*360+1):ndays])
aamass_herb<-mean(herb_o[((nyears-1)*360+1):ndays])
aamass_carn<-mean(carn_o[((nyears-1)*360+1):ndays])
aamass_benthslar<-mean(benthslar_o[((nyears-1)*360+1):ndays])
aamass_benths<-mean(benths_o[((nyears-1)*360+1):ndays])
aamass_benthclar<-mean(benthclar_o[((nyears-1)*360+1):ndays])
aamass_benthc<-mean(benthc_o[((nyears-1)*360+1):ndays])
aamass_fishplar<-mean(fishplar_o[((nyears-1)*360+1):ndays])
aamass_fishp<-mean(fishp_o[((nyears-1)*360+1):ndays])
aamass_fishm<-mean(fishm_o[((nyears-1)*360+1):ndays])
aamass_fishdlar<-mean(fishdlar_o[((nyears-1)*360+1):ndays])
aamass_fishd<-mean(fishd_o[((nyears-1)*360+1):ndays])
aamass_bird<-mean(bird_o[((nyears-1)*360+1):ndays])

aamass_seal<-mean(seal_o[((nyears-1)*360+1):ndays])
aamass_ceta<-mean(ceta_o[((nyears-1)*360+1):ndays])

aamass_totalN<-mean(totalN_o[((nyears-1)*360+1):ndays])

#.........................................

mass_results<-data.frame(rep(0,55))

mass_results[1,1]<-aamass_s_detritus
mass_results[2,1]<-aamass_d_detritus
mass_results[3,1]<-aamass_x_detritus
mass_results[4,1]<-aamass_xR_detritus
mass_results[5,1]<-aamass_discard
mass_results[6,1]<-aamass_corpse

mass_results[7,1]<-aamass_kelpdebris

mass_results[8,1]<-aamass_s_ammonia
mass_results[9,1]<-aamass_d_ammonia
mass_results[10,1]<-aamass_x_ammonia
mass_results[11,1]<-aamass_s_nitrate
mass_results[12,1]<-aamass_d_nitrate
mass_results[13,1]<-aamass_x_nitrate

mass_results[14,1]<-aamass_kelpN

mass_results[15,1]<-aamass_s_phyt
mass_results[16,1]<-aamass_d_phyt
mass_results[17,1]<-aamass_herb
mass_results[18,1]<-aamass_carn
mass_results[19,1]<-aamass_benthslar
mass_results[20,1]<-aamass_benths
mass_results[21,1]<-aamass_benthclar
mass_results[22,1]<-aamass_benthc
mass_results[23,1]<-aamass_fishplar
mass_results[24,1]<-aamass_fishp
mass_results[25,1]<-aamass_fishm
mass_results[26,1]<-aamass_fishdlar
mass_results[27,1]<-aamass_fishd
mass_results[28,1]<-aamass_bird

mass_results[29,1]<-aamass_seal
mass_results[30,1]<-aamass_ceta


mass_results[31,1]<-aamass_totalN

mass_results[32,1]<-x_shallowprop
mass_results[33,1]<-si_depth
mass_results[34,1]<-so_depth
mass_results[35,1]<-d_depth

mass_results[36,1]<-x_area_s0

mass_results[37,1]<-x_area_s1
mass_results[38,1]<-x_area_s2
mass_results[39,1]<-x_area_s3

mass_results[40,1]<-x_area_d0

mass_results[41,1]<-x_area_d1
mass_results[42,1]<-x_area_d2
mass_results[43,1]<-x_area_d3
mass_results[44,1]<-x_depth_s1
mass_results[45,1]<-x_depth_s2
mass_results[46,1]<-x_depth_s3
mass_results[47,1]<-x_depth_d1
mass_results[48,1]<-x_depth_d2
mass_results[49,1]<-x_depth_d3
mass_results[50,1]<-x_poros_s1
mass_results[51,1]<-x_poros_s2
mass_results[52,1]<-x_poros_s3
mass_results[53,1]<-x_poros_d1
mass_results[54,1]<-x_poros_d2
mass_results[55,1]<-x_poros_d3

mass_results[,2]<-"mMN_in_the_whole model_domain_(1m2)"

mass_results[c(32,36:43,50:55),2]<-"dimensionless"
mass_results[c(33:35,44:49),2]<-"m"

mass_results[,3]<-c(
"Surface_layer_detritus",
"Deep_layer_detritus",
"Sediment_labile_plus_refractory_detritus",
"Sediment_refractory_detritus",
"Fishery_discards",
"Corpses",
"Kelp_debris",
"Surface_layer_ammonia",
"Deep_layer_ammonia",
"Sediment_porewater_ammonia",
"Surface_layer_nitrate",
"Deep_layer_nitrate",
"Sediment_porewater_nitrate",
"Kelp_nitrogen",
"Surface_layer_phytoplankton",
"Deep_layer_phytoplankton",
"Omnivorous_zooplankton",
"Carnivorous_zooplankton",
"Benthos_susp/dep_feeders_larvae",
"Benthos_susp/dep_feeders",
"Benthos_carn/scav_feeders_larvae",
"Benthos_carn/scav_feeders",
"Planktivorous_fish_larvae",
"Planktivorous_fish",
"Migratory_fish",
"Demersal_fish_larvae",
"Demersal_fish",
"Bird",
"Seals",
"Cetaceans",
"Total_nitrogen_mass",
"Area_proportion_of_inshore_zone",
"Thickness_of_inshore_surface_layer",
"Thickness_of_offshore_surface_layer",
"Thickness_of_offshore_deep_layer",
"Area_proportion_inshore_rock",
"Area_proportion_inshore_sediment_s1",
"Area_proportion_inshore_sediment_s2",
"Area_proportion_inshore_sediment_s3",
"Area_proportion_offshore_rock",
"Area_proportion_offshore_sediment_d1",
"Area_proportion_offshore_sediment_d2",
"Area_proportion_offshore_sediment_d3",
"Thickness_of_inshore_sediment_layer_s1",
"Thickness_of_inshore_sediment_layer_s2",
"Thickness_of_inshore_sediment_layer_s3",
"Thickness_of_offshore_sediment_layer_d1",
"Thickness_of_offshore_sediment_layer_d2",
"Thickness_of_offshore_sediment_layer_d3",
"Porosity_of_inshore_sediment_layer_s1",
"Porosity_of_inshore_sediment_layer_s2",
"Porosity_of_inshore_sediment_layer_s3",
"Porosity_of_offshore_sediment_layer_d1",
"Porosity_of_offshore_sediment_layer_d2",
"Porosity_of_offshore_sediment_layer_d3")



colnames(mass_results)<-c("Model_annual_mean","Units","Description")

mass_results


#Print the data to a csv file
#-----------------------------------------------------------------
write.table(mass_results,file=paste(oudir,"OFFSHORE_model_anav_biomass","-",AAA,".csv",sep=""),sep=",",row.names=FALSE)

#-------------------------------------------------------------------------------------------------------

#copy the mass_results dataframe as a template for the annual maximum data
maxmass_results<-mass_results
maxmass_results[1:31,1]<-0


aamaxmass_s_detritus<-max(detritus_so[((nyears-1)*360+1):ndays])
aamaxmass_d_detritus<-max(detritus_d[((nyears-1)*360+1):ndays])
aamaxmass_x_detritus<-max(x_detritus_o[((nyears-1)*360+1):ndays]) #Includes both labile and refractory detritus
aamaxmass_xR_detritus<-max(xR_detritus_d1[((nyears-1)*360+1):ndays]
                       + xR_detritus_d2[((nyears-1)*360+1):ndays]
                       + xR_detritus_d3[((nyears-1)*360+1):ndays])
aamaxmass_discard<-max(discard_o[((nyears-1)*360+1):ndays])
aamaxmass_corpse<-max(corpse_o[((nyears-1)*360+1):ndays])

#aamaxmass_kelpdebris<-max(kelpdebris[((nyears-1)*360+1):ndays])
aamaxmass_kelpdebris<-NA

aamaxmass_s_ammonia<-max(ammonia_so[((nyears-1)*360+1):ndays])
aamaxmass_d_ammonia<-max(ammonia_d[((nyears-1)*360+1):ndays])
aamaxmass_x_ammonia<-max(x_ammonia_o[((nyears-1)*360+1):ndays])
aamaxmass_s_nitrate<-max(nitrate_so[((nyears-1)*360+1):ndays])
aamaxmass_d_nitrate<-max(nitrate_d[((nyears-1)*360+1):ndays])
aamaxmass_x_nitrate<-max(x_nitrate_o[((nyears-1)*360+1):ndays])

#aamaxmass_kelpN<-max(kelpN[((nyears-1)*360+1):ndays])
aamaxmass_kelpN<-NA

aamaxmass_s_phyt<-max(phyt_so[((nyears-1)*360+1):ndays])
aamaxmass_d_phyt<-max(phyt_d[((nyears-1)*360+1):ndays])
aamaxmass_herb<-max(herb_o[((nyears-1)*360+1):ndays])
aamaxmass_carn<-max(carn_o[((nyears-1)*360+1):ndays])
aamaxmass_benthslar<-max(benthslar_o[((nyears-1)*360+1):ndays])
aamaxmass_benths<-max(benths_o[((nyears-1)*360+1):ndays])
aamaxmass_benthclar<-max(benthclar_o[((nyears-1)*360+1):ndays])
aamaxmass_benthc<-max(benthc_o[((nyears-1)*360+1):ndays])
aamaxmass_fishplar<-max(fishplar_o[((nyears-1)*360+1):ndays])
aamaxmass_fishp<-max(fishp_o[((nyears-1)*360+1):ndays])
aamaxmass_fishm<-max(fishm_o[((nyears-1)*360+1):ndays])
aamaxmass_fishdlar<-max(fishdlar_o[((nyears-1)*360+1):ndays])
aamaxmass_fishd<-max(fishd_o[((nyears-1)*360+1):ndays])
aamaxmass_bird<-max(bird_o[((nyears-1)*360+1):ndays])

aamaxmass_seal<-max(seal_o[((nyears-1)*360+1):ndays])
aamaxmass_ceta<-max(ceta_o[((nyears-1)*360+1):ndays])

aamaxmass_totalN<-max(totalN_o[((nyears-1)*360+1):ndays])


#..........................................

maxmass_results[1,1]<-aamaxmass_s_detritus
maxmass_results[2,1]<-aamaxmass_d_detritus
maxmass_results[3,1]<-aamaxmass_x_detritus
maxmass_results[4,1]<-aamaxmass_xR_detritus
maxmass_results[5,1]<-aamaxmass_discard
maxmass_results[6,1]<-aamaxmass_corpse

maxmass_results[7,1]<-aamaxmass_kelpdebris

maxmass_results[8,1]<-aamaxmass_s_ammonia
maxmass_results[9,1]<-aamaxmass_d_ammonia
maxmass_results[10,1]<-aamaxmass_x_ammonia
maxmass_results[11,1]<-aamaxmass_s_nitrate
maxmass_results[12,1]<-aamaxmass_d_nitrate
maxmass_results[13,1]<-aamaxmass_x_nitrate

maxmass_results[14,1]<-aamaxmass_kelpN

maxmass_results[15,1]<-aamaxmass_s_phyt
maxmass_results[16,1]<-aamaxmass_d_phyt
maxmass_results[17,1]<-aamaxmass_herb
maxmass_results[18,1]<-aamaxmass_carn
maxmass_results[19,1]<-aamaxmass_benthslar
maxmass_results[20,1]<-aamaxmass_benths
maxmass_results[21,1]<-aamaxmass_benthclar
maxmass_results[22,1]<-aamaxmass_benthc
maxmass_results[23,1]<-aamaxmass_fishplar
maxmass_results[24,1]<-aamaxmass_fishp
maxmass_results[25,1]<-aamaxmass_fishm
maxmass_results[26,1]<-aamaxmass_fishdlar
maxmass_results[27,1]<-aamaxmass_fishd
maxmass_results[28,1]<-aamaxmass_bird

maxmass_results[29,1]<-aamaxmass_seal
maxmass_results[30,1]<-aamaxmass_ceta


maxmass_results[31,1]<-aamaxmass_totalN


colnames(maxmass_results)<-c("Model_annual_maximum","Units","Description")


maxmass_results


#Print the data to a csv file
#-----------------------------------------------------------------
write.table(maxmass_results,file=paste(oudir,"OFFSHORE_model_maximum_biomass","-",AAA,".csv",sep=""),sep=",",row.names=FALSE)

#-------------------------------------------------------------------------------------------------------


#copy the mass_results dataframe as a template for the annual minimum data
minmass_results<-mass_results
minmass_results[1:31,1]<-0


aaminmass_s_detritus<-min(detritus_so[((nyears-1)*360+1):ndays])
aaminmass_d_detritus<-min(detritus_d[((nyears-1)*360+1):ndays])
aaminmass_x_detritus<-min(x_detritus_o[((nyears-1)*360+1):ndays]) #Includes both labile and refractory detritus
aaminmass_xR_detritus<-min(xR_detritus_d1[((nyears-1)*360+1):ndays]
                       + xR_detritus_d2[((nyears-1)*360+1):ndays]
                       + xR_detritus_d3[((nyears-1)*360+1):ndays])
aaminmass_discard<-min(discard_o[((nyears-1)*360+1):ndays])
aaminmass_corpse<-min(corpse_o[((nyears-1)*360+1):ndays])

#aaminmass_kelpdebris<-min(kelpdebris[((nyears-1)*360+1):ndays])
aaminmass_kelpdebris<-NA

aaminmass_s_ammonia<-min(ammonia_so[((nyears-1)*360+1):ndays])
aaminmass_d_ammonia<-min(ammonia_d[((nyears-1)*360+1):ndays])
aaminmass_x_ammonia<-min(x_ammonia_o[((nyears-1)*360+1):ndays])
aaminmass_s_nitrate<-min(nitrate_so[((nyears-1)*360+1):ndays])
aaminmass_d_nitrate<-min(nitrate_d[((nyears-1)*360+1):ndays])
aaminmass_x_nitrate<-min(x_nitrate_o[((nyears-1)*360+1):ndays])

#aaminmass_kelpN<-min(kelpN[((nyears-1)*360+1):ndays])
aaminmass_kelpN<-NA

aaminmass_s_phyt<-min(phyt_so[((nyears-1)*360+1):ndays])
aaminmass_d_phyt<-min(phyt_d[((nyears-1)*360+1):ndays])
aaminmass_herb<-min(herb_o[((nyears-1)*360+1):ndays])
aaminmass_carn<-min(carn_o[((nyears-1)*360+1):ndays])
aaminmass_benthslar<-min(benthslar_o[((nyears-1)*360+1):ndays])
aaminmass_benths<-min(benths_o[((nyears-1)*360+1):ndays])
aaminmass_benthclar<-min(benthclar_o[((nyears-1)*360+1):ndays])
aaminmass_benthc<-min(benthc_o[((nyears-1)*360+1):ndays])
aaminmass_fishplar<-min(fishplar_o[((nyears-1)*360+1):ndays])
aaminmass_fishp<-min(fishp_o[((nyears-1)*360+1):ndays])
aaminmass_fishm<-min(fishm_o[((nyears-1)*360+1):ndays])
aaminmass_fishdlar<-min(fishdlar_o[((nyears-1)*360+1):ndays])
aaminmass_fishd<-min(fishd_o[((nyears-1)*360+1):ndays])
aaminmass_bird<-min(bird_o[((nyears-1)*360+1):ndays])

aaminmass_seal<-min(seal_o[((nyears-1)*360+1):ndays])
aaminmass_ceta<-min(ceta_o[((nyears-1)*360+1):ndays])

aaminmass_totalN<-min(totalN_o[((nyears-1)*360+1):ndays])


#..........................................

minmass_results[1,1]<-aaminmass_s_detritus
minmass_results[2,1]<-aaminmass_d_detritus
minmass_results[3,1]<-aaminmass_x_detritus
minmass_results[4,1]<-aaminmass_xR_detritus
minmass_results[5,1]<-aaminmass_discard
minmass_results[6,1]<-aaminmass_corpse

minmass_results[7,1]<-aaminmass_kelpdebris

minmass_results[8,1]<-aaminmass_s_ammonia
minmass_results[9,1]<-aaminmass_d_ammonia
minmass_results[10,1]<-aaminmass_x_ammonia
minmass_results[11,1]<-aaminmass_s_nitrate
minmass_results[12,1]<-aaminmass_d_nitrate
minmass_results[13,1]<-aaminmass_x_nitrate

minmass_results[14,1]<-aaminmass_kelpN

minmass_results[15,1]<-aaminmass_s_phyt
minmass_results[16,1]<-aaminmass_d_phyt
minmass_results[17,1]<-aaminmass_herb
minmass_results[18,1]<-aaminmass_carn
minmass_results[19,1]<-aaminmass_benthslar
minmass_results[20,1]<-aaminmass_benths
minmass_results[21,1]<-aaminmass_benthclar
minmass_results[22,1]<-aaminmass_benthc
minmass_results[23,1]<-aaminmass_fishplar
minmass_results[24,1]<-aaminmass_fishp
minmass_results[25,1]<-aaminmass_fishm
minmass_results[26,1]<-aaminmass_fishdlar
minmass_results[27,1]<-aaminmass_fishd
minmass_results[28,1]<-aaminmass_bird

minmass_results[29,1]<-aaminmass_seal
minmass_results[30,1]<-aaminmass_ceta


minmass_results[31,1]<-aaminmass_totalN


colnames(minmass_results)<-c("Model_annual_minimum","Units","Description")


minmass_results


#Print the data to a csv file
#-----------------------------------------------------------------
write.table(minmass_results,file=paste(oudir,"OFFSHORE_model_minimum_biomass","-",AAA,".csv",sep=""),sep=",",row.names=FALSE)

#-------------------------------------------------------------------------------------------------------





#Extract all of the derived rate variables for the final year


DINinflow<-(fluxAMMinflow_o[ndays]-fluxAMMinflow_o[((nyears-1)*360+1)]) + (fluxNITinflow_o[ndays]-fluxNITinflow_o[((nyears-1)*360+1)])
DINoutflow<-(fluxAMMoutflow_o[ndays]-fluxAMMoutflow_o[((nyears-1)*360+1)]) + (fluxNIToutflow_o[ndays]-fluxNIToutflow_o[((nyears-1)*360+1)]) 
PARTinflow<-(fluxPHYTinflow_o[ndays]-fluxPHYTinflow_o[((nyears-1)*360+1)]) + (fluxDETinflow_o[ndays]-fluxDETinflow_o[((nyears-1)*360+1)])
PARToutflow<-(fluxPHYToutflow_o[ndays]-fluxPHYToutflow_o[((nyears-1)*360+1)]) + (fluxDEToutflow_o[ndays]-fluxDEToutflow_o[((nyears-1)*360+1)])
atmosphereDINinput<-(atmosAMMinput_o[ndays]-atmosAMMinput_o[((nyears-1)*360+1)]) + (atmosNITinput_o[ndays]-atmosNITinput_o[((nyears-1)*360+1)])

#riverDINinflow<-(rivAMMinflow[ndays]-rivAMMinflow[((nyears-1)*360+1)]) + (rivNITinflow[ndays]-rivNITinflow[((nyears-1)*360+1)])
#riverPARTinflow<-rivPARTinflow[ndays]-rivPARTinflow[((nyears-1)*360+1)]
riverDINinflow<-0
riverPARTinflow<-0
#!!!!!!

sumDINinflow<-(fluxAMMinflow_o[ndays-90]-fluxAMMinflow_o[((nyears-1)*360+1+89)]) + (fluxNITinflow_o[ndays-90]-fluxNITinflow_o[((nyears-1)*360+1+89)])
sumDINoutflow<-(fluxAMMoutflow_o[ndays-90]-fluxAMMoutflow_o[((nyears-1)*360+1+89)]) + (fluxNIToutflow_o[ndays-90]-fluxNIToutflow_o[((nyears-1)*360+1+89)])
sumPARTinflow<-(fluxPHYTinflow_o[ndays-90]-fluxPHYTinflow_o[((nyears-1)*360+1+89)]) + (fluxDETinflow_o[ndays-90]-fluxDETinflow_o[((nyears-1)*360+1+89)]) 
sumPARToutflow<-(fluxPHYToutflow_o[ndays-90]-fluxPHYToutflow_o[((nyears-1)*360+1+89)]) + (fluxDEToutflow_o[ndays-90]-fluxDEToutflow_o[((nyears-1)*360+1+89)])

#sumriverDINinflow<-(rivAMMinflow[ndays-90]-rivAMMinflow[((nyears-1)*360+1+89)]) + (rivNITinflow[ndays-90]-rivNITinflow[((nyears-1)*360+1+89)])
sumriverDINinflow<-0

sumatmosDINinput<-(atmosAMMinput_o[ndays-90]-atmosAMMinput_o[((nyears-1)*360+1+89)]) + (atmosNITinput_o[ndays-90]-atmosNITinput_o[((nyears-1)*360+1+89)])
surfvertnitflux<-vertnitflux[ndays]-vertnitflux[((nyears-1)*360+1)]
#surfvertnitflux<-NA
#surfhoriznitflux<-horiznitflux[ndays]-horiznitflux[((nyears-1)*360+1)]
surfhoriznitflux<-NA


Flux_sedboundary <- (fluxsedboundary_o[ndays]-fluxsedboundary_o[((nyears-1)*360+1)]) 

#kelp_beachcast<-((fluxkelpdebris_beachexport[ndays]-fluxkelpdebris_beachexport[((nyears-1)*360+1)]))   
kelp_beachcast<-NA

DIN_NET_flux_o_i   <-  DIN_NET_flux_o_i[ndays]-DIN_NET_flux_o_i[((nyears-1)*360+1)]
PART_NET_flux_o_i  <-  PART_NET_flux_o_i[ndays]-PART_NET_flux_o_i[((nyears-1)*360+1)]
NET_activemigpelfish_o_i  <-  NET_activemigpelfish_o_i[ndays]-NET_activemigpelfish_o_i[((nyears-1)*360+1)]

NET_activemigmigfish_o_i  <-  NET_activemigmigfish_o_i[ndays]-NET_activemigmigfish_o_i[((nyears-1)*360+1)]
NET_activemigdemfish_o_i  <-  NET_activemigdemfish_o_i[ndays]-NET_activemigdemfish_o_i[((nyears-1)*360+1)]
NET_activemigbird_o_i  <-  NET_activemigbird_o_i[ndays]-NET_activemigbird_o_i[((nyears-1)*360+1)]
NET_activemigseal_o_i  <-  NET_activemigseal_o_i[ndays]-NET_activemigseal_o_i[((nyears-1)*360+1)]
NET_activemigceta_o_i  <-  NET_activemigceta_o_i[ndays]-NET_activemigceta_o_i[((nyears-1)*360+1)]

NET_mfish_ext_o    <-  NET_mfish_ext_o[ndays]-NET_mfish_ext_o[((nyears-1)*360+1)]

Mfish_annual_imig <-    mfish_imigration[ndays]-mfish_imigration[((nyears-1)*360+1)]
Mfish_annual_emig  <-   mfish_emigration[ndays]-mfish_emigration[((nyears-1)*360+1)]

NETPrimaryP<-netpprod_o[ndays]-netpprod_o[((nyears-1)*360+1)]
MMP<-(max(nitrate_so[((nyears-1)*360+1):ndays]+nitrate_d[((nyears-1)*360+1):ndays])) - (min(nitrate_so[((nyears-1)*360+1):ndays]+nitrate_d[((nyears-1)*360+1):ndays]))
PNP<-PNP_o[ndays]-PNP_o[((nyears-1)*360+1)]
PhytNitUp<-fluxwcnit_phyt_o[ndays]-fluxwcnit_phyt_o[((nyears-1)*360+1)]
PhytAmmUp<-fluxwcamm_phyt_o[ndays]-fluxwcamm_phyt_o[((nyears-1)*360+1)]
NewP<-MMP+sumriverDINinflow+sumatmosDINinput+sumDINinflow-sumDINoutflow
fratio<-NewP/NETPrimaryP
Tfratio<-PhytNitUp/(PhytNitUp+PhytAmmUp)

#KelpNitUp<-fluxwcnit_kelp[ndays]-fluxwcnit_kelp[((nyears-1)*360+1)]
#KelpAmmUp<-fluxwcamm_kelp[ndays]-fluxwcamm_kelp[((nyears-1)*360+1)]
KelpNitUp<-NA
KelpAmmUp<-NA


#KelpNprod       <-   kelpNprod_i[ndays]      -     kelpNprod_i[((nyears-1)*360+1)]
KelpNprod       <-   NA

Phytgrossprod        <-   phytgrossprod_o[ndays]      -     phytgrossprod_o[((nyears-1)*360+1)]

Herbgrossprod        <-   herbgrossprod_o[ndays]      -     herbgrossprod_o[((nyears-1)*360+1)]
Carngrossprod        <-   carngrossprod_o[ndays]      -     carngrossprod_o[((nyears-1)*360+1)]
Fishplargrossprod    <-   pfishlargrossprod_o[ndays]  -     pfishlargrossprod_o[((nyears-1)*360+1)]
Fishdlargrossprod    <-   dfishlargrossprod_o[ndays]  -     dfishlargrossprod_o[((nyears-1)*360+1)]
Fishpgrossprod       <-   pfishgrossprod_o[ndays]     -     pfishgrossprod_o[((nyears-1)*360+1)]
Fishmgrossprod       <-   mfishgrossprod_o[ndays]     -     mfishgrossprod_o[((nyears-1)*360+1)]
Fishdgrossprod       <-   dfishgrossprod_o[ndays]     -     dfishgrossprod_o[((nyears-1)*360+1)]
Benthslargrossprod   <-   benthslargrossprod_o[ndays] -     benthslargrossprod_o[((nyears-1)*360+1)]
Benthclargrossprod   <-   benthclargrossprod_o[ndays] -     benthclargrossprod_o[((nyears-1)*360+1)]
Benthsgrossprod      <-   benthsgrossprod_o[ndays]    -     benthsgrossprod_o[((nyears-1)*360+1)]
Benthcgrossprod      <-   benthcgrossprod_o[ndays]    -     benthcgrossprod_o[((nyears-1)*360+1)]
Birdgrossprod        <-   birdgrossprod_o[ndays]      -     birdgrossprod_o[((nyears-1)*360+1)]
Sealgrossprod        <-   sealgrossprod_o[ndays]      -     sealgrossprod_o[((nyears-1)*360+1)]
Cetagrossprod        <-   cetagrossprod_o[ndays]      -     cetagrossprod_o[((nyears-1)*360+1)]

Herbnetprod        <-   herbnetprod_o[ndays]      -     herbnetprod_o[((nyears-1)*360+1)]
Carnnetprod        <-   carnnetprod_o[ndays]      -     carnnetprod_o[((nyears-1)*360+1)]
Fishplarnetprod    <-   pfishlarnetprod_o[ndays]  -     pfishlarnetprod_o[((nyears-1)*360+1)]
Fishdlarnetprod    <-   dfishlarnetprod_o[ndays]  -     dfishlarnetprod_o[((nyears-1)*360+1)]
Fishpnetprod       <-   pfishnetprod_o[ndays]     -     pfishnetprod_o[((nyears-1)*360+1)]
Fishmnetprod       <-   mfishnetprod_o[ndays]     -     mfishnetprod_o[((nyears-1)*360+1)]
Fishdnetprod       <-   dfishnetprod_o[ndays]     -     dfishnetprod_o[((nyears-1)*360+1)]
Benthslarnetprod   <-   benthslarnetprod_o[ndays] -     benthslarnetprod_o[((nyears-1)*360+1)]
Benthclarnetprod   <-   benthclarnetprod_o[ndays] -     benthclarnetprod_o[((nyears-1)*360+1)]
Benthsnetprod      <-   benthsnetprod_o[ndays]    -     benthsnetprod_o[((nyears-1)*360+1)]
Benthcnetprod      <-   benthcnetprod_o[ndays]    -     benthcnetprod_o[((nyears-1)*360+1)]
Birdnetprod        <-   birdnetprod_o[ndays]      -     birdnetprod_o[((nyears-1)*360+1)]
Sealnetprod        <-   sealnetprod_o[ndays]      -     sealnetprod_o[((nyears-1)*360+1)]
Cetanetprod        <-   cetanetprod_o[ndays]      -     cetanetprod_o[((nyears-1)*360+1)]


#WCdetritusprod      <-   fluxcorp_wcdet[ndays]      -     fluxcorp_wcdet[((nyears-1)*360+1)] +
#                        +fluxkelpdebris_wcdet[ndays]      -     fluxkelpdebris_wcdet[((nyears-1)*360+1)] +
#                        +fluxphyt_wcdet[ndays]      -     fluxphyt_wcdet[((nyears-1)*360+1)] +
#                        +fluxherb_wcdet[ndays]      -     fluxherb_wcdet[((nyears-1)*360+1)] +
#                        +fluxcarn_wcdet[ndays]      -     fluxcarn_wcdet[((nyears-1)*360+1)] +
#                        +fluxpfishlar_wcdet[ndays]      -     fluxpfishlar_wcdet[((nyears-1)*360+1)] +
#                        +fluxdfishlar_wcdet[ndays]      -     fluxdfishlar_wcdet[((nyears-1)*360+1)] +
#                        +fluxpfish_wcdet[ndays]      -     fluxpfish_wcdet[((nyears-1)*360+1)] +
#                        +fluxmfish_wcdet[ndays]      -     fluxmfish_wcdet[((nyears-1)*360+1)] +
#                        +fluxdfish_wcdet[ndays]      -     fluxdfish_wcdet[((nyears-1)*360+1)] +
#                        +fluxbenthslar_wcdet[ndays]      -     fluxbenthslar_wcdet[((nyears-1)*360+1)] +
#                        +fluxbenthclar_wcdet[ndays]      -     fluxbenthclar_wcdet[((nyears-1)*360+1)] +
#                        +fluxbenths_wcdet[ndays]      -     fluxbenths_wcdet[((nyears-1)*360+1)] +
#                        +fluxbenthc_wcdet[ndays]      -     fluxbenthc_wcdet[((nyears-1)*360+1)] +
#                        +fluxbird_wcdet[ndays]      -     fluxbird_wcdet[((nyears-1)*360+1)] +
#                        +fluxseal_wcdet[ndays]      -     fluxseal_wcdet[((nyears-1)*360+1)] +
#                        +fluxceta_wcdet[ndays]      -     fluxceta_wcdet[((nyears-1)*360+1)]
WCdetritusprod      <- NA

#SEDdetritusprod     <-   fluxcorp_xdet[ndays]      -     fluxcorp_xdet[((nyears-1)*360+1)] +
#                        +fluxbenths_xdet[ndays]      -     fluxbenths_xdet[((nyears-1)*360+1)] +
#                        +fluxbenthc_xdet[ndays]      -     fluxbenthc_xdet[((nyears-1)*360+1)]
SEDdetritusprod     <- NA

#Kelpdebrisprod     <-   fluxkelp_kelpdebris[ndays]      -     fluxkelp_kelpdebris[((nyears-1)*360+1)]
Kelpdebrisprod     <- NA

#Corpseprod          <-   fluxdisc_corp[ndays]      -     fluxdisc_corp[((nyears-1)*360+1)] +
#                        +fluxpfish_corp[ndays]      -     fluxpfish_corp[((nyears-1)*360+1)] +
#                        +fluxmfish_corp[ndays]      -     fluxmfish_corp[((nyears-1)*360+1)] +
#                        +fluxdfish_corp[ndays]      -     fluxdfish_corp[((nyears-1)*360+1)] +
#                        +fluxbenths_corp[ndays]      -     fluxbenths_corp[((nyears-1)*360+1)] +
#                        +fluxbenthc_corp[ndays]      -     fluxbenthc_corp[((nyears-1)*360+1)] +
#                        +fluxbird_corp[ndays]      -     fluxbird_corp[((nyears-1)*360+1)] +
#                        +fluxseal_corp[ndays]      -     fluxseal_corp[((nyears-1)*360+1)] +
#                        +fluxceta_corp[ndays]      -     fluxceta_corp[((nyears-1)*360+1)]
Corpseprod <- NA

#Fluxpartwc_sed      <-   fluxwcdet_xdet[ndays]      -     fluxwcdet_xdet[((nyears-1)*360+1)] +
#                        +fluxdisc_corp[ndays]      -     fluxdisc_corp[((nyears-1)*360+1)] +
#                        +fluxpfish_corp[ndays]      -     fluxpfish_corp[((nyears-1)*360+1)] +
#                        +fluxmfish_corp[ndays]      -     fluxmfish_corp[((nyears-1)*360+1)] +
#                        +fluxdfish_corp[ndays]      -     fluxdfish_corp[((nyears-1)*360+1)] +
#                        +fluxbird_corp[ndays]      -     fluxbird_corp[((nyears-1)*360+1)] +
#                        +fluxphyt_benths[ndays]      -     fluxphyt_benths[((nyears-1)*360+1)]+
#                        +fluxwcdet_benths[ndays]      -     fluxwcdet_benths[((nyears-1)*360+1)]
##Includes the water column feeding flux of benthos
Fluxpartwc_sed <- NA

#Fluxdisc_corp       <-   fluxdisc_corp[ndays]      -     fluxdisc_corp[((nyears-1)*360+1)]
Fluxdisc_corp       <- NA

#Pelagammprod        <-   fluxherb_wcamm[ndays]      -     fluxherb_wcamm[((nyears-1)*360+1)] +
#                        +fluxcarn_wcamm[ndays]      -     fluxcarn_wcamm[((nyears-1)*360+1)] +
#                        +fluxpfishlar_wcamm[ndays]      -     fluxpfishlar_wcamm[((nyears-1)*360+1)] +
#                        +fluxdfishlar_wcamm[ndays]      -     fluxdfishlar_wcamm[((nyears-1)*360+1)] +
#                        +fluxpfish_wcamm[ndays]      -     fluxpfish_wcamm[((nyears-1)*360+1)] +
#                        +fluxmfish_wcamm[ndays]      -     fluxmfish_wcamm[((nyears-1)*360+1)] +
#                        +fluxdfish_wcamm[ndays]      -     fluxdfish_wcamm[((nyears-1)*360+1)] +
#                        +fluxbenthslar_wcamm[ndays]      -     fluxbenthslar_wcamm[((nyears-1)*360+1)] +
#                        +fluxbenthclar_wcamm[ndays]      -     fluxbenthclar_wcamm[((nyears-1)*360+1)] +
#                        +fluxbird_wcamm[ndays]      -     fluxbird_wcamm[((nyears-1)*360+1)] + 
#                        +fluxseal_wcamm[ndays]      -     fluxseal_wcamm[((nyears-1)*360+1)] +
#                        +fluxceta_wcamm[ndays]      -     fluxceta_wcamm[((nyears-1)*360+1)] 
##Excludes mineralisation of detritus
Pelagammprod        <- NA

#Benthammprod        <-   fluxbenths_wcamm[ndays]      -     fluxbenths_wcamm[((nyears-1)*360+1)]+
#                        +fluxbenthc_wcamm[ndays]      -     fluxbenthc_wcamm[((nyears-1)*360+1)]
##Excludes mineralisation of detritus
Benthammprod        <- NA

#WCmineralisation    <-   fluxwcdet_wcamm[ndays]      -     fluxwcdet_wcamm[((nyears-1)*360+1)]
WCmineralisation    <- NA

#SEDmineralisation   <-   fluxxdet_sedamm[ndays]      -     fluxxdet_sedamm[((nyears-1)*360+1)]+
#                        +fluxxRdet_sedamm[ndays]      -     fluxxRdet_sedamm[((nyears-1)*360+1)]
SEDmineralisation   <- NA

#WCnitrification     <-   fluxwcamm_wcnit[ndays]      -     fluxwcamm_wcnit[((nyears-1)*360+1)]
WCnitrification     <- NA

#SEDnitrification    <-   fluxsedamm_sednit[ndays]      -     fluxsedamm_sednit[((nyears-1)*360+1)]
SEDnitrification    <- NA

#WCdenitrification   <-   wcdenitrif[ndays]      -     wcdenitrif[((nyears-1)*360+1)]
WCdenitrification   <- NA

#SEDdenitrification  <-   seddenitrif[ndays]      -     seddenitrif[((nyears-1)*360+1)]
SEDdenitrification  <- NA

#SEDWCammflux        <-   fluxsedamm_wcamm[ndays]      -     fluxsedamm_wcamm[((nyears-1)*360+1)]+
#                        +fluxbenths_wcamm[ndays]      -     fluxbenths_wcamm[((nyears-1)*360+1)]+
#                        +fluxbenthc_wcamm[ndays]      -     fluxbenthc_wcamm[((nyears-1)*360+1)]
##Includes excretion by benthos
SEDWCammflux        <- NA


#SEDWCnitflux        <-   fluxsednit_wcnit[ndays]      -     fluxsednit_wcnit[((nyears-1)*360+1)]
SEDWCnitflux        <- NA


#Fluxdet_herb           <-   fluxwcdet_herb[ndays]      -     fluxwcdet_herb[((nyears-1)*360+1)]
#Fluxphyt_herb          <-   fluxphyt_herb[ndays]      -     fluxphyt_herb[((nyears-1)*360+1)]
#Fluxbenthslar_herb          <-   fluxbenthslar_herb[ndays]      -     fluxbenthslar_herb[((nyears-1)*360+1)]
#Fluxbenthclar_herb          <-   fluxbenthclar_herb[ndays]      -     fluxbenthclar_herb[((nyears-1)*360+1)]
Fluxdet_herb           <- NA
Fluxphyt_herb          <- NA
Fluxbenthslar_herb          <- NA
Fluxbenthclar_herb          <- NA


#Fluxherb_carn           <-   fluxherb_carn[ndays]      -     fluxherb_carn[((nyears-1)*360+1)]
#Fluxpfishlar_carn       <-   fluxpfishlar_carn[ndays]      -     fluxpfishlar_carn[((nyears-1)*360+1)]
#Fluxdfishlar_carn       <-   fluxdfishlar_carn[ndays]      -     fluxdfishlar_carn[((nyears-1)*360+1)]   
#Fluxbenthslar_carn       <-   fluxbenthslar_carn[ndays]      -     fluxbenthslar_carn[((nyears-1)*360+1)]    
#Fluxbenthclar_carn       <-   fluxbenthclar_carn[ndays]      -     fluxbenthclar_carn[((nyears-1)*360+1)]    
Fluxherb_carn           <- NA
Fluxpfishlar_carn       <- NA
Fluxdfishlar_carn       <- NA
Fluxbenthslar_carn       <- NA
Fluxbenthclar_carn       <- NA


#Fluxherb_pfishlar           <-   fluxherb_pfishlar[ndays]      -     fluxherb_pfishlar[((nyears-1)*360+1)]   
#Fluxbenthslar_pfishlar        <-   fluxbenthslar_pfishlar[ndays]      -     fluxbenthslar_pfishlar[((nyears-1)*360+1)]   
#Fluxbenthclar_pfishlar        <-   fluxbenthclar_pfishlar[ndays]      -     fluxbenthclar_pfishlar[((nyears-1)*360+1)]   
Fluxherb_pfishlar           <-  NA
Fluxbenthslar_pfishlar        <-  NA
Fluxbenthclar_pfishlar        <-  NA



#Fluxherb_dfishlar           <-   fluxherb_dfishlar[ndays]      -     fluxherb_dfishlar[((nyears-1)*360+1)]   
#Fluxbenthslar_dfishlar        <-   fluxbenthslar_dfishlar[ndays]      -     fluxbenthslar_dfishlar[((nyears-1)*360+1)]   
#Fluxbenthclar_dfishlar        <-   fluxbenthclar_dfishlar[ndays]      -     fluxbenthclar_dfishlar[((nyears-1)*360+1)]   
Fluxherb_dfishlar           <-  NA
Fluxbenthslar_dfishlar        <- NA
Fluxbenthclar_dfishlar        <- NA



#Fluxherb_pfish            <-   fluxherb_pfish[ndays]      -     fluxherb_pfish[((nyears-1)*360+1)]     
#Fluxcarn_pfish            <-   fluxcarn_pfish[ndays]      -     fluxcarn_pfish[((nyears-1)*360+1)]     
#Fluxpfishlar_pfish        <-   fluxpfishlar_pfish[ndays]      -     fluxpfishlar_pfish[((nyears-1)*360+1)]     
#Fluxdfishlar_pfish        <-   fluxdfishlar_pfish[ndays]      -     fluxdfishlar_pfish[((nyears-1)*360+1)]     
#Fluxbenthslar_pfish         <-   fluxbenthslar_pfish[ndays]      -     fluxbenthslar_pfish[((nyears-1)*360+1)]     
#Fluxbenthclar_pfish         <-   fluxbenthclar_pfish[ndays]      -     fluxbenthclar_pfish[((nyears-1)*360+1)]     
Fluxherb_pfish            <-  NA
Fluxcarn_pfish            <-  NA
Fluxpfishlar_pfish        <-  NA
Fluxdfishlar_pfish        <-  NA
Fluxbenthslar_pfish         <- NA
Fluxbenthclar_pfish         <-  NA



#Fluxherb_mfish            <-   fluxherb_mfish[ndays]      -     fluxherb_mfish[((nyears-1)*360+1)]     
#Fluxcarn_mfish            <-   fluxcarn_mfish[ndays]      -     fluxcarn_mfish[((nyears-1)*360+1)]     
#Fluxpfishlar_mfish        <-   fluxpfishlar_mfish[ndays]      -     fluxpfishlar_mfish[((nyears-1)*360+1)]     
#Fluxdfishlar_mfish        <-   fluxdfishlar_mfish[ndays]      -     fluxdfishlar_mfish[((nyears-1)*360+1)]     
#Fluxbenthslar_mfish       <-   fluxbenthslar_mfish[ndays]      -     fluxbenthslar_mfish[((nyears-1)*360+1)]     
#Fluxbenthclar_mfish       <-   fluxbenthclar_mfish[ndays]      -     fluxbenthclar_mfish[((nyears-1)*360+1)]     
Fluxherb_mfish            <- NA
Fluxcarn_mfish            <- NA
Fluxpfishlar_mfish        <- NA
Fluxdfishlar_mfish        <- NA
Fluxbenthslar_mfish       <- NA
Fluxbenthclar_mfish       <- NA



#Fluxcorp_dfish            <-   fluxcorp_dfish[ndays]      -     fluxcorp_dfish[((nyears-1)*360+1)]     
#Fluxdisc_dfish            <-   fluxdisc_dfish[ndays]      -     fluxdisc_dfish[((nyears-1)*360+1)]     
#Fluxcarn_dfish            <-   fluxcarn_dfish[ndays]      -     fluxcarn_dfish[((nyears-1)*360+1)]     
#Fluxpfishlar_dfish        <-   fluxpfishlar_dfish[ndays]      -     fluxpfishlar_dfish[((nyears-1)*360+1)]     
#Fluxdfishlar_dfish        <-   fluxdfishlar_dfish[ndays]      -     fluxdfishlar_dfish[((nyears-1)*360+1)]     
#Fluxpfish_dfish           <-   fluxpfish_dfish[ndays]      -     fluxpfish_dfish[((nyears-1)*360+1)]     
#Fluxmfish_dfish           <-   fluxmfish_dfish[ndays]      -     fluxmfish_dfish[((nyears-1)*360+1)]     
#Fluxdfish_dfish           <-   fluxdfish_dfish[ndays]      -     fluxdfish_dfish[((nyears-1)*360+1)]     
#Fluxbenths_dfish          <-   fluxbenths_dfish[ndays]      -     fluxbenths_dfish[((nyears-1)*360+1)]     
#Fluxbenthc_dfish          <-   fluxbenthc_dfish[ndays]      -     fluxbenthc_dfish[((nyears-1)*360+1)]     
Fluxcorp_dfish            <-  NA
Fluxdisc_dfish            <-  NA
Fluxcarn_dfish            <-  NA
Fluxpfishlar_dfish        <-  NA
Fluxdfishlar_dfish        <-  NA
Fluxpfish_dfish           <-  NA
Fluxmfish_dfish           <-  NA
Fluxdfish_dfish           <-  NA
Fluxbenths_dfish          <-  NA
Fluxbenthc_dfish          <-  NA


#Fluxdet_benthslar            <-   fluxwcdet_benthslar[ndays]      -     fluxwcdet_benthslar[((nyears-1)*360+1)]  
#Fluxphyt_benthslar           <-   fluxphyt_benthslar[ndays]      -     fluxphyt_benthslar[((nyears-1)*360+1)]    
Fluxdet_benthslar            <-  NA
Fluxphyt_benthslar           <-  NA

#Fluxdet_benthclar            <-   fluxwcdet_benthclar[ndays]      -     fluxwcdet_benthclar[((nyears-1)*360+1)]  
#Fluxphyt_benthclar           <-   fluxphyt_benthclar[ndays]      -     fluxphyt_benthclar[((nyears-1)*360+1)]  
Fluxdet_benthclar            <- NA
Fluxphyt_benthclar           <- NA

#Fluxdet_benths             <-   fluxwcdet_benths[ndays]      -     fluxwcdet_benths[((nyears-1)*360+1)]    
#Fluxseddet_benths          <-   fluxxdet_benths[ndays]      -     fluxxdet_benths[((nyears-1)*360+1)] +   
#Fluxphyt_benths            <-   fluxphyt_benths[ndays]      -     fluxphyt_benths[((nyears-1)*360+1)]    
Fluxdet_benths             <- NA
Fluxseddet_benths          <- NA
Fluxphyt_benths            <- NA

#Fluxkelp_benthc            <-   fluxkelp_benthc[ndays]      -     fluxkelp_benthc[((nyears-1)*360+1)]    
#Fluxkelpdebris_benthc            <-   fluxkelpdebris_benthc[ndays]      -     fluxkelpdebris_benthc[((nyears-1)*360+1)]    
#Fluxcorp_benthc            <-   fluxcorp_benthc[ndays]      -     fluxcorp_benthc[((nyears-1)*360+1)]    
#Fluxbenths_benthc          <-   fluxbenths_benthc[ndays]      -     fluxbenths_benthc[((nyears-1)*360+1)]    
Fluxkelp_benthc            <- NA
Fluxkelpdebris_benthc            <- NA
Fluxcorp_benthc            <- NA
Fluxbenths_benthc          <- NA


#Fluxcorp_bird            <-   fluxcorp_bird[ndays]      -     fluxcorp_bird[((nyears-1)*360+1)]      
#Fluxdisc_bird            <-   fluxdisc_bird[ndays]      -     fluxdisc_bird[((nyears-1)*360+1)]      
##Fluxherb_bird            <-   fluxherb_bird[ndays]      -     fluxherb_bird[((nyears-1)*360+1)]      
#Fluxcarn_bird            <-   fluxcarn_bird[ndays]      -     fluxcarn_bird[((nyears-1)*360+1)]      
#Fluxpfish_bird           <-   fluxpfish_bird[ndays]      -     fluxpfish_bird[((nyears-1)*360+1)]      
#Fluxmfish_bird           <-   fluxmfish_bird[ndays]      -     fluxmfish_bird[((nyears-1)*360+1)]      
#Fluxdfish_bird           <-   fluxdfish_bird[ndays]      -     fluxdfish_bird[((nyears-1)*360+1)]      
#Fluxbenths_bird          <-   fluxbenths_bird[ndays]      -     fluxbenths_bird[((nyears-1)*360+1)]      
#Fluxbenthc_bird          <-   fluxbenthc_bird[ndays]      -     fluxbenthc_bird[((nyears-1)*360+1)]      

Fluxcorp_bird            <-  NA
Fluxdisc_bird            <-  NA
#Fluxherb_bird            <- NA
Fluxcarn_bird            <-  NA
Fluxpfish_bird           <-  NA
Fluxmfish_bird           <-  NA
Fluxdfish_bird           <-  NA
Fluxbenths_bird          <-  NA
Fluxbenthc_bird          <-  NA


     
#Fluxcorp_seal            <-   fluxcorp_seal[ndays]      -     fluxcorp_seal[((nyears-1)*360+1)]      
#Fluxdisc_seal            <-   fluxdisc_seal[ndays]      -     fluxdisc_seal[((nyears-1)*360+1)]      
#Fluxcarn_seal            <-   fluxcarn_seal[ndays]      -     fluxcarn_seal[((nyears-1)*360+1)]      
#Fluxpfish_seal           <-   fluxpfish_seal[ndays]      -     fluxpfish_seal[((nyears-1)*360+1)]      
#Fluxmfish_seal           <-   fluxmfish_seal[ndays]      -     fluxmfish_seal[((nyears-1)*360+1)]      
#Fluxdfish_seal           <-   fluxdfish_seal[ndays]      -     fluxdfish_seal[((nyears-1)*360+1)]      
#Fluxbenths_seal          <-   fluxbenths_seal[ndays]      -     fluxbenths_seal[((nyears-1)*360+1)]      
#Fluxbenthc_seal          <-   fluxbenthc_seal[ndays]      -     fluxbenthc_seal[((nyears-1)*360+1)]      
#Fluxbird_seal          <-   fluxbird_seal[ndays]      -     fluxbird_seal[((nyears-1)*360+1)]      
Fluxcorp_seal            <- NA
Fluxdisc_seal            <- NA
Fluxcarn_seal            <- NA
Fluxpfish_seal           <- NA
Fluxmfish_seal           <- NA
Fluxdfish_seal           <- NA
Fluxbenths_seal          <- NA
Fluxbenthc_seal          <- NA
Fluxbird_seal          <-   NA


#Fluxdisc_ceta            <-   fluxdisc_ceta[ndays]      -     fluxdisc_ceta[((nyears-1)*360+1)]      
#Fluxherb_ceta            <-   fluxherb_ceta[ndays]      -     fluxherb_ceta[((nyears-1)*360+1)]      
#Fluxcarn_ceta            <-   fluxcarn_ceta[ndays]      -     fluxcarn_ceta[((nyears-1)*360+1)]      
#Fluxpfish_ceta           <-   fluxpfish_ceta[ndays]      -     fluxpfish_ceta[((nyears-1)*360+1)]      
#Fluxmfish_ceta           <-   fluxmfish_ceta[ndays]      -     fluxmfish_ceta[((nyears-1)*360+1)]      
#Fluxdfish_ceta           <-   fluxdfish_ceta[ndays]      -     fluxdfish_ceta[((nyears-1)*360+1)]      
#Fluxbenths_ceta          <-   fluxbenths_ceta[ndays]      -     fluxbenths_ceta[((nyears-1)*360+1)]      
#Fluxbenthc_ceta          <-   fluxbenthc_ceta[ndays]      -     fluxbenthc_ceta[((nyears-1)*360+1)]      
#Fluxbird_ceta            <-   fluxbird_ceta[ndays]      -     fluxbird_ceta[((nyears-1)*360+1)]      
#Fluxseal_ceta            <-   fluxseal_ceta[ndays]      -     fluxseal_ceta[((nyears-1)*360+1)]      
Fluxdisc_ceta            <-  NA
Fluxherb_ceta            <-  NA
Fluxcarn_ceta            <-  NA
Fluxpfish_ceta           <-  NA
Fluxmfish_ceta           <-  NA
Fluxdfish_ceta           <-  NA
Fluxbenths_ceta          <-  NA
Fluxbenthc_ceta          <-  NA
Fluxbird_ceta            <-  NA
Fluxseal_ceta            <-  NA

HTLP<-   Herbnetprod +
       + Carnnetprod +
       + Benthsnetprod +
       + Benthcnetprod +
       + Benthslarnetprod +
       + Benthclarnetprod +
       + Fishplarnetprod +
       + Fishdlarnetprod +
       + Fishpnetprod +
       + Fishmnetprod +
       + Fishdnetprod +
       + Birdnetprod +
       + Sealnetprod +
       + Cetanetprod 


#export_from_2prod<-  Fluxherb_carn +
#                   + Fluxherb_pfishlar + 
#                   + Fluxherb_dfishlar +
#                   + Fluxherb_pfish +
#                   + Fluxherb_ceta +
#                   + Fluxbenths_dfish +
#                   + Fluxbenths_benthc +
#                   + Fluxbenths_bird +
#                   + Fluxbenths_seal +
#                   + Fluxbenths_ceta
export_from_2prod<- NA

#Pfish_annual_spawn   <- Pfish_spawn[ndays]-Pfish_spawn[((nyears-1)*360+1)]
#Pfish_annual_recruit <- Pfish_recruit[ndays]-Pfish_recruit[((nyears-1)*360+1)]
#Dfish_annual_spawn   <- Dfish_spawn[ndays]-Dfish_spawn[((nyears-1)*360+1)]
#Dfish_annual_recruit <- Dfish_recruit[ndays]-Dfish_recruit[((nyears-1)*360+1)]
Pfish_annual_spawn   <- NA
Pfish_annual_recruit <- NA
Dfish_annual_spawn   <- NA
Dfish_annual_recruit <- NA



#Benths_annual_spawn   <- Bs_spawn[ndays]-Bs_spawn[((nyears-1)*360+1)]
#Benths_annual_recruit <- Bs_recruit[ndays]-Bs_recruit[((nyears-1)*360+1)]
#Benthc_annual_spawn   <- Bc_spawn[ndays]-Bc_spawn[((nyears-1)*360+1)]
#Benthc_annual_recruit <- Bc_recruit[ndays]-Bc_recruit[((nyears-1)*360+1)]
Benths_annual_spawn   <- NA
Benths_annual_recruit <- NA
Benthc_annual_spawn   <- NA
Benthc_annual_recruit <- NA



FishpLand_livewt<-landp_o[ndays]-landp_o[((nyears-1)*360+1)]
FishmLand_livewt<-landm_o[ndays]-landm_o[((nyears-1)*360+1)]
FishdLand_livewt<-landd_o[ndays]-landd_o[((nyears-1)*360+1)]
Fishd_qLand_livewt<-landd_quota_o[ndays]-landd_quota_o[((nyears-1)*360+1)]
Fishd_nqLand_livewt<-landd_nonquota_o[ndays]-landd_nonquota_o[((nyears-1)*360+1)]
BenthsLand_livewt <- landsb_o[ndays]-landsb_o[((nyears-1)*360+1)]
BenthcLand_livewt <- landcb_o[ndays]-landcb_o[((nyears-1)*360+1)]
CarnzLand_livewt <- landcz_o[ndays]-landcz_o[((nyears-1)*360+1)]
BirdLand_livewt <- landbd_o[ndays]-landbd_o[((nyears-1)*360+1)]
SealLand_livewt <- landsl_o[ndays]-landsl_o[((nyears-1)*360+1)]
CetaLand_livewt <- landct_o[ndays]-landct_o[((nyears-1)*360+1)]
#KelpLand_livewt <- landkp_i[ndays]-landkp_i[((nyears-1)*360+1)]
KelpLand_livewt <- NA

FishpDiscard<-discpel_o[ndays]-discpel_o[((nyears-1)*360+1)]
FishmDiscard<-discmig_o[ndays]-discmig_o[((nyears-1)*360+1)]
FishdDiscard<-discdem_o[ndays]-discdem_o[((nyears-1)*360+1)]
Fishd_qDiscard<-discdem_quota_o[ndays]-discdem_quota_o[((nyears-1)*360+1)]
Fishd_nqDiscard<-discdem_nonquota_o[ndays]-discdem_nonquota_o[((nyears-1)*360+1)]
BenthsDiscard <- discsb_o[ndays]-discsb_o[((nyears-1)*360+1)]
BenthcDiscard <- disccb_o[ndays]-disccb_o[((nyears-1)*360+1)]
CarnzDiscard  <- disccz_o[ndays]-disccz_o[((nyears-1)*360+1)]
BirdDiscard   <- discbd_o[ndays]-discbd_o[((nyears-1)*360+1)]
SealDiscard   <- discsl_o[ndays]-discsl_o[((nyears-1)*360+1)]
CetaDiscard   <- discct_o[ndays]-discct_o[((nyears-1)*360+1)]
#KelpDiscard   <- disckp_i[ndays]-disckp_i[((nyears-1)*360+1)]
KelpDiscard   <- NA

FishpOffal<-offalpel_o[ndays]-offalpel_o[((nyears-1)*360+1)]
FishmOffal<-offalmig_o[ndays]-offalmig_o[((nyears-1)*360+1)]
FishdOffal<-offaldem_o[ndays]-offaldem_o[((nyears-1)*360+1)]
Fishd_qOffal<-offaldem_quota_o[ndays]-offaldem_quota_o[((nyears-1)*360+1)]
Fishd_nqOffal<-offaldem_nonquota_o[ndays]-offaldem_nonquota_o[((nyears-1)*360+1)]
BenthsOffal <- offalsb_o[ndays]-offalsb_o[((nyears-1)*360+1)]
BenthcOffal <- offalcb_o[ndays]-offalcb_o[((nyears-1)*360+1)]
CarnzOffal  <- offalcz_o[ndays]-offalcz_o[((nyears-1)*360+1)]
BirdOffal   <- offalbd_o[ndays]-offalbd_o[((nyears-1)*360+1)]
SealOffal   <- offalsl_o[ndays]-offalsl_o[((nyears-1)*360+1)]
CetaOffal   <- offalct_o[ndays]-offalct_o[((nyears-1)*360+1)]
#KelpOffal   <- offalkp_i[ndays]-offalkp_i[((nyears-1)*360+1)]
KelpOffal   <- NA

FishpLand_processedwt<- FishpLand_livewt - FishpOffal
FishmLand_processedwt<- FishmLand_livewt - FishmOffal
FishdLand_processedwt<- FishdLand_livewt - FishdOffal
Fishd_qLand_processedwt<- Fishd_qLand_livewt - Fishd_qOffal
Fishd_nqLand_processedwt<- Fishd_nqLand_livewt - Fishd_nqOffal
BenthsLand_processedwt <- BenthsLand_livewt - BenthsOffal
BenthcLand_processedwt <- BenthcLand_livewt - BenthcOffal
CarnzLand_processedwt <- CarnzLand_livewt - CarnzOffal
BirdLand_processedwt <- BirdLand_livewt - BirdOffal
SealLand_processedwt <- SealLand_livewt - SealOffal
CetaLand_processedwt <- CetaLand_livewt - CetaOffal
#KelpLand_processedwt <- KelpLand_livewt - KelpOffal
KelpLand_processedwt <- NA



annual_flux_results<-data.frame(rep(0,240))

#EXTRACT THE RESULTS INTO A DATA FRAME
#COLUMN 1 IS IN NITROGEN UNITS (mM N/m2/y)

#DERIVED ANNUAL VALUES IN UNITS OF mM N/m2 and fluxes in mMN/m2/y

annual_flux_results[1,1]<-DINinflow
annual_flux_results[2,1]<-DINoutflow
annual_flux_results[3,1]<-PARTinflow
annual_flux_results[4,1]<-PARToutflow
annual_flux_results[5,1]<-atmosphereDINinput
annual_flux_results[6,1]<-riverDINinflow
annual_flux_results[7,1]<-riverPARTinflow
annual_flux_results[8,1]<-sumDINinflow
annual_flux_results[9,1]<-sumDINoutflow
annual_flux_results[10,1]<-sumPARTinflow
annual_flux_results[11,1]<-sumPARToutflow
annual_flux_results[12,1]<-sumriverDINinflow
annual_flux_results[13,1]<-sumatmosDINinput
annual_flux_results[14,1]<-surfvertnitflux
annual_flux_results[15,1]<-surfhoriznitflux
annual_flux_results[16,1]<-Flux_sedboundary

annual_flux_results[17,1]<-kelp_beachcast


annual_flux_results[18,1]<-DIN_NET_flux_o_i
annual_flux_results[19,1]<-PART_NET_flux_o_i
annual_flux_results[20,1]<-NET_activemigpelfish_o_i

annual_flux_results[21,1]<-NET_activemigmigfish_o_i
annual_flux_results[22,1]<-NET_activemigdemfish_o_i
annual_flux_results[23,1]<-NET_activemigbird_o_i
annual_flux_results[24,1]<-NET_activemigseal_o_i
annual_flux_results[25,1]<-NET_activemigceta_o_i

annual_flux_results[26,1]<-NET_mfish_ext_o 
annual_flux_results[27,1]<-Mfish_annual_imig
annual_flux_results[28,1]<-Mfish_annual_emig
annual_flux_results[29,1]<-NETPrimaryP
annual_flux_results[30,1]<-MMP
annual_flux_results[31,1]<-PNP
annual_flux_results[32,1]<-PhytNitUp
annual_flux_results[33,1]<-PhytAmmUp
annual_flux_results[34,1]<-NewP
annual_flux_results[35,1]<-fratio
annual_flux_results[36,1]<-Tfratio

annual_flux_results[37,1]<-KelpNitUp
annual_flux_results[38,1]<-KelpAmmUp

annual_flux_results[39,1]<-KelpNprod


annual_flux_results[40,1]<-Phytgrossprod

annual_flux_results[41,1]<-Herbgrossprod
annual_flux_results[42,1]<-Carngrossprod
annual_flux_results[43,1]<-Fishplargrossprod
annual_flux_results[44,1]<-Fishdlargrossprod
annual_flux_results[45,1]<-Fishpgrossprod 
annual_flux_results[46,1]<-Fishmgrossprod 
annual_flux_results[47,1]<-Fishdgrossprod 
annual_flux_results[48,1]<-Benthslargrossprod 
annual_flux_results[49,1]<-Benthclargrossprod 
annual_flux_results[50,1]<-Benthsgrossprod    
annual_flux_results[51,1]<-Benthcgrossprod    
annual_flux_results[52,1]<-Birdgrossprod
annual_flux_results[53,1]<-Sealgrossprod
annual_flux_results[54,1]<-Cetagrossprod

annual_flux_results[55,1]<-Herbnetprod
annual_flux_results[56,1]<-Carnnetprod
annual_flux_results[57,1]<-Fishplarnetprod
annual_flux_results[58,1]<-Fishdlarnetprod
annual_flux_results[59,1]<-Fishpnetprod 
annual_flux_results[60,1]<-Fishmnetprod 
annual_flux_results[61,1]<-Fishdnetprod 
annual_flux_results[62,1]<-Benthslarnetprod 
annual_flux_results[63,1]<-Benthclarnetprod 
annual_flux_results[64,1]<-Benthsnetprod    
annual_flux_results[65,1]<-Benthcnetprod    
annual_flux_results[66,1]<-Birdnetprod
annual_flux_results[67,1]<-Sealnetprod
annual_flux_results[68,1]<-Cetanetprod


      
annual_flux_results[69,1]<-WCdetritusprod  
annual_flux_results[70,1]<-SEDdetritusprod 
annual_flux_results[71,1]<-Corpseprod      
annual_flux_results[72,1]<-Fluxpartwc_sed  
annual_flux_results[73,1]<-Fluxdisc_corp   
annual_flux_results[74,1]<-Pelagammprod    
annual_flux_results[75,1]<-Benthammprod    
annual_flux_results[76,1]<-WCmineralisation
annual_flux_results[77,1]<-SEDmineralisation
annual_flux_results[78,1]<-WCnitrification  
annual_flux_results[79,1]<-SEDnitrification 
annual_flux_results[80,1]<-WCdenitrification
annual_flux_results[81,1]<-SEDdenitrification
annual_flux_results[82,1]<-SEDWCammflux      
annual_flux_results[83,1]<-SEDWCnitflux      
annual_flux_results[84,1]<-Fluxdet_herb    
annual_flux_results[85,1]<-Fluxphyt_herb   
annual_flux_results[85,1]<-Fluxbenthslar_herb 
annual_flux_results[87,1]<-Fluxbenthclar_herb 
annual_flux_results[88,1]<-Fluxherb_carn      
annual_flux_results[89,1]<-Fluxpfishlar_carn  
annual_flux_results[90,1]<-Fluxdfishlar_carn  
annual_flux_results[91,1]<-Fluxbenthslar_carn   
annual_flux_results[92,1]<-Fluxbenthclar_carn   
annual_flux_results[93,1]<-Fluxherb_pfishlar   
annual_flux_results[94,1]<-Fluxbenthslar_pfishlar
annual_flux_results[95,1]<-Fluxbenthclar_pfishlar
annual_flux_results[96,1]<-Fluxherb_dfishlar   
annual_flux_results[97,1]<-Fluxbenthslar_dfishlar
annual_flux_results[98,1]<-Fluxbenthclar_dfishlar
annual_flux_results[99,1]<-Fluxherb_pfish       
annual_flux_results[100,1]<-Fluxcarn_pfish       
annual_flux_results[101,1]<-Fluxpfishlar_pfish   
annual_flux_results[102,1]<-Fluxdfishlar_pfish   
annual_flux_results[103,1]<-Fluxbenthslar_pfish    
annual_flux_results[104,1]<-Fluxbenthclar_pfish    
annual_flux_results[105,1]<-Fluxherb_mfish      
annual_flux_results[106,1]<-Fluxcarn_mfish      
annual_flux_results[107,1]<-Fluxpfishlar_mfish  
annual_flux_results[108,1]<-Fluxdfishlar_mfish  
annual_flux_results[109,1]<-Fluxbenthslar_mfish 
annual_flux_results[110,1]<-Fluxbenthclar_mfish 
annual_flux_results[111,1]<-Fluxcorp_dfish      
annual_flux_results[112,1]<-Fluxdisc_dfish
annual_flux_results[113,1]<-Fluxcarn_dfish      
annual_flux_results[114,1]<-Fluxpfishlar_dfish  
annual_flux_results[115,1]<-Fluxdfishlar_dfish  
annual_flux_results[116,1]<-Fluxpfish_dfish     
annual_flux_results[117,1]<-Fluxmfish_dfish     
annual_flux_results[118,1]<-Fluxdfish_dfish     
annual_flux_results[119,1]<-Fluxbenths_dfish    
annual_flux_results[120,1]<-Fluxbenthc_dfish    
annual_flux_results[121,1]<-Fluxdet_benthslar   
annual_flux_results[122,1]<-Fluxphyt_benthslar  
annual_flux_results[123,1]<-Fluxdet_benthclar   
annual_flux_results[124,1]<-Fluxphyt_benthclar  
annual_flux_results[125,1]<-Fluxdet_benths      
annual_flux_results[126,1]<-Fluxseddet_benths   
annual_flux_results[127,1]<-Fluxphyt_benths     
annual_flux_results[128,1]<-Fluxkelpdebris_benthc     
annual_flux_results[129,1]<-Fluxcorp_benthc     
annual_flux_results[130,1]<-Fluxkelp_benthc     
annual_flux_results[131,1]<-Fluxbenths_benthc   
annual_flux_results[132,1]<-Fluxcorp_bird       
annual_flux_results[133,1]<-Fluxdisc_bird       

#annual_flux_results[xxx,1]<-Fluxherb_bird       

annual_flux_results[134,1]<-Fluxcarn_bird       
annual_flux_results[135,1]<-Fluxpfish_bird      
annual_flux_results[136,1]<-Fluxmfish_bird      
annual_flux_results[137,1]<-Fluxdfish_bird      
annual_flux_results[138,1]<-Fluxbenths_bird     
annual_flux_results[139,1]<-Fluxbenthc_bird     
annual_flux_results[140,1]<-Fluxcorp_seal       
annual_flux_results[141,1]<-Fluxdisc_seal       
annual_flux_results[142,1]<-Fluxcarn_seal       
annual_flux_results[143,1]<-Fluxpfish_seal      
annual_flux_results[144,1]<-Fluxmfish_seal      
annual_flux_results[145,1]<-Fluxdfish_seal      
annual_flux_results[146,1]<-Fluxbenths_seal     
annual_flux_results[147,1]<-Fluxbenthc_seal     
annual_flux_results[148,1]<-Fluxbird_seal     
annual_flux_results[149,1]<-Fluxdisc_ceta       
annual_flux_results[150,1]<-Fluxherb_ceta       
annual_flux_results[151,1]<-Fluxcarn_ceta       
annual_flux_results[152,1]<-Fluxpfish_ceta      
annual_flux_results[153,1]<-Fluxmfish_ceta      
annual_flux_results[154,1]<-Fluxdfish_ceta      
annual_flux_results[155,1]<-Fluxbenths_ceta     
annual_flux_results[156,1]<-Fluxbenthc_ceta     
annual_flux_results[157,1]<-Fluxbird_ceta     
annual_flux_results[158,1]<-Fluxseal_ceta     

annual_flux_results[159,1]<-HTLP
annual_flux_results[160,1]<-export_from_2prod
annual_flux_results[161,1]<-Pfish_annual_spawn   
annual_flux_results[162,1]<-Pfish_annual_recruit 
annual_flux_results[163,1]<-Dfish_annual_spawn   
annual_flux_results[164,1]<-Dfish_annual_recruit 
annual_flux_results[165,1]<-Benths_annual_spawn   
annual_flux_results[166,1]<-Benths_annual_recruit 
annual_flux_results[167,1]<-Benthc_annual_spawn   
annual_flux_results[168,1]<-Benthc_annual_recruit 

annual_flux_results[169,1]<-FishpLand_livewt
annual_flux_results[170,1]<-FishmLand_livewt
annual_flux_results[171,1]<-FishdLand_livewt
annual_flux_results[172,1]<-Fishd_qLand_livewt
annual_flux_results[173,1]<-Fishd_nqLand_livewt
annual_flux_results[174,1]<-BenthsLand_livewt
annual_flux_results[175,1]<-BenthcLand_livewt 
annual_flux_results[176,1]<-CarnzLand_livewt 
annual_flux_results[177,1]<-BirdLand_livewt 
annual_flux_results[178,1]<-SealLand_livewt 
annual_flux_results[179,1]<-CetaLand_livewt 
annual_flux_results[180,1]<-KelpLand_livewt 

annual_flux_results[181,1]<-FishpDiscard
annual_flux_results[182,1]<-FishmDiscard
annual_flux_results[183,1]<-FishdDiscard
annual_flux_results[184,1]<-Fishd_qDiscard
annual_flux_results[185,1]<-Fishd_nqDiscard
annual_flux_results[186,1]<-BenthsDiscard
annual_flux_results[187,1]<-BenthcDiscard
annual_flux_results[188,1]<-CarnzDiscard 
annual_flux_results[189,1]<-BirdDiscard
annual_flux_results[190,1]<-SealDiscard
annual_flux_results[191,1]<-CetaDiscard
annual_flux_results[192,1]<-KelpDiscard

annual_flux_results[193,1]<-FishpOffal
annual_flux_results[194,1]<-FishmOffal
annual_flux_results[195,1]<-FishdOffal
annual_flux_results[196,1]<-Fishd_qOffal
annual_flux_results[197,1]<-Fishd_nqOffal
annual_flux_results[198,1]<-BenthsOffal
annual_flux_results[199,1]<-BenthcOffal
annual_flux_results[200,1]<-CarnzOffal 
annual_flux_results[201,1]<-BirdOffal
annual_flux_results[202,1]<-SealOffal
annual_flux_results[203,1]<-CetaOffal
annual_flux_results[204,1]<-KelpOffal

annual_flux_results[205,1]<-FishpLand_processedwt
annual_flux_results[206,1]<-FishmLand_processedwt
annual_flux_results[207,1]<-FishdLand_processedwt
annual_flux_results[208,1]<-Fishd_qLand_processedwt
annual_flux_results[209,1]<-Fishd_nqLand_processedwt
annual_flux_results[210,1]<-BenthsLand_processedwt
annual_flux_results[211,1]<-BenthcLand_processedwt 
annual_flux_results[212,1]<-CarnzLand_processedwt 
annual_flux_results[213,1]<-BirdLand_processedwt 
annual_flux_results[214,1]<-SealLand_processedwt 
annual_flux_results[215,1]<-CetaLand_processedwt 
annual_flux_results[216,1]<-KelpLand_processedwt 

annual_flux_results[217,1]<-x_shallowprop
annual_flux_results[218,1]<-si_depth
annual_flux_results[219,1]<-so_depth
annual_flux_results[220,1]<-d_depth

annual_flux_results[221,1]<-x_area_s0

annual_flux_results[222,1]<-x_area_s1
annual_flux_results[223,1]<-x_area_s2
annual_flux_results[224,1]<-x_area_s3

annual_flux_results[225,1]<-x_area_d0

annual_flux_results[226,1]<-x_area_d1
annual_flux_results[227,1]<-x_area_d2
annual_flux_results[228,1]<-x_area_d3
annual_flux_results[229,1]<-x_depth_s1
annual_flux_results[230,1]<-x_depth_s2
annual_flux_results[231,1]<-x_depth_s3
annual_flux_results[232,1]<-x_depth_d1
annual_flux_results[233,1]<-x_depth_d2
annual_flux_results[234,1]<-x_depth_d3
annual_flux_results[235,1]<-x_poros_s1
annual_flux_results[236,1]<-x_poros_s2
annual_flux_results[237,1]<-x_poros_s3
annual_flux_results[238,1]<-x_poros_d1
annual_flux_results[239,1]<-x_poros_d2
annual_flux_results[240,1]<-x_poros_d3




annual_flux_results[,2]<-rep("mMN/whole_model_domain_(1m2)/y",240)
annual_flux_results[8:13,2]<-("mMN/whole_model_domain_(1m2)/summer_period_AMJJAS")
annual_flux_results[c(217,221:228,235:240),2]<-"dimensionless"
annual_flux_results[c(218:220,229:234),2]<-"m"


annual_flux_results[,3]<-c(
"DIN_inflow",
"DIN_outflow",
"Particulate_inflow",
"Particulate_outflow",
"Atmosphere_DIN_input",
"River_DIN_inflow",
"River_particulate_inflow",
"Summer_DIN_inflow",
"Summer_DIN_outflow",
"Summer_particulate_inflow",
"Summer_particulate_outflow",
"Summer_river_DIN_inflow",
"Summer_atmosphere_DIN_input",
"Vertical_nitrate_flux",
"Surface_horizontal_nitrate_flux",
"Net_import/export_flux_in_the_sediment",
"Beachcast_export_of_kelp_debris",
"DIN_Net_flux_offshore_to_inshore",
"Particulate_net_flux_offshore_to_inshore",
"Plank.fish_net_active_migration_offshore_to_inshore",
"Mig.fish_net_active_migration_offshore_to_inshore",
"Dem.fish_net_active_migration_offshore_to_inshore",
"Bird_net_active_migration_offshore_to_inshore",
"Seal_net_active_migration_offshore_to_inshore",
"Cetacean_net_active_migration_offshore_to_inshore",
"Mig.fish_net_migration_external_offshore",
"Mig.fish_annual_immigration",
"Mig.fish_annual_emigration",
"Phytoplankton_net_primary_production",
"Phytoplankton_new_production_(nitrate_drawdown)",
"Phytoplankton_new_production_(traditional)",
"Phytoplankton_new_production_(Heath&Beare)",
"Phytoplankton_nitrate_uptake",
"Phytoplankton_ammonia_uptake",
"Phytoplankton_fratio_(H&B/NetPP)",
"Phytoplankton_fratio_(traditional)",
"Kelp_nitrate_uptake",
"Kelp_ammonia_uptake",
"Kelp_gross_production",
"Phytoplankton_gross_production",
"Omniv.zooplankton_gross_production",
"Carniv.zooplankton_gross_production",
"Planktiv.fish_larvae_gross_production",
"Dem.fish_larvae_gross_production",
"Planktiv.fish_gross_production", 
"Mig.fish_gross_production", 
"Dem.fish_gross_production", 
"Benthos_filt/dep_larvae_gross_production", 
"Benthos_carn/scav_larvae_gross_production", 
"Benthos_filt/dep_gross_production",
"Benthos_carn/scav_gross_production",  
"Bird_gross_production",  
"Seal_gross_production",  
"Cetacean_gross_production",  


"Omniv.zooplankton_net_production",
"Carniv.zooplankton_net_production",
"Planktiv.fish_larvae_net_production",
"Dem.fish_larvae_net_production",
"Planktiv.fish_net_production", 
"Mig.fish_net_production", 
"Dem.fish_net_production", 
"Benthos_filt/dep_larvae_net_production", 
"Benthos_carn/scav_larvae_net_production", 
"Benthos_filt/dep_net_production",
"Benthos_carn/scav_net_production",  
"Bird_net_production",  
"Seal_net_production",  
"Cetacean_net_production",  





"Water_column_detritus_production",  
"Sediment_detritus_production", 
"Corpse_production",
"Flux_of_detritus_from_water_to_sediment",  
"Flux_of_discards_to_corpses",
"Pelagic_fauna_ammonia_production", 
"Benthic_fauna_ammonia_production",
"Water_column_detritus_mineralisation",
"Sediment_detritus_mineralisation",
"Water_column_nitrification",
"Sediment_nitrification", 
"Water_column_denitrification",
"Sediment_denitrification", 
"Sediment_to_water_ammonia_flux",
"Sediment_to_water_nitrate_flux",

"Flux_detritus_to_omniv.zooplankton",  
"Flux_phytoplankton_to_omniv.zooplankton",  
"Flux_benthosf/d.larvae_to_omniv.zooplankton",  
"Flux_benthosc/s.larvae_to_omniv.zooplankton",  

"Flux_omniv.zooplankton_to_carniv.zooplankton",  
"Flux_plank.fish.larvae_to_carniv.zooplankton",  
"Flux_dem.fish.larvae_to_carniv.zooplankton",  
"Flux_benthosf/d.larvae_to_carniv.zooplankton",  
"Flux_benthosc/s.larvae_to_carniv.zooplankton",  

"Flux_omniv.zooplankton_to_plank.fish.larvae",   
"Flux_benthosf/d.larvae_to_plank.fish.larvae",   
"Flux_benthosc/s.larvae_to_plank.fish.larvae",   

"Flux_omniv.zooplankton_to_dem.fish.larvae",   
"Flux_benthosf/d.larvae_to_dem.fish.larvae",   
"Flux_benthosc/s.larvae_to_dem.fish.larvae",   

"Flux_omniv.zooplankton_to_plank.fish",   
"Flux_carniv.zooplankton_to_plank.fish",   
"Flux_plank.fish.larvae_to_plank.fish",   
"Flux_dem.fish.larvae_to_plank.fish",   
"Flux_benthosf/d.larvae_to_plank.fish",   
"Flux_benthosc/s.larvae_to_plank.fish",   

"Flux_omniv.zooplankton_to_mig.fish",   
"Flux_carniv.zooplankton_to_mig.fish",   
"Flux_plank.fish.larvae_to_mig.fish",   
"Flux_dem.fish.larvae_to_mig.fish",   
"Flux_benthosf/d.larvae_to_mig.fish",   
"Flux_benthosc/s.larvae_to_mig.fish",   

"Flux_corpses_to_dem.fish",   
"Flux_discards_to_dem.fish",   
"Flux_carniv.zooplankton_to_dem.fish",   
"Flux_plank.fish.larvae_to_dem.fish",   
"Flux_dem.fish.larvae_to_dem.fish",   
"Flux_plank.fish_to_dem.fish",   
"Flux_mig.fish_to_dem.fish",   
"Flux_dem.fish_to_dem.fish",   
"Flux_benthosf/d_to_dem.fish",   
"Flux_benthosc/s_to_dem.fish",   

"Flux_detritus_to_benthosf/d.larvae",  
"Flux_phytoplankton_to_benthosf/d.larvae",  

"Flux_detritus_to_benthosc/s.larvae",  
"Flux_phytoplankton_to_benthosc/s.larvae",  

"Flux_detritus_to_benthosf/d",  
"Flux_sediment.detritus_to_benthosf/d",  
"Flux_phytoplankton_to_benthosf/d",  

"Flux_kelp.debris_to_benthosc/s",   
"Flux_corpses_to_benthosc/s",   
"Flux_kelp_to_benthosc/s",   
"Flux_benthosf/d_to_benthosc/sc",   

"Flux_corpses_to_birds", 
"Flux_discards_to_birds",     
#"Flux_ominiv.zooplankton_to_birds",     
"Flux_carniv.zooplankton_to_birds",     
"Flux_plank.fish_to_birds",     
"Flux_mig.fish_to_birds",     
"Flux_dem.fish_to_birds",     
"Flux_benthosf/d_to_birds",     
"Flux_benthosc/s_to_birds",     

"Flux_corpses_to_seals", 
"Flux_discards_to_seals",     
"Flux_carniv.zooplankton_to_seals",     
"Flux_plank.fish_to_seals",     
"Flux_mig.fish_to_seals",     
"Flux_dem.fish_to_seals",     
"Flux_benthosf/d_to_seals",     
"Flux_benthosc/s_to_seals",     
"Flux_birds_to_seals",     

"Flux_discards_to_cetaceans",     
"Flux_ominiv.zooplankton_to_cetaceans",     
"Flux_carniv.zooplankton_to_cetaceans",     
"Flux_plank.fish_to_cetaceans",     
"Flux_mig.fish_to_cetaceans",     
"Flux_dem.fish_to_cetaceans",     
"Flux_benthosf/d_to_cetaceans",     
"Flux_benthosc/s_to_cetaceans",     
"Flux_birds_to_cetaceans",     
"Flux_seals_to_cetaceans",     


"Net_production_of_all_secondary_and_higher_trophic_levels",
"Export_from_secondary_producers",
"Plank.fish_annual_spawning",  
"Plank.fish_annual_recruitment", 
"Dem.fish_annual_spawning",
"Dem.fish_annual_recruitment", 
"Benthosf/d_annual_spawning",
"Benthosf/d_annual_recruitment", 
"Benthosc/s_annual_spawning",
"Benthosc/sc_annual_recruitment",

"Plank.fish_landings_live_weight",
"Mig.fish_landings_live_weight",
"Dem.fish_landings_live_weight",
"Dem.fish_quota_limited_landings_live_weight",
"Dem.fish_non.quota_landings_live_weight",
"Benthosf/d_landings_live_weight",
"Benthosc/s_landings_live_weight",
"Carniv.zooplankton_landings_live_weight",
"Bird_landings_live_weight",
"Seal_landings_live_weight",
"Cetacean_landings_live_weight",
"Kelp_landings_live_weight",


"Plank.fish_discards",
"Mig.fish_discards",
"Dem.fish_discards",
"Dem.fish_quota_limited_discards",
"Dem.fish_non.quota_discards",
"Benthosf/d_discards",
"Benthosc/s_discards",
"Carniv.zooplankton_discards",
"Bird_discards",
"Seal_discards",
"Cetacean_discards",
"Kelp_discards",

"Plank.fish_offal",
"Mig.fish_offal",
"Dem.fish_offal",
"Dem.fish_quota_limited_offal",
"Dem.fish_non.quota_offal",
"Benthosf/d_offal",
"Benthosc/s_offal",
"Carniv.zooplankton_offal",
"Bird_offal",
"Seal_offal",
"Cetacean_offal",
"Kelp_offal",

"Plank.fish_landings_processed_weight",
"Mig.fish_landings_processed_weight",
"Dem.fish_landings_processed_weight",
"Dem.fish_quota_limited_landings_processed_weight",
"Dem.fish_non.quota_landings_processed_weight",
"Benthosf/d_landings_processed_weight",
"Benthosc/s_landings_processed_weight",
"Carniv.zooplankton_landings_processed_weight",
"Bird_landings_processed_weight",
"Seal_landings_processed_weight",
"Cetacean_landings_processed_weight",
"Kelp_landings_processed_weight",

"Area_proportion_of_inshore_zone",
"Thickness_of_inshore_surface_layer",
"Thickness_of_offshore_surface_layer",
"Thickness_of_offshore_deep_layer",
"Area_proportion_inshore_rock",
"Area_proportion_inshore_sediment_s1",
"Area_proportion_inshore_sediment_s2",
"Area_proportion_inshore_sediment_s3",
"Area_proportion_offshore_rock",
"Area_proportion_offshore_sediment_d1",
"Area_proportion_offshore_sediment_d2",
"Area_proportion_offshore_sediment_d3",
"Thickness_of_inshore_sediment_layer_s1",
"Thickness_of_inshore_sediment_layer_s2",
"Thickness_of_inshore_sediment_layer_s3",
"Thickness_of_offshore_sediment_layer_d1",
"Thickness_of_offshore_sediment_layer_d2",
"Thickness_of_offshore_sediment_layer_d3",
"Porosity_of_inshore_sediment_layer_s1",
"Porosity_of_inshore_sediment_layer_s2",
"Porosity_of_inshore_sediment_layer_s3",
"Porosity_of_offshore_sediment_layer_d1",
"Porosity_of_offshore_sediment_layer_d2",
"Porosity_of_offshore_sediment_layer_d3")




names(annual_flux_results)<-c("Model_annual_flux","Units","Description")


annual_flux_results


#Print the data to a csv file
#-----------------------------------------------------------------
write.table(annual_flux_results,file=paste(oudir,"OFFSHORE_model_annualresults","-",AAA,".csv",sep=""),sep=",",row.names=FALSE)

#-------------------------------------------------------------------------------------------------------

}

