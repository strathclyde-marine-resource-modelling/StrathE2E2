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
	run		<- elt(model, "run")
	setup		<- elt(model, "setup")
	data		<- elt(model, "data")

	ndays		<- elt(run, "ndays")
	nyears		<- elt(run, "nyears")

	identifier	<- elt(setup, "model.ident")
	resultsdir	<- elt(setup, "resultsdir")

	physical.parms	<- elt(data, "physical.parameters")

	# extract physical parameters:
	so_depth		<- elt(physical.parms, "so_depth")
	d_depth			<- elt(physical.parms, "d_depth")
	si_depth		<- elt(physical.parms, "si_depth")
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
	x_area_d1		<- elt(physical.parms, "x_area_d1")
	x_area_d2		<- elt(physical.parms, "x_area_d2")
	x_area_d3		<- elt(physical.parms, "x_area_d3")
	x_poros_s1		<- elt(physical.parms, "x_poros_s1")
	x_poros_s2		<- elt(physical.parms, "x_poros_s2")
	x_poros_s3		<- elt(physical.parms, "x_poros_s3")
	x_poros_d1		<- elt(physical.parms, "x_poros_d1")
	x_poros_d2		<- elt(physical.parms, "x_poros_d2")
	x_poros_d3		<- elt(physical.parms, "x_poros_d3")
	x_shallowprop		<- elt(physical.parms, "x_shallowprop")

	# extract output:
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
	x_ammonia		<- elt(aggregates, "x_ammonia")
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

