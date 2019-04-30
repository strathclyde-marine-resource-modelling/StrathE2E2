#
# build_model_parameters.R
#
#' Build entire set of model parameters combining fixed, physical, fleet model output, plus some biomass thresholds
#'
#' @param model model object
#' @param fleet.model.output fleet model output
#'
#' @return model parameter vector
#'
#' @export
#
build_model_parameters <- function(model, fleet.model.output) {

	# ZZ can you just add fitted.parms/fixed.parms/uptakes vectors to model parms vector build? i.e. just concat them?
	# ZZ you would have to reorder the definitions to allow use of physical.parms directly (and some name changes?), and there would be some unused stuff going through to C model
	# Unpack:
	data		<- el(model, "data")
	fixed.parms	<- el(data, "fixed.parameters")
	xprotect_PF	<- el(fixed.parms, "xprotect_PF")	# used in calculations
	xprotect_DF	<- el(fixed.parms, "xprotect_DF")
	xprotect_MF	<- el(fixed.parms, "xprotect_MF")
	xprotect_SB	<- el(fixed.parms, "xprotect_SB")
	xprotect_CB	<- el(fixed.parms, "xprotect_CB")
	xprotect_CZ	<- el(fixed.parms, "xprotect_CZ")
	xprotect_BD	<- el(fixed.parms, "xprotect_BD")
	xprotect_SL	<- el(fixed.parms, "xprotect_SL")
	xprotect_CT	<- el(fixed.parms, "xprotect_CT")
	xprotect_KP	<- el(fixed.parms, "xprotect_KP")
	CZ_inedible	<- el(fixed.parms, "CZ_inedible")

	physical.parms	<- el(data, "physical.parameters")
	x_shallowprop	<- el(physical.parms, "x_shallowprop")

	fleet.vector	<- el(fleet.model.output, "fleet_vector")

	fitted.parms	<- el(data, "fitted.parameters")
	uptakes		<- el(data, "uptakes")

	#--------------------------------------------------------------------------
	#Load all the parameter values into a single vector which will be passed to the model function.

	model.parameters <- c(
	
		#First the parameters for morphological and physical configuration of the model
		#These occupy elements 1:55 of parms
		thik_so			= el(physical.parms, "so_depth"),
		thik_d			= el(physical.parms, "d_depth"),
		thik_si			= el(physical.parms, "si_depth"),
		thik_b			= el(physical.parms, "bx_depth"),
	
		shallowprop		= x_shallowprop,
		#	volume_s=xs_volume,
		#	volume_d=xd_volume,
	
		area_s0			= el(physical.parms, "x_area_s0"),
		area_s1			= el(physical.parms, "x_area_s1"),
		area_s2			= el(physical.parms, "x_area_s2"),
		area_s3			= el(physical.parms, "x_area_s3"),

		area_d0			= el(physical.parms, "x_area_d0"),
		area_d1			= el(physical.parms, "x_area_d1"),
		area_d2			= el(physical.parms, "x_area_d2"),
		area_d3			= el(physical.parms, "x_area_d3"),
	
		rock_s1			= el(physical.parms, "x_rock_s1"),
		rock_s2			= el(physical.parms, "x_rock_s2"),
		rock_s3			= el(physical.parms, "x_rock_s3"),
		rock_d1			= el(physical.parms, "x_rock_d1"),
		rock_d2			= el(physical.parms, "x_rock_d2"),
		rock_d3			= el(physical.parms, "x_rock_d3"),
	
		nonrock_s		= el(physical.parms, "x_nonrock_s"),
		nonrock_d		= el(physical.parms, "x_nonrock_d"),
	
		thik_x_s1		= el(physical.parms, "x_depth_s1"),
		thik_x_s2		= el(physical.parms, "x_depth_s2"),
		thik_x_s3		= el(physical.parms, "x_depth_s3"),
		thik_x_d1		= el(physical.parms, "x_depth_d1"),
		thik_x_d2		= el(physical.parms, "x_depth_d2"),
		thik_x_d3		= el(physical.parms, "x_depth_d3"),

		porosity_s1		= el(physical.parms, "x_poros_s1"),
		porosity_s2		= el(physical.parms, "x_poros_s2"),
		porosity_s3		= el(physical.parms, "x_poros_s3"),
		porosity_d1		= el(physical.parms, "x_poros_d1"),
		porosity_d2		= el(physical.parms, "x_poros_d2"),
		porosity_d3		= el(physical.parms, "x_poros_d3"),
	
		sed_wat_dif_s1		= el(physical.parms, "Kxw_s1"),
		sed_wat_dif_s2		= el(physical.parms, "Kxw_s2"),
		sed_wat_dif_s3		= el(physical.parms, "Kxw_s3"),
		sed_wat_dif_d1		= el(physical.parms, "Kxw_d1"),
		sed_wat_dif_d2		= el(physical.parms, "Kxw_d2"),
		sed_wat_dif_d3		= el(physical.parms, "Kxw_d3"),
	
		#Reference value of log10(Kxw) for estimating habitat specific sediment geochemistry rates
		sed_ref_Kxw		= el(physical.parms, "ref_Kxw"),
	
		#Bioturbation depth and erosion depths of the sediment as proportions of active sediment layer thickness
		bioturb_depth_s1	= el(physical.parms, "xbioturb_depth_s1"),
		bioturb_depth_s2	= el(physical.parms, "xbioturb_depth_s2"),
		bioturb_depth_s3	= el(physical.parms, "xbioturb_depth_s3"),
		bioturb_depth_d1	= el(physical.parms, "xbioturb_depth_d1"),
		bioturb_depth_d2	= el(physical.parms, "xbioturb_depth_d2"),
		bioturb_depth_d3	= el(physical.parms, "xbioturb_depth_d3"),
	
		erosion_depth_s1	= el(physical.parms, "xerosion_depth_s1"),
		erosion_depth_s2	= el(physical.parms, "xerosion_depth_s2"),
		erosion_depth_s3	= el(physical.parms, "xerosion_depth_s3"),
		erosion_depth_d1	= el(physical.parms, "xerosion_depth_d1"),
		erosion_depth_d2	= el(physical.parms, "xerosion_depth_d2"),
		erosion_depth_d3	= el(physical.parms, "xerosion_depth_d3"),
	
		lightSPM_intercept	= el(physical.parms, "xlightSPM_intercept"),
		lightSPM_slope		= el(physical.parms, "xlightSPM_slope"),

		inshore_phyt_prop_depth	= el(physical.parms, "xinshore_phyt_prop_depth"),

		inshore_kelp_prop_depth	= el(physical.parms, "xinshore_kelp_prop_depth"), 

		# parms so far: 56

		#------ Parameters transferred in from the fishing fleet model ----------
		# The fleet vector has 250 elements, so
		# these occupy elemnts 57:307 of the parms vector
	
		#Vector of parameters from the fishing fleet configuration model
		fleet.vector,
	
		#----- Ecological model parameters -------
	
		#Q10 temperature coefficients for heteroptophs, autotrophs and reference temperature
		qtena			= el(fixed.parms, "qtenauto"),
		qtenh			= el(fixed.parms, "qtenhetero"),
		qtenm			= el(fixed.parms, "qtenmetabol"),
		qtenr			= el(fixed.parms, "qtenrreft"),
	
		#Light saturation parameter for phytoplankton N uptake
		Lmaxup_phyt		= el(fixed.parms, "satlight_phyt"),

		#Light saturation parameter for kelp C uptake
		Lmaxup_kelp		= el(fixed.parms, "satlight_kelp"),

		#Maximum and minimum values of NC ratio in kelp
		NCmax_kelp		= el(fixed.parms, "kelp_ncmax"),
		NCmin_kelp		= el(fixed.parms, "kelp_ncmin"),

		#Then the fitted parameters

		#Wave dependent beach-case rate parameter for kelp_debris
		wave_beach_kelpdebris	= el(fitted.parms, "xwave_kelpdebris"),

		#Maximum uptake rate of carbon by kelp
		umaxC_kelp		= el(fitted.parms, "uC_kelp"),

		#Density dependent carbon exudation rate parameter for kelp
		exudeC_kelp		= el(fitted.parms, "ddexudC_kelp"),

		#Density dependent self shading paramater for carbon uptake by kelp
		selfshade_kelp		= el(fitted.parms, "xkelpshade"),

		#Nutrient uptake by kelp at the reference temperature
		uNIT_kelpt		= el(uptakes, "uNIT_kelpt"),
		hsNIT_kelp		= el(uptakes, "hsNIT_kelp"),
		uAMM_kelpt		= el(uptakes, "uAMM_kelpt"),
		hsAMM_kelp		= el(uptakes, "hsAMM_kelp"),

		#Nutrient uptake by phytoplankton at the reference temperature
		uNIT_phytt		= el(uptakes, "uNIT_phytt"),
		hsNIT_phyt		= el(uptakes, "hsNIT_phyt"),
		uAMM_phytt		= el(uptakes, "uAMM_phytt"),
		hsAMM_phyt		= el(uptakes, "hsAMM_phyt"),
	
		#Feeding by mesozooplankton at the reference temperature
		uphyt_herbt		= el(uptakes, "uphyt_herbt"),
		hsphyt_herb		= el(uptakes, "hsphyt_herb"),
		udet_herbt		= el(uptakes, "udet_herbt"),
		hsdet_herb		= el(uptakes, "hsdet_herb"),
		ubenthslar_herbt	= el(uptakes, "ubenthslar_herbt"),
		hsbenthslar_herb	= el(uptakes, "hsbenthslar_herb"),
		ubenthclar_herbt	= el(uptakes, "ubenthclar_herbt"),
		hsbenthclar_herb	= el(uptakes, "hsbenthclar_herb"),
	
		#Feeding by carnivorous zooplankton at the reference temperature
		uherb_carnt		= el(uptakes, "uherb_carnt"),
		hsherb_carn		= el(uptakes, "hsherb_carn"),
		ubenthslar_carnt	= el(uptakes, "ubenthslar_carnt"),
		hsbenthslar_carn	= el(uptakes, "hsbenthslar_carn"),
		ubenthclar_carnt	= el(uptakes, "ubenthclar_carnt"),
		hsbenthclar_carn	= el(uptakes, "hsbenthclar_carn"),
		ufishplar_carnt		= el(uptakes, "ufishplar_carnt"),
		hsfishplar_carn		= el(uptakes, "hsfishplar_carn"),
		ufishdlar_carnt		= el(uptakes, "ufishdlar_carnt"),
		hsfishdlar_carn		= el(uptakes, "hsfishdlar_carn"),
	
		#Feeding by larvae of pelagic fish at the reference temperature
		uherb_fishplart		= el(uptakes, "uherb_fishplart"),
		hsherb_fishplar		= el(uptakes, "hsherb_fishplar"),
		ubenthslar_fishplart	= el(uptakes, "ubenthslar_fishplart"),
		hsbenthslar_fishplar	= el(uptakes, "hsbenthslar_fishplar"),
		ubenthclar_fishplart	= el(uptakes, "ubenthclar_fishplart"),
		hsbenthclar_fishplar	= el(uptakes, "hsbenthclar_fishplar"),
	
		#Feeding by pelagic fish at the reference temperature
		uherb_fishpt		= el(uptakes, "uherb_fishpt"),
		hsherb_fishp		= el(uptakes, "hsherb_fishp"),
		ucarn_fishpt		= el(uptakes, "ucarn_fishpt"),
		hscarn_fishp		= el(uptakes, "hscarn_fishp"),
		ubenthslar_fishpt	= el(uptakes, "ubenthslar_fishpt"),
		hsbenthslar_fishp	= el(uptakes, "hsbenthslar_fishp"),
		ubenthclar_fishpt	= el(uptakes, "ubenthclar_fishpt"),
		hsbenthclar_fishp	= el(uptakes, "hsbenthclar_fishp"),
		ufishdlar_fishpt	= el(uptakes, "ufishdlar_fishpt"),
		hsfishdlar_fishp	= el(uptakes, "hsfishdlar_fishp"),
		ufishplar_fishpt	= el(uptakes, "ufishplar_fishpt"),
		hsfishplar_fishp	= el(uptakes, "hsfishplar_fishp"),
	
		#Feeding by migratory fish at the reference temperature
		uherb_fishmt		= el(uptakes, "uherb_fishmt"),
		hsherb_fishm		= el(uptakes, "hsherb_fishm"),
		ucarn_fishmt		= el(uptakes, "ucarn_fishmt"),
		hscarn_fishm		= el(uptakes, "hscarn_fishm"),
		ubenthslar_fishmt	= el(uptakes, "ubenthslar_fishmt"),
		hsbenthslar_fishm	= el(uptakes, "hsbenthslar_fishm"),
		ubenthclar_fishmt	= el(uptakes, "ubenthclar_fishmt"),
		hsbenthclar_fishm	= el(uptakes, "hsbenthclar_fishm"),
		ufishdlar_fishmt	= el(uptakes, "ufishdlar_fishmt"),
		hsfishdlar_fishm	= el(uptakes, "hsfishdlar_fishm"),
		ufishplar_fishmt	= el(uptakes, "ufishplar_fishmt"),
		hsfishplar_fishm	= el(uptakes, "hsfishplar_fishm"),
	
		#Feeding by larvae of demersal fish at the reference temperature
		uherb_fishdlart		= el(uptakes, "uherb_fishdlart"),
		hsherb_fishdlar		= el(uptakes, "hsherb_fishdlar"),
		ubenthslar_fishdlart	= el(uptakes, "ubenthslar_fishdlart"),
		hsbenthslar_fishdlar	= el(uptakes, "hsbenthslar_fishdlar"),
		ubenthclar_fishdlart	= el(uptakes, "ubenthclar_fishdlart"),
		hsbenthclar_fishdlar	= el(uptakes, "hsbenthclar_fishdlar"),
	
		#Feeding by demersal fish at the reference temperature
		ucarn_fishdt		= el(uptakes, "ucarn_fishdt"),
		hscarn_fishd		= el(uptakes, "hscarn_fishd"),
		ubenths_fishdt		= el(uptakes, "ubenths_fishdt"),
		hsbenths_fishd		= el(uptakes, "hsbenths_fishd"),
		ubenthc_fishdt		= el(uptakes, "ubenthc_fishdt"),
		hsbenthc_fishd		= el(uptakes, "hsbenthc_fishd"),
		ufishplar_fishdt	= el(uptakes, "ufishplar_fishdt"),
		hsfishplar_fishd	= el(uptakes, "hsfishplar_fishd"),
		ufishdlar_fishdt	= el(uptakes, "ufishdlar_fishdt"),
		hsfishdlar_fishd	= el(uptakes, "hsfishdlar_fishd"),
		ufishp_fishdt		= el(uptakes, "ufishp_fishdt"),
		hsfishp_fishd		= el(uptakes, "hsfishp_fishd"),
		ufishm_fishdt		= el(uptakes, "ufishm_fishdt"),
		hsfishm_fishd		= el(uptakes, "hsfishm_fishd"),
		ufishd_fishdt		= el(uptakes, "ufishd_fishdt"),
		hsfishd_fishd		= el(uptakes, "hsfishd_fishd"),
		udisc_fishdt		= el(uptakes, "udisc_fishdt"),
		hsdisc_fishd		= el(uptakes, "hsdisc_fishd"),
		ucorp_fishdt		= el(uptakes, "ucorp_fishdt"),
		hscorp_fishd		= el(uptakes, "hscorp_fishd"),
		
		#Feeding by larvae of suspension feeding benthos at the reference temperature
		uphyt_benthslart	= el(uptakes, "uphyt_benthslart"),
		hsphyt_benthslar	= el(uptakes, "hsphyt_benthslar"),
		udet_benthslart		= el(uptakes, "udet_benthslart"),
		hsdet_benthslar		= el(uptakes, "hsdet_benthslar"),
	
		#Feeding by larvae of carnivore/scavenge feeding benthos at the reference temperature
		uphyt_benthclart	= el(uptakes, "uphyt_benthclart"),
		hsphyt_benthclar	= el(uptakes, "hsphyt_benthclar"),
		udet_benthclart		= el(uptakes, "udet_benthclart"),
		hsdet_benthclar		= el(uptakes, "hsdet_benthclar"),
	
		#Feeding by suspension feeding benthos at the reference temperature
		uphyt_benthst		= el(uptakes, "uphyt_benthst"),
		hsphyt_benths		= el(uptakes, "hsphyt_benths"),
		udet_benthst		= el(uptakes, "udet_benthst"),
		hsdet_benths		= el(uptakes, "hsdet_benths"),
		used_benthst		= el(uptakes, "used_benthst"),
		hssed_benths		= el(uptakes, "hssed_benths"),
	
		#Feeding by carnivorous benthos at the reference temperature
		ubenths_benthct		= el(uptakes, "ubenths_benthct"),
		hsbenths_benthc		= el(uptakes, "hsbenths_benthc"),
		ukelp_benthct		= el(uptakes, "ukelp_benthct"),
		hskelp_benthc		= el(uptakes, "hskelp_benthc"),
		ukelpdebris_benthct	= el(uptakes, "ukelpdebris_benthct"),
		hskelpdebris_benthc	= el(uptakes, "hskelpdebris_benthc"),
		ucorp_benthct		= el(uptakes, "ucorp_benthct"),
		hscorp_benthc		= el(uptakes, "hscorp_benthc"),
	
		#Feeding by birds and mammals - temperature independent
	
		##uherb_bird		= el(uptakes, "uherb_bird"),
		##hsherb_bird		= el(uptakes, "hsherb_bird"),
		ucarn_bird		= el(uptakes, "ucarn_bird"),
		hscarn_bird		= el(uptakes, "hscarn_bird"),
		ubenths_bird		= el(uptakes, "ubenths_bird"),
		hsbenths_bird		= el(uptakes, "hsbenths_bird"),
		ubenthc_bird		= el(uptakes, "ubenthc_bird"),
		hsbenthc_bird		= el(uptakes, "hsbenthc_bird"),
	
		ufishp_bird		= el(uptakes, "ufishp_bird"),
		hsfishp_bird		= el(uptakes, "hsfishp_bird"),
		ufishm_bird		= el(uptakes, "ufishm_bird"),
		hsfishm_bird		= el(uptakes, "hsfishm_bird"),
		ufishd_bird		= el(uptakes, "ufishd_bird"),
		hsfishd_bird		= el(uptakes, "hsfishd_bird"),
		udisc_bird		= el(uptakes, "udisc_bird"),
		hsdisc_bird		= el(uptakes, "hsdisc_bird"),
		ucorp_bird		= el(uptakes, "ucorp_bird"),
		hscorp_bird		= el(uptakes, "hscorp_bird"),
	
		bda_par_bird		= el(fitted.parms, "bda_par_bird"),

		ucarn_seal		= el(uptakes, "ucarn_seal"),
		hscarn_seal		= el(uptakes, "hscarn_seal"),
		ubenths_seal		= el(uptakes, "ubenths_seal"),
		hsbenths_seal		= el(uptakes, "hsbenths_seal"),
		ubenthc_seal		= el(uptakes, "ubenthc_seal"),
		hsbenthc_seal		= el(uptakes, "hsbenthc_seal"),

		ufishp_seal		= el(uptakes, "ufishp_seal"),
		hsfishp_seal		= el(uptakes, "hsfishp_seal"),
		ufishm_seal		= el(uptakes, "ufishm_seal"),
		hsfishm_seal		= el(uptakes, "hsfishm_seal"),
		ufishd_seal		= el(uptakes, "ufishd_seal"),
		hsfishd_seal		= el(uptakes, "hsfishd_seal"),

		ubird_seal		= el(uptakes, "ubird_seal"),
		hsbird_seal		= el(uptakes, "hsbird_seal"),

		udisc_seal		= el(uptakes, "udisc_seal"),
		hsdisc_seal		= el(uptakes, "hsdisc_seal"),
		ucorp_seal		= el(uptakes, "ucorp_seal"),
		hscorp_seal		= el(uptakes, "hscorp_seal"),

		bdapar_seal		= el(uptakes, "bdapar_seal"),

		uherb_ceta		= el(uptakes, "uherb_ceta"),
		hsherb_ceta		= el(uptakes, "hsherb_ceta"),
		ucarn_ceta		= el(uptakes, "ucarn_ceta"),
		hscarn_ceta		= el(uptakes, "hscarn_ceta"),
		ubenths_ceta		= el(uptakes, "ubenths_ceta"),
		hsbenths_ceta		= el(uptakes, "hsbenths_ceta"),
		ubenthc_ceta		= el(uptakes, "ubenthc_ceta"),
		hsbenthc_ceta		= el(uptakes, "hsbenthc_ceta"),

		ufishp_ceta		= el(uptakes, "ufishp_ceta"),
		hsfishp_ceta		= el(uptakes, "hsfishp_ceta"),
		ufishm_ceta		= el(uptakes, "ufishm_ceta"),
		hsfishm_ceta		= el(uptakes, "hsfishm_ceta"),
		ufishd_ceta		= el(uptakes, "ufishd_ceta"),
		hsfishd_ceta		= el(uptakes, "hsfishd_ceta"),

		ubird_ceta		= el(uptakes, "ubird_ceta"),
		hsbird_ceta		= el(uptakes, "hsbird_ceta"),
		useal_ceta		= el(uptakes, "useal_ceta"),
		hsseal_ceta		= el(uptakes, "hsseal_ceta"),

		udisc_ceta		= el(uptakes, "udisc_ceta"),
		hsdisc_ceta		= el(uptakes, "hsdisc_ceta"),
		## ucorp_ceta=u_ceta*PREF_corp_ceta,hscorp_ceta=h_ceta,

		bdapar_ceta		= el(uptakes, "bdapar_ceta"),

		#Proportion of consumption converted to growth - temperature independent
		aH			= el(fixed.parms, "asimH"),
		aC			= el(fixed.parms, "asimC"),
		aBslar			= el(fixed.parms, "asimBslar"),
		aBclar			= el(fixed.parms, "asimBclar"),
		aBs			= el(fixed.parms, "asimBs"),
		aBc			= el(fixed.parms, "asimBc"),
		aFplar			= el(fixed.parms, "asimFplar"),
		aFdlar			= el(fixed.parms, "asimFdlar"),
		aFp			= el(fixed.parms, "asimFp"),
		aFm			= el(fixed.parms, "asimFm"),
		aFd			= el(fixed.parms, "asimFd"),
		abird			= el(fixed.parms, "asimbird"),
		aseal			= el(fixed.parms, "asimseal"),
		aceta			= el(fixed.parms, "asimceta"),
	
		#Background proportion of biomass converted to ammonia per day at the reference temperature (birds and mammals T independent)
		eHt			= el(fixed.parms, "excrHt"),
		eCt			= el(fixed.parms, "excrCt"),
		eBslart			= el(fixed.parms, "excrBslart"),
		eBclart			= el(fixed.parms, "excrBclart"),
		eBst			= el(fixed.parms, "excrBst"),
		eBct			= el(fixed.parms, "excrBct"),
		eFplart			= el(fixed.parms, "excrFplart"),
		eFdlart			= el(fixed.parms, "excrFdlart"),
		eFpt			= el(fixed.parms, "excrFpt"),
		eFmt			= el(fixed.parms, "excrFmt"),
		eFdt			= el(fixed.parms, "excrFdt"),
		ebirdt			= el(fixed.parms, "excrbird"),
		esealt			= el(fixed.parms, "excrseal"),
		ecetat			= el(fixed.parms, "excrceta"),
	
		#Mineralisation, nitrification and denitrification raees per day at the reference temperature
		xmt			= el(fitted.parms, "xmt"),
		xnst			= el(fitted.parms, "xnst"),
		xdst			= el(fitted.parms, "xdst"),
		xndt			= el(fitted.parms, "xndt"),
		xddt			= el(fitted.parms, "xddt"),
	
		xqs_p1			= el(fitted.parms, "xqs_p1"),
		xqs_p2			= el(fitted.parms, "xqs_p2"),
		xqs_p3			= el(fitted.parms, "xqs_p3"),
	
		xmsedt			= el(fitted.parms, "xmsedt"),
		xmsens			= el(fitted.parms, "xmsens"),
	
		xnsedt			= el(fitted.parms, "xnsedt"),
		xnsens			= el(fitted.parms, "xnsens"),
	
		xdsedt			= el(fitted.parms, "xdsedt"),
		xdsens			= el(fitted.parms, "xdsens"),
	
		#Density and wave dependent destruction rate paramater for kelp
		xwave_kelp		= el(fitted.parms, "xxwave_kelp"),

		#Death rates of phytoplankton at the reference temperature
		xxst			= el(fitted.parms, "xxst"),
		xxdt			= el(fitted.parms, "xxdt"),
	
		#Death rate per unit biomass for carnivores fish birds and mammals
		xxherb			= el(fitted.parms, "xxherb"),
		xxcarn			= el(fitted.parms, "xxcarn"),
		xxbenthslar		= el(fitted.parms, "xxbenthslar"),
		xxbenthclar		= el(fitted.parms, "xxbenthclar"),
		xxbenths		= el(fitted.parms, "xxbenths"),
		xxbenthc		= el(fitted.parms, "xxbenthc"),
		xxpfishlar		= el(fitted.parms, "xxpfishlar"),
		xxdfishlar		= el(fitted.parms, "xxdfishlar"),
		xxpfish			= el(fitted.parms, "xxpfish"),
		xxmfish			= el(fitted.parms, "xxmfish"),
		xxdfish			= el(fitted.parms, "xxdfish"),
		xxbird			= el(fitted.parms, "xxbird"),
		xxseal			= el(fitted.parms, "xxseal"),
		xxceta			= el(fitted.parms, "xxceta"),

		#Proportion of kelp debris becoming detritus per day
		kelpdebris_det		= el(fitted.parms, "xkelpdebris_det"),

		#Proportion of seabed corpses becoming seabed detritus per day
		xxcorp_det		= el(fitted.parms, "xxcorp_det"),
	
		#Proportion of discarded fish sinking to become seabed corpses per day
		xdisc_corp		= el(fitted.parms, "xdisc_corp"),
	
		#deleted
		#Bioturbation scaling factor for deposit feeding benthos - scales xdet uptake/xdet to fraction of sedimnent excavated
		#	xbioturb=xxbioturb,
	
		#Sinking rates for detritus in the surface and deep layers
		#	dsink_s=xdsink_s,dsink_d_Klow=xdsink_d_Klow,dsink_d_Khi=xdsink_d_Khi,
		xdsink_s			= el(fitted.parms, "xdsink_s"),
		xdsink_d			= el(fitted.parms, "xdsink_d"),
	
		#Fitting parameter for scaling between whole model region demersal fish N mass and the survey
		#index on which the empirical relationships for pNQ and dsacrad ates of Q and NQ species are based.
		#Expect this to be abouut 2
		xdfdp			= el(fitted.parms, "xdfdp"),
	
		#Fitted parameters for food gradient dependent fish migration rates
		xpfish_migcoef		= el(fitted.parms, "xpfish_migcoef"),
		xmfish_migcoef		= el(fitted.parms, "xmfish_migcoef"),
		xdfish_migcoef		= el(fitted.parms, "xdfish_migcoef"),
		xbird_migcoef		= el(fitted.parms, "xbird_migcoef"),
		xseal_migcoef		= el(fitted.parms, "xseal_migcoef"),
		xceta_migcoef		= el(fitted.parms, "xceta_migcoef"),
	
		#Protected biomass thresholds
		protect_PF_o = xprotect_PF * (1-x_shallowprop),
		protect_DF_o = xprotect_DF * (1-x_shallowprop),
		protect_MF_o = xprotect_MF * (1-x_shallowprop),
		protect_SB_o = xprotect_SB * (1-x_shallowprop),
		protect_CB_o = xprotect_CB * (1-x_shallowprop),
		protect_CZ_o = xprotect_CZ * (1-x_shallowprop),
		protect_BD_o = xprotect_BD * (1-x_shallowprop),
		protect_SL_o = xprotect_SL * (1-x_shallowprop),
		protect_CT_o = xprotect_CT * (1-x_shallowprop),
	
		protect_PF_i = xprotect_PF * (x_shallowprop),
		protect_DF_i = xprotect_DF * (x_shallowprop),
		protect_MF_i = xprotect_MF * (x_shallowprop),
		protect_SB_i = xprotect_SB * (x_shallowprop),
		protect_CB_i = xprotect_CB * (x_shallowprop),
		protect_CZ_i = xprotect_CZ * (x_shallowprop),
		protect_BD_i = xprotect_BD * (x_shallowprop),
		protect_SL_i = xprotect_SL * (x_shallowprop),
		protect_CT_i = xprotect_CT * (x_shallowprop),
		protect_KP_i = xprotect_KP,
	
		xmax_exploitable_f_PF		= el(fitted.parms, "xmax_exploitable_f_PF"),
		xmax_exploitable_f_DF		= el(fitted.parms, "xmax_exploitable_f_DF"),
		xmax_exploitable_f_MF		= el(fitted.parms, "xmax_exploitable_f_MF"),
		xmax_exploitable_f_SB		= el(fitted.parms, "xmax_exploitable_f_SB"),
		xmax_exploitable_f_CB		= el(fitted.parms, "xmax_exploitable_f_CB"),
		xmax_exploitable_f_CZ		= el(fitted.parms, "xmax_exploitable_f_CZ"),
		xmax_exploitable_f_BD		= el(fitted.parms, "xmax_exploitable_f_BD"),
		xmax_exploitable_f_SL		= el(fitted.parms, "xmax_exploitable_f_SL"),
		xmax_exploitable_f_CT		= el(fitted.parms, "xmax_exploitable_f_CT"),
		xmax_exploitable_f_KP		= el(fitted.parms, "xmax_exploitable_f_KP"),

		PF_fec				= el(fixed.parms, "P_an_fec"),			# annual weight specific fecundities added to the end of the paramer list
		DF_fec				= el(fixed.parms, "D_an_fec"),
		BS_fec				= el(fixed.parms, "BS_an_fec"),
		BC_fec				= el(fixed.parms, "BC_an_fec"),

		CZ_inedible_biomass_o		= CZ_inedible * (1-x_shallowprop),		# residual inedible biomass of carnzoo offshore
		CZ_inedible_biomass_i		= CZ_inedible * (x_shallowprop)			# residual inedible biomass of carnzoo inshore
	)

	model.parameters
} 
