#
# perturb_parameters.R
#
#' perturb parameters a bit
#'
#' @param datastore parameters to be perturbed
#' @param annealing.parms annealing paramters
#'
#' @return perturbed parameter vector
#'
#' @export
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subroutine to load and set the parameter values to be use din the model run
#
# Parameters which have been established by the simulated annealing scheme are
# loaded from a csv file which is made from the last line of the 'accepted parameters'
# file produced by the annealing programme.
#
# A variety of other parameters which are not optimised by simulated annealing are
# hard-wired in this subroutine
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# VECTORS BUILT UP:
#	prefstore	~ 67 pars
#	ustore		~ 13 pars
#	hstore		~ 14 pars
#	biogeostore	~ 18 pars
#	mortstore	~ 14 pars
#	reststore	~ 14 pars
#	parms		~ 270 pars
#
#In this bit of code the paraeters can be varied by a random value drawen froma Gaussian distribution. The sd of the
#distribution is constant across groups of parameters.
#Intended for use in MCMC
#
perturb_parameters <- function(datastore, annealing.parms=list()) {

	# Unpack:
	#model.path		<- el(model, "path")

	#data			<- el(model, "data")

	#fixed.parms		<- el(data, "fixed.parameters")
	#asimH			<- el(fixed.parms, "asimH")
	#asimC			<- el(fixed.parms, "asimC")
	#asimBslar		<- el(fixed.parms, "asimBslar")
	#asimBclar		<- el(fixed.parms, "asimBclar")
	#asimBs			<- el(fixed.parms, "asimBs")
	#asimBc			<- el(fixed.parms, "asimBc")
	#asimFplar		<- el(fixed.parms, "asimFplar")
	#asimFdlar		<- el(fixed.parms, "asimFdlar")
	#asimFp			<- el(fixed.parms, "asimFp")
	#asimFm			<- el(fixed.parms, "asimFm")
	#asimFd			<- el(fixed.parms, "asimFd")
	#asimbird		<- el(fixed.parms, "asimbird")
	#excrHt			<- el(fixed.parms, "excrHt")
	##excrCt			<- el(fixed.parms, "excrCt")
	#excrBslart		<- el(fixed.parms, "excrBslart")
	#excrBclart		<- el(fixed.parms, "excrBclart")
	#excrBst			<- el(fixed.parms, "excrBst")
	#excrBct			<- el(fixed.parms, "excrBct")
	#excrFplart		<- el(fixed.parms, "excrFplart")
	#excrFdlart		<- el(fixed.parms, "excrFdlart")
	#excrFpt			<- el(fixed.parms, "excrFpt")
	#excrFmt			<- el(fixed.parms, "excrFmt")
	#excrFdt			<- el(fixed.parms, "excrFdt")
	#excrbird		<- el(fixed.parms, "excrbird")
	#xprotect_PF		<- el(fixed.parms, "xprotect_PF")
	#xprotect_DF		<- el(fixed.parms, "xprotect_DF")
	#xprotect_MF		<- el(fixed.parms, "xprotect_MF")
	#xprotect_SB		<- el(fixed.parms, "xprotect_SB")
	#xprotect_CB		<- el(fixed.parms, "xprotect_CB")
	#xprotect_CZ		<- el(fixed.parms, "xprotect_CZ")
	#xprotect_BM		<- el(fixed.parms, "xprotect_BM")
	#satlight		<- el(fixed.parms, "satlight")
	#qtenauto		<- el(fixed.parms, "qtenauto")
	#qtenhetero		<- el(fixed.parms, "qtenhetero")
	#qtenmetabol		<- el(fixed.parms, "qtenmetabol")
	#qtenrreft		<- el(fixed.parms, "qtenrreft")
#
	#physical.parms		<- el(data, "physical.parameters")
	#so_depth		<- el(physical.parms, "so_depth")
	#d_depth			<- el(physical.parms, "d_depth")
	#si_depth		<- el(physical.parms, "si_depth")
	#bx_depth		<- el(physical.parms, "bx_depth")
	#x_area_s1		<- el(physical.parms, "x_area_s1")
	#x_area_s2		<- el(physical.parms, "x_area_s2")
	#x_area_s3		<- el(physical.parms, "x_area_s3")
	#x_area_d1		<- el(physical.parms, "x_area_d1")
	#x_area_d2		<- el(physical.parms, "x_area_d2")
	#x_area_d3		<- el(physical.parms, "x_area_d3")
	#x_rock_s1		<- el(physical.parms, "x_rock_s1")
	#x_rock_s2		<- el(physical.parms, "x_rock_s2")
	#x_rock_s3		<- el(physical.parms, "x_rock_s3")
	#x_rock_d1		<- el(physical.parms, "x_rock_d1")
	#x_rock_d2		<- el(physical.parms, "x_rock_d2")
	#x_rock_d3		<- el(physical.parms, "x_rock_d3")
	#x_nonrock_s		<- el(physical.parms, "x_nonrock_s")
	#x_nonrock_d		<- el(physical.parms, "x_nonrock_d")
	#x_depth_s1		<- el(physical.parms, "x_depth_s1")
	#x_depth_s2		<- el(physical.parms, "x_depth_s2")
	#x_depth_s3		<- el(physical.parms, "x_depth_s3")
	#x_depth_d1		<- el(physical.parms, "x_depth_d1")
	#x_depth_d2		<- el(physical.parms, "x_depth_d2")
	#x_depth_d3		<- el(physical.parms, "x_depth_d3")
	#x_poros_s1		<- el(physical.parms, "x_poros_s1")
	#x_poros_s2		<- el(physical.parms, "x_poros_s2")
	#x_poros_s3		<- el(physical.parms, "x_poros_s3")
	#x_poros_d1		<- el(physical.parms, "x_poros_d1")
	#x_poros_d2		<- el(physical.parms, "x_poros_d2")
	#x_poros_d3		<- el(physical.parms, "x_poros_d3")
	#Kxw_s1			<- el(physical.parms, "Kxw_s1")
	#Kxw_s2			<- el(physical.parms, "Kxw_s2")
	#Kxw_s3			<- el(physical.parms, "Kxw_s3")
	#Kxw_d1			<- el(physical.parms, "Kxw_d1")
	#Kxw_d2			<- el(physical.parms, "Kxw_d2")
	#Kxw_d3			<- el(physical.parms, "Kxw_d3")
	#ref_Kxw			<- el(physical.parms, "ref_Kxw")
	#xbioturb_depth_s1	<- el(physical.parms, "xbioturb_depth_s1")
	#xbioturb_depth_s2	<- el(physical.parms, "xbioturb_depth_s2")
	#xbioturb_depth_s3	<- el(physical.parms, "xbioturb_depth_s3")
	#xbioturb_depth_d1	<- el(physical.parms, "xbioturb_depth_d1")
	#xbioturb_depth_d2	<- el(physical.parms, "xbioturb_depth_d2")
	#xbioturb_depth_d3	<- el(physical.parms, "xbioturb_depth_d3")
	#xerosion_depth_s1	<- el(physical.parms, "xerosion_depth_s1")
	#xerosion_depth_s2	<- el(physical.parms, "xerosion_depth_s2")
	#xerosion_depth_s3	<- el(physical.parms, "xerosion_depth_s3")
	#xerosion_depth_d1	<- el(physical.parms, "xerosion_depth_d1")
	#xerosion_depth_d2	<- el(physical.parms, "xerosion_depth_d2")
	#xerosion_depth_d3	<- el(physical.parms, "xerosion_depth_d3")
	#xlightSPM_intercept	<- el(physical.parms, "xlightSPM_intercept")
	#xlightSPM_slope		<- el(physical.parms, "xlightSPM_slope")
	#x_shallowprop		<- el(physical.parms, "x_shallowprop")


	#Set the SDs for the different classes of parameters
	# if the annealing parms are not present then set them to 0.0
	Prefsd			<- el(annealing.parms, "Prefsd", 0.0)		# Preference parameter sd
	u_sd			<- el(annealing.parms, "u_sd", 0.0)		# Maximum uptake rate sd
	h_sd			<- el(annealing.parms, "h_sd", 0.0)		# Half saturation density sd
	biogeo_sd		<- el(annealing.parms, "biogeo_sd", 0.0)	# microbial parameter sd
	mort_sd			<- el(annealing.parms, "mort_sd", 0.0)		# density dependent mortality rate sd
	ressd			<- el(annealing.parms, "ressd", 0.0)		# other parameters sd

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	#Now pick out the stored parameters
	#
	PREF_NIT_phyt		<- el(datastore, "PREF_NIT_phyt")
	PREF_AMM_phyt		<- el(datastore, "PREF_AMM_phyt")

	PREF_phyt_herb		<- el(datastore, "PREF_phyt_herb")
	PREF_det_herb		<- el(datastore, "PREF_det_herb")
	PREF_benthslar_herb	<- el(datastore, "PREF_benthslar_herb")
	PREF_benthclar_herb	<- el(datastore, "PREF_benthclar_herb")

	PREF_herb_carn		<- el(datastore, "PREF_herb_carn")
	PREF_benthslar_carn	<- el(datastore, "PREF_benthslar_carn")
	PREF_benthclar_carn	<- el(datastore, "PREF_benthclar_carn")
	PREF_fishplar_carn	<- el(datastore, "PREF_fishplar_carn")
	PREF_fishdlar_carn	<- el(datastore, "PREF_fishdlar_carn")

	PREF_herb_fishplar	<- el(datastore, "PREF_herb_fishplar")
	PREF_benthslar_fishplar	<- el(datastore, "PREF_benthslar_fishplar")
	PREF_benthclar_fishplar	<- el(datastore, "PREF_benthclar_fishplar")

	PREF_herb_fishp		<- el(datastore, "PREF_herb_fishp")
	PREF_carn_fishp		<- el(datastore, "PREF_carn_fishp")
	PREF_benthslar_fishp	<- el(datastore, "PREF_benthslar_fishp")
	PREF_benthclar_fishp	<- el(datastore, "PREF_benthclar_fishp")
	PREF_fishdlar_fishp	<- el(datastore, "PREF_fishdlar_fishp")
	PREF_fishplar_fishp	<- el(datastore, "PREF_fishplar_fishp")

	PREF_herb_fishm		<- el(datastore, "PREF_herb_fishm")
	PREF_carn_fishm		<- el(datastore, "PREF_carn_fishm")
	PREF_benthslar_fishm	<- el(datastore, "PREF_benthslar_fishm")
	PREF_benthclar_fishm	<- el(datastore, "PREF_benthclar_fishm")
	PREF_fishdlar_fishm	<- el(datastore, "PREF_fishdlar_fishm")
	PREF_fishplar_fishm	<- el(datastore, "PREF_fishplar_fishm")

	PREF_herb_fishdlar	<- el(datastore, "PREF_herb_fishdlar")
	PREF_benthslar_fishdlar	<- el(datastore, "PREF_benthslar_fishdlar")
	PREF_benthclar_fishdlar	<- el(datastore, "PREF_benthclar_fishdlar")

	PREF_carn_fishd		<- el(datastore, "PREF_carn_fishd")
	PREF_benths_fishd	<- el(datastore, "PREF_benths_fishd")
	PREF_benthc_fishd	<- el(datastore, "PREF_benthc_fishd")
	PREF_fishplar_fishd	<- el(datastore, "PREF_fishplar_fishd")
	PREF_fishdlar_fishd	<- el(datastore, "PREF_fishdlar_fishd")
	PREF_fishp_fishd	<- el(datastore, "PREF_fishp_fishd")
	PREF_fishm_fishd	<- el(datastore, "PREF_fishm_fishd")
	PREF_fishd_fishd	<- el(datastore, "PREF_fishd_fishd")
	PREF_disc_fishd		<- el(datastore, "PREF_disc_fishd")
	PREF_corp_fishd		<- el(datastore, "PREF_corp_fishd")

	PREF_phyt_benthslar	<- el(datastore, "PREF_phyt_benthslar")
	PREF_phyt_benthclar	<- el(datastore, "PREF_phyt_benthclar")
	PREF_det_benthslar	<- el(datastore, "PREF_det_benthslar")
	PREF_det_benthclar	<- el(datastore, "PREF_det_benthclar")


	PREF_phyt_benths	<- el(datastore, "PREF_phyt_benths")
	PREF_det_benths		<- el(datastore, "PREF_det_benths")
	PREF_sed_benths		<- el(datastore, "PREF_sed_benths")

	PREF_benths_benthc	<- el(datastore, "PREF_benths_benthc")
	#PREF_benthd_benthc	<- el(datastore, "PREF_benthd_benthc")	ZZ unknown - old par?
	PREF_corp_benthc	<- el(datastore, "PREF_corp_benthc")

	PREF_herb_bird		<- el(datastore, "PREF_herb_bird")
	PREF_carn_bird		<- el(datastore, "PREF_carn_bird")
	PREF_benths_bird	<- el(datastore, "PREF_benths_bird")
	PREF_benthc_bird	<- el(datastore, "PREF_benthc_bird")

	PREF_fishp_bird		<- el(datastore, "PREF_fishp_bird")
	PREF_fishm_bird		<- el(datastore, "PREF_fishm_bird")
	PREF_fishd_bird		<- el(datastore, "PREF_fishd_bird")
	PREF_disc_bird		<- el(datastore, "PREF_disc_bird")
	PREF_corp_bird		<- el(datastore, "PREF_corp_bird")


	#Then we set the rate parameters for each predator at the reference temperature

	u_phyt			<- el(datastore, "u_phyt")
	u_herb			<- el(datastore, "u_herb")
	u_carn			<- el(datastore, "u_carn")
	u_fishplar		<- el(datastore, "u_fishplar")
	u_fishp			<- el(datastore, "u_fishp")
	u_fishm			<- el(datastore, "u_fishm")
	u_fishdlar		<- el(datastore, "u_fishdlar")
	u_fishd			<- el(datastore, "u_fishd")
	u_benthslar		<- el(datastore, "u_benthslar")
	u_benthclar		<- el(datastore, "u_benthclar")
	u_benths		<- el(datastore, "u_benths")
	u_benthc		<- el(datastore, "u_benthc")
	u_bird			<- el(datastore, "u_bird")

	h_phyt			<- el(datastore, "h_phyt")
	h_herb			<- el(datastore, "h_herb")
	h_carn			<- el(datastore, "h_carn")
	h_fishplar		<- el(datastore, "h_fishplar")
	h_fishp			<- el(datastore, "h_fishp")
	h_fishm			<- el(datastore, "h_fishm")
	h_fishdlar		<- el(datastore, "h_fishdlar")
	h_fishd			<- el(datastore, "h_fishd")
	h_benthslar		<- el(datastore, "h_benthslar")
	h_benthclar		<- el(datastore, "h_benthclar")
	h_benths		<- el(datastore, "h_benths")
	h_benthc		<- el(datastore, "h_benthc")
	h_bird			<- el(datastore, "h_bird")


	bda_par_bird		<- el(datastore, "bda_par_bird")


	#Mineralisation, nitrification and denitrification rates per day at the reference temperature
	xmt			<- el(datastore, "xmt")
	xnst			<- el(datastore, "xnst")
	xdst			<- el(datastore, "xdst")
	xndt			<- el(datastore, "xndt")
	xddt			<- el(datastore, "xddt")

	xqs_p1			<- el(datastore, "xqs_p1")	# proportion of detritus which become refractory when minearlised
	xqs_p2			<- el(datastore, "xqs_p2")	# ratio of refratory to labile detritus minearalisation rates
	xqs_p3			<- el(datastore, "xqs_p3")	# proportion of refractory whiuch becomes labile when re-oxygenated

	xmsedt			<- el(datastore, "xmsedt")
	xmsens			<- el(datastore, "xmsens")

	xnsedt			<- el(datastore, "xnsedt")
	xnsens			<- el(datastore, "xnsens")

	xdsedt			<- el(datastore, "xdsedt")
	xdsens			<- el(datastore, "xdsens")


	#Death rates of phytoplankton at the reference temperature
	# IN THIS VERSION (NOV 2015) THESE PARAMATERS ARE APPLIED AS A DENSITY DEPENDENT RATE
	# RATE RTHAN AS A DENSITY INDEPENDENT RATE AS IN THE ORIGINAL MODEL
	xxst			<- el(datastore, "xxst")
	xxdt			<- el(datastore, "xxdt")

	#Death rate of carnivores fish birds and mammals per unit biomass - temperature independent
	xxherb			<- el(datastore, "xxherb")
	xxcarn			<- el(datastore, "xxcarn")
	xxbenthslar		<- el(datastore, "xxbenthslar")
	xxbenthclar		<- el(datastore, "xxbenthclar")
	xxbenths		<- el(datastore, "xxbenths")
	xxbenthc		<- el(datastore, "xxbenthc")
	xxpfishlar		<- el(datastore, "xxpfishlar")
	xxdfishlar		<- el(datastore, "xxdfishlar")
	xxpfish			<- el(datastore, "xxpfish")
	xxmfish			<- el(datastore, "xxmfish")
	xxdfish			<- el(datastore, "xxdfish")
	xxbird			<- el(datastore, "xxbird")
	

	#Proportion of corpse mass converted to detritus per day at the reference temperature
	xxcorp_det		<- el(datastore, "xxcorp_det")

	#Proportion of discards sinking to become seabed corpses per day - temperature independent
	xdisc_corp		<- el(datastore, "xdisc_corp")

	#Sinking rates and their dependence on mixing - temperature independent
	xdsink_s		<- el(datastore, "xdsink_s")
	#	xdsink_d_Klow	<- el(datastore, "xdsink_d_Klow")
	xdsink_d		<- el(datastore, "xdsink_d")


	#Fitting parameter for scaling between whole model region demersal fish N mass and the survey
	#index on which the empirical relationships for pNQ and dsacrad ates of Q and NQ species are based.
	#Expect this to be abouut 2
	xdfdp			<- el(datastore, "xdfdp")


	#Parameters for food gradient migration rates of pelagic and migratory fish
	xpfish_migcoef 		<- el(datastore, "xpfish_migcoef")
	xmfish_migcoef 		<- el(datastore, "xmfish_migcoef")
	xdfish_migcoef 		<- el(datastore, "xdfish_migcoef")
	xbird_migcoef 		<- el(datastore, "xbird_migcoef")


	#Maximum proportions of the stock biomass which is accessible to the fisheries
	#Units proportions
	xmax_exploitable_f_PF	<- el(datastore, "xmax_exploitable_f_PF")
	xmax_exploitable_f_DF	<- el(datastore, "xmax_exploitable_f_DF")
	xmax_exploitable_f_MF	<- el(datastore, "xmax_exploitable_f_MF")
	xmax_exploitable_f_SB	<- el(datastore, "xmax_exploitable_f_SB")
	xmax_exploitable_f_CB	<- el(datastore, "xmax_exploitable_f_CB")
	xmax_exploitable_f_CZ	<- el(datastore, "xmax_exploitable_f_CZ")
	xmax_exploitable_f_BM	<- el(datastore, "xmax_exploitable_f_BM")

	annual_obj		<- el(datastore, "annual_obj")

	#----------------------------------------------------------------------------

	#Now randomise the parameters

	#First we set up the preferences
	PREF_NIT_phytx<-max(0,rnorm(1,PREF_NIT_phyt,Prefsd*PREF_NIT_phyt))
	PREF_AMM_phytx<-max(0,rnorm(1,PREF_AMM_phyt,Prefsd*PREF_AMM_phyt))
	#Renormalise
	prefsum<-PREF_NIT_phytx+PREF_AMM_phytx
	PREF_NIT_phyt<-PREF_NIT_phytx/prefsum
	PREF_AMM_phyt<-PREF_AMM_phytx/prefsum

	PREF_phyt_herbx<-max(0,rnorm(1,PREF_phyt_herb,Prefsd*PREF_phyt_herb))
	PREF_det_herbx<-max(0,rnorm(1,PREF_det_herb,Prefsd*PREF_det_herb))
	PREF_benthslar_herbx<-max(0,rnorm(1,PREF_benthslar_herb,Prefsd*PREF_benthslar_herb))
	PREF_benthclar_herbx<-max(0,rnorm(1,PREF_benthclar_herb,Prefsd*PREF_benthclar_herb))
	#Renormalise
	prefsum<-PREF_phyt_herbx+PREF_det_herbx+PREF_benthslar_herbx+PREF_benthclar_herbx
	PREF_phyt_herb<-PREF_phyt_herbx/prefsum
	PREF_det_herb<-PREF_det_herbx/prefsum
	PREF_benthslar_herb<-PREF_benthslar_herbx/prefsum
	PREF_benthclar_herb<-PREF_benthclar_herbx/prefsum

	PREF_herb_carnx<-max(0,rnorm(1,PREF_herb_carn,Prefsd*PREF_herb_carn))
	PREF_fishplar_carnx<-max(0,rnorm(1,PREF_fishplar_carn,Prefsd*PREF_fishplar_carn))
	PREF_fishdlar_carnx<-max(0,rnorm(1,PREF_fishdlar_carn,Prefsd*PREF_fishdlar_carn))
	PREF_benthslar_carnx<-max(0,rnorm(1,PREF_benthslar_carn,Prefsd*PREF_benthslar_carn))
	PREF_benthclar_carnx<-max(0,rnorm(1,PREF_benthclar_carn,Prefsd*PREF_benthclar_carn))
	#Renormalise
	prefsum<-PREF_herb_carnx+PREF_fishplar_carnx+PREF_fishdlar_carnx+PREF_benthslar_carnx+PREF_benthclar_carnx
	PREF_herb_carn<-PREF_herb_carnx/prefsum
	PREF_fishplar_carn<-PREF_fishplar_carnx/prefsum
	PREF_fishdlar_carn<-PREF_fishdlar_carnx/prefsum
	PREF_benthslar_carn<-PREF_benthslar_carnx/prefsum
	PREF_benthclar_carn<-PREF_benthclar_carnx/prefsum


	PREF_herb_fishplarx<-max(0,rnorm(1,PREF_herb_fishplar,Prefsd*PREF_herb_fishplar))
	PREF_benthslar_fishplarx<-max(0,rnorm(1,PREF_benthslar_fishplar,Prefsd*PREF_benthslar_fishplar))
	PREF_benthclar_fishplarx<-max(0,rnorm(1,PREF_benthclar_fishplar,Prefsd*PREF_benthclar_fishplar))
	#Renormalise
	prefsum<-PREF_herb_fishplarx+PREF_benthslar_fishplarx+PREF_benthclar_fishplarx
	PREF_herb_fishplar<-PREF_herb_fishplarx/prefsum
	PREF_benthslar_fishplar<-PREF_benthslar_fishplarx/prefsum
	PREF_benthclar_fishplar<-PREF_benthclar_fishplarx/prefsum

	PREF_herb_fishpx<-max(0,rnorm(1,PREF_herb_fishp,Prefsd*PREF_herb_fishp))
	PREF_carn_fishpx<-max(0,rnorm(1,PREF_carn_fishp,Prefsd*PREF_carn_fishp))
	PREF_fishdlar_fishpx<-max(0,rnorm(1,PREF_fishdlar_fishp,Prefsd*PREF_fishdlar_fishp))
	PREF_fishplar_fishpx<-max(0,rnorm(1,PREF_fishplar_fishp,Prefsd*PREF_fishplar_fishp))
	PREF_benthslar_fishpx<-max(0,rnorm(1,PREF_benthslar_fishp,Prefsd*PREF_benthslar_fishp))
	PREF_benthclar_fishpx<-max(0,rnorm(1,PREF_benthclar_fishp,Prefsd*PREF_benthclar_fishp))
	#Renormalise
	prefsum<-PREF_herb_fishpx+PREF_carn_fishpx+PREF_fishdlar_fishpx+PREF_fishplar_fishpx+PREF_benthslar_fishpx+PREF_benthclar_fishpx
	PREF_herb_fishp<-PREF_herb_fishpx/prefsum
	PREF_carn_fishp<-PREF_carn_fishpx/prefsum
	PREF_fishdlar_fishp<-PREF_fishdlar_fishpx/prefsum
	PREF_fishplar_fishp<-PREF_fishplar_fishpx/prefsum
	PREF_benthslar_fishp<-PREF_benthslar_fishpx/prefsum
	PREF_benthclar_fishp<-PREF_benthclar_fishpx/prefsum


	PREF_herb_fishmx<-max(0,rnorm(1,PREF_herb_fishm,Prefsd*PREF_herb_fishm))              # NEW NEW
	PREF_carn_fishmx<-max(0,rnorm(1,PREF_carn_fishm,Prefsd*PREF_carn_fishm))              # NEW NEW
	PREF_fishdlar_fishmx<-max(0,rnorm(1,PREF_fishdlar_fishm,Prefsd*PREF_fishdlar_fishm))       # NEW NEW
	PREF_fishplar_fishmx<-max(0,rnorm(1,PREF_fishplar_fishm,Prefsd*PREF_fishplar_fishm))        # NEW NEW
	PREF_benthslar_fishmx<-max(0,rnorm(1,PREF_benthslar_fishm,Prefsd*PREF_benthslar_fishm))    # NEW NEW
	PREF_benthclar_fishmx<-max(0,rnorm(1,PREF_benthclar_fishm,Prefsd*PREF_benthclar_fishm))    # NEW NEW
	#Renormalise
	prefsum<-PREF_herb_fishmx+PREF_carn_fishmx+PREF_benthslar_fishmx+PREF_benthclar_fishmx+PREF_fishdlar_fishmx+PREF_fishplar_fishmx
	PREF_herb_fishm<-PREF_herb_fishmx/prefsum
	PREF_carn_fishm<-PREF_carn_fishmx/prefsum
	PREF_fishdlar_fishm<-PREF_fishdlar_fishmx/prefsum
	PREF_fishplar_fishm<-PREF_fishplar_fishmx/prefsum
	PREF_benthslar_fishm<-PREF_benthslar_fishmx/prefsum
	PREF_benthclar_fishm<-PREF_benthclar_fishmx/prefsum


	PREF_herb_fishdlarx<-max(0,rnorm(1,PREF_herb_fishdlar,Prefsd*PREF_herb_fishdlar))
	PREF_benthslar_fishdlarx<-max(0,rnorm(1,PREF_benthslar_fishdlar,Prefsd*PREF_benthslar_fishdlar))
	PREF_benthclar_fishdlarx<-max(0,rnorm(1,PREF_benthclar_fishdlar,Prefsd*PREF_benthclar_fishdlar))
	#Renormalise
	prefsum<-PREF_herb_fishdlarx+PREF_benthslar_fishdlarx+PREF_benthclar_fishdlarx
	PREF_herb_fishdlar<-PREF_herb_fishdlarx/prefsum
	PREF_benthslar_fishdlar<-PREF_benthslar_fishdlarx/prefsum
	PREF_benthclar_fishdlar<-PREF_benthclar_fishdlarx/prefsum

	PREF_carn_fishdx<-max(0,rnorm(1,PREF_carn_fishd,Prefsd*PREF_carn_fishd))
	PREF_benths_fishdx<-max(0,rnorm(1,PREF_benths_fishd,Prefsd*PREF_benths_fishd))
	PREF_benthc_fishdx<-max(0,rnorm(1,PREF_benthc_fishd,Prefsd*PREF_benthc_fishd))
	PREF_fishplar_fishdx<-max(0,rnorm(1,PREF_fishplar_fishd,Prefsd*PREF_fishplar_fishd))
	PREF_fishdlar_fishdx<-max(0,rnorm(1,PREF_fishdlar_fishd,Prefsd*PREF_fishdlar_fishd))
	PREF_fishp_fishdx<-max(0,rnorm(1,PREF_fishp_fishd,Prefsd*PREF_fishp_fishd))
	PREF_fishm_fishdx<-max(0,rnorm(1,PREF_fishm_fishd,Prefsd*PREF_fishm_fishd))
	PREF_fishd_fishdx<-max(0,rnorm(1,PREF_fishd_fishd,Prefsd*PREF_fishd_fishd))
	PREF_disc_fishdx<-max(0,rnorm(1,PREF_disc_fishd,Prefsd*PREF_disc_fishd))
	PREF_corp_fishdx<-max(0,rnorm(1,PREF_corp_fishd,Prefsd*PREF_corp_fishd))
	#Renormalise
	prefsum<-PREF_carn_fishdx+PREF_benths_fishdx+PREF_benthc_fishdx+PREF_fishplar_fishdx+PREF_fishdlar_fishdx+PREF_fishp_fishdx+PREF_fishm_fishdx+PREF_fishd_fishdx+PREF_disc_fishdx+PREF_corp_fishdx
	PREF_carn_fishd<-PREF_carn_fishdx/prefsum
	PREF_benths_fishd<-PREF_benths_fishdx/prefsum
	PREF_benthc_fishd<-PREF_benthc_fishdx/prefsum
	PREF_fishplar_fishd<-PREF_fishplar_fishdx/prefsum
	PREF_fishdlar_fishd<-PREF_fishdlar_fishdx/prefsum
	PREF_fishp_fishd<-PREF_fishp_fishdx/prefsum
	PREF_fishm_fishd<-PREF_fishm_fishdx/prefsum
	PREF_fishd_fishd<-PREF_fishd_fishdx/prefsum
	PREF_disc_fishd<-PREF_disc_fishdx/prefsum
	PREF_corp_fishd<-PREF_corp_fishdx/prefsum

	PREF_phyt_benthslarx<-max(0,rnorm(1,PREF_phyt_benthslar,Prefsd*PREF_phyt_benthslar))
	PREF_det_benthslarx<-max(0,rnorm(1,PREF_det_benthslar,Prefsd*PREF_det_benthslar))
	#Renormalise
	prefsum<-PREF_phyt_benthslarx+PREF_det_benthslarx
	PREF_phyt_benthslar<-PREF_phyt_benthslarx/prefsum
	PREF_det_benthslar<-PREF_det_benthslarx/prefsum

	PREF_phyt_benthclarx<-max(0,rnorm(1,PREF_phyt_benthclar,Prefsd*PREF_phyt_benthclar))
	PREF_det_benthclarx<-max(0,rnorm(1,PREF_det_benthclar,Prefsd*PREF_det_benthclar))
	#Renormalise
	prefsum<-PREF_phyt_benthclarx+PREF_det_benthclarx
	PREF_phyt_benthclar<-PREF_phyt_benthclarx/prefsum
	PREF_det_benthclar<-PREF_det_benthclarx/prefsum


	PREF_phyt_benthsx<-max(0,rnorm(1,PREF_phyt_benths,Prefsd*PREF_phyt_benths))
	PREF_det_benthsx<-max(0,rnorm(1,PREF_det_benths,Prefsd*PREF_det_benths))
	PREF_sed_benthsx<-max(0,rnorm(1,PREF_sed_benths,Prefsd*PREF_sed_benths))
	#Renormalise
	prefsum<-PREF_phyt_benthsx+PREF_det_benthsx+PREF_sed_benthsx
	PREF_phyt_benths<-PREF_phyt_benthsx/prefsum
	PREF_det_benths<-PREF_det_benthsx/prefsum
	PREF_sed_benths<-PREF_sed_benthsx/prefsum

	PREF_benths_benthcx<-max(0,rnorm(1,PREF_benths_benthc,Prefsd*PREF_benths_benthc))
	PREF_corp_benthcx<-max(0,rnorm(1,PREF_corp_benthc,Prefsd*PREF_corp_benthc))
	#Renormalise
	prefsum<-PREF_benths_benthcx+PREF_corp_benthcx
	PREF_benths_benthc<-PREF_benths_benthcx/prefsum
	PREF_corp_benthc<-PREF_corp_benthcx/prefsum

	PREF_herb_birdx<-max(0,rnorm(1,PREF_herb_bird,Prefsd*PREF_herb_bird))
	PREF_carn_birdx<-max(0,rnorm(1,PREF_carn_bird,Prefsd*PREF_carn_bird))
	PREF_benths_birdx<-max(0,rnorm(1,PREF_benths_bird,Prefsd*PREF_benths_bird))
	PREF_benthc_birdx<-max(0,rnorm(1,PREF_benthc_bird,Prefsd*PREF_benthc_bird))

	PREF_fishp_birdx<-max(0,rnorm(1,PREF_fishp_bird,Prefsd*PREF_fishp_bird))
	PREF_fishm_birdx<-max(0,rnorm(1,PREF_fishm_bird,Prefsd*PREF_fishm_bird))
	PREF_fishd_birdx<-max(0,rnorm(1,PREF_fishd_bird,Prefsd*PREF_fishd_bird))
	PREF_disc_birdx<-max(0,rnorm(1,PREF_disc_bird,Prefsd*PREF_disc_bird))
	PREF_corp_birdx<-max(0,rnorm(1,PREF_corp_bird,Prefsd*PREF_corp_bird))
	#Renormalise
	prefsum<-PREF_herb_birdx+PREF_carn_birdx+PREF_benths_birdx+PREF_benthc_birdx+PREF_fishp_birdx+PREF_fishm_birdx+PREF_fishd_birdx+PREF_disc_birdx+PREF_corp_birdx

	PREF_herb_bird<-PREF_herb_birdx/prefsum
	PREF_carn_bird<-PREF_carn_birdx/prefsum
	PREF_benths_bird<-PREF_benths_birdx/prefsum
	PREF_benthc_bird<-PREF_benthc_birdx/prefsum

	PREF_fishp_bird<-PREF_fishp_birdx/prefsum
	PREF_fishm_bird<-PREF_fishm_birdx/prefsum
	PREF_fishd_bird<-PREF_fishd_birdx/prefsum
	PREF_disc_bird<-PREF_disc_birdx/prefsum
	PREF_corp_bird<-PREF_corp_birdx/prefsum

	prefstore<-list(PREF_NIT_phyt,PREF_AMM_phyt,PREF_phyt_herb,PREF_det_herb,PREF_benthslar_herb,PREF_benthclar_herb,PREF_herb_carn,PREF_benthslar_carn,PREF_benthclar_carn,PREF_fishplar_carn,PREF_fishdlar_carn,	# ZZ make a list
             PREF_herb_fishplar,PREF_benthslar_fishplar,PREF_benthclar_fishplar,
             PREF_herb_fishp,PREF_carn_fishp,PREF_benthslar_fishp,PREF_benthclar_fishp,PREF_fishdlar_fishp,PREF_fishplar_fishp,
             PREF_herb_fishm,PREF_carn_fishm,PREF_benthslar_fishm,PREF_benthclar_fishm,PREF_fishdlar_fishm,PREF_fishplar_fishm,
             PREF_herb_fishdlar,PREF_benthslar_fishdlar,PREF_benthclar_fishdlar,
             PREF_carn_fishd,PREF_benths_fishd,PREF_benthc_fishd,PREF_fishplar_fishd,PREF_fishdlar_fishd,PREF_fishp_fishd,PREF_fishm_fishd,PREF_fishd_fishd,PREF_disc_fishd,PREF_corp_fishd,
             PREF_phyt_benthslar,PREF_phyt_benthclar,
             PREF_det_benthslar,PREF_det_benthclar,
             PREF_phyt_benths,PREF_det_benths,PREF_sed_benths,PREF_benths_benthc,PREF_corp_benthc,
             PREF_herb_bird,PREF_carn_bird,PREF_benths_bird,PREF_benthc_bird,PREF_fishp_bird,PREF_fishm_bird,PREF_fishd_bird,PREF_disc_bird,PREF_corp_bird)
	names(prefstore)<-c("PREF_NIT_phyt","PREF_AMM_phyt","PREF_phyt_herb","PREF_det_herb","PREF_benthslar_herb","PREF_benthclar_herb","PREF_herb_carn","PREF_benthslar_carn","PREF_benthclar_carn","PREF_fishplar_carn","PREF_fishdlar_carn",
            "PREF_herb_fishplar","PREF_benthslar_fishplar","PREF_benthclar_fishplar",
            "PREF_herb_fishp","PREF_carn_fishp","PREF_benthslar_fishp","PREF_benthclar_fishp","PREF_fishdlar_fishp","PREF_fishplar_fishp",
            "PREF_herb_fishm","PREF_carn_fishm","PREF_benthslar_fishm","PREF_benthclar_fishm","PREF_fishdlar_fishm","PREF_fishplar_fishm",
            "PREF_herb_fishdlar","PREF_benthslar_fishdlar","PREF_benthclar_fishdlar",
            "PREF_carn_fishd","PREF_benths_fishd","PREF_benthc_fishd","PREF_fishplar_fishd","PREF_fishdlar_fishd","PREF_fishp_fishd","PREF_fishm_fishd","PREF_fishd_fishd","PREF_disc_fishd","PREF_corp_fishd",
            "PREF_phyt_benthslar","PREF_phyt_benthclar",
            "PREF_det_benthslar","PREF_det_benthclar",
            "PREF_phyt_benths","PREF_det_benths","PREF_sed_benths","PREF_benths_benthc","PREF_corp_benthc",
            "PREF_herb_bird","PREF_carn_bird","PREF_benths_bird","PREF_benthc_bird","PREF_fishp_bird","PREF_fishm_bird","PREF_fishd_bird","PREF_disc_bird","PREF_corp_bird")
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	u_phyt<-max(0,rnorm(1,u_phyt,u_phyt*u_sd))
	u_herb<-max(0,rnorm(1,u_herb,u_herb*u_sd))
	u_carn<-max(0,rnorm(1,u_carn,u_carn*u_sd))
	u_fishplar<-max(0,rnorm(1,u_fishplar,u_fishplar*u_sd))
	u_fishp<-max(0,rnorm(1,u_fishp,u_fishp*u_sd))
	u_fishm<-max(0,rnorm(1,u_fishm,u_fishm*u_sd))
	u_fishdlar<-max(0,rnorm(1,u_fishdlar,u_fishdlar*u_sd))
	u_fishd<-max(0,rnorm(1,u_fishd,u_fishd*u_sd))
	u_benthslar<-max(0,rnorm(1,u_benthslar,u_benthslar*u_sd))
	u_benthclar<-max(0,rnorm(1,u_benthclar,u_benthclar*u_sd))
	u_benths<-max(0,rnorm(1,u_benths,u_benths*u_sd))
	u_benthc<-max(0,rnorm(1,u_benthc,u_benthc*u_sd))
	u_bird<-max(0,rnorm(1,u_bird,u_bird*u_sd))

	ustore<-list(u_phyt,u_herb,u_carn,u_fishplar,u_fishp,u_fishm,u_fishdlar,u_fishd,u_benthslar,u_benthclar,u_benths,u_benthc,u_bird)	# ZZ make a list
	names(ustore)<-c("u_phyt","u_herb","u_carn","u_fishplar","u_fishp","u_fishm","u_fishdlar","u_fishd","u_benthslar","u_benthclar","u_benths","u_benthc","u_bird")
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	h_phyt<-max(0,rnorm(1,h_phyt,h_phyt*h_sd))
	h_herb<-max(0,rnorm(1,h_herb,h_herb*h_sd))
	h_carn<-max(0,rnorm(1,h_carn,h_carn*h_sd))
	h_fishplar<-max(0,rnorm(1,h_fishplar,h_fishplar*h_sd))
	h_fishp<-max(0,rnorm(1,h_fishp,h_fishp*h_sd))
	h_fishm<-max(0,rnorm(1,h_fishm,h_fishm*h_sd))
	h_fishdlar<-max(0,rnorm(1,h_fishdlar,h_fishdlar*h_sd))
	h_fishd<-max(0,rnorm(1,h_fishd,h_fishd*h_sd))
	h_benthslar<-max(0,rnorm(1,h_benthslar,h_benthslar*h_sd))
	h_benthclar<-max(0,rnorm(1,h_benthclar,h_benthclar*h_sd))
	h_benths<-max(0,rnorm(1,h_benths,h_benths*h_sd))
	h_benthc<-max(0,rnorm(1,h_benthc,h_benthc*h_sd))
	h_bird<-max(0,rnorm(1,h_bird,h_bird*h_sd))
	bda_par_bird<-max(0,rnorm(1,bda_par_bird,bda_par_bird*h_sd))

	hstore<-list(h_phyt,h_herb,h_carn,h_fishplar,h_fishp,h_fishm,h_fishdlar,h_fishd,h_benthslar,h_benthclar,h_benths,h_benthc,h_bird,bda_par_bird)	# ZZ make a list
	names(hstore)<-c("h_phyt","h_herb","h_carn","h_fishplar","h_fishp","h_fishm","h_fishdlar","h_fishd","h_benthslar","h_benthclar","h_benths","h_benthc","h_bird","bda_par_bird")
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	xmt<-max(0,rnorm(1,xmt,xmt*biogeo_sd))
	xnst<-max(0,rnorm(1,xnst,xnst*biogeo_sd))
	xdst<-max(0,rnorm(1,xdst,xdst*biogeo_sd))
	xndt<-max(0,rnorm(1,xndt,xndt*biogeo_sd))
	xddt<-max(0,rnorm(1,xddt,xddt*biogeo_sd))

	xqs_p1<-max(0,rnorm(1,xqs_p1,xqs_p1*biogeo_sd))
	#xqs_p1 is guessed to be less than qsp1lim so we need to trap cases where the jiggling violates this
	qsp1lim<-0.5
	if(xqs_p1>qsp1lim) {xqs_p1<-qsp1lim}
	if(xqs_p1<0) {xqs_p1<-0}

	xqs_p2<-max(0,rnorm(1,xqs_p2,xqs_p2*biogeo_sd))
	#xqs_p2 is guessed to be less than qsp2lim so we need to trap cases where the jiggling violates this
	qsp2lim<-0.001
	if(xqs_p2>qsp2lim) {xqs_p2<-qsp2lim}
	if(xqs_p2<0) {xqs_p2<-0}

	xqs_p3<-max(0,rnorm(1,xqs_p3,xqs_p3*biogeo_sd))
	#xqs_p3 is guessed to be less than qsp3lim so we need to trap cases where the jiggling violates this
	qsp3lim<-0.025
	if(xqs_p3<qsp3lim) {xqs_p3<-qsp3lim}
	if(xqs_p3<0) {xqs_p3<-0}

	xmsedt<-max(0,rnorm(1,xmsedt,xmsedt*biogeo_sd))
	if(xmsedt>0.015)        {xmsedt<-0.015}
	if((xqs_p1*xmsedt)>0.2) {xqs_p1 <- 0.2/xmsedt}

	#xqs_p1<-10
	#xqs_p3<-0.75

	#xmsens is expected to be negative so convert to positive first...
	xmsens<--1*xmsens
	xmsens<-max(0,rnorm(1,xmsens,xmsens*biogeo_sd))
	#convert back to negative
	xmsens<--1*xmsens

	xnsedt<-max(0,rnorm(1,xnsedt,xnsedt*biogeo_sd))

	#xnsens is expected to be negative so convert to positive first...
	xnsens<--1*xnsens
	xnsens<-max(0,rnorm(1,xnsens,xnsens*biogeo_sd))
	#convert back to negative
	xnsens<--1*xnsens

	xdsedt<-max(0,rnorm(1,xdsedt,xdsedt*biogeo_sd))

	#xdsens is expected tpo be positive so nothing to be done here
	xdsens<-max(0,rnorm(1,xdsens,xdsens*biogeo_sd))

	xdisc_corp<-max(0,rnorm(1,xdisc_corp,xdisc_corp*biogeo_sd))
	if(xdisc_corp>0.7) xdisc_corp<-0.7


	xxcorp_det<-max(0,rnorm(1,xxcorp_det,xxcorp_det*biogeo_sd))
	if(xxcorp_det>0.5) xdisc_corp<-0.5

	xdsink_s<-max(0,rnorm(1,xdsink_s,xdsink_s*biogeo_sd))
	xdsink_d<-max(0,rnorm(1,xdsink_d,xdsink_d*biogeo_sd))

	if(xdsink_s>1) xdsink_s<-1
	if(xdsink_d<0) xdsink_s<-0

	#xdsink_d_Klow<-max(0,rnorm(1,xdsink_d_Klow,xdsink_d_Klow*biogeo_sd))
	#xdsink_d_Khi<-max(0,rnorm(1,xdsink_d_Khi,xdsink_d_Khi*biogeo_sd))

	biogeostore<-list(xmt,xnst,xdst,xndt,xddt,xqs_p1,xqs_p2,xqs_p3,xmsedt,xmsens,xnsedt,xnsens,xdsedt,xdsens,xdisc_corp,xxcorp_det,xdsink_s,xdsink_d)	# ZZ make a list, then bio$xmt works
	names(biogeostore)<-c("xmt","xnst","xdst","xndt","xddt","xqs_p1","xqs_p2","xqs_p3","xmsedt","xmsens","xnsedt","xnsens","xdsedt","xdsens","xdisc_corp","xxcorp_det","xdsink_s","xdsink_d")
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	xxst<-max(0,rnorm(1,xxst,xxst*mort_sd))
	xxdt<-max(0,rnorm(1,xxdt,xxdt*mort_sd))
	xxherb<-max(0,rnorm(1,xxherb,xxherb*mort_sd))
	xxcarn<-max(0,rnorm(1,xxcarn,xxcarn*mort_sd))
	xxbenthslar<-max(0,rnorm(1,xxbenthslar,xxbenthslar*mort_sd))
	xxbenthclar<-max(0,rnorm(1,xxbenthclar,xxbenthclar*mort_sd))
	xxbenths<-max(0,rnorm(1,xxbenths,xxbenths*mort_sd))
	xxbenthc<-max(0,rnorm(1,xxbenthc,xxbenthc*mort_sd))
	xxpfishlar<-max(0,rnorm(1,xxpfishlar,xxpfishlar*mort_sd))
	xxdfishlar<-max(0,rnorm(1,xxdfishlar,xxdfishlar*mort_sd))
	xxpfish<-max(0,rnorm(1,xxpfish,xxpfish*mort_sd))
	xxmfish<-max(0,rnorm(1,xxmfish,xxmfish*mort_sd))
	xxdfish<-max(0,rnorm(1,xxdfish,xxdfish*mort_sd))
	xxbird<-max(0,rnorm(1,xxbird,xxbird*mort_sd))

	#xxbioturb<-max(0,rnorm(1,xxbioturb,xxbioturb*mort_sd))

	#xxpfish<-max(0,rnorm(1,xxpfish,xxpfish*ressd))
	#xxdfish<-max(0,rnorm(1,xxdfish,xxdfish*ressd))

	mortstore<-list(xxst,xxdt,xxherb,xxcarn,xxbenthslar,xxbenthclar,xxbenths,xxbenthc,xxpfishlar,xxdfishlar,xxpfish,xxmfish,xxdfish,xxbird)	# ZZ make a list
	names(mortstore)<-c("xxst","xxdt","xxherb","xxcarn","xxbenthslar","xxbenthclar","xxbenths","xxbenthc","xxpfishlar","xxdfishlar","xxpfish","xxmfish","xxdfish","xxbird")


	xdfdp<-max(0,rnorm(1,xdfdp,xdfdp*ressd))
	xpfish_migcoef <- max(0,rnorm(1,xpfish_migcoef,xpfish_migcoef*ressd))
	xmfish_migcoef <- max(0,rnorm(1,xmfish_migcoef,xmfish_migcoef*ressd))
	xdfish_migcoef <- max(0,rnorm(1,xdfish_migcoef,xdfish_migcoef*ressd))
	xbird_migcoef <- max(0,rnorm(1,xbird_migcoef,xbird_migcoef*ressd))

	if(xpfish_migcoef>0.01) xpfish_migcoef<-0.01
	if(xmfish_migcoef>0.01) xmfish_migcoef<-0.01
	if(xdfish_migcoef>0.01) xdfish_migcoef<-0.01
	if(xbird_migcoef >0.01) xbird_migcoef <-0.01


	xmax_exploitable_f_PF <- max(0,rnorm(1,xmax_exploitable_f_PF,xmax_exploitable_f_PF*ressd))
	if(xmax_exploitable_f_PF>0.99) xmax_exploitable_f_PF<-0.99

	xmax_exploitable_f_DF <- max(0,rnorm(1,xmax_exploitable_f_DF,xmax_exploitable_f_DF*ressd))
	if(xmax_exploitable_f_DF>0.99) xmax_exploitable_f_DF<-0.99

	xmax_exploitable_f_MF <- max(0,rnorm(1,xmax_exploitable_f_MF,xmax_exploitable_f_MF*ressd))
	if(xmax_exploitable_f_MF>1) xmax_exploitable_f_MF<-1

	xmax_exploitable_f_SB <- max(0,rnorm(1,xmax_exploitable_f_SB,xmax_exploitable_f_SB*ressd))
	if(xmax_exploitable_f_SB>0.5) xmax_exploitable_f_SB<-0.5

	xmax_exploitable_f_CB <- max(0,rnorm(1,xmax_exploitable_f_CB,xmax_exploitable_f_CB*ressd))
	if(xmax_exploitable_f_CB>0.5) xmax_exploitable_f_CB<-0.5

	xmax_exploitable_f_CZ <- max(0,rnorm(1,xmax_exploitable_f_CZ,xmax_exploitable_f_CZ*ressd))
	if(xmax_exploitable_f_CZ>0.5) xmax_exploitable_f_CZ<-0.5

	xmax_exploitable_f_BM <- max(0,rnorm(1,xmax_exploitable_f_BM,xmax_exploitable_f_BM*ressd))
	if(xmax_exploitable_f_BM>0.5) xmax_exploitable_f_BM<-0.5

	reststore<-list(	# ZZ make a list
		xdfdp,			# used in error function
         	xpfish_migcoef,
         	xmfish_migcoef,
         	xdfish_migcoef,
         	xbird_migcoef,
             	xmax_exploitable_f_PF,
             	xmax_exploitable_f_DF,
             	xmax_exploitable_f_MF,
             	xmax_exploitable_f_SB,
             	xmax_exploitable_f_CB,
             	xmax_exploitable_f_CZ,
             	xmax_exploitable_f_BM
	)

	names(reststore)<-c(
		"xdfdp",
         	"xpfish_migcoef",
         	"xmfish_migcoef",
         	"xdfish_migcoef",
         	"xbird_migcoef",
             	"xmax_exploitable_f_PF",
             	"xmax_exploitable_f_DF",
             	"xmax_exploitable_f_MF",
             	"xmax_exploitable_f_SB",
             	"xmax_exploitable_f_CB",
             	"xmax_exploitable_f_CZ",
             	"xmax_exploitable_f_BM"
	)


	# prepare annual_obj for appending:
	obj <- list(	# ZZ make a list
		"annual_obj"	= annual_obj
	)

	perturbed <- c(
		prefstore,
		ustore,
		hstore,
		biogeostore,
		mortstore,
		reststore,
		obj
	)
}

