#
# plot_final_year_MERP_HTL_inshore_vs_offshore.R
#
#' Plot full time series of out
#'
#' Plot set of time series from model out
#'
#' @param model model object
#' @param results full model results
#'
#' @export
#
# most fixed/fitted parms etc are in the results$out from the ode
#
plot_final_year_MERP_HTL_inshore_vs_offshore <- function(model, results) {

	build		<- elt(results, "build")
	run		<- elt(build, "run")
	nyears		<- elt(run, "nyears")
	ndays		<- elt(run, "ndays")

	data		<- elt(model, "data")
	physical.parms	<- elt(data, "physical.parameters")
	si_depth	<- elt(physical.parms, "si_depth")
	so_depth	<- elt(physical.parms, "so_depth")
	d_depth		<- elt(physical.parms, "d_depth")
	x_shallowprop	<- elt(physical.parms, "x_shallowprop")

	output		<- elt(results, "output")
	benthc_o	<- elt(output, "benthc_o")
	benthc_i	<- elt(output, "benthc_i")
	benthclar_o	<- elt(output, "benthclar_o")
	benthclar_i	<- elt(output, "benthclar_i")
	fishp_o		<- elt(output, "fishp_o")
	fishp_i		<- elt(output, "fishp_i")
	fishplar_o	<- elt(output, "fishplar_o")
	fishplar_i	<- elt(output, "fishplar_i")
	fishd_o		<- elt(output, "fishd_o")
	fishd_i		<- elt(output, "fishd_i")
	fishdlar_o	<- elt(output, "fishdlar_o")
	fishdlar_i	<- elt(output, "fishdlar_i")
	fishm_o		<- elt(output, "fishm_o")
	fishm_i		<- elt(output, "fishm_i")
	bird_o		<- elt(output, "bird_o")
	bird_i		<- elt(output, "bird_i")
	ceta_o		<- elt(output, "ceta_o")
	ceta_i		<- elt(output, "ceta_i")
	seal_o		<- elt(output, "seal_o")
	seal_i		<- elt(output, "seal_i")

	aggregates	<- elt(results, "aggregates")
	x_poros		<- elt(aggregates, "x_poros")
	x_depth		<- elt(aggregates, "x_depth")

	xvolume_si<-si_depth*x_shallowprop
	xvolume_so<-so_depth*(1-x_shallowprop)
	xd_volume<-d_depth*(1-x_shallowprop)

	#Plot the final year of output

	par(mfrow=c(3,3))

	l1<-benthc_o[((nyears-1)*360+1):ndays]/(1-x_shallowprop)
	l2<-benthc_i[((nyears-1)*360+1):ndays]/x_shallowprop
	tsplot22("Benthos c/s","Nitrogen/m2","Offshore","Inshore",l1,l2)

	l1<-benthclar_o[((nyears-1)*360+1):ndays]/(xvolume_so+xd_volume)
	l2<-benthclar_i[((nyears-1)*360+1):ndays]/xvolume_si
	tsplot22("Benthos c/s larvae","Nitrogen/m3","Offshore","Inshore",l1,l2)

	l1<-fishp_o[((nyears-1)*360+1):ndays]/(1-x_shallowprop)
	l2<-fishp_i[((nyears-1)*360+1):ndays]/x_shallowprop
	tsplot22("Pelagic fish","Nitrogen/m2","Offshore","Inshore",l1,l2)

	l1<-fishplar_o[((nyears-1)*360+1):ndays]/(1-x_shallowprop)
	l2<-fishplar_i[((nyears-1)*360+1):ndays]/x_shallowprop
	tsplot22("Pelagic fish larvae","Nitrogen/m2","Offshore","Inshore",l1,l2)

	l1<-fishd_o[((nyears-1)*360+1):ndays]/(1-x_shallowprop)
	l2<-fishd_i[((nyears-1)*360+1):ndays]/x_shallowprop
	tsplot22("Demersal fish","Nitrogen/m2","Offshore","Inshore",l1,l2)

	l1<-fishdlar_o[((nyears-1)*360+1):ndays]/(1-x_shallowprop)
	l2<-fishdlar_i[((nyears-1)*360+1):ndays]/x_shallowprop
	tsplot22("Demersal fish larvae","Nitrogen/m2","Offshore","Inshore",l1,l2)

	l1<-fishm_o[((nyears-1)*360+1):ndays]/(1-x_shallowprop)
	l2<-fishm_i[((nyears-1)*360+1):ndays]/x_shallowprop
	tsplot22("Migratory fish","Nitrogen/m2","Offshore","Inshore",l1,l2)

	l1<-bird_o[((nyears-1)*360+1):ndays]/(1-x_shallowprop)
	l2<-bird_i[((nyears-1)*360+1):ndays]/x_shallowprop
	tsplot22("Birds","Nitrogen/m2","Offshore","Inshore",l1,l2)

	l1<-ceta_o[((nyears-1)*360+1):ndays]/(1-x_shallowprop)
	l2<-ceta_i[((nyears-1)*360+1):ndays]/x_shallowprop
	l3<-seal_o[((nyears-1)*360+1):ndays]/(1-x_shallowprop)
	l4<-seal_i[((nyears-1)*360+1):ndays]/x_shallowprop
	tsplot44("Mammals","Nitrogen/m2","Ceatacean_o","Cetaceans_i", "Seals_o","Seals_i",l1,l2,l3,l4)
}

