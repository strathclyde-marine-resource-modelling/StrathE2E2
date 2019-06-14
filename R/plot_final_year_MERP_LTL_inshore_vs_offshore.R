#
# plot_final_year_MERP_LTL_inshore_vs_offshore.R
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
plot_final_year_MERP_LTL_inshore_vs_offshore <- function(model, results) {

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
	corpse_d1	<- elt(output, "corpse_d1")
	corpse_d2	<- elt(output, "corpse_d2")
	corpse_d3	<- elt(output, "corpse_d3")
	corpse_s1	<- elt(output, "corpse_s1")
	corpse_s2	<- elt(output, "corpse_s2")
	corpse_s3	<- elt(output, "corpse_s3")
	kelpdebris	<- elt(output, "kelpdebris")
	detritus_so	<- elt(output, "detritus_so")
	detritus_si	<- elt(output, "detritus_si")
	detritus_d	<- elt(output, "detritus_d")
	nitrate_so	<- elt(output, "nitrate_so")
	nitrate_si	<- elt(output, "nitrate_si")
	nitrate_d	<- elt(output, "nitrate_d")
	ammonia_so	<- elt(output, "ammonia_so")
	ammonia_si	<- elt(output, "ammonia_si")
	ammonia_d	<- elt(output, "ammonia_d")
	phyt_so		<- elt(output, "phyt_so")
	phyt_si		<- elt(output, "phyt_si")
	phyt_d		<- elt(output, "phyt_d")
	herb_o		<- elt(output, "herb_o")
	herb_i		<- elt(output, "herb_i")
	carn_o		<- elt(output, "carn_o")
	carn_i		<- elt(output, "carn_i")
	benths_o	<- elt(output, "benths_o")
	benths_i	<- elt(output, "benths_i")
	kelpN		<- elt(output, "kelpN")
	benthslar_o	<- elt(output, "benthslar_o")
	benthslar_i	<- elt(output, "benthslar_i")


	aggregates	<- elt(results, "aggregates")
	x_poros		<- elt(aggregates, "x_poros")
	x_depth		<- elt(aggregates, "x_depth")

	xvolume_si<-si_depth*x_shallowprop
	xvolume_so<-so_depth*(1-x_shallowprop)
	xd_volume<-d_depth*(1-x_shallowprop)

	par(mfrow=c(3,3))

	l1<-(corpse_d1[((nyears-1)*360+1):ndays]+corpse_d2[((nyears-1)*360+1):ndays]+corpse_d3[((nyears-1)*360+1):ndays])/(1-x_shallowprop)
	l2<-(corpse_s1[((nyears-1)*360+1):ndays]+corpse_s2[((nyears-1)*360+1):ndays]+corpse_s3[((nyears-1)*360+1):ndays])/x_shallowprop
	l3<-(kelpdebris[((nyears-1)*360+1):ndays])/x_shallowprop
	tsplot33("Corpses&kelp debris","Nitrogen/m2","Offshore","Inshore","Kelp debris",l1,l2,l3)

	l1<-detritus_so[((nyears-1)*360+1):ndays]/xvolume_so
	l2<-detritus_si[((nyears-1)*360+1):ndays]/xvolume_si
	l3<-detritus_d[((nyears-1)*360+1):ndays]/xd_volume
	#l3<-1000*100*(((x_detritus[((nyears-1)*360+1):ndays])*14)/1000)/(x_depth*(((1-x_poros)*(2650*1000))))
	#This converts the sediment detritus into units of %N by dry wt (100*gN/g-drysediment) (density of dry solid matter = 2.65g/cm3)
	#Then scale by 1000 to get on same axes as water detritus mMN/m3 water
	tsplot33("Detritus","Nitrogen/m3","S-offshore","S-inshore","Deep",l1,l2,l3)

	l1<-nitrate_so[((nyears-1)*360+1):ndays]/xvolume_so
	l2<-nitrate_si[((nyears-1)*360+1):ndays]/xvolume_si
	l3<-nitrate_d[((nyears-1)*360+1):ndays]/xd_volume
	#l3<-x_nitrate[((nyears-1)*360+1):ndays]/(x_depth*x_poros)
	#This converts the sediment nitrate into units of N /m3 in the pore water)
	tsplot33("Nitrate","Nitrogen/m3","S-offshore","S-inshore","Deep",l1,l2,l3)

	l1<-ammonia_so[((nyears-1)*360+1):ndays]/xvolume_so
	l2<-ammonia_si[((nyears-1)*360+1):ndays]/xvolume_si
	l3<-ammonia_d[((nyears-1)*360+1):ndays]/xd_volume
	#l3<-(x_ammonia[((nyears-1)*360+1):ndays]/(x_depth*x_poros))/10
	#This converts the sediment nitrate into units of N /m3 in the pore water)
	tsplot33("Ammonia","Nitrogen/m3","S-offshore","S-inshore","Deep",l1,l2,l3)

	l1<-phyt_so[((nyears-1)*360+1):ndays]/xvolume_so
	l2<-phyt_si[((nyears-1)*360+1):ndays]/xvolume_si
	l3<-phyt_d[((nyears-1)*360+1):ndays]/xd_volume
	tsplot33("Phytoplankton","Nitrogen/m3","S-offshore","S-inshore","Deep",l1,l2,l3)

	l1<-herb_o[((nyears-1)*360+1):ndays]/(xvolume_so+xd_volume)
	l2<-herb_i[((nyears-1)*360+1):ndays]/xvolume_si
	tsplot22("Zooplankton","Nitrogen/m3","Offshore","Inshore",l1,l2)

	l1<-carn_o[((nyears-1)*360+1):ndays]/(xvolume_so+xd_volume)
	l2<-carn_i[((nyears-1)*360+1):ndays]/xvolume_si
	tsplot22("Carn_Zooplankton","Nitrogen/m3","Offshore","Inshore",l1,l2)

	l1<-benths_o[((nyears-1)*360+1):ndays]/(1-x_shallowprop)
	l2<-benths_i[((nyears-1)*360+1):ndays]/x_shallowprop
	l3<-kelpN[((nyears-1)*360+1):ndays]/x_shallowprop
	tsplot33("Kelp & Benthos f/d","Nitrogen/m2","OffshoreB","InshoreB","Kelp",l1,l2,l3)

	l1<-benthslar_o[((nyears-1)*360+1):ndays]/(xvolume_so+xd_volume)
	l2<-benthslar_i[((nyears-1)*360+1):ndays]/xvolume_si
	tsplot22("Benthos f/d larvae","Nitrogen/m3","Offshore","Inshore",l1,l2)
}

