#
# plot_final_year.R
#
#' Plot final year time series
#'
#' @param model model object
#' @param results full model results
#'
#' @export
#
# most fixed/fitted parms etc are in the results$out from the ode
#
plot_final_year <- function(model, results) {

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
	detritus_d	<- elt(output, "detritus_d")
	nitrate_d	<- elt(output, "nitrate_d")
	ammonia_d	<- elt(output, "ammonia_d")
	phyt_d		<- elt(output, "phyt_d")

	aggregates	<- elt(results, "aggregates")
	x_poros		<- elt(aggregates, "x_poros")
	x_depth		<- elt(aggregates, "x_depth")
	s_detritus	<- elt(aggregates, "s_detritus")
	x_detritus	<- elt(aggregates, "x_detritus")
	s_nitrate	<- elt(aggregates, "s_nitrate")
	x_nitrate	<- elt(aggregates, "x_nitrate")
	s_ammonia	<- elt(aggregates, "s_ammonia")
	x_ammonia	<- elt(aggregates, "x_ammonia")
	s_phyt		<- elt(aggregates, "s_phyt")
	herb		<- elt(aggregates, "herb")
	carn		<- elt(aggregates, "carn")
	benths		<- elt(aggregates, "benths")
	benthc		<- elt(aggregates, "benthc")
	benthslar	<- elt(aggregates, "benthslar")
	benthclar	<- elt(aggregates, "benthclar")
	fishp		<- elt(aggregates, "fishp")
	fishplar	<- elt(aggregates, "fishplar")
	fishm		<- elt(aggregates, "fishm")
	fishd		<- elt(aggregates, "fishd")
	fishdlar	<- elt(aggregates, "fishdlar")
	bird		<- elt(aggregates, "bird")
	seal		<- elt(aggregates, "seal")
	ceta		<- elt(aggregates, "ceta")

	xvolume_si<-si_depth*x_shallowprop
	xvolume_so<-so_depth*(1-x_shallowprop)
	xd_volume<-d_depth*(1-x_shallowprop)
	xs_volume <- xvolume_si + xvolume_so

par(mfrow=c(3,3))

l1<-s_detritus[((nyears-1)*360+1):ndays]/xs_volume
l2<-detritus_d[((nyears-1)*360+1):ndays]/xd_volume
l3<-1000*100*(((x_detritus[((nyears-1)*360+1):ndays])*14)/1000)/(x_depth*(((1-x_poros)*(2650*1000))))
#This converts the sediment detritus into units of %N by dry wt (100*gN/g-drysediment) (density of dry solid matter = 2.65g/cm3)
#Then scale by 1000 to get on same axes as water detritus mMN/m3 water
fyplot3("Detritus","Nitrogen/m3","Surface","Deep","DrySed %N*1000",l1,l2,l3)

l1<-s_nitrate[((nyears-1)*360+1):ndays]/xs_volume
l2<-nitrate_d[((nyears-1)*360+1):ndays]/xd_volume
l3<-x_nitrate[((nyears-1)*360+1):ndays]/(x_depth*x_poros)
#This converts the sediment nitrate into units of N /m3 in the pore water)
fyplot3("Nitrate","Nitrogen/m3","Surface","Deep","Pore water",l1,l2,l3)

l1<-s_ammonia[((nyears-1)*360+1):ndays]/xs_volume
l2<-ammonia_d[((nyears-1)*360+1):ndays]/xd_volume
l3<-(x_ammonia[((nyears-1)*360+1):ndays]/(x_depth*x_poros))/10
#This converts the sediment nitrate into units of N /m3 in the pore water)
fyplot3("Ammonia","Nitrogen/m3","Surface","Deep","Pore water/10",l1,l2,l3)

l1<-s_phyt[((nyears-1)*360+1):ndays]/xs_volume
l2<-phyt_d[((nyears-1)*360+1):ndays]/xd_volume
fyplot2("Phytoplankton","Nitrogen/m3","Surface","Deep",l1,l2)

l1<-herb[((nyears-1)*360+1):ndays]/(xs_volume+xd_volume)
l2<-carn[((nyears-1)*360+1):ndays]*10/(xs_volume+xd_volume)
fyplot2("Zooplankton","Nitrogen/m3","Omnivores","Carnivores*10^1",l1,l2)

#l1<-benths[((nyears-1)*360+1):ndays]
#l2<-benthc[((nyears-1)*360+1):ndays]*10
#fyplot2("Benthos","Nitrogen/m2","Susp/det","Carnivores*10^1",l1,l2)

l1<-benths[((nyears-1)*360+1):ndays]
l2<-benthc[((nyears-1)*360+1):ndays]*10
l3<-benthslar[((nyears-1)*360+1):ndays]*10
l4<-benthclar[((nyears-1)*360+1):ndays]*100
fyplot4("Benthos","Nitrogen/m2","Susp/det","Carniv*10","Susp/detlar*10","Carnlar*100",l1,l2,l3,l4)

l1<-fishp[((nyears-1)*360+1):ndays]
l2<-fishplar[((nyears-1)*360+1):ndays]
l3<-fishm[((nyears-1)*360+1):ndays]
fyplot3("Pelagic fish","Nitrogen/m2","Adults","Larvae","Migratory",l1,l2,l3)

l1<-fishd[((nyears-1)*360+1):ndays]
l2<-fishdlar[((nyears-1)*360+1):ndays]
fyplot2("Demersal fish","Nitrogen/m2","Adults","Larvae",l1,l2)

l1<-bird[((nyears-1)*360+1):ndays]
l2<-seal[((nyears-1)*360+1):ndays]
l3<-ceta[((nyears-1)*360+1):ndays]
#fyplot1("Birds & mammals","Nitrogen/m2",l1)
fyplot3("Birds & mammals","Nitrogen/m2","Birds","Seals","Cetaceans",l1,l2,l3)


}

