#
# plot_full_length_timeseries.R
#
#' Plot full time series of output
#'
#' @param model model object
#' @param results full model results
#'
#' @export
#
# most fixed/fitted parms etc are in the results$out from the ode
#
plot_full_length_timeseries <- function(model, results) {

	output		<- elt(results, "output")
	detritus_d	<- elt(output, "detritus_d")
	nitrate_d	<- elt(output, "nitrate_d")
	ammonia_d	<- elt(output, "ammonia_d")
	phyt_d		<- elt(output, "phyt_d")

	aggregates	<- elt(results, "aggregates")
	totalN		<- elt(aggregates, "totalN")
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
	benthslar	<- elt(aggregates, "benthslar")
	benthc		<- elt(aggregates, "benthc")
	benthclar	<- elt(aggregates, "benthclar")
	fishp		<- elt(aggregates, "fishp")
	fishplar	<- elt(aggregates, "fishplar")
	fishm		<- elt(aggregates, "fishm")
	fishd		<- elt(aggregates, "fishd")
	fishdlar	<- elt(aggregates, "fishdlar")
	bird		<- elt(aggregates, "bird")
	seal		<- elt(aggregates, "seal")
	ceta		<- elt(aggregates, "ceta")

	#Plot full time series of output

	par(mfrow=c(4,4))

	tsplot1("Total nitrogen",totalN)

	tsplot2("Susp. detritus",s_detritus,detritus_d)
	legend(x=((length(s_detritus)-1)/360)/2,y=max(max(s_detritus),max(detritus_d)),legend=c("Surface","Deep"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=0.8)


	tsplot1("Sediment detritus",x_detritus)

	tsplot2("Nitrate",s_nitrate,nitrate_d)
	legend(((length(s_nitrate)-1)/360)/2,plmax<-max(max(s_nitrate),max(nitrate_d)),c("Surface","Deep"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))


	tsplot1("Pore_w nitrate",x_nitrate)

	tsplot2("Ammonia",s_ammonia,ammonia_d)
	legend(((length(s_ammonia)-1)/360)/2,plmax<-max(max(s_ammonia),max(ammonia_d)),c("Surface","Deep"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	tsplot1("Pore_w ammonia",x_ammonia)

	tsplot2("Phytoplankton",s_phyt,phyt_d)
	legend(((length(s_phyt)-1)/360)/2,plmax<-max(max(s_phyt),max(phyt_d)),c("Surface","Deep"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	tsplot1("Omniv. zoop",herb)
	tsplot1("Carniv. zoo",carn)

	#tsplot1("Susp/dep. benthos",benths)
	#tsplot1("Carniv. benthos",benthc)

	tsplot2("Susp/dep. benthos",benths,benthslar)
	legend(((length(benths)-1)/360)/2,plmax<-max(max(benths),max(benthslar)),c("Adults","Larvae"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	tsplot2("Carniv. benthos",benthc,benthclar)
	legend(((length(benthc)-1)/360)/2,plmax<-max(max(benthc),max(benthclar)),c("Adults","Larvae"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	#tsplot3("Pelagic fish",fishp,fishplar,fishm)
	tsplot2("Pelagic fish",fishp,fishplar)
	#tsplot1("Pel. fish adults",fishp)
	legend(((length(fishp)-1)/360)/2,plmax<-max(max(fishp),max(fishplar)),c("Adults","Larvae"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	tsplot1("Migratory fish",fishm)

	tsplot2("Demersal fish",fishd,fishdlar)
	#tsplot1("Dem. fish adults",fishd)
	legend(((length(fishd)-1)/360)/2,plmax<-max(max(fishd),max(fishdlar)),c("Adults","Larvae"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	#tsplot1("Birds/mammals (*10^-2)",bird*1e2)
	#tsplot2("Birds/mammals (*10^-2)",bird*1e2,rep(0,nrow(out)))
	tsplot3("Birds/mammals (*10^-2)",bird*1e2,seal*1e2,ceta*1e2)
	legend(((length(bird)-1)/360)/2,plmax<-max(max(bird*1e2),max(seal*1e2),max(ceta*1e2)),c("Birds","Pinnipeds","Cetaceans"),col=c("black","black","red"),lty=c(1,2,2),pt.cex=c(1,1,1),cex=c(0.7))

	#print("Black = birds, dashed = seals, red = cetaceans")

}


