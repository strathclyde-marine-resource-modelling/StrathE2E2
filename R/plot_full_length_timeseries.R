#
# plot_full_length_timeseries.R
#
#' Plot full time series of out
#'
#' Plot set of time series from model out
#'
#' @param results full model results
#'
#' @export
#
# most fixed/fitted parms etc are in the results$out from the ode
#
plot_full_length_timeseries <- function(results) {

	out <- elt(results, "output")
	agg <- elt(results, "aggregates")

	# move all the elt(out, "name") stuff up here

	par(mfrow=c(4,4))

	tsplot1("Total nitrogen",elt(agg, "totalN"))

	tsplot2("Susp. detritus",elt(agg, "s_detritus"),elt(out, "detritus_d"))
	legend(x=((length(elt(agg, "s_detritus"))-1)/360)/2,y=max(max(elt(agg, "s_detritus")),max(elt(out, "detritus_d"))),legend=c("Surface","Deep"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=0.8)

	tsplot1("Sediment detritus",elt(agg, "x_detritus"))

	tsplot2("Nitrate",elt(agg, "s_nitrate"),elt(out, "nitrate_d"))
	legend(((length(elt(agg, "s_nitrate"))-1)/360)/2,plmax<-max(max(elt(agg, "s_nitrate")),max(elt(out, "nitrate_d"))),c("Surface","Deep"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	tsplot1("Pore_w nitrate",elt(agg, "x_nitrate"))

	tsplot2("Ammonia",elt(agg, "s_ammonia"),elt(out, "ammonia_d"))
	legend(((length(elt(agg, "s_ammonia"))-1)/360)/2,plmax<-max(max(elt(agg, "s_ammonia")),max(elt(out, "ammonia_d"))),c("Surface","Deep"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	tsplot1("Pore_w ammonia",elt(agg, "x_ammonia"))

	tsplot2("Phytoplankton",elt(agg, "s_phyt"),elt(out, "phyt_d"))
	legend(((length(elt(agg, "s_phyt"))-1)/360)/2,plmax<-max(max(elt(agg, "s_phyt")),max(elt(out, "phyt_d"))),c("Surface","Deep"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	tsplot1("Omniv. zoop",elt(agg, "herb"))
	tsplot1("Carniv. zoo",elt(agg, "carn"))

	#tsplot1("Susp/dep. benthos",elt(agg, "benths"))
	#tsplot1("Carniv. benthos",elt(agg, "benthc"))

	tsplot2("Susp/dep. benthos",elt(agg, "benths"),elt(agg, "benthslar"))
	legend(((length(elt(agg, "benths"))-1)/360)/2,plmax<-max(max(elt(agg, "benths")),max(elt(agg, "benthslar"))),c("Adults","Larvae"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	tsplot2("Carniv. benthos",elt(agg, "benthc"),elt(agg, "benthclar"))
	legend(((length(elt(agg, "benthc"))-1)/360)/2,plmax<-max(max(elt(agg, "benthc")),max(elt(agg, "benthclar"))),c("Adults","Larvae"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	#tsplot3("Pelagic fish",elt(agg, "fishp"),elt(agg, "fishplar"),elt(agg, "fishm"))
	tsplot2("Pelagic fish",elt(agg, "fishp"),elt(agg, "fishplar"))
	#tsplot1("Pel. fish adults",elt(agg, "fishp"))
	legend(((length(elt(agg, "fishp"))-1)/360)/2,plmax<-max(max(elt(agg, "fishp")),max(elt(agg, "fishplar"))),c("Adults","Larvae"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	tsplot1("Migratory fish",elt(agg, "fishm"))

	tsplot2("Demersal fish",elt(agg, "fishd"),elt(agg, "fishdlar"))
	#tsplot1("Dem. fish adults",elt(agg, "fishd"))
	legend(((length(elt(agg, "fishd"))-1)/360)/2,plmax<-max(max(elt(agg, "fishd")),max(elt(agg, "fishdlar"))),c("Adults","Larvae"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	#tsplot1("Birds/mammals (*10^-2)",elt(agg, "bird")*1e2)
	#tsplot2("Birds/mammals (*10^-2)",elt(agg, "bird")*1e2,rep(0,nrow(out)))
	tsplot3("Birds/mammals (*10^-2)",elt(agg, "bird")*1e2,elt(agg, "seal")*1e2,elt(agg, "ceta")*1e2)
	legend(((length(elt(agg, "bird"))-1)/360)/2,plmax<-max(max(elt(agg, "bird")*1e2),max(elt(agg, "seal")*1e2),max(elt(agg, "ceta")*1e2)),c("Birds","Pinnipeds","Cetaceans"),col=c("black","black","red"),lty=c(1,2,2),pt.cex=c(1,1,1),cex=c(0.7))

	#print("Black = birds, dashed = seals, red = cetaceans")
}


