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

	out <- el(results, "output")
	agg <- el(results, "aggregates")

	# move all the el(out, "name") stuff up here

	par(mfrow=c(4,4))

	tsplot1("Total nitrogen",el(agg, "totalN"))

	tsplot2("Susp. detritus",el(agg, "s_detritus"),el(out, "detritus_d"))
	legend(x=((length(el(agg, "s_detritus"))-1)/360)/2,y=max(max(el(agg, "s_detritus")),max(el(out, "detritus_d"))),legend=c("Surface","Deep"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=0.8)

	tsplot1("Sediment detritus",el(agg, "x_detritus"))

	tsplot2("Nitrate",el(agg, "s_nitrate"),el(out, "nitrate_d"))
	legend(((length(el(agg, "s_nitrate"))-1)/360)/2,plmax<-max(max(el(agg, "s_nitrate")),max(el(out, "nitrate_d"))),c("Surface","Deep"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	tsplot1("Pore_w nitrate",el(agg, "x_nitrate"))

	tsplot2("Ammonia",el(agg, "s_ammonia"),el(out, "ammonia_d"))
	legend(((length(el(agg, "s_ammonia"))-1)/360)/2,plmax<-max(max(el(agg, "s_ammonia")),max(el(out, "ammonia_d"))),c("Surface","Deep"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	tsplot1("Pore_w ammonia",el(agg, "x_ammonia"))

	tsplot2("Phytoplankton",el(agg, "s_phyt"),el(out, "phyt_d"))
	legend(((length(el(agg, "s_phyt"))-1)/360)/2,plmax<-max(max(el(agg, "s_phyt")),max(el(out, "phyt_d"))),c("Surface","Deep"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	tsplot1("Omniv. zoop",el(agg, "herb"))
	tsplot1("Carniv. zoo",el(agg, "carn"))

	#tsplot1("Susp/dep. benthos",el(agg, "benths"))
	#tsplot1("Carniv. benthos",el(agg, "benthc"))

	tsplot2("Susp/dep. benthos",el(agg, "benths"),el(agg, "benthslar"))
	legend(((length(el(agg, "benths"))-1)/360)/2,plmax<-max(max(el(agg, "benths")),max(el(agg, "benthslar"))),c("Adults","Larvae"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	tsplot2("Carniv. benthos",el(agg, "benthc"),el(agg, "benthclar"))
	legend(((length(el(agg, "benthc"))-1)/360)/2,plmax<-max(max(el(agg, "benthc")),max(el(agg, "benthclar"))),c("Adults","Larvae"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	#tsplot3("Pelagic fish",el(agg, "fishp"),el(agg, "fishplar"),el(agg, "fishm"))
	tsplot2("Pelagic fish",el(agg, "fishp"),el(agg, "fishplar"))
	#tsplot1("Pel. fish adults",el(agg, "fishp"))
	legend(((length(el(agg, "fishp"))-1)/360)/2,plmax<-max(max(el(agg, "fishp")),max(el(agg, "fishplar"))),c("Adults","Larvae"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	tsplot1("Migratory fish",el(agg, "fishm"))

	tsplot2("Demersal fish",el(agg, "fishd"),el(agg, "fishdlar"))
	#tsplot1("Dem. fish adults",el(agg, "fishd"))
	legend(((length(el(agg, "fishd"))-1)/360)/2,plmax<-max(max(el(agg, "fishd")),max(el(agg, "fishdlar"))),c("Adults","Larvae"),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1),cex=c(0.8))

	#tsplot1("Birds/mammals (*10^-2)",el(agg, "bird")*1e2)
	#tsplot2("Birds/mammals (*10^-2)",el(agg, "bird")*1e2,rep(0,nrow(out)))
	tsplot3("Birds/mammals (*10^-2)",el(agg, "bird")*1e2,el(agg, "seal")*1e2,el(agg, "ceta")*1e2)
	legend(((length(el(agg, "bird"))-1)/360)/2,plmax<-max(max(el(agg, "bird")*1e2),max(el(agg, "seal")*1e2),max(el(agg, "ceta")*1e2)),c("Birds","Pinnipeds","Cetaceans"),col=c("black","black","red"),lty=c(1,2,2),pt.cex=c(1,1,1),cex=c(0.7))

	#print("Black = birds, dashed = seals, red = cetaceans")
}


