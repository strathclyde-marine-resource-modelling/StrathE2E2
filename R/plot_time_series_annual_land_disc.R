#
# plot_time_series_annual_land_disc.R
#
#' plot results as lineplots by group
#'
#' @param model model object
#' @param results model results object
#'
#' @importFrom graphics barplot
#'
#' @export
#
plot_time_series_annual_land_disc <- function(model, results) {
    
	total.annual.catch <- elt(results, "total.annual.catch")

        ngroups<-10 # this is the number of groups in gear_group_proportions
   ngroups<-5 # this is the number of groups in gear_group_proportions

        offshore_annual_group_land_disc <- elt(total.annual.catch, "offshore_annual_group_land_disc")
        inshore_annual_group_land_disc  <- elt(total.annual.catch, "inshore_annual_group_land_disc")

	titles <- c(
		"Pelagic fish",
		"Quota-limited Demersal fish",
		"Non-quota demersal fish",
		"Migratory fish",
		"Susp/dep feeding benthos",
		"Carn/scav feeding benthos",
		"Pelagic invertebrates",
		"Birds",
		"Seals",
		"Cetaceans",
		"Kelp"
	)

	par(mfrow=c(6,2))

	par(mar=c(4,5,1,0.5))

	#for(dsa in 1:8) {
	for(dsa in 1:(ngroups+1)) {

		mt<-titles[dsa]

		offshore_data2plot<-rbind(offshore_annual_group_land_disc[,(ngroups+2)+dsa],offshore_annual_group_land_disc[,1+dsa])
		inshore_data2plot<-rbind(inshore_annual_group_land_disc[,(ngroups+2)+dsa],inshore_annual_group_land_disc[,1+dsa])

		offshore_catch<-colSums(offshore_data2plot)
		inshore_catch<-colSums(inshore_data2plot)

		colnames(offshore_data2plot)<-offshore_annual_group_land_disc[,1]

		barplot(offshore_data2plot,col=c("black","green"),ylim=c(0,0.0000001+(1.2*max(c(offshore_catch,inshore_catch)))),xlim=c(0,(ncol(offshore_data2plot)+1)),width=rep(0.4,ncol(offshore_data2plot)), space=c(0.5,rep(1.2,ncol(offshore_data2plot)-1)),yaxt="n",ann=FALSE,main=mt,cex.axis=0.9)
		axis(side=2,las=1,cex.axis=0.9)
		if(dsa==1)  mtext("Catch",cex=0.7,side=2,line=3.5)
		if(dsa==3)  mtext("Catch",cex=0.7,side=2,line=3.5)
		if(dsa==5)  mtext("Catch",cex=0.7,side=2,line=3.5)
		if(dsa==7)  mtext("Catch",cex=0.7,side=2,line=3.5)
		if(dsa==7)  mtext("Years",cex=0.7,side=1,line=2)
		if(dsa==8)  mtext("Years",cex=0.7,side=1,line=2)
		if(dsa==9)  mtext("Years",cex=0.7,side=1,line=2)
		if(dsa==10)  mtext("Years",cex=0.7,side=1,line=2)

		barplot(inshore_data2plot,col=c("grey","blue"),add=T,width=rep(0.4,12), space=c(1.3,rep(1.2,11)),yaxt="n",xaxt="n",ann=FALSE)
readline(prompt="plot_final_year")

	}
}

