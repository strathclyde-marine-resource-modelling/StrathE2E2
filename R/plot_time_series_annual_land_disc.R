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
    
	#----------------------------------------------------------------
	#In each plot panel:
	#    black and grey are offshore and inshore discards respectively
	#    green and blue are offshore and inshore landings respectively
	#----------------------------------------------------------------

	total.annual.catch <- elt(results, "total.annual.catch")

        ngroups<-10 # this is the number of groups in gear_group_proportions

        offshore_annual_group_land_disc <- elt(total.annual.catch, "offshore_annual_group_land_disc")
        inshore_annual_group_land_disc  <- elt(total.annual.catch, "inshore_annual_group_land_disc")

	group_names <- c(
		"Planktivorous fish",
		"Quota-limited Demersal fish",
		"Non-quota demersal fish",
		"Migratory fish",
		"Susp/dep feeding benthos",
		"Carn/scav feeding benthos",
		"Pelagic invertebrates",
		"Birds",
		"Pinnipeds",
		"Cetaceans","Macrophytes"
	)

	par(mfrow=c(6,2))
	par(mar=c(4,5,1,0.5))


	for(dsa in 1:(ngroups+1)) {

		mt<-group_names[dsa]

		offshore_data2plot<-rbind(offshore_annual_group_land_disc[,(ngroups+2)+dsa],offshore_annual_group_land_disc[,1+dsa])
		inshore_data2plot<-rbind(inshore_annual_group_land_disc[,(ngroups+2)+dsa],inshore_annual_group_land_disc[,1+dsa])

		offshore_catch<-colSums(offshore_data2plot)
		inshore_catch<-colSums(inshore_data2plot)

		colnames(offshore_data2plot)<-offshore_annual_group_land_disc[,1]

		barplot(offshore_data2plot,col=c("black","green"),ylim=c(0,0.0000001+(1.2*max(c(offshore_catch,inshore_catch)))),xlim=c(0,(ncol(offshore_data2plot)+1)),width=rep(0.4,ncol(offshore_data2plot)), space=c(0.5,rep(1.2,ncol(offshore_data2plot)-1)),yaxt="n",ann=FALSE,cex.axis=0.9)
		axis(side=2,las=1,cex.axis=0.9)
		if(dsa==1)  mtext("Catch",cex=0.7,side=2,line=3.5)
		if(dsa==3)  mtext("Catch",cex=0.7,side=2,line=3.5)
		if(dsa==5)  mtext("Catch",cex=0.7,side=2,line=3.5)
		if(dsa==7)  mtext("Catch",cex=0.7,side=2,line=3.5)
		if(dsa==0)  mtext("Catch",cex=0.7,side=2,line=3.5)
		if(dsa==11)  mtext("Catch",cex=0.7,side=2,line=3.5)
		#if(dsa==7)  mtext("Years",cex=0.7,side=1,line=2)
		#if(dsa==8)  mtext("Years",cex=0.7,side=1,line=2)
		#if(dsa==9)  mtext("Years",cex=0.7,side=1,line=2)
		#if(dsa==10)  mtext("Years",cex=0.7,side=1,line=2)
		if(dsa==11)  mtext("Years",cex=0.7,side=1,line=2)
		if(dsa==12)  mtext("Years",cex=0.7,side=1,line=2)
		title(main=mt,cex.main=1.0)
		barplot(inshore_data2plot,col=c("grey","blue"),add=T,width=rep(0.4,ncol(offshore_data2plot)), space=c(1.5,rep(1.2,ncol(offshore_data2plot)-1)),yaxt="n",xaxt="n",ann=FALSE)
	}


	#Calculate total landings and disards avross all guilds
	offshore_data2plot<-rbind(rowSums(offshore_annual_group_land_disc[,((ngroups+2)+1) : ((ngroups+2)+(ngroups+1))]),rowSums(offshore_annual_group_land_disc[,2:(ngroups+2)]))
	inshore_data2plot<- rbind(rowSums( inshore_annual_group_land_disc[,((ngroups+2)+1) : ((ngroups+2)+(ngroups+1))]),rowSums( inshore_annual_group_land_disc[,2:(ngroups+2)]))

	offshore_catch<-colSums(offshore_data2plot)
	inshore_catch<-colSums(inshore_data2plot)

	colnames(offshore_data2plot)<-offshore_annual_group_land_disc[,1]

	mt<-"All guilds combined"

	barplot(offshore_data2plot,col=c("black","green"),ylim=c(0,0.0000001+(1.2*max(c(offshore_catch,inshore_catch)))),xlim=c(0,(ncol(offshore_data2plot)+1)),width=rep(0.4,ncol(offshore_data2plot)), space=c(0.5,rep(1.2,ncol(offshore_data2plot)-1)),yaxt="n",ann=FALSE,cex.axis=0.9)
	axis(side=2,las=1,cex.axis=0.9)
	mtext("Years",cex=0.7,side=1,line=2)
	title(main=mt,cex.main=1.0)
	barplot(inshore_data2plot,col=c("grey","blue"),add=T,width=rep(0.4,ncol(offshore_data2plot)), space=c(1.5,rep(1.2,ncol(offshore_data2plot)-1)),yaxt="n",xaxt="n",ann=FALSE)
}


