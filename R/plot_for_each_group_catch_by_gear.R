#
# plot_for_each_group_catch_by_gear.R
#
#' plot results as lineplots by group
#'
#' @param model model object
#' @param catch.land.disc catch, landings and discards
#'
#' @importFrom graphics barplot
#'
#' @export
#
plot_for_each_group_catch_by_gear <- function(model, catch.land.disc) {

	data			<- elt(model, "data")
	fleet.model		<- elt(data, "fleet.model")
	gear_codes		<- elt(fleet.model, "gear_codes")
	
	offshore_catchmat	<- elt(catch.land.disc, "offshore_catchmat")
	offshore_discmat	<- elt(catch.land.disc, "offshore_discmat")
	offshore_landmat	<- elt(catch.land.disc, "offshore_landmat")

	inshore_catchmat	<- elt(catch.land.disc, "inshore_catchmat")
	inshore_discmat		<- elt(catch.land.disc, "inshore_discmat")
	inshore_landmat		<- elt(catch.land.disc, "inshore_landmat")

	ngroups <- 10

	#par(mfrow=c(6,2))
	par(mfrow=c(5,2))

	#par(mar=c(4,5,1,0.5))
	par(mar=c(2.5,5,1,0.5))

	#for(dsa in 1:(ngroups+1)) {
	for(dsa in 1:(ngroups+1-1)) {

		mt<-(rownames(offshore_catchmat))[dsa]

		offshore_data2plot<-rbind(offshore_discmat[dsa,],offshore_landmat[dsa,])
		inshore_data2plot<-rbind(inshore_discmat[dsa,],inshore_landmat[dsa,])

		#colnames(offshore_data2plot)<-c("PT","PS","LLm","BT","DS","OT","GN","ST","NT","CR","MD","--")
		colnames(offshore_data2plot)<-gear_codes

		barplot(offshore_data2plot,col=c("black","green"),ylim=c(0,(1.2*max(c(offshore_catchmat[dsa,],inshore_catchmat[dsa,])))),xlim=c(0,12),width=rep(0.4,12), space=c(0.5,rep(1.2,11)),yaxt="n",ann=FALSE,main=mt,cex.axis=0.9)
		axis(side=2,las=1,cex.axis=0.9)
		if(dsa==1)  mtext("Catch",cex=0.7,side=2,line=3.5)
		if(dsa==3)  mtext("Catch",cex=0.7,side=2,line=3.5)
		if(dsa==5)  mtext("Catch",cex=0.7,side=2,line=3.5)
		if(dsa==7)  mtext("Catch",cex=0.7,side=2,line=3.5)
		if(dsa==9)  mtext("Catch",cex=0.7,side=2,line=3.5)
		if(dsa==11)  mtext("Catch",cex=0.7,side=2,line=3.5)

		barplot(inshore_data2plot,col=c("grey","blue"),add=T,width=rep(0.4,12), space=c(1.5,rep(1.2,11)),yaxt="n",xaxt="n",ann=FALSE)
	}
}

