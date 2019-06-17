#
# plot_for_each_gear_catch_by_group.R
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
plot_for_each_gear_catch_by_group <- function(model, results) {

	final.year.outputs	<- elt(results, "final.year.outputs")
	offshore_catchmat	<- elt(final.year.outputs, "offshore_catchmat")
	offshore_discmat	<- elt(final.year.outputs, "offshore_discmat")
	offshore_landmat	<- elt(final.year.outputs, "offshore_landmat")

	inshore_catchmat	<- elt(final.year.outputs, "inshore_catchmat")
	inshore_discmat		<- elt(final.year.outputs, "inshore_discmat")
	inshore_landmat		<- elt(final.year.outputs, "inshore_landmat")
 
	ngears <- 12

	#Plot results as lineplots by group

	#expects the matrices catchmat and catchmat_t, landmat and landmat_t, doscmat and discmat_t

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	par(mfrow=c(4,3))

	par(mar=c(3,5,1.5,0.5))

	for(dsa in 1:ngears) {

		mt<-(colnames(offshore_catchmat))[dsa]

		offshore_data2plot<-rbind(offshore_discmat[,dsa],offshore_landmat[,dsa])
		inshore_data2plot<-rbind(inshore_discmat[,dsa],inshore_landmat[,dsa])



		colnames(offshore_data2plot)<-c("PF","DFq","DFnq","MF","Bfd","Bcs","CZ","BD","PIN","CT","MP")

		barplot(offshore_data2plot,col=c("black","green"),ylim=c(0,(1.2*max(c(offshore_catchmat[,dsa],inshore_catchmat[,dsa])))),xlim=c(0,12),width=rep(0.5,11), space=c(0.5,rep(1.2,10)),yaxt="n",ann=FALSE,main=mt,cex.axis=0.9)
axis(side=2,las=1,cex.axis=0.9)
		if(dsa==1)  mtext("Catch",cex=0.7,side=2,line=3.5)
		if(dsa==4)  mtext("Catch",cex=0.7,side=2,line=3.5)
		if(dsa==7)  mtext("Catch",cex=0.7,side=2,line=3.5)
		if(dsa==10) mtext("Catch",cex=0.7,side=2,line=3.5)

		barplot(inshore_data2plot,col=c("grey","blue"),add=T,width=rep(0.5,11), space=c(1.5,rep(1.2,10)),yaxt="n",xaxt="n",ann=FALSE)


	}

}

