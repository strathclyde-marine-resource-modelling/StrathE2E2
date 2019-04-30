#
# plot_for_each_group_proportion_of_catch_by_gear.R
#
#' plot results as bargraphs by group along with observed data
#'
#' @param model model object
#' @param catch.land.disc catch, landings and discards
#'
#' @importFrom graphics barplot
#'
#' @export
#
plot_for_each_group_proportion_of_catch_by_gear <- function(model, catch.land.disc) {

	path			<- el(model, "path")

        offshore_catchmat       <- el(catch.land.disc, "offshore_catchmat")
        offshore_discmat        <- el(catch.land.disc, "offshore_discmat")
        offshore_landmat        <- el(catch.land.disc, "offshore_landmat")

        inshore_catchmat        <- el(catch.land.disc, "inshore_catchmat")
        inshore_discmat         <- el(catch.land.disc, "inshore_discmat")
        inshore_landmat         <- el(catch.land.disc, "inshore_landmat")

	catchproptarget <- readcsv(path, "Target_data/disc_and_land_as_prop_of_group_catch_per_gear.csv")	# ZZ should be a filevar

	targetdiscpropcatch<-t(catchproptarget[,3:9])
	targetlandpropcatch<-t(catchproptarget[,10:16])

	par(mfrow=c(4,2))

	par(mar=c(4,5,1,0.5))

	for(dsa in c(1,3:8)) {

		if(dsa==1) colintragetdata<-1
		if(dsa==2) colintragetdata<-2
		if(dsa==3) colintragetdata<-2
		if(dsa==4) colintragetdata<-3
		if(dsa==5) colintragetdata<-4
		if(dsa==6) colintragetdata<-5
		if(dsa==7) colintragetdata<-6
		if(dsa==8) colintragetdata<-7

		mt<-(rownames(offshore_catchmat))[dsa]
		if(dsa==3) mt<-"Demersal fish"

		#offshore_data2plot<-rbind(offshore_discmat[dsa,],offshore_landmat[dsa,])
		#inshore_data2plot<-rbind(inshore_discmat[dsa,],inshore_landmat[dsa,])

		#wholearea_data2plot <- offshore_catchmat[dsa,] + inshore_catchmat[dsa,]
		#wholearea_data2plot_P <- wholearea_data2plot / (sum(wholearea_data2plot,na.rm=TRUE))

		wholearea_data2plot<- rbind( (offshore_discmat[dsa,]+inshore_discmat[dsa,]),(offshore_landmat[dsa,]+inshore_landmat[dsa,]))

		if(dsa==3){
			wholearea_data2plot[1,] <- (wholearea_data2plot[1,]+(offshore_discmat[dsa-1,]+inshore_discmat[dsa-1,]))
			wholearea_data2plot[2,] <- (wholearea_data2plot[2,]+(offshore_landmat[dsa-1,]+inshore_landmat[dsa-1,]))
		}
		wholearea_data2plot_P <- wholearea_data2plot / (sum(wholearea_data2plot,na.rm=TRUE))

		target_data2plot<-rbind(targetdiscpropcatch[colintragetdata,],targetlandpropcatch[colintragetdata,])


		colnames(wholearea_data2plot_P)<-c("PT","PS","LLm","BT","DS","OT","GN","ST","NT","CR","MD","--")
		colnames(target_data2plot)<-c("PT","PS","LLm","BT","DS","OT","GN","ST","NT","CR","MD","--")


		#barplot(wholearea_data2plot_P,col=c("grey"),ylim=c(0,(1.2*max(c(wholearea_data2plot_P)))),xlim=c(0,12),width=rep(0.4,12), space=c(0.5,rep(1.2,11)),yaxt="n",ann=FALSE,main=mt,cex.axis=0.9)
		#barplot(wholearea_data2plot_P,col=c("black","green"),ylim=c(0,1),xlim=c(0,12),width=rep(0.4,12), space=c(0.5,rep(1.2,11)),yaxt="n",ann=FALSE,main=mt,cex.axis=0.9)
		barplot(wholearea_data2plot_P,col=c("grey","darkseagreen1"),ylim=c(0,1.1),xlim=c(0,12),width=rep(0.4,12), space=c(0.5,rep(1.2,11)),yaxt="n",ann=FALSE,main=mt,cex.axis=0.9)
		axis(side=2,las=1,cex.axis=0.9)
		if(dsa==1)  mtext("Propn of Catch",cex=0.7,side=2,line=3.5)
		if(dsa==3)  mtext("Propn of Catch",cex=0.7,side=2,line=3.5)
		if(dsa==5)  mtext("Propn of Catch",cex=0.7,side=2,line=3.5)
		if(dsa==7)  mtext("Propn of Catch",cex=0.7,side=2,line=3.5)

		#barplot(catchproptarget[,colintragetdata],col=c("grey"),add=T,width=rep(0.2,12), space=c(3,rep(3.4,11)),yaxt="n",xaxt="n",ann=FALSE)
		#barplot(target_data2plot,col=c("grey","darkseagreen1"),add=T,width=rep(0.2,12), space=c(3,rep(3.4,11)),yaxt="n",xaxt="n",ann=FALSE)
		barplot(target_data2plot,col=c("black","green"),add=T,width=rep(0.2,12), space=c(3,rep(3.4,11)),yaxt="n",xaxt="n",ann=FALSE)


	}
}




