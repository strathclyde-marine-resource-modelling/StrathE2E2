#
# box_and_whisker_monthly_plots.R
#
#' plot some monthly result sumamries
#'
#' @param model model object
#' @param results model results object
#'
#' @export
#
box_and_whisker_monthly_plots <- function(model, results) {

	final.year.outputs	<- elt(results, "final.year.outputs")
	modelmonthly		<- elt(final.year.outputs, "monthly.averages")
	obstargetdataset	<- elt(final.year.outputs, "monthly.target.data")

#Code to plot observed and modelled distributions of the monthly ecosystem data


#FOR WINDOWS ONLY - COMMENT OUT FOR LINUX
#path  <- "C:/Users/ais04103/Documents/D/E2E_ecomodel/code/StrathE2E_MERP_6habs/"
#setwd(path)
#drivingpath    <-paste(path,"Driving data/NorthSea/",sep="")
#targetpath         <-paste(path,"Target data/NorthSea/",sep="")
#oudir          <-paste(path,"results/",sep="")

#FOR LINUX ONLY (launch R from the dir containing this file) - COMMENT OUT FOR WINDOWS
#drivingpath    <- "../Driving data/NorthSea/"
#targetpath     <- "../Target data/NorthSea/"
#oudir          <- "../results/"



#Read the observed data file
#Format expected = 7 columns
#Month	Variable	median	lower_centile	upper_centile	Units	low_cent_value	upp_cent_value	Comments
#The variable names expected are:
#surface_nitrate
#deep_nitrate
#surface_ammonia
#deep_ammonia
#surface_chlorophyll
#omniv_zooplankton
#carniv_zooplankton
#larvae_susp_dep_benthos
#larvae_carn_scav_benthos
#obstargetdataset<-read.csv(paste(targetpath,"Combined_monthly_data.csv",sep=""),header=TRUE)
#obstargetdataset<-read.csv(monthlytargetfile,header=TRUE)



#Read in the model run monthly results output file
#modelmonthly<-read.csv(paste(oudir,"model_monthlyresults",AAA,".csv",sep=""),header=TRUE)
# modelmonthly<-results$final.year.outputs$monthly.averages  ## line of code to extract model rata from R package results object

#Make an array to hold the synthesised observational monthly data
ntargobs<-100
monthtarget<-array(0,dim=c(ntargobs,12,10))
#dimensions observations,month, parameter, 

#Make an array to hold the modelled monthly data
nmodobs<-10
MODmonthtarget<-array(0,dim=c(nmodobs,12,10))
#dimensions observations,month, parameter, 
MODmonthtarget[1:nmodobs,1,1]<-modelmonthly$surfnitratemMm3[1]
MODmonthtarget[1:nmodobs,2,1]<-modelmonthly$surfnitratemMm3[2]
MODmonthtarget[1:nmodobs,3,1]<-modelmonthly$surfnitratemMm3[3]
MODmonthtarget[1:nmodobs,4,1]<-modelmonthly$surfnitratemMm3[4]
MODmonthtarget[1:nmodobs,5,1]<-modelmonthly$surfnitratemMm3[5]
MODmonthtarget[1:nmodobs,6,1]<-modelmonthly$surfnitratemMm3[6]
MODmonthtarget[1:nmodobs,7,1]<-modelmonthly$surfnitratemMm3[7]
MODmonthtarget[1:nmodobs,8,1]<-modelmonthly$surfnitratemMm3[8]
MODmonthtarget[1:nmodobs,9,1]<-modelmonthly$surfnitratemMm3[9]
MODmonthtarget[1:nmodobs,10,1]<-modelmonthly$surfnitratemMm3[10]
MODmonthtarget[1:nmodobs,11,1]<-modelmonthly$surfnitratemMm3[11]
MODmonthtarget[1:nmodobs,12,1]<-modelmonthly$surfnitratemMm3[12]

MODmonthtarget[1:nmodobs,1,2]<-modelmonthly$deepnitratemMm3[1]
MODmonthtarget[1:nmodobs,2,2]<-modelmonthly$deepnitratemMm3[2]
MODmonthtarget[1:nmodobs,3,2]<-modelmonthly$deepnitratemMm3[3]
MODmonthtarget[1:nmodobs,4,2]<-modelmonthly$deepnitratemMm3[4]
MODmonthtarget[1:nmodobs,5,2]<-modelmonthly$deepnitratemMm3[5]
MODmonthtarget[1:nmodobs,6,2]<-modelmonthly$deepnitratemMm3[6]
MODmonthtarget[1:nmodobs,7,2]<-modelmonthly$deepnitratemMm3[7]
MODmonthtarget[1:nmodobs,8,2]<-modelmonthly$deepnitratemMm3[8]
MODmonthtarget[1:nmodobs,9,2]<-modelmonthly$deepnitratemMm3[9]
MODmonthtarget[1:nmodobs,10,2]<-modelmonthly$deepnitratemMm3[10]
MODmonthtarget[1:nmodobs,11,2]<-modelmonthly$deepnitratemMm3[11]
MODmonthtarget[1:nmodobs,12,2]<-modelmonthly$deepnitratemMm3[12]

MODmonthtarget[1:nmodobs,1,3]<-modelmonthly$surfammoniamMm3[1]
MODmonthtarget[1:nmodobs,2,3]<-modelmonthly$surfammoniamMm3[2]
MODmonthtarget[1:nmodobs,3,3]<-modelmonthly$surfammoniamMm3[3]
MODmonthtarget[1:nmodobs,4,3]<-modelmonthly$surfammoniamMm3[4]
MODmonthtarget[1:nmodobs,5,3]<-modelmonthly$surfammoniamMm3[5]
MODmonthtarget[1:nmodobs,6,3]<-modelmonthly$surfammoniamMm3[6]
MODmonthtarget[1:nmodobs,7,3]<-modelmonthly$surfammoniamMm3[7]
MODmonthtarget[1:nmodobs,8,3]<-modelmonthly$surfammoniamMm3[8]
MODmonthtarget[1:nmodobs,9,3]<-modelmonthly$surfammoniamMm3[9]
MODmonthtarget[1:nmodobs,10,3]<-modelmonthly$surfammoniamMm3[10]
MODmonthtarget[1:nmodobs,11,3]<-modelmonthly$surfammoniamMm3[11]
MODmonthtarget[1:nmodobs,12,3]<-modelmonthly$surfammoniamMm3[12]

MODmonthtarget[1:nmodobs,1,4]<-modelmonthly$deepammoniamMm3[1]
MODmonthtarget[1:nmodobs,2,4]<-modelmonthly$deepammoniamMm3[2]
MODmonthtarget[1:nmodobs,3,4]<-modelmonthly$deepammoniamMm3[3]
MODmonthtarget[1:nmodobs,4,4]<-modelmonthly$deepammoniamMm3[4]
MODmonthtarget[1:nmodobs,5,4]<-modelmonthly$deepammoniamMm3[5]
MODmonthtarget[1:nmodobs,6,4]<-modelmonthly$deepammoniamMm3[6]
MODmonthtarget[1:nmodobs,7,4]<-modelmonthly$deepammoniamMm3[7]
MODmonthtarget[1:nmodobs,8,4]<-modelmonthly$deepammoniamMm3[8]
MODmonthtarget[1:nmodobs,9,4]<-modelmonthly$deepammoniamMm3[9]
MODmonthtarget[1:nmodobs,10,4]<-modelmonthly$deepammoniamMm3[10]
MODmonthtarget[1:nmodobs,11,4]<-modelmonthly$deepammoniamMm3[11]
MODmonthtarget[1:nmodobs,12,4]<-modelmonthly$deepammoniamMm3[12]

MODmonthtarget[1:nmodobs,1,5]<-modelmonthly$surfchlmgm3[1]
MODmonthtarget[1:nmodobs,2,5]<-modelmonthly$surfchlmgm3[2]
MODmonthtarget[1:nmodobs,3,5]<-modelmonthly$surfchlmgm3[3]
MODmonthtarget[1:nmodobs,4,5]<-modelmonthly$surfchlmgm3[4]
MODmonthtarget[1:nmodobs,5,5]<-modelmonthly$surfchlmgm3[5]
MODmonthtarget[1:nmodobs,6,5]<-modelmonthly$surfchlmgm3[6]
MODmonthtarget[1:nmodobs,7,5]<-modelmonthly$surfchlmgm3[7]
MODmonthtarget[1:nmodobs,8,5]<-modelmonthly$surfchlmgm3[8]
MODmonthtarget[1:nmodobs,9,5]<-modelmonthly$surfchlmgm3[9]
MODmonthtarget[1:nmodobs,10,5]<-modelmonthly$surfchlmgm3[10]
MODmonthtarget[1:nmodobs,11,5]<-modelmonthly$surfchlmgm3[11]
MODmonthtarget[1:nmodobs,12,5]<-modelmonthly$surfchlmgm3[12]

MODmonthtarget[1:nmodobs,1,6]<-modelmonthly$onmizoomMNm3[1]
MODmonthtarget[1:nmodobs,2,6]<-modelmonthly$onmizoomMNm3[2]
MODmonthtarget[1:nmodobs,3,6]<-modelmonthly$onmizoomMNm3[3]
MODmonthtarget[1:nmodobs,4,6]<-modelmonthly$onmizoomMNm3[4]
MODmonthtarget[1:nmodobs,5,6]<-modelmonthly$onmizoomMNm3[5]
MODmonthtarget[1:nmodobs,6,6]<-modelmonthly$onmizoomMNm3[6]
MODmonthtarget[1:nmodobs,7,6]<-modelmonthly$onmizoomMNm3[7]
MODmonthtarget[1:nmodobs,8,6]<-modelmonthly$onmizoomMNm3[8]
MODmonthtarget[1:nmodobs,9,6]<-modelmonthly$onmizoomMNm3[9]
MODmonthtarget[1:nmodobs,10,6]<-modelmonthly$onmizoomMNm3[10]
MODmonthtarget[1:nmodobs,11,6]<-modelmonthly$onmizoomMNm3[11]
MODmonthtarget[1:nmodobs,12,6]<-modelmonthly$onmizoomMNm3[12]

MODmonthtarget[1:nmodobs,1,7]<-modelmonthly$carnzoomMNm3[1]
MODmonthtarget[1:nmodobs,2,7]<-modelmonthly$carnzoomMNm3[2]
MODmonthtarget[1:nmodobs,3,7]<-modelmonthly$carnzoomMNm3[3]
MODmonthtarget[1:nmodobs,4,7]<-modelmonthly$carnzoomMNm3[4]
MODmonthtarget[1:nmodobs,5,7]<-modelmonthly$carnzoomMNm3[5]
MODmonthtarget[1:nmodobs,6,7]<-modelmonthly$carnzoomMNm3[6]
MODmonthtarget[1:nmodobs,7,7]<-modelmonthly$carnzoomMNm3[7]
MODmonthtarget[1:nmodobs,8,7]<-modelmonthly$carnzoomMNm3[8]
MODmonthtarget[1:nmodobs,9,7]<-modelmonthly$carnzoomMNm3[9]
MODmonthtarget[1:nmodobs,10,7]<-modelmonthly$carnzoomMNm3[10]
MODmonthtarget[1:nmodobs,11,7]<-modelmonthly$carnzoomMNm3[11]
MODmonthtarget[1:nmodobs,12,7]<-modelmonthly$carnzoomMNm3[12]

MODmonthtarget[1:nmodobs,1,8]<-modelmonthly$benthslarmMNm3[1]
MODmonthtarget[1:nmodobs,2,8]<-modelmonthly$benthslarmMNm3[2]
MODmonthtarget[1:nmodobs,3,8]<-modelmonthly$benthslarmMNm3[3]
MODmonthtarget[1:nmodobs,4,8]<-modelmonthly$benthslarmMNm3[4]
MODmonthtarget[1:nmodobs,5,8]<-modelmonthly$benthslarmMNm3[5]
MODmonthtarget[1:nmodobs,6,8]<-modelmonthly$benthslarmMNm3[6]
MODmonthtarget[1:nmodobs,7,8]<-modelmonthly$benthslarmMNm3[7]
MODmonthtarget[1:nmodobs,8,8]<-modelmonthly$benthslarmMNm3[8]
MODmonthtarget[1:nmodobs,9,8]<-modelmonthly$benthslarmMNm3[9]
MODmonthtarget[1:nmodobs,10,8]<-modelmonthly$benthslarmMNm3[10]
MODmonthtarget[1:nmodobs,11,8]<-modelmonthly$benthslarmMNm3[11]
MODmonthtarget[1:nmodobs,12,8]<-modelmonthly$benthslarmMNm3[12]

MODmonthtarget[1:nmodobs,1,9]<-modelmonthly$benthclarmMNm3[1]
MODmonthtarget[1:nmodobs,2,9]<-modelmonthly$benthclarmMNm3[2]
MODmonthtarget[1:nmodobs,3,9]<-modelmonthly$benthclarmMNm3[3]
MODmonthtarget[1:nmodobs,4,9]<-modelmonthly$benthclarmMNm3[4]
MODmonthtarget[1:nmodobs,5,9]<-modelmonthly$benthclarmMNm3[5]
MODmonthtarget[1:nmodobs,6,9]<-modelmonthly$benthclarmMNm3[6]
MODmonthtarget[1:nmodobs,7,9]<-modelmonthly$benthclarmMNm3[7]
MODmonthtarget[1:nmodobs,8,9]<-modelmonthly$benthclarmMNm3[8]
MODmonthtarget[1:nmodobs,9,9]<-modelmonthly$benthclarmMNm3[9]
MODmonthtarget[1:nmodobs,10,9]<-modelmonthly$benthclarmMNm3[10]
MODmonthtarget[1:nmodobs,11,9]<-modelmonthly$benthclarmMNm3[11]
MODmonthtarget[1:nmodobs,12,9]<-modelmonthly$benthclarmMNm3[12]

MODmonthtarget[1:nmodobs,1:12,10]<-MODmonthtarget[1:nmodobs,1:12,8]+MODmonthtarget[1:nmodobs,1:12,9]
#Combine the s/d and c/s benthos




monlab<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

	#.......................

	installtargdata<-function(qr,obspar,monthtarget){
		#Make sure that the selected data are in the correct order
		data<-data.frame("Month"=monlab,"median"=rep(NA,12),"17centile"=rep(NA,12),"83centile"=rep(NA,12))
		for(mmm in 1:12){
			data[mmm,2:4]<-seldata[(which(seldata$Month==data$Month[mmm])),3:5]
		}
		for(mmm in 1:12){
			sdpos<-(data[mmm,4]-data[mmm,2])/(qr/2)
			sdneg<-(data[mmm,2]-data[mmm,3])/(qr/2)
			for(kkk in 1:ntargobs){
				rand<-rnorm(1,0,1)
				if(rand<0) dev<-rand*sdneg
				if(rand>=0) dev<-rand*sdpos
				monthtarget[kkk,mmm,obspar]<-data[mmm,2]+dev
			}
		}

		return(monthtarget)
	}
	#~~~~~~~~~~~~~




#~~~~~~~~~~~~~
#Read in the monthly surface nitrate target data file
#qr=number of sd in the observed quantile range
#obspar=position of the data in the data array
seldata<-subset(obstargetdataset,obstargetdataset$Variable=="surface_nitrate")
obspar<-1
qr<-3.5
if(seldata[1,7]==17 & seldata[1,8]==83) qr<-3.5
if(seldata[1,7]==5 & seldata[1,8]==95) qr<-7
monthtarget<-installtargdata(qr,obspar,monthtarget)


#~~~~~~~~~~~~~
#Read in the monthly deep nitrate target data file
#qr=number of sd in the observed quantile range
#obspar=position of the data in the data array
seldata<-subset(obstargetdataset,obstargetdataset$Variable=="deep_nitrate")
obspar<-2
qr<-3.5
if(seldata[1,7]==17 & seldata[1,8]==83) qr<-3.5
if(seldata[1,7]==5 & seldata[1,8]==95) qr<-7
monthtarget<-installtargdata(qr,obspar,monthtarget)


#~~~~~~~~~~~~~
#Read in the monthly surface ammonia target data file
#qr=number of sd in the observed quantile range
#obspar=position of the data in the data array
seldata<-subset(obstargetdataset,obstargetdataset$Variable=="surface_ammonia")
obspar<-3
qr<-3.5
if(seldata[1,7]==17 & seldata[1,8]==83) qr<-3.5
if(seldata[1,7]==5 & seldata[1,8]==95) qr<-7
monthtarget<-installtargdata(qr,obspar,monthtarget)


#~~~~~~~~~~~~~
#Read in the monthly deep ammonia target data file
#qr=number of sd in the observed quantile range
#obspar=position of the data in the data array
seldata<-subset(obstargetdataset,obstargetdataset$Variable=="deep_ammonia")
obspar<-4
qr<-3.5
if(seldata[1,7]==17 & seldata[1,8]==83) qr<-3.5
if(seldata[1,7]==5 & seldata[1,8]==95) qr<-7
monthtarget<-installtargdata(qr,obspar,monthtarget)


#~~~~~~~~~~~~~
#Read in the monthly surface chlorophyll target data file
#qr=number of sd in the observed quantile range
#obspar=position of the data in the data array
seldata<-subset(obstargetdataset,obstargetdataset$Variable=="surface_chlorophyll")
obspar<-5
qr<-3.5
if(seldata[1,7]==17 & seldata[1,8]==83) qr<-3.5
if(seldata[1,7]==5 & seldata[1,8]==95) qr<-7
monthtarget<-installtargdata(qr,obspar,monthtarget)


#~~~~~~~~~~~~~
#Read in the monthly omniv zooplankton target data file
#qr=number of sd in the observed quantile range
#obspar=position of the data in the data array
seldata<-subset(obstargetdataset,obstargetdataset$Variable=="omniv_zooplankton")
obspar<-6
qr<-3.5
if(seldata[1,7]==17 & seldata[1,8]==83) qr<-3.5
if(seldata[1,7]==5 & seldata[1,8]==95) qr<-7
monthtarget<-installtargdata(qr,obspar,monthtarget)


#~~~~~~~~~~~~~
#Read in the monthly carniv zooplankton target data file
#qr=number of sd in the observed quantile range
#obspar=position of the data in the data array
seldata<-subset(obstargetdataset,obstargetdataset$Variable=="carniv_zooplankton")
obspar<-7
qr<-3.5
if(seldata[1,7]==17 & seldata[1,8]==83) qr<-3.5
if(seldata[1,7]==5 & seldata[1,8]==95) qr<-7
monthtarget<-installtargdata(qr,obspar,monthtarget)


#~~~~~~~~~~~~~
#Read in the monthly larvae of susp/dep feeding benthos target data file
#qr=number of sd in the observed quantile range
#obspar=position of the data in the data array
seldata<-subset(obstargetdataset,obstargetdataset$Variable=="larvae_susp_dep_benthos")
obspar<-8
qr<-3.5
if(seldata[1,7]==17 & seldata[1,8]==83) qr<-3.5
if(seldata[1,7]==5 & seldata[1,8]==95) qr<-7
monthtarget<-installtargdata(qr,obspar,monthtarget)


#~~~~~~~~~~~~~
#Read in the monthly larvae of carn/scav feeding benthos target data file
#qr=number of sd in the observed quantile range
#obspar=position of the data in the data array
seldata<-subset(obstargetdataset,obstargetdataset$Variable=="larvae_carn_scav_benthos")
obspar<-9
qr<-3.5
if(seldata[1,7]==17 & seldata[1,8]==83) qr<-3.5
if(seldata[1,7]==5 & seldata[1,8]==95) qr<-7
monthtarget<-installtargdata(qr,obspar,monthtarget)



#~~~~~~~~~~~~~
#COMBINE THE TWO TYPES OF BENTHIC LARVAE
#Read in the monthly susp benlar target data file
seldata<-subset(obstargetdataset,obstargetdataset$Variable=="larvae_susp_dep_benthos")
#Read in the monthly carn benlar target data file
seldata2<-subset(obstargetdataset,obstargetdataset$Variable=="larvae_carn_scav_benthos")
seldata[,3]<-seldata[,3]+seldata2[,3]
seldata[,4]<-seldata[,4]+seldata2[,4]
seldata[,5]<-seldata[,5]+seldata2[,5]
obspar<-10
qr<-3.5
if(seldata[1,7]==17 & seldata[1,8]==83) qr<-3.5
if(seldata[1,7]==5 & seldata[1,8]==95) qr<-7
monthtarget<-installtargdata(qr,obspar,monthtarget)



	#~~~~~~~~~~~~~~~
	plotdata<-function(obspar){
		obsplot<-as.data.frame(monthtarget[,,obspar])
		names(obsplot)<-monlab
		modplot<-as.data.frame(MODmonthtarget[,,obspar])
		if(obspar==1) boxplot(obsplot,range=0,boxwex=0.25,ylim=c(0,30),las=1,par(mar=c(1.5,5,1,1)),cex.axis=1.3,cex.lab=1.3,ylab=expression(paste("Surf.nitrate mM.m"^{-3},sep="")))
		if(obspar==2) boxplot(obsplot,range=0,boxwex=0.25,ylim=c(0,30),las=1,par(mar=c(1.5,5,1,1)),cex.axis=1.3,cex.lab=1.3,ylab=expression(paste("Deep nitrate mM.m"^{-3},sep="")))
		if(obspar==3) boxplot(obsplot,range=0,boxwex=0.25,ylim=c(0,10),las=1,par(mar=c(1.5,5,1,1)),cex.axis=1.3,cex.lab=1.3,ylab=expression(paste("Surf.ammonia mM.m"^{-3},sep="")))
		if(obspar==4) boxplot(obsplot,range=0,boxwex=0.25,ylim=c(0,10),las=1,par(mar=c(1.5,5,1,1)),cex.axis=1.3,cex.lab=1.3,ylab=expression(paste("Deep ammonia mM.m"^{-3},sep="")))
		if(obspar==5) boxplot(obsplot,range=0,boxwex=0.25,ylim=c(0,8),las=1,par(mar=c(1.5,5,1,1)),cex.axis=1.3,cex.lab=1.3,ylab=expression(paste("Chlorophyll mg.m"^{-3},sep="")))
		if(obspar==6) boxplot(obsplot,range=0,boxwex=0.25,ylim=c(0,3),las=1,par(mar=c(1.5,5,1,1)),cex.axis=1.3,cex.lab=1.3,ylab=expression(paste("Omniv.zoo. mMN.m"^{-3},sep="")))
		if(obspar==7) boxplot(obsplot,range=0,boxwex=0.25,ylim=c(0,0.5),las=1,par(mar=c(1.5,5,1,1)),cex.axis=1.3,cex.lab=1.3,ylab=expression(paste("Carniv.zoo. mMN.m"^{-3},sep="")))
		if(obspar==8) boxplot(obsplot,range=0,boxwex=0.25,ylim=c(0,3),las=1,par(mar=c(1.5,5,1,1)),cex.axis=1.3,cex.lab=1.3,ylab=expression(paste("Larv.s/d.benth. mMN.m"^{-3},sep="")))
		if(obspar==9) boxplot(obsplot,range=0,boxwex=0.25,ylim=c(0,0.6),las=1,par(mar=c(1.5,5,1,1)),cex.axis=1.3,cex.lab=1.3,ylab=expression(paste("Larv.c/s.benth. mMN.m"^{-3},sep="")))
		if(obspar==10) boxplot(obsplot,range=0,boxwex=0.25,ylim=c(0,3.0),las=1,par(mar=c(1.5,5,1,1)),cex.axis=1.3,cex.lab=1.3,ylab=expression(paste("Larv.benth. mMN.m"^{-3},sep="")))

		boxplot(modplot,add=TRUE,range=0,boxwex=0.25,at=1:12+0.3,par(lty=1),yaxt="n",xaxt="n")
	}
	#~~~~~~~~~~~~~~~

#jpeg(paste(oudir,"monthly_model_vs_data.jpg",sep=""),width=500,height=500)
#pdf(paste(oudir,"monthly_model_vs_data.pdf",sep=""),width=6,height=9)
#postscript(paste(oudir,"monthly_model_vs_data.ps",sep=""),width=6,height=6,horizontal=FALSE)


par(mfrow=c(5,2))

plotdata(1)

plotdata(2)

plotdata(3)

plotdata(4)

plotdata(5)

plotdata(6)

plotdata(7)

#plotdata(8)

#plotdata(9)

plotdata(10)


#dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

}


