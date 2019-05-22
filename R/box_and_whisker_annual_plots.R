#
# box_and_whisker_annual_plots.R
#
#' plot observed and modelled distributions of the annual ecosystem flux data
#'
#' @param model model object
#' @param annual.target.data annual target data
#' @param opt_results model target results
#'
#' @export
#
box_and_whisker_annual_plots <- function(model, annual.target.data, opt_results) {

	run		<- elt(model, "run")
	resultsdir	<- elt(run, "resultsdir")

#Requires the dataframe opt_results produced by the programme which calculates the model outputs corresponding to each observed variable

#Column 1 is the observed value, column 2 is the sd, column 3 is the fitted model value

#Column 4 is a flag to say whether the observed value was used in the fitting 1=yes, 0=no




sim_targetdata<-opt_results[,1:3]
names(sim_targetdata)<-c("observed","observed_sd","model")

annualtargetnames<-rep("xx",nrow(opt_results))

annualtargetnames[which(opt_results[,6]=="Obs_TAPP")] <- "Total phyt."
annualtargetnames[which(opt_results[,6]=="Obs_NP")] <- "New primary phyt."
annualtargetnames[which(opt_results[,6]=="Obs_KelpP")] <- "Kelp carbon"
annualtargetnames[which(opt_results[,6]=="Obs_OmnizooP")] <- "Omniv.zooplankton"
annualtargetnames[which(opt_results[,6]=="Obs_CarnzooP")] <- "Carniv.zooplankton"
annualtargetnames[which(opt_results[,6]=="Obs_PFishP")] <- "Planktiv.fish"
annualtargetnames[which(opt_results[,6]=="Obs_DFishP")] <- "Demersal fish"
annualtargetnames[which(opt_results[,6]=="Obs_BensuspP")] <- "Filt/dep.benthos"
annualtargetnames[which(opt_results[,6]=="Obs_BencarnP")] <- "Carn/scav.benthos"
annualtargetnames[which(opt_results[,6]=="Obs_birdP")] <- "Seabird"
annualtargetnames[which(opt_results[,6]=="Obs_sealP")] <- "Seal"
annualtargetnames[which(opt_results[,6]=="Obs_cetaP")] <- "Ceatacean"
annualtargetnames[which(opt_results[,6]=="Obs_maxbenthslar")] <- "Filt/dep.benthos larv"
annualtargetnames[which(opt_results[,6]=="Obs_maxbenthclar")] <- "Carn/scav.benthos larv"
annualtargetnames[which(opt_results[,6]=="Obs_Conpfishfish")] <- "Pel.fish by fish"
annualtargetnames[which(opt_results[,6]=="Obs_Condfishfish")] <- "Dem.fish by fish"
annualtargetnames[which(opt_results[,6]=="Obs_Conzoofish")] <- "Zooplankton by fish"
annualtargetnames[which(opt_results[,6]=="Obs_Conzoocarnz")] <- "Meso-zoo by carniv.zoo."
annualtargetnames[which(opt_results[,6]=="Obs_Conbenfish")] <- "Benthos by fish"
annualtargetnames[which(opt_results[,6]=="Obs_Contotal_bird")] <- "Total by birds"
annualtargetnames[which(opt_results[,6]=="Obs_Proppfishbird")] <- "Plank.fish in bird diet"
annualtargetnames[which(opt_results[,6]=="Obs_Propdfishbird")] <- "Dem.fish in bird diet"
annualtargetnames[which(opt_results[,6]=="Obs_Propmfishbird")] <- "Mig.fish in bird diet"
annualtargetnames[which(opt_results[,6]=="Obs_Propdiscbird")] <- "Disc. in bird diet"
annualtargetnames[which(opt_results[,6]=="Obs_Contotal_seal")] <- "Total by seals"
annualtargetnames[which(opt_results[,6]=="Obs_Proppfishseal")] <- "Plank.fish in seal diet"
annualtargetnames[which(opt_results[,6]=="Obs_Propdfishseal")] <- "Dem.fish in seal diet"
annualtargetnames[which(opt_results[,6]=="Obs_Propmfishseal")] <- "Mig.fish in seal diet"
annualtargetnames[which(opt_results[,6]=="Obs_Contotal_ceta")] <- "Total by cetaceans"
annualtargetnames[which(opt_results[,6]=="Obs_Proppfishceta")] <- "Plank.fish in cet. diet"
annualtargetnames[which(opt_results[,6]=="Obs_Propdfishceta")] <- "Dem.fish in cet. diet"
annualtargetnames[which(opt_results[,6]=="Obs_Propmfishceta")] <- "Mig.fish in cet. diet"
annualtargetnames[which(opt_results[,6]=="Obs_Pland_livewt")] <- "Plank.fish landings"
annualtargetnames[which(opt_results[,6]=="Obs_Dland_livewt")] <- "Dem.fish landings"
annualtargetnames[which(opt_results[,6]=="Obs_Mland_livewt")] <- "Mig.fish landings"
annualtargetnames[which(opt_results[,6]=="Obs_Bsland_livewt")] <- "Filt/dep.benthos landings" 
annualtargetnames[which(opt_results[,6]=="Obs_Bcland_livewt")] <- "Carn/scav.benthos landings"
annualtargetnames[which(opt_results[,6]=="Obs_Zcland_livewt")] <- "Pel.invert. landings"
annualtargetnames[which(opt_results[,6]=="Obs_Kland_livewt")] <- "Kelp harvest"
annualtargetnames[which(opt_results[,6]=="Obs_kelp_pb")] <- "Kelp P/B"
annualtargetnames[which(opt_results[,6]=="Obs_benslar_pb")] <- "Filt/dep.benthos larv. P/B"
annualtargetnames[which(opt_results[,6]=="Obs_benclar_pb")] <- "Carn/scav.benthos larv. P/B"
annualtargetnames[which(opt_results[,6]=="Obs_bens_pb")] <- "Filt/dep.benthos P/B"
annualtargetnames[which(opt_results[,6]=="Obs_benc_pb")] <- "Carn/scav.benthos P/B"
annualtargetnames[which(opt_results[,6]=="Obs_herb_pb")] <- "Omniv.zooplankton P/B"
annualtargetnames[which(opt_results[,6]=="Obs_carn_pb")] <- "Carniv.zooplankton P/B"
annualtargetnames[which(opt_results[,6]=="Obs_fishplar_pb")] <- "Plank.fish larvae P/B"
annualtargetnames[which(opt_results[,6]=="Obs_fishdlar_pb")] <- "Dem.fish larvae P/B"
annualtargetnames[which(opt_results[,6]=="Obs_fishp_pb")] <- "Plank.fish P/B"
annualtargetnames[which(opt_results[,6]=="Obs_fishd_pb")] <- "Dem.fish P/B"
annualtargetnames[which(opt_results[,6]=="Obs_fishm_pb")] <- "Mig.fish P/B"
annualtargetnames[which(opt_results[,6]=="Obs_bird_pb")] <- "Bird P/B"
annualtargetnames[which(opt_results[,6]=="Obs_seal_pb")] <- "Seal P/B"
annualtargetnames[which(opt_results[,6]=="Obs_ceta_pb")] <- "Cetacean P/B"
annualtargetnames[which(opt_results[,6]=="Obs_exud_C_kelp")] <- "Prop. kelp prod. exuded"
annualtargetnames[which(opt_results[,6]=="Obs_kelp_NC")] <- "Kelp N/C ratio"
annualtargetnames[which(opt_results[,6]=="Obs_Denitrif")] <- "Denitrification"
annualtargetnames[which(opt_results[,6]=="Obs_Dfdiscardp")] <- "Dem.fish discard/catch"
annualtargetnames[which(opt_results[,6]=="Obs_s_x_ammonia")] <- "Sand porewater ammonia"
annualtargetnames[which(opt_results[,6]=="Obs_d_x_ammonia")] <- "Mud porewater ammonia"
annualtargetnames[which(opt_results[,6]=="Obs_s_x_nitrate")] <- "Sand porewater nitrate"
annualtargetnames[which(opt_results[,6]=="Obs_d_x_nitrate")] <- "Mud porewater nitrate"
annualtargetnames[which(opt_results[,6]=="Obs_s_x_TON")] <- "Sand %TON"
annualtargetnames[which(opt_results[,6]=="Obs_d_x_TON")] <- "Mud %TON"
annualtargetnames[which(opt_results[,6]=="Obs_NDJF_s_nitrate")] <- "Winter surf.nitrate"
annualtargetnames[which(opt_results[,6]=="Obs_MJJA_s_nitrate")] <- "Summer surf.nitrate"
annualtargetnames[which(opt_results[,6]=="Obs_NDJF_d_nitrate")] <- "Winter deep nitrate"
annualtargetnames[which(opt_results[,6]=="Obs_MJJA_d_nitrate")] <- "Summer deep nitrate"
annualtargetnames[which(opt_results[,6]=="Obs_NDJF_s_ammonia")] <- "Winter surf.ammonia"
annualtargetnames[which(opt_results[,6]=="Obs_MJJA_s_ammonia")] <- "Summer surf.ammonia"
annualtargetnames[which(opt_results[,6]=="Obs_NDJF_d_ammonia")] <- "Winter deep ammonia"
annualtargetnames[which(opt_results[,6]=="Obs_MJJA_d_ammonia")] <- "Summer deep ammonia"
annualtargetnames[which(opt_results[,6]=="Obs_carn_io_ratio")] <- "Carniv.zooplanton"
annualtargetnames[which(opt_results[,6]=="Obs_herb_io_ratio")] <- "Omniv.zooplankton"
annualtargetnames[which(opt_results[,6]=="Obs_phyt_io_ratio")] <- "Surf.phytoplankton"
annualtargetnames[which(opt_results[,6]=="Obs_nit_io_ratio")] <- "Surf.nitrate"
annualtargetnames[which(opt_results[,6]=="Obs_amm_io_ratio")] <- "Surf.ammomnia"
annualtargetnames[which(opt_results[,6]=="Obs_pfish_io_ratio")] <- "Plank.fish"
annualtargetnames[which(opt_results[,6]=="Obs_dfish_io_ratio")] <- "Dem.fish"
annualtargetnames[which(opt_results[,6]=="Obs_birddisc")] <- "Bird by-catch"
annualtargetnames[which(opt_results[,6]=="Obs_sealdisc")] <- "Seal by-catch"
annualtargetnames[which(opt_results[,6]=="Obs_cetadisc")] <- "Cetacean by-catch"
annualtargetnames[which(opt_results[,6]=="Obs_kelp_beachcast")] <- "Kelp beach-cast"
annualtargetnames[which(opt_results[,6]=="Obs_Ctland_livewt")] <- "Whale_landings"



nmeasures<-nrow(sim_targetdata)
ntargobs<-1000

annualtarget<-array(0,dim=c(ntargobs,nmeasures))
#dimensions observations,parameter 
for(kkk in 1:ntargobs){
rand<-rnorm(nmeasures,0,1)
annualtarget[kkk,1:nmeasures]<-sim_targetdata[,1]+(rand*sim_targetdata[,2])
}
colnames(annualtarget)<-annualtargetnames
#~~~~~~~~~~~~~

ntargobs<-10
modeltarget<-array(0,dim=c(ntargobs,nmeasures))
#dimensions observations,parameter 
for(kkk in 1:ntargobs){
rand<-rnorm(nmeasures,0,1)
modeltarget[kkk,1:nmeasures]<-sim_targetdata[,3]+(rand*0)
}
colnames(modeltarget)<-annualtargetnames
#~~~~~~~~~~~~~

#IDs for Annual production rates
set2plot1.1<-c(
   which(opt_results[,6]=="Obs_KelpP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_TAPP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_NP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Denitrif" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_OmnizooP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_CarnzooP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_BensusP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_BencarnP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_PFishP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_DFishP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_birdP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_sealP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_cetaP" & opt_results[,4]==1)   )

#IDs for Annual fishery landings and by-catch (live weights)
set2plot1.2<-c(
   which(opt_results[,6]=="Obs_Pland_livewt" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Dland_livewt" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Mland_livewt" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Bsland_livewt" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Bcland_livewt" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Zcland_livewt" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Ctland_livewt" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Kland_livewt" & opt_results[,4]==1)  ,
   which(opt_results[,6]=="Obs_cetadisc" & opt_results[,4]==1)  ,
   which(opt_results[,6]=="Obs_sealdisc" & opt_results[,4]==1)  ,
   which(opt_results[,6]=="Obs_birddisc" & opt_results[,4]==1)  ,
   which(opt_results[,6]=="Obs_Dfdiscardp" & opt_results[,4]==1)  )

#IDs for annual consumption rates
set2plot2<-c(
   which(opt_results[,6]=="Obs_Conzoocarnz" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Conzoofish" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Conbenfish" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Conpfishfish" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Condfishfish" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Contotal_bird" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Contotal_seal" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Contotal_ceta" & opt_results[,4]==1) ,

   which(opt_results[,6]=="Obs_Proppfishbird" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Propdfishbird" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Propmfishbird" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Propdiscbird" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Proppfishseal" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Propdfishseal" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Propmfishseal" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Proppfishceta" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Propdfishceta" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Propmfishceta" & opt_results[,4]==1) )

#Annual PB and other ratios
set2plot3<-c(
   which(opt_results[,6]=="Obs_kelp_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_kelp_NC" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_exud_C_kelp" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_kelp_beachcast" & opt_results[,4]==1) ,

   which(opt_results[,6]=="Obs_herb_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_benslar_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_benclar_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_fishplar_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_fishdlar_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_carn_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_bens_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_benc_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_fishp_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_fishd_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_fishm_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_bird_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_seal_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_ceta_pb" & opt_results[,4]==1) )

#Average nutrient concentaryions in water and sediments
set2plot4<-c(
   which(opt_results[,6]=="Obs_s_x_ammonia" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_d_x_ammonia" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_s_x_nitrate" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_d_x_nitrate" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_s_x_TON" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_d_x_TON" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_NDJF_s_nitrate" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_MJJA_s_nitrate" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_NDJF_d_nitrate" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_MJJA_d_nitrate" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_NDJF_s_ammonia" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_MJJA_s_ammonia" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_NDJF_d_ammonia" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_MJJA_d_ammonia" & opt_results[,4]==1) )

#Inshore:offshore ratios
set2plot5<-c(
   which(opt_results[,6]=="Obs_amm_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_nit_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_phyt_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_herb_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_carn_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_pfish_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_dfish_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_bird_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_seal_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_ceta_io_ratio" & opt_results[,4]==1) )


#jpeg(paste(resultsdir,"annual_model_vs_data.jpg",sep=""),width=500,height=500)
# png(paste(resultsdir,"annual_model_vs_data.png",sep=""),width=700,height=500)
#pdf(paste(resultsdir,"annual_model_vs_data.pdf",sep=""),width=6,height=9)

par(mfrow=c(3,2))

tfnt<-1.0

if(length(set2plot1.1)>0){
#Catch values which are negative and set them to NA
for(qqqqq in 1:length(set2plot1.1)){
if(length(which(annualtarget[,set2plot1.1[qqqqq]]<0)>0)){
annualtarget[which(annualtarget[,set2plot1.1[qqqqq]]<0),set2plot1.1[qqqqq]] <- (NA)
}}
boxplot(as.data.frame(log10(annualtarget[,set2plot1.1])),range=0,boxwex=0.3,ylim=c(log10(0.0001),log10(2000)),horizontal=TRUE,las=1,par(mar=c(4,14,.5,1.75),lty=1,cex=0.6,cex.lab=tfnt,cex.axis=tfnt))
mtext(expression(paste("Annual production Log"[{10}]," mMN.m"^{-2},".y"^{-1},sep="")),cex=0.9,side=1,line=2.5)
boxplot(as.data.frame(log10(modeltarget[,set2plot1.1])),add=TRUE,boxcol="red",whiskcol="red",whisklty="solid",medcol="red",staplecol="red",range=0,boxwex=0.3,at=seq(1,length(set2plot1.1))+0.35,horizontal=TRUE,par(lty=1),yaxt="n",xaxt="n")
}

if(length(set2plot1.2)>0){
#Catch values which are negative and set them to NA
for(qqqqq in 1:length(set2plot1.2)){
if(length(which(annualtarget[,set2plot1.2[qqqqq]]<0)>0)){
annualtarget[which(annualtarget[,set2plot1.2[qqqqq]]<0),set2plot1.2[qqqqq]] <- (NA)
}}
boxplot(as.data.frame(log10(annualtarget[,set2plot1.2])),range=0,boxwex=0.3,ylim=c(log10(0.0000001),log10(100)),horizontal=TRUE,las=1,par(mar=c(4,14,.5,1.75),lty=1,cex=0.6,cex.lab=tfnt,cex.axis=tfnt))
mtext(expression(paste("Annual fishery yields Log"[{10}]," mMN.m"^{-2},".y"^{-1},sep="")),cex=0.9,side=1,line=2.5)
boxplot(as.data.frame(log10(modeltarget[,set2plot1.2])),add=TRUE,boxcol="red",whiskcol="red",whisklty="solid",medcol="red",staplecol="red",range=0,boxwex=0.3,at=seq(1,length(set2plot1.2))+0.35,horizontal=TRUE,par(lty=1),yaxt="n",xaxt="n")
}

if(length(set2plot2)>0){
#Catch values which are negative and set them to NA
for(qqqqq in 1:length(set2plot2)){
if(length(which(annualtarget[,set2plot2[qqqqq]]<0)>0)){
annualtarget[which(annualtarget[,set2plot2[qqqqq]]<0),set2plot2[qqqqq]] <- (NA)
}}
boxplot(as.data.frame(log10(annualtarget[,set2plot2])),range=0,boxwex=0.3,ylim=c(log10(0.00001),log10(200)),horizontal=TRUE,las=1,par(mar=c(4,14,.5,1.75),lty=1,cex=0.6,cex.lab=tfnt,cex.axis=tfnt))
mtext(expression(paste("Annual consumption Log"[{10}]," mMN.m"^{-2},".y"^{-1},sep="")),cex=0.9,side=1,line=2.5)
boxplot(as.data.frame(log10(modeltarget[,set2plot2])),add=TRUE,boxcol="red",whiskcol="red",whisklty="solid",medcol="red",staplecol="red",range=0,boxwex=0.3,at=seq(1,length(set2plot2))+0.35,horizontal=TRUE,par(lty=1),yaxt="n",xaxt="n")
}

if(length(set2plot3)>0){
#Catch values which are negative and set them to NA
for(qqqqq in 1:length(set2plot3)){
if(length(which(annualtarget[,set2plot3[qqqqq]]<0)>0)){
annualtarget[which(annualtarget[,set2plot3[qqqqq]]<0),set2plot3[qqqqq]] <- (NA)
}}
boxplot(as.data.frame(log10(annualtarget[,set2plot3])),range=0,boxwex=0.3,ylim=c(log10(0.001),log10(100)),horizontal=TRUE,las=1,par(mar=c(4,14,.5,1.75),lty=1,cex=0.6,cex.lab=tfnt,cex.axis=tfnt))
mtext(expression(paste("Log"[{10}]," Annual ratio",sep="")),cex=0.9,side=1,line=2.5)
boxplot(as.data.frame(log10(modeltarget[,set2plot3])),add=TRUE,boxcol="red",whiskcol="red",whisklty="solid",medcol="red",staplecol="red",range=0,boxwex=0.3,at=seq(1,length(set2plot3))+0.35,horizontal=TRUE,par(lty=1),yaxt="n",xaxt="n")
}

if(length(set2plot4)>0){
#Catch values which are negative and set them to NA
for(qqqqq in 1:length(set2plot4)){
if(length(which(annualtarget[,set2plot4[qqqqq]]<0)>0)){
annualtarget[which(annualtarget[,set2plot4[qqqqq]]<0),set2plot4[qqqqq]] <- (NA)
}}
boxplot(as.data.frame(log10(annualtarget[,set2plot4])),range=0,boxwex=0.3,ylim=c(log10(0.0001),log10(1000)),horizontal=TRUE,las=1,par(mar=c(4,14,.5,1.75),lty=1,cex=0.6,cex.lab=tfnt,cex.axis=tfnt))
#mtext(expression(paste("Annual mean conc. Log"[{10}]," mMN.m"^{-3}," or % by weight",sep="")),cex=0.9,side=1,line=2.5)
mtext(expression(paste("Annual conc. Log"[{10}]," mMN.m"^{-3}," or % by weight",sep="")),cex=0.9,side=1,line=2.5)
boxplot(as.data.frame(log10(modeltarget[,set2plot4])),add=TRUE,boxcol="red",whiskcol="red",whisklty="solid",medcol="red",staplecol="red",range=0,boxwex=0.3,at=seq(1,length(set2plot4))+0.35,horizontal=TRUE,par(lty=1),yaxt="n",xaxt="n")
}

if(length(set2plot5)>0){
#Catch values which are negative and set them to NA
for(qqqqq in 1:length(set2plot5)){
if(length(which(annualtarget[,set2plot5[qqqqq]]<0)>0)){
annualtarget[which(annualtarget[,set2plot5[qqqqq]]<0),set2plot5[qqqqq]] <- (NA)
}}
boxplot(as.data.frame((annualtarget[,set2plot5])),range=0,boxwex=0.3,ylim=c(0,8),horizontal=TRUE,las=1,par(mar=c(4,14,.5,1.75),lty=1,cex=0.6,cex.lab=tfnt,cex.axis=tfnt))
mtext(expression(paste("Inshore:offshore ratios of annual mean conc.")),cex=0.9,side=1,line=2.5)
boxplot(as.data.frame((modeltarget[,set2plot5])),add=TRUE,boxcol="red",whiskcol="red",whisklty="solid",medcol="red",staplecol="red",range=0,boxwex=0.3,at=seq(1,length(set2plot5))+0.35,horizontal=TRUE,par(lty=1),yaxt="n",xaxt="n")
}


}

