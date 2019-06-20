#
# plot_sediment_habitats_final_year.R
#
#' Plot full time series of out
#'
#' Plot set of time series from model out
#'
#' @param model model object
#' @param results full model results
#'
#' @export
#
# most fixed/fitted parms etc are in the results$out from the ode
#
plot_sediment_habitats_final_year <- function(model, results) {

	data			<- elt(model, "data")
	physical.parameters	<- elt(data, "physical.parameters")
	x_poros_s1		<- elt(physical.parameters, "x_poros_s1")
	x_poros_s2		<- elt(physical.parameters, "x_poros_s2")
	x_poros_s3		<- elt(physical.parameters, "x_poros_s3")
	x_area_s1		<- elt(physical.parameters, "x_area_s1")
	x_area_s2		<- elt(physical.parameters, "x_area_s2")
	x_area_s3		<- elt(physical.parameters, "x_area_s3")
	x_depth_s1		<- elt(physical.parameters, "x_depth_s1")
	x_depth_s2		<- elt(physical.parameters, "x_depth_s2")
	x_depth_s3		<- elt(physical.parameters, "x_depth_s3")

	x_poros_d1		<- elt(physical.parameters, "x_poros_d1")
	x_poros_d2		<- elt(physical.parameters, "x_poros_d2")
	x_poros_d3		<- elt(physical.parameters, "x_poros_d3")
	x_area_d1		<- elt(physical.parameters, "x_area_d1")
	x_area_d2		<- elt(physical.parameters, "x_area_d2")
	x_area_d3		<- elt(physical.parameters, "x_area_d3")
	x_depth_d1		<- elt(physical.parameters, "x_depth_d1")
	x_depth_d2		<- elt(physical.parameters, "x_depth_d2")
	x_depth_d3		<- elt(physical.parameters, "x_depth_d3")

	build			<- elt(results, "build")
	run			<- elt(build, "run")
	nyears			<- elt(run, "nyears")
	ndays			<- elt(run, "ndays")

	output		<- elt(results, "output")
	x_ammonia_s1	<- elt(output, "x_ammonia_s1")
	x_ammonia_s2	<- elt(output, "x_ammonia_s2")
	x_ammonia_s3	<- elt(output, "x_ammonia_s3")
	x_ammonia_d1	<- elt(output, "x_ammonia_d1")
	x_ammonia_d2	<- elt(output, "x_ammonia_d2")
	x_ammonia_d3	<- elt(output, "x_ammonia_d3")
	x_nitrate_s1	<- elt(output, "x_nitrate_s1")
	x_nitrate_s2	<- elt(output, "x_nitrate_s2")
	x_nitrate_s3	<- elt(output, "x_nitrate_s3")
	x_nitrate_d1	<- elt(output, "x_nitrate_d1")
	x_nitrate_d2	<- elt(output, "x_nitrate_d2")
	x_nitrate_d3	<- elt(output, "x_nitrate_d3")
	x_detritus_s1	<- elt(output, "x_detritus_s1")
	xR_detritus_s1	<- elt(output, "xR_detritus_s1")
	x_detritus_s2	<- elt(output, "x_detritus_s2")
	xR_detritus_s2	<- elt(output, "xR_detritus_s2")
	x_detritus_s3	<- elt(output, "x_detritus_s3")
	xR_detritus_s3	<- elt(output, "xR_detritus_s3")
	x_detritus_d1	<- elt(output, "x_detritus_d1")
	xR_detritus_d1	<- elt(output, "xR_detritus_d1")
	x_detritus_d2	<- elt(output, "x_detritus_d2")
	xR_detritus_d2	<- elt(output, "xR_detritus_d2")
	x_detritus_d3	<- elt(output, "x_detritus_d3")
	xR_detritus_d3	<- elt(output, "xR_detritus_d3")
	corpse_s1	<- elt(output, "corpse_s1")
	corpse_s2	<- elt(output, "corpse_s2")
	corpse_s3	<- elt(output, "corpse_s3")
	corpse_d1	<- elt(output, "corpse_d1")
	corpse_d2	<- elt(output, "corpse_d2")
	corpse_d3	<- elt(output, "corpse_d3")

	#Plot the final year of output for the sediment habitats separately

	#Plots are:
	#    Shallow layer - wc ammonia, hab1 pw ammonia, hab2 pw ammonia, hab3 pw ammonia
	#    Deep layer    - wc ammonia, hab1 pw ammonia, hab2 pw ammonia, hab3 pw ammonia

	#    Shallow layer - wc nitrate, hab1 pw nitrate, hab2 pw nitrate, hab3 pw nitrate
	#    Deep layer    - wc nitrate, hab1 pw nitrate, hab2 pw nitrate, hab3 pw nitrate

	#    Shallow layer - wc detritus, hab1 detritus, hab2 detritus, hab3 pw detritus
	#    Deep layer    - wc detritus, hab1 detritus, hab2 detritus, hab3 pw detritus

	par(mfrow=c(4,2))

	#Shallow sediment ammonia

	if(x_poros_s1>0 && x_area_s1>0){
		l1<-x_ammonia_s1[((nyears-1)*360+1):ndays]/(x_area_s1*x_depth_s1*x_poros_s1)
	} else {
		l1<-rep(NA,361)
	}

	if(x_poros_s2>0 && x_area_s2>0){
		l2<-x_ammonia_s2[((nyears-1)*360+1):ndays]/(x_area_s2*x_depth_s2*x_poros_s2)
	} else {
		l2<-rep(NA,361)
	}

	if(x_poros_s3>0 && x_area_s3>0){
		l3<-x_ammonia_s3[((nyears-1)*360+1):ndays]/(x_area_s3*x_depth_s3*x_poros_s3)
	} else {
		l3<-rep(NA,361)
	}

	#This converts the sediment ammonia into units of N /m3 in the pore water)
	#tsplot3_hab("Inshore ammonia","Nitrogen/m3","Water","Muddy porewater","Coarse porewater",l1,l2,l3,50)
	tsplot3_hab("Inshore ammonia","Nitrogen/m3","Area_s1 porewater","Area_s2 porewater","Area_s3 porewater",l1,l2,l3,150)

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	#Deep sediment ammonia

	if(x_poros_d1>0 && x_area_d1>0){
		l1<-x_ammonia_d1[((nyears-1)*360+1):ndays]/(x_area_d1*x_depth_d1*x_poros_d1)
	} else {
		l1<-rep(NA,361)
	}

	if(x_poros_d2>0 && x_area_d2>0){
		l2<-x_ammonia_d2[((nyears-1)*360+1):ndays]/(x_area_d2*x_depth_d2*x_poros_d2)
	} else {
		l2<-rep(NA,361)
	}

	if(x_poros_d3>0 && x_area_d3>0){
		l3<-x_ammonia_d3[((nyears-1)*360+1):ndays]/(x_area_d3*x_depth_d3*x_poros_d3)
	} else {
		l3<-rep(NA,361)
	}

	#This converts the sediment ammonia into units of N /m3 in the pore water)
	#tsplot3_hab("Offshore ammonia","Nitrogen/m3","Water","Muddy porewater","Coarse porewater",l1,l2,l3,50)
	tsplot3_hab("Offshore ammonia","Nitrogen/m3","Area_d1 porewater","Area_d2 porewater","Area_d3 porewater",l1,l2,l3,150)

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Shallow sediment nitrate

if(x_poros_s1>0 && x_area_s1>0){
l1<-x_nitrate_s1[((nyears-1)*360+1):ndays]/(x_area_s1*x_depth_s1*x_poros_s1)
} else {
l1<-rep(NA,361)
}

if(x_poros_s2>0 && x_area_s2>0){
l2<-x_nitrate_s2[((nyears-1)*360+1):ndays]/(x_area_s2*x_depth_s2*x_poros_s2)
} else {
l3<-rep(NA,361)
}

if(x_poros_s3>0 && x_area_s3>0){
l3<-x_nitrate_s3[((nyears-1)*360+1):ndays]/(x_area_s3*x_depth_s3*x_poros_s3)
} else {
l3<-rep(NA,361)
}

#This converts the sediment nitrate into units of N /m3 in the pore water)
#tsplot3_hab("Inshore nitrate","Nitrogen/m3","Water","Muddy porewater","Coarse porewater",l1,l2,l3,15)
tsplot3_hab("Inshore nitrate","Nitrogen/m3","Area_s1 porewater","Area_s2 porewater","Area_s3 porewater",l1,l2,l3,15)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Deep sediment nitrate

if(x_poros_d1>0 && x_area_d1>0){
l1<-x_nitrate_d1[((nyears-1)*360+1):ndays]/(x_area_d1*x_depth_d1*x_poros_d1)
} else {
l1<-rep(NA,361)
}

if(x_poros_d2>0 && x_area_d2>0){
l2<-x_nitrate_d2[((nyears-1)*360+1):ndays]/(x_area_d2*x_depth_d2*x_poros_d2)
} else {
l2<-rep(NA,361)
}

if(x_poros_d3>0 && x_area_d3>0){
l3<-x_nitrate_d3[((nyears-1)*360+1):ndays]/(x_area_d3*x_depth_d3*x_poros_d3)
} else {
l3<-rep(NA,361)
}

#This converts the sediment nitrate into units of N /m3 in the pore water)
#tsplot3_hab("Offshore nitrate","Nitrogen/m3","Water","Muddy porewater","Coarse porewater",l1,l2,l3,15)
tsplot3_hab("Offshore nitrate","Nitrogen/m3","Area_d1 porewater","Area_d2 porewater","Area_d3 porewater",l1,l2,l3,15)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Shallow sediment total detritus

if(x_poros_s1>0 && x_area_s1>0){
l1a<-100*(((x_detritus_s1[((nyears-1)*360+1):ndays])*14)/1000)/(x_area_s1*x_depth_s1*(((1-x_poros_s1)*(2650*1000))))
l1b<-100*(((xR_detritus_s1[((nyears-1)*360+1):ndays])*14)/1000)/(x_area_s1*x_depth_s1*(((1-x_poros_s1)*(2650*1000))))
} else {
l1a<-rep(NA,361)
l1b<-rep(NA,361)
}

if(x_poros_s2>0 && x_area_s2>0){
l2a<-100*(((x_detritus_s2[((nyears-1)*360+1):ndays])*14)/1000)/(x_area_s2*x_depth_s2*(((1-x_poros_s2)*(2650*1000))))
l2b<-100*(((xR_detritus_s2[((nyears-1)*360+1):ndays])*14)/1000)/(x_area_s2*x_depth_s2*(((1-x_poros_s2)*(2650*1000))))
} else {
l2a<-rep(NA,361)
l2b<-rep(NA,361)
}

if(x_poros_s3>0 && x_area_s3>0){
l3a<-100*(((x_detritus_s3[((nyears-1)*360+1):ndays])*14)/1000)/(x_area_s3*x_depth_s3*(((1-x_poros_s3)*(2650*1000))))
l3b<-100*(((xR_detritus_s3[((nyears-1)*360+1):ndays])*14)/1000)/(x_area_s3*x_depth_s3*(((1-x_poros_s3)*(2650*1000))))
} else {
l3a<-rep(NA,361)
l3b<-rep(NA,361)
}
 
l1<-l1a+l1b
l2<-l2a+l2b
l3<-l3a+l3b
#This converts the sediment detritus into units of %N by dry wt (100*gN/g-drysediment) (density of dry solid matter = 2.65g/cm3)
#Then scale by 1000 to get on same axes as water detritus mMN/m3 water
#tsplot3_hab("Inshore detritus","Nitrogen/m3","Water","Muddy porewater","Coarse porewater",l1,l2,l3,1.5)
tsplot3_hab("Inshore detritus","Nitrogen/DW (g/g %)","Area_s1 sediment","Area_s2 sediment","Area_s3 sediment",l1,l2,l3,0.25)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Deep sediment total detritus


if(x_poros_d1>0 && x_area_d1>0){
l1a<-100*(((x_detritus_d1[((nyears-1)*360+1):ndays])*14)/1000)/(x_area_d1*x_depth_d1*(((1-x_poros_d1)*(2650*1000))))
l1b<-100*(((xR_detritus_d1[((nyears-1)*360+1):ndays])*14)/1000)/(x_area_d1*x_depth_d1*(((1-x_poros_d1)*(2650*1000))))
} else {
l1a<-rep(NA,361)
l1b<-rep(NA,361)
}

if(x_poros_d2>0 && x_area_d2>0){
l2a<-100*(((x_detritus_d2[((nyears-1)*360+1):ndays])*14)/1000)/(x_area_d2*x_depth_d2*(((1-x_poros_d2)*(2650*1000))))
l2b<-100*(((xR_detritus_d2[((nyears-1)*360+1):ndays])*14)/1000)/(x_area_d2*x_depth_d2*(((1-x_poros_d2)*(2650*1000))))
} else {
l2a<-rep(NA,361)
l2b<-rep(NA,361)
}

if(x_poros_d3>0 && x_area_d3>0){
l3a<-100*(((x_detritus_d3[((nyears-1)*360+1):ndays])*14)/1000)/(x_area_d3*x_depth_d3*(((1-x_poros_d3)*(2650*1000))))
l3b<-100*(((xR_detritus_d3[((nyears-1)*360+1):ndays])*14)/1000)/(x_area_d3*x_depth_d3*(((1-x_poros_d3)*(2650*1000))))
} else {
l3a<-rep(NA,361)
l3b<-rep(NA,361)
}

l1<-l1a+l1b
l2<-l2a+l2b
l3<-l3a+l3b
#This converts the sediment detritus into units of %N by dry wt (100*gN/g-drysediment) (density of dry solid matter = 2.65g/cm3)
#Then scale by 1000 to get on same axes as water detritus mMN/m3 water
#tsplot3_hab("Offshore detritus","Nitrogen/m3","Water","Muddy porewater","Coarse porewater",l1,l2,l3,1.5)
tsplot3_hab("Offshore detritus","Nitrogen/DW (g/g %)","Area_d1 sediment","Area_d2 sediment","Area_d3 sediment",l1,l2,l3,0.25)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Shallow sediment corpses

if(x_area_s1>0){
l1<-corpse_s1[((nyears-1)*360+1):ndays]/(x_area_s1)
} else {
l1<-rep(NA,361)
}

if(x_area_s2>0){
l2<-corpse_s2[((nyears-1)*360+1):ndays]/(x_area_s2)
} else {
l2<-rep(NA,361)
}

if(x_area_s3>0){
l3<-corpse_s3[((nyears-1)*360+1):ndays]/(x_area_s3)
} else {
l3<-rep(NA,361)
}

#This converts the sediment corpse mass into units of N /m2 of sediment surface)
#tsplot3_hab("Inshore corpses","Nitrogen/m3","Water","Muddy porewater","Coarse porewater",l1,l2,l3,15)
tsplot3_hab("Inshore corpses","Nitrogen/m2","Area_s1","Area_s2","Area_s3",l1,l2,l3,30)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Deep sediment corpses

if(x_area_d1>0){
l1<-corpse_d1[((nyears-1)*360+1):ndays]/(x_area_d1)
} else {
l1<-rep(NA,361)
}

if(x_area_d2>0){
l2<-corpse_d2[((nyears-1)*360+1):ndays]/(x_area_d2)
} else {
l2<-rep(NA,361)
}

if(x_area_d3>0){
l3<-corpse_d3[((nyears-1)*360+1):ndays]/(x_area_d3)
} else {
l3<-rep(NA,361)
}

#This converts the sediment corpse mass into units of N /m2 of sediment surface)
#tsplot3_hab("Offshore corpses","Nitrogen/m3","Water","Muddy porewater","Coarse porewater",l1,l2,l3,15)
tsplot3_hab("Offshore corpses","Nitrogen/m2","Area_d1","Area_d2","Area_d3",l1,l2,l3,30)


#-------------------------------------------------------------------------------------------------------


}

