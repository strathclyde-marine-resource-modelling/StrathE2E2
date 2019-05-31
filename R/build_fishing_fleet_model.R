#
# build_fishing_fleet_model.R
#
#' build fishing fleet model taking notice of any changes to physical depth parameters
#'
#' @param model.path path to users model folder, otherwise read package model
#'
#' @return model object
#'
#' @export
#
build_fishing_fleet_model <- function(fleet.model, physical.parms) {

	# Unpack:
	plough_thickness <- elt(fleet.model, "plough_thickness")

	x_depth_s1 <- elt(physical.parms, "x_depth_s1")
	x_depth_s2 <- elt(physical.parms, "x_depth_s2")
	x_depth_s3 <- elt(physical.parms, "x_depth_s3")
	x_depth_d1 <- elt(physical.parms, "x_depth_d1")
	x_depth_d2 <- elt(physical.parms, "x_depth_d2")
	x_depth_d3 <- elt(physical.parms, "x_depth_d3")

	plough_depth_s0 <- 0
	plough_depth_d0 <- 0

	if(x_depth_s1>0){
		plough_depth_s1<-plough_thickness/x_depth_s1 
	} else {
		plough_depth_s1<-0
	}

	if(x_depth_s2>0){
		plough_depth_s2<-plough_thickness/x_depth_s2 
	} else {
		plough_depth_s2<-0
	}

	if(x_depth_s3>0){
		plough_depth_s3<-plough_thickness/x_depth_s3 
	} else {
		plough_depth_s3<-0
	}

	if(x_depth_d1>0){
		plough_depth_d1<-plough_thickness/x_depth_d1 
	} else {
		plough_depth_d1<-0
	}

	if(x_depth_d2>0){
		plough_depth_d2<-plough_thickness/x_depth_d2 
	} else {
		plough_depth_d2<-0
	}

	if(x_depth_d3>0){
		plough_depth_d3<-plough_thickness/x_depth_d3 
	} else {
		plough_depth_d3<-0
	}

	fleet.model$plough_depth_vector <- c(
		plough_depth_s0,
		plough_depth_s1,
		plough_depth_s2,
		plough_depth_s3,
		plough_depth_d0,
		plough_depth_d1,
		plough_depth_d2,
		plough_depth_d3
	)

	fleet.model
}

