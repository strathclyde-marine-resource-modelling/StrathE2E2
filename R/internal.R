#
# Internal functions - not for export
#
#' @importFrom graphics axis legend lines mtext par plot points boxplot
#' @importFrom utils read.csv
#' @importFrom methods show
#'
#' @keywords internal
#

MODEL_SETUP_SCRIPT	= "MODEL_SETUP_SCRIPT.R"	# located in the Model/Version/ directory
MODEL_RESULTS_DIR	= "results"			# located in the current directory

PARAMETERS_DIR		= "Parameters"			# sub-directories of the Model/Version/ directory
DRIVING_DATA_DIR	= "Driving_data"
TARGET_DATA_DIR		= "Target_data"
SD_CONTROL_DIR		= "Parameters/Parameter_SD_control"

check.exists <- function(filename) {
	if (! file.exists(filename)) {
		stop("Error: could not find file '", filename, "' !\n")
	}
}

# read CSV data from the path units
# by default first line of file is treated as a header line
#	readcsv(MODELPATH, VERSION, "fitted_parameters.csv")
#
readcsv <- function(..., header=TRUE) {

	units <- list(...)
	last <- units[[length(units)]]
	filename <- makepath(...)

	check.exists(filename)

	cat(" Reading input: ", last, "\n", sep="")
	data <- read.csv(filename, header=header)

	data
}

# wrapper around source() to produce consistent error messages
#
sourcefile <- function(filename) {
	check.exists(filename)

	source(filename)
}

# return the full path to the requested model/variant
# either a system packaged model or a user supplied one
#
get.model.path <- function(model.name, model.variant, user.path="") {

	if (!nchar(model.name)) {
		stop("Error: please supply a model name!")
	}

	if (!nchar(model.variant)) {
		stop("Error: please supply a model variant!")
	}

	# get path to model:
	if (user.path == "") {
		# System model:
		model.path <- system.file("extdata/Models", model.name, package="StrathE2E2")

		if (model.path == "") {
			stop("Error: system model: '", model.name, "' does not exist! (use list_models() to show system models)")
		}
	} else {
		# User model:
		if (! dir.exists(user.path)) {
			stop("Error: user model path: '", user.path, "' does not exist!")
		}

		model.path <- paste0(user.path, .Platform$file.sep, model.name)
		if (! dir.exists(model.path)) {
			stop("Error: user model: '", model.name, "' does not exist! (use list_models(", user.path, ") to show user models)")
		}
	}

	# get path to model variant:
	full.path <- paste0(model.path, "/", model.variant)
	if (! dir.exists(full.path)) {
		cat("Cannot find model variant: '", model.variant, "' !\n", sep="")
		cat("Looking in model path: '", model.path, "'\n", sep="")
		stop("Error: cannot find model/variant, stopping")
	}

	full.path
}

# build up a filename path
#
makepath <- function(...) {
	paste(..., sep="/")	# this is portable Windows/Linux
}

# check folder exists and create if not present
#
create.folder <- function(folder) {
	if (! dir.exists(folder)) {
		cat("Creating folder '", folder, "'\n", sep="")
		if (!dir.create(folder, recursive=TRUE, showWarnings=FALSE)) {
			stop("Error: could not create folder '", folder, "'\n", sep="")
		}
	}
}

# check element exists in list/data.frame and return it if present
# if the element does not exist then print out a warning and a call trace UNLESS the default value is
# set in which case return that
# In R if you access a non existant list element:
#	el <- list$notpresent
# then el will be NULL and you get no warning (this could just be a typo)
#	x <- el(list, "notpresent")		print warning and trace
#	x <- el(list, "notpresent", 0.0)	uses default to return 0.0
# 
# 
el <- function(data, element, default="NOTSET") {
	if (element %in% names(data)) {
		ret <- data[[element]]
	} else if (default != "NOTSET") {
		ret <- default
	} else {
		cat("Error: unknown list/data.frame element '", element, "'\n", sep="")
		cat("Trace:")
		# print call list:
		calls <- sys.calls()
		for (c in 1:length(calls)) {
			cat("\t", c, ": ", sep="")
			print(calls[[c]])
		}
		ret <- NULL
	}

	ret
}

# print out entire R object
# relies on max.print being set to a big number:
#	options(max.print=999999)
#
showall <- function(title, v) {
	cat(title, ":\n")
	show(v)
}


fyplot1 <- function(tspmain,axtitle,tspvar1) {
	par(mar=c(3,3.8,2.5,0.4))
	tsyears<-length(tspvar1)
	tseq<-seq(0,360,by=1)
	plmax<-1.5*max(max(tspvar1))
	plmin<-0
	plot(tseq,tspvar1,type="l",yaxt="n",xlim=c(0,360),ylim=c(plmin,plmax),xaxt="n",ann=FALSE,main=tspmain)
	axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=0.9)
	axis(side=2,las=1,cex.axis=0.9)
	mtext("Days",cex=0.7,side=1,line=2)
	mtext(axtitle,cex=0.7,side=2,line=2.8)
	mtext(tspmain,cex=0.7,side=3,line=0.5)
}

fyplot2 <- function(tspmain,axtitle,tsptitle1,tsptitle2,tspvar1,tspvar2) {
	par(mar=c(3,3.8,2.5,0.4))
	tsyears<-length(tspvar1)
	tseq<-seq(0,360,by=1)
	plmax<-max(max(tspvar1),max(tspvar2))
	plmin<-0
	plot(tseq,tspvar1,type="l",yaxt="n",xlim=c(0,360),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
	lines(tseq,tspvar2,type="l",lty="dashed")
	axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=0.9)
	axis(side=2,las=1,cex.axis=0.9)
	mtext("Days",cex=0.7,side=1,line=2)
	mtext(axtitle,cex=0.7,side=2,line=2.8)
	mtext(tspmain,cex=0.7,side=3,line=0.5)
	legend(5,plmax,c(tsptitle1,tsptitle2),col=c("black","black"),lty=c(1,2),pt.cex=c(1,1))
}

fyplot3 <- function(tspmain,axtitle,tsptitle1,tsptitle2,tsptitle3,tspvar1,tspvar2,tspvar3) {
	par(mar=c(3,3.8,2.5,0.4))
	tsyears<-length(tspvar1)
	tseq<-seq(0,360,by=1)
	plmax<-max(max(tspvar1),max(tspvar2),max(tspvar3))
	plmin<-0
	plot(tseq,tspvar1,type="l",col="red",yaxt="n",xlim=c(0,360),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
	lines(tseq,tspvar2,type="l",col="red",lty="dashed")
	lines(tseq,tspvar3,type="l",col="black",lty="dashed")
	axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=0.9)
	axis(side=2,las=1,cex.axis=0.9)
	mtext("Days",cex=0.7,side=1,line=2)
	mtext(axtitle,cex=0.7,side=2,line=2.8)
	mtext(tspmain,cex=0.7,side=3,line=0.5)
	legend(5,plmax,c(tsptitle1,tsptitle2,tsptitle3),col=c("red","red","black"),lty=c(1,2,2),pt.cex=c(1,1,1))
}

#Plot full time series of output

tsplot1 <- function(tsptitle,tspvar1){
	par(mar=c(3,3.8,0.5,0.4))
	tsyears<-length(tspvar1)
	tseq<-seq(0,(tsyears-1)/360,by=1/360)
	plot(tseq,tspvar1,type="l",yaxt="n",xaxt="n",ann=FALSE)
	axis(side=1,las=1,cex.axis=0.9)
	axis(side=2,las=1,cex.axis=0.9)
	mtext("Year",cex=0.7,side=1,line=2)
	mtext(tsptitle,cex=0.7,side=2,line=2.8)
}

tsplot2 <- function(tsptitle,tspvar1,tspvar2){
	par(mar=c(3,3.8,0.5,0.4))
	tsyears<-length(tspvar1)
	tseq<-seq(0,(tsyears-1)/360,by=1/360)
	plmax<-max(max(tspvar1),max(tspvar2))
	plmin<-min(min(tspvar1),min(tspvar2))
	plot(tseq,tspvar1,type="l",yaxt="n",ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
	lines(tseq,tspvar2,type="l",lty="dashed")
	axis(side=1,las=1,cex.axis=0.9)
	axis(side=2,las=1,cex.axis=0.9)
	mtext("Year",cex=0.7,side=1,line=2)
	mtext(tsptitle,cex=0.7,side=2,line=2.8)
}

tsplot3 <- function(tsptitle,tspvar1,tspvar2,tspvar3){
	par(mar=c(3,3.8,0.5,0.4))
	tsyears<-length(tspvar1)
	tseq<-seq(0,(tsyears-1)/360,by=1/360)
	plmax<-max(max(tspvar1),max(tspvar2),max(tspvar3))
	plmin<-min(min(tspvar1),min(tspvar2),min(tspvar3))
	plot(tseq,tspvar1,type="l",yaxt="n",ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
	lines(tseq,tspvar2,type="l",lty="dashed")
	lines(tseq,tspvar3,type="l",lty="dashed",col="red")
	axis(side=1,las=1,cex.axis=0.9)
	axis(side=2,las=1,cex.axis=0.9)
	mtext("Year",cex=0.7,side=1,line=2)
	mtext(tsptitle,cex=0.7,side=2,line=2.8)
}

