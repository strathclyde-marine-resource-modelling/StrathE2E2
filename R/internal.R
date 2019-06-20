#
# Internal functions - not for export
#
#' @importFrom graphics axis legend lines mtext par plot points boxplot
#' @importFrom utils read.csv write.table
#' @importFrom methods show
#'
#' @keywords internal
#'
#' @export showall
#

PACKAGE_NAME			<- "StrathE2E2"

MODEL_SETUP			<- "MODEL_SETUP.csv"	# located in the Model/Version/ directory
MODEL_RESULTS_DIR		<- "results"		# located in the current directory

PARAMETERS_DIR			<- "Parameters"		# sub-directories of the Model/Version/ directory
DRIVING_DATA_DIR		<- "Driving_data"
TARGET_DATA_DIR			<- "Target_data"

SD_CONTROL_DIR			<- "Parameters/Parameter_SD_control"
SD_CONTROL_FILE_ECOLOGY		<- "annealing_SD_ecology.csv"
SD_CONTROL_FILE_FISHING		<- "annealing_SD_fishing.csv"
SD_CONTROL_FILE_CREDINT		<- "CredIntSim_SD.csv"
SD_CONTROL_FILE_SENSITIVITY	<- "OATsensitivity_SD.csv"

PHYSICAL_PARAMETERS		<- "physical_parameters"				# look for csv file containing these string patterns
PHYSICS_DRIVERS			<- "physics_drivers"
CHEMISTRY_DRIVERS		<- "boundary_data"
INITIAL_STATE			<- "model_endstate"
BIOLOGICAL_EVENTS		<- "biological_event_timing_parameters"
FIXED_PARAMETERS_CONSUMER	<- "fixed_parameters_for_consumer_groups"
FIXED_PARAMETERS_MISCELLANEOUS	<- "fixed_parameters_miscellaneous"
FITTED_PARAMETERS_PREFERENCE	<- "fitted_parameters_preference_matrix"
FITTED_PARAMETERS_UPTAKE_MORT	<- "fitted_parameters_uptake_and_mortality_rates"
FITTED_PARAMETERS_MICROBIOLOGY	<- "fitted_parameters_microbiology"
FISHING_FLEET_PARAMETERS	<- "fishing_fleet_parameters"
FISHING_ACTIVITY_PARAMETERS	<- "fishing_activity_parameters"
FISHING_POWER_PARAMETERS	<- "fishing_power_parameters"
FISHING_DISCARD_PARAMETERS	<- "fishing_discard_parameters"
FISHING_PROCESSING_PARAMETERS	<- "fishing_processing"
FISHING_DISTRIBUTION_PARAMETERS	<- "fishing_distribution"
FISHING_ACTIVITY_SCALING_VALUES	<- "fishing_activity_scaling_values"
HARVEST_RATIO_SCALING_VALUES	<- "harvest_ratio_scaling_values"
ANNUAL_TARGET_DATA		<- "annual_target_data"
MONTHLY_TARGET_DATA		<- "monthly_target_data"
FOOD_WEB_FLOW_MATRIX		<- "food_web_flow_matrix"

pkg.env				<- new.env()
pkg.env$SETUPFILES		<- character()

check.exists <- function(filename) {
	if (! file.exists(filename)) {
		stop("Error: could not find file '", filename, "' !\n")
	}
}

isdefined <- function(var, val) {
	exists(var) && (get(var) == val)
}

# reads the setup csv which specifies the names for all the model input and output files
#
read.model.setup <- function(model.path) {
	setup <- readcsv(model.path, MODEL_SETUP, header=TRUE)		# DF
	pkg.env$SETUPFILES <- as.character(levels(setup[[1]]))		# character vector
}

# fitted.pars <- get.model.file(MODEL_DIR, PARAMETER_DIR, file.pattern=FITTED_PARAMETERS)
# using the file patterns listed above
#
get.model.file <- function(..., file.pattern, header=TRUE) {

	matches <- grep(file.pattern, pkg.env$SETUPFILES, value=TRUE)

	if (length(matches) != 1) {
		cat("Matches=",length(matches), "\n")
		showall("setupfiles",pkg.env$SETUPFILES)
		if (length(matches) == 0) {
			# no match!
			cat("Error: could not find model file using pattern '", file.pattern, "' !\n", sep="")
		} else if (length(matches) > 1) {
			# more than 1 match!
			cat("Error: matched more than one model file using pattern '", file.pattern, "' !\n", sep="")
			for (m in matches) {
				cat(" matched:", m, "\n")
			}
		}
		stop("Cannot find requested model filename!")
	}

	# found it:
	filename <- matches[[1]]
	readcsv(..., filename, header=header)
}

# read CSV data from the path units
# by default first line of file is treated as a header line (header=TRUE)
#	readcsv(model.path, PARAMETERS_DIR, "fitted_parameters.csv")
# R read.csv() will interpret first column as row names if the 1st line of the file has 1 less column than the rest!
# We now standardise on completely regular CSV files (same number of columns in every row including header, if present)
# If we want the 1st column to be treated as row names, then we must explicity request it:
#	readcsv(model.path, PARAMETERS_DIR, "fitted_parameters_prefs.csv", row.names=1)
# of course header is TRUE by default.
#
readcsv <- function(..., header=TRUE, row.names=NULL) {

	filepath <- makepath(...)

	check.exists(filepath)

	file <- basename(filepath)
	cat(" Reading CSV file: ", file, "\n", sep="")
	data <- read.csv(filepath, header=header, row.names=row.names)

	data
}

# writecsv(): write data to CSV file
# the dir is created if it doesn't already exist
# file will be overwritten if it already exists
# we use write.table() instead of write.csv() for more flexibility
# defaults are same as write.table(), col.names(header)=TRUE, row.names=TRUE
# assume a CSV file:
#	"names,"kelp","phyt"
#	"ammonia","0.11","0.22"
#	"nitrate","0.33","0.44"
# read it:
#	csv <- read.csv("test1.csv", row.names=1)
# To re-write this use:
#	writecsv(csv, "test1.csv")
# To omit the header, header=FALSE:
#	"ammonia","0.11","0.22"
#	"nitrate","0.33","0.44"
# To omit the row.names as well, header=FALSE, row.names=FALSE:
#	0.11, 0.22
#	0.33, 0.44
# Just the headers, row.names=FALSE:
#	"kelp","phyt"
#	0.11,0.22
#	0.33,0.44
#
writecsv <- function(data, filepath, header=TRUE, row.names=TRUE) {

	dir <- dirname(filepath)
	file <- basename(filepath)

	create.folder(dir)

	# compatibility with write.table:
	col.names <- header
	if ((row.names == TRUE) && (col.names == TRUE)) {
		# writes an element into the header line for the row.names:
		col.names = NA
	}

	cat(" Writing CVS file: ", filepath, "\n", sep="")
	#cat(" row.names=",row.names, " col.names=",col.names,"\n")
	write.table(data, file=filepath, row.names=row.names, col.names=col.names, sep=",")
}

# simple wrapper around writecsv() to save a list with the element names as row names in 1st column, data in 2nd, with no header:
# 	"el1",33
#	"el2",44
#
writecsv.list <- function(data, filepath) {
	writecsv(data, filepath, row.names=TRUE, header=FALSE)
}

# create a csv filename:
#	output/dir/file-identifier.csv
# or:
#	output/dir/file.csv
# if identifier not set
#
csvname <- function(dir, file, identifier)
{
	name <- dir

	# add a dir sep if missing:
	if (! endsWith(dir, .Platform$file.sep)) {
		name <- paste0(name, .Platform$file.sep)
	}

	name <- paste0(name, file)

	if (nchar(identifier)) {
		# add "-ident" if set:
		name <- paste0(name, "-", identifier)
	}
	name <- paste0(name, ".csv")

	name
}

# wrapper around source() to produce consistent error messages
#
sourcefile <- function(filename) {
	check.exists(filename)

	source(filename)
}

# return the full path to the requested model
# either a system packaged model or a user supplied one
#
get.model.path <- function(model.name, user.path="") {

	if (!nchar(model.name)) {
		stop("Error: please supply a model name!")
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

	model.path
}

# return the full path to the requested model and variant
#
get.variant.path <- function(model.name, model.variant, user.path="") {

	model.path <- get.model.path(model.name, user.path)

	if (!nchar(model.variant)) {
		stop("Error: please supply a model variant!")
	}

	# get path to model variant:
	full.path <- paste0(model.path, "/", model.variant)
	if (! dir.exists(full.path)) {
		cat("Cannot find model variant: '", model.variant, "' !\n", sep="")
		cat("Looking in model path: '", model.path, "'\n", sep="")
		stop("Error: cannot find requested variant, stopping")
	}

	full.path
}

# build up a filename path
#
makepath <- function(...) {
	paste(..., sep=.Platform$file.sep)
}

# check folder exists and create if not present
#
create.folder <- function(folder) {
	if (! dir.exists(folder)) {
		cat(" Creating folder : ", folder, "\n", sep="")
		if (!dir.create(folder, recursive=TRUE, showWarnings=FALSE)) {
			stop("Error: could not create folder '", folder, "'\n", sep="")
		}
	}
}

# given a set of list/data.frame element names, extract the value if present
# if the element does not exist then print out a warning and a call trace UNLESS the default value is
# set in which case return that
# In R if you access a non existant list element:
#	x <- list$notpresent
# then x will be NULL and you get no warning!
# Using this elt() function will generate warnings (and hence pick up typos, etc.)
#	x <- elt(list, "notpresent")			looks for list$notpresent, print warning and trace
#	x <- elt(list, "notpresent", default=10.0)	looks for list$notpresent, uses default to return 10.0
#	x <- elt(list, "exists")			looks for list$exists and returns it
#	x <- elt(list, "exists1", "exists2")		looks for list$exists1$exists2 and returns it
# 
elt <- function(data, ..., default="NOTSET") {
	elements <- list(...)
	ret <- data
	for (element in elements) {
		if (element %in% names(ret)) {
			ret <- ret[[element]]		# element exists
		} else if (default != "NOTSET") {
			ret <- default			# element does not exist, but caller has set a default
			break
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
			break
		}
	}

	ret
}

# print out entire R object
#
showall <- function(title, v) {
	# save current settings:
	max <- options("max.print")
	dig <- options("digits")

	# print everything to high precision:
	options(digits=20)
	options(max.print=999999)
	cat(title, ":\n")
	if (is.list(v)) cat("Length=",length(v), "\n", sep="")
	cat("Class: ", class(v), "\n")
	show(v)

	# restore original settings:
	options(max.print=max$max.print)
	options(digits=dig$digits)
}


showallsort <-function(title, v) {
	showall(title, v[order(names(v))])
}

# print out elements of a dataframe or list suitable for printing:
#
genshowall <- function(v, prefix="") {
	for (i in names(v)) {
		cat("showall(\"", i, "\", ", prefix, i, ")\n", sep="")
	}
}


#Plot the final year of output

fyplot1<-function(tspmain,axtitle,tspvar1){
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

fyplot2<-function(tspmain,axtitle,tsptitle1,tsptitle2,tspvar1,tspvar2){
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

fyplot3<-function(tspmain,axtitle,tsptitle1,tsptitle2,tsptitle3,tspvar1,tspvar2,tspvar3){
par(mar=c(3,3.8,2.5,0.4))
tsyears<-length(tspvar1)
tseq<-seq(0,360,by=1)
plmax<-max(max(tspvar1),max(tspvar2),max(tspvar3))
plmin<-0
plot(tseq,tspvar1,type="l",col="black",yaxt="n",xlim=c(0,360),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
lines(tseq,tspvar2,type="l",col="black",lty="dashed")
lines(tseq,tspvar3,type="l",col="red",lty="dashed")
axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=0.9)
axis(side=2,las=1,cex.axis=0.9)
mtext("Days",cex=0.7,side=1,line=2)
mtext(axtitle,cex=0.7,side=2,line=2.8)
mtext(tspmain,cex=0.7,side=3,line=0.5)
legend(5,plmax,c(tsptitle1,tsptitle2,tsptitle3),col=c("black","black","red"),lty=c(1,2,2),pt.cex=c(1,1,1))
}

fyplot4<-function(tspmain,axtitle,tsptitle1,tsptitle2,tsptitle3,tsptitle4,tspvar1,tspvar2,tspvar3,tspvar4){
par(mar=c(3,3.8,2.5,0.4))
tsyears<-length(tspvar1)
tseq<-seq(0,360,by=1)
plmax<-max(max(tspvar1),max(tspvar2),max(tspvar3),max(tspvar4))
plmin<-0
plot(tseq,tspvar1,type="l",col="black",yaxt="n",xlim=c(0,360),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
lines(tseq,tspvar2,type="l",col="black",lty="dashed")
lines(tseq,tspvar3,type="l",col="red")
lines(tseq,tspvar4,type="l",col="red",lty="dashed")
axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=0.9)
axis(side=2,las=1,cex.axis=0.9)
mtext("Days",cex=0.7,side=1,line=2)
mtext(axtitle,cex=0.7,side=2,line=2.8)
mtext(tspmain,cex=0.7,side=3,line=0.5)
legend(5,plmax,c(tsptitle1,tsptitle2,tsptitle3,tsptitle4),col=c("black","black","red","red"),lty=c(1,2,1,2),pt.cex=c(1,1,1,1))
}

#Plot full time series of output

tsplot1 <- function(tsptitle, tspvar1) {
	par(mar=c(3,3.8,0.5,0.4))
	tsyears<-length(tspvar1)
	tseq<-seq(0,(tsyears-1)/360,by=1/360)
	plot(tseq,tspvar1,type="l",yaxt="n",xaxt="n",ann=FALSE)
	axis(side=1,las=1,cex.axis=0.9)
	axis(side=2,las=1,cex.axis=0.9)
	mtext("Year",cex=0.7,side=1,line=2)
	mtext(tsptitle,cex=0.7,side=2,line=2.8)
}

tsplot2 <- function(tsptitle,tspvar1,tspvar2) {
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

tsplot3 <- function(tsptitle,tspvar1,tspvar2,tspvar3) {
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

tsplot11 <- function(tspmain,axtitle,tspvar1) {
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

tsplot22 <- function(tspmain,axtitle,tsptitle1,tsptitle2,tspvar1,tspvar2) {
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

tsplot33 <- function(tspmain,axtitle,tsptitle1,tsptitle2,tsptitle3,tspvar1,tspvar2,tspvar3) {
	par(mar=c(3,3.8,2.5,0.4))
	tsyears<-length(tspvar1)
	tseq<-seq(0,360,by=1)
	plmax<-max(max(tspvar1),max(tspvar2),max(tspvar3))
	plmin<-0
	plot(tseq,tspvar1,type="l",col="black",yaxt="n",xlim=c(0,360),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
	lines(tseq,tspvar2,type="l",col="black",lty="dashed")
	lines(tseq,tspvar3,type="l",col="red",lty="dashed")
	axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=0.9)
	axis(side=2,las=1,cex.axis=0.9)
	mtext("Days",cex=0.7,side=1,line=2)
	mtext(axtitle,cex=0.7,side=2,line=2.8)
	mtext(tspmain,cex=0.7,side=3,line=0.5)
	legend(5,plmax,c(tsptitle1,tsptitle2,tsptitle3),col=c("black","black","red"),lty=c(1,2,2),pt.cex=c(1,1,1))
}

tsplot44 <- function(tspmain,axtitle,tsptitle1,tsptitle2,tsptitle3,tsptitle4,tspvar1,tspvar2,tspvar3,tspvar4) {
	par(mar=c(3,3.8,2.5,0.4))
	tsyears<-length(tspvar1)
	tseq<-seq(0,360,by=1)
	plmax<-max(max(tspvar1),max(tspvar2),max(tspvar3),max(tspvar4))
	plmin<-0
	plot(tseq,tspvar1,type="l",col="black",yaxt="n",xlim=c(0,360),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
	lines(tseq,tspvar2,type="l",col="black",lty="dashed")
	lines(tseq,tspvar3,type="l",col="red")
	lines(tseq,tspvar4,type="l",col="red",lty="dashed")
	axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=0.9)
	axis(side=2,las=1,cex.axis=0.9)
	mtext("Days",cex=0.7,side=1,line=2)
	mtext(axtitle,cex=0.7,side=2,line=2.8)
	mtext(tspmain,cex=0.7,side=3,line=0.5)
	legend(5,plmax,c(tsptitle1,tsptitle2,tsptitle3,tsptitle4),col=c("black","black","red","red"),lty=c(1,2,1,2),pt.cex=c(1,1,1,1))
}


tsplot3_hab <- function(tspmain,axtitle,tsptitle1,tsptitle2,tsptitle3,tspvar1,tspvar2,tspvar3,plmax) {
	par(mar=c(3,3.8,1.3,0.4))
	tsyears<-length(tspvar1)
	tseq<-seq(0,360,by=1)
	#plmax<-max(max(tspvar1),max(tspvar2),max(tspvar3))
	plmin<-0
	plot(tseq,tspvar1,type="l",col="red",yaxt="n",xlim=c(0,360),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
	lines(tseq,tspvar2,type="l",col="red",lty="dashed")
	lines(tseq,tspvar3,type="l",col="black",lty="dashed")
	axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=0.9)
	axis(side=2,las=1,cex.axis=0.9)
	mtext("Days",cex=0.7,side=1,line=2)
	mtext(axtitle,cex=0.7,side=2,line=2.8)
	mtext(tspmain,cex=0.7,side=3,line=0.2)
	legend(5,plmax,c(tsptitle1,tsptitle2,tsptitle3),col=c("red","red","black"),lty=c(1,2,2),pt.cex=c(1,1,1))
}

get.dyn.path <- function() {
	# relative path from top of install tree:				# Windows			Linux
	dyn.dir <- paste0("libs", .Platform$file.sep, .Platform$r_arch)		# libs/x64			libs	(r_arch is "" !)
	dyn.name <- paste0(PACKAGE_NAME, .Platform$dynlib.ext)			# StrathE2E2.dll		StrathE2E2.so
	dyn.path <- system.file(dyn.dir, dyn.name, package=PACKAGE_NAME)	# libs/x64/StrathE2E2.dll	libs/StrathE2E2.so

	dyn.path
}

StrathE2E.load <- function() {
	dyn.path <- get.dyn.path()
	dyn.load(dyn.path)
}

StrathE2E.unload <- function() {
	dyn.path <- get.dyn.path()
	dyn.unload(dyn.path)
}

