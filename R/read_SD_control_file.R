#
# read_SD_control_ecology.R
#
#' return list of annealing paramters
#'
#' @param model.path model path
#' @param control.file ecology control filename
#'
#' @return list of annealing parameters for fitting ecology model
#'
#' @export
#
read_SD_control_ecology <- function(model.path, control.file) {

	annealing_control_data <- readcsv(model.path, SD_CONTROL_DIR, control.file)

	#Set the axis min and max for the display plot
	axmin		<- annealing_control_data[1,1]
	axmax		<- annealing_control_data[2,1]

	#Set the SDs for the different classes of parameters
	Prefsd		<- annealing_control_data[3,1]	# Preference parameter sd
	u_sd		<- annealing_control_data[4,1]	# Maximum uptake rate sd
	h_sd		<- annealing_control_data[5,1]	# Half saturation density sd
	biogeo_sd	<- annealing_control_data[6,1]	# microbial parameter sd
	mort_sd		<- annealing_control_data[7,1]	# density dependent mortality rate sd
	ressd		<- annealing_control_data[8,1]	# other parameters sd

	annealing.parms <- list(
		axmin		= axmin,
		axmax		= axmax,
		Prefsd		= Prefsd,
		u_sd		= u_sd,
		h_sd		= h_sd,
		biogeo_sd	= biogeo_sd,
		mort_sd		= mort_sd,
		ressd		= ressd
	)
}
