#
# read_annual_target_data.R
#
#' read annual target data
#'
#' returns target data
#'
#' @param model.path path to current model/version folder
#'
#' @return target data
#'
#' @export
#
read_annual_target_data <- function(model.path) {

	annualtargetdata <- readcsv(model.path, TARGET_DATA_DIR, annualtargetfile)

	#Column 1 = Target value
	#Column 2 = sd of target value
	#Column 3 = switch to determine whether to be included in likelihood evaluation (1=yes, 0 = no)
	#Column 4 = name of target value
	#Column 5 = units of target value
	#Column 6 = description of target value
	#Column 7 = Region
	#Column 8 = time period
	#Column 9 = source

	annualtargetdata
}

