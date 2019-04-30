#
# copy_model.R
#
#' make a copy of the named model (and all it's variants)
#'
#' @param model.name	name of model to copy
#' @param dest.path	destination folder to write copy of model, will be created if necessary, default is "Models"
#' @param overwrite	set to TRUE to enable overwrite of existing model folder
#
#' @export
#
copy_model <- function(model.name, dest.path="Models", overwrite=FALSE) {

	if (dir.exists(dest.path)) {
		if (overwrite) {
			cat("Overwriting destination '", dest.path, "'!\n", sep="")
		} else {
			stop("Error: copy_model(): destination folder '", dest.path, "' already exists, use overwrite=TRUE to force copy!\n", sep="")
		}
	} else {
		create.folder(dest.path)
	}

	model.path <- model.path(model.name)

	file.copy(model.path, dest.path, recursive=TRUE)
}

