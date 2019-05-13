#
# copy_model.R
#
#' make a copy of the named model (and all it's variants)
#'
#' @param model.name	name of model to copy
#' @param dest.path	destination folder to write copy of model, will be created if necessary, default is "Models" in current folder
#' @param user.path	path to users top level model folder if copying a user model
#' @param overwrite	set to TRUE to enable overwrite of existing model folder
#
#' @export
#
copy_model <- function(model.name, dest.path="Models", user.path="", overwrite=FALSE) {

	src.path <- get.model.path(model.name, user.path)	# path to top-level model folder (i.e. not variant folder)
	dst.path <- makepath(dest.path, model.name)
	if (dir.exists(dst.path)) {
		if (overwrite) {
			cat("Overwriting destination '", dst.path, "'!\n", sep="")
		} else {
			stop("Error: copy_model(): destination folder '", dst.path, "' already exists, use overwrite=TRUE to force copy!\n", sep="")
		}
	} else {
		create.folder(dst.path)
	}

	file.copy(model.path, dst.path, recursive=TRUE)
}

