#
# copy_model.R
#
#' make a copy of the named model/variant (or all variants if none specified)
#'
#' @param model.name	name of model to copy
#' @param model.variant	name of model variant to copy (copies all variants by default)
#' @param dest.path	destination folder to write copy of model, will be created if necessary, default is "Models" in current folder
#' @param user.path	path to users top level model folder if copying a user model
#
#' @export
#
copy_model <- function(model.name, model.variant="", dest.path="Models", user.path="") {

	if (nchar(model.variant)) {
		src.path <- get.variant.path(model.name, model.variant, user.path)	# path to model variant folder, either system or user folder
	} else {
		src.path <- get.model.path(model.name, user.path)			# path to top-level model folder (i.e. not variant folder), either system or user folder
	}
	dst.path <- makepath(dest.path, model.name, model.variant)
	if (dir.exists(dst.path)) {
		stop("destination folder '", dst.path, "' already exists !\n", sep="")
	} else {
		create.folder(dst.path)
	}

	cat(" Copying model: '", model.name, "'", sep="")
	if (nchar(model.variant)) {
		cat(", variant '", model.variant, "':\n", sep="")
	} else {
		cat(" and all it's variants:\n")
	}
	cat("  from '", src.path, "' to '", dst.path, "'\n", sep="")
	file.copy(src.path, dst.path, recursive=TRUE)

	invisible(NULL)
}

