#
# list_models.R
#
#' list available models
#'
#' @param model.path path to users model folder, otherwise read package model folder
#'
#' @export
#
list_models <- function(model.path="") {

	if (model.path == "") {
		# get path to internal model folder
		full.path <- system.file("extdata/Models", package="StrathE2E2")
	}
	else {
		full.path <- model.path
	}

	# build up list of models:
	models <- list()
	for (model.dir in list.files(full.path)) {
		model.path <- makepath(full.path, model.dir)					# path to model dir
		if (dir.exists(model.path)) {
			variants <- list()
			for (variant.dir in list.files(model.path)) {
				variant.path <- makepath(model.path, variant.dir)	# path to variant dir
				if (dir.exists(variant.path)) {
					# exists, but check for model setup file:
					setup.file <- makepath(variant.path, MODEL_SETUP)
					if (file.exists(setup.file)) {
						# model/variant/setup.csv exist, so treat this as a model
						variants <- c(variants, variant.dir)
					}
				}
			}
			if (length(variants)) {
				models[[model.dir]] <- variants
			}
		}
	}

	if (length(models)) {
		if (model.path == "") {
			cat("List of models in system folder")
		} else {
			cat("List of models in user folder")
		}
		cat(", with helpful commands to load them:\n\n")
		for (model in names(models)) {
			cat(" Model: \"", model, "\"\n", sep="")
			for (variant in models[[model]]) {
				cat ("  Variant: \"", variant, "\"", sep="")
				cat("\tmodel <- read_model(\"", model, "\", \"", variant, "\")\n", sep="")
			}
			cat("\n")
		}
	}
	else {
		cat("Error: could not find any models in path '", full.path, "' !\n", sep="")
	}
}

