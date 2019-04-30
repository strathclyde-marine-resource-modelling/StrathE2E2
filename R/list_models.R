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

	# ZZ if you supply a path it just uses it without checking they are really model folders...
	# ZZ should really generate list of models/variants then look for MODEL_SETUP.R files to check
	if (model.path == "") {
		# get path to internal model folder
		full.path <- system.file("extdata/Models", package="StrathE2E2")
		cat("List of models in system folder:\n\n")
	}
	else {
		full.path <- model.path
		cat("List of models in user folder:\n\n")
	}

	models <- list.files(full.path)

	if (length(models) > 0) {
		for (model in models) {
			cat(" Model: \"", model, "\"\n", sep="")
			variant.path <- makepath(full.path, model)
			variants <- list.files(variant.path)
			for (variant in variants) {
				cat ("  Variant: \"", variant, "\"", sep="")
				setup.file <- makepath(variant.path, variant, MODEL_SETUP_SCRIPT)
				if (! file.exists(setup.file)) {
					cat("  ** NOT A VALID MODEL/VARIANT !!")
				}
				cat("\n")
			}
			cat("\n")
		}
	} else {
		cat("  no models found!\n")
	}
}

