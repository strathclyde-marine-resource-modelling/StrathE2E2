% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_model.R
\name{read_model}
\alias{read_model}
\title{read designated model}
\usage{
read_model(model.name, model.variant, model.ident = "base",
  model.subdir = "", user.path = "")
}
\arguments{
\item{model.name}{name of model to read}

\item{model.variant}{read the designated model variant (no default)}

\item{model.ident}{appended to output files (e.g. OFFSHORE_model_annualresults-TAG.csv instead of just OFFSHORE_model_annualresults.csv)}

\item{model.subdir}{store results in this sub directory of the main results folder}

\item{user.path}{path to users top level model folder}
}
\value{
model object
}
\description{
loads the named model and returns a model object with model setup and data slots. By default, the model is loaded from the list of package supplied models.
If user.path is set then the model is loaded from this location instead.
}
