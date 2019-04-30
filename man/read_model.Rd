% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_model.R
\name{read_model}
\alias{read_model}
\title{read designated model}
\usage{
read_model(model.name, model.variant, model.tag = "", user.path = "",
  nyears = 20, annealing = FALSE)
}
\arguments{
\item{model.name}{name of model to read}

\item{model.variant}{read the designated model variant (no default)}

\item{model.tag}{appended to output files (e.g. OFFSHORE_model_annualresults-TAG.csv instead of just OFFSHORE_model_annualresults.csv)}

\item{user.path}{path to users top level model folder}

\item{nyears}{number of years that model will run}

\item{annealing}{boolean stating whether annealing settings are to be returned}
}
\value{
model object
}
\description{
loads the named model and returns a model object with run and data slots. By default, the model is loaded from the list of package supplied models.
If user.path is set then the model is loaded from this location instead.
}