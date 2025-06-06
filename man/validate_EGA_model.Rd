% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_input_validation_helpers.R
\name{validate_EGA_model}
\alias{validate_EGA_model}
\title{Validate EGA Model}
\usage{
validate_EGA_model(EGA.model)
}
\arguments{
\item{EGA.model}{A character string specifying the model to be used with EGA, one of \code{"tmfg"}, \code{"glasso"}, or \code{NULL}.}
}
\value{
EGA.model string in lowercase; the function stops with an error message if validation fails.
}
\description{
Validates the \code{EGA.model} parameter to ensure it is either \code{"tmfg"}, \code{"glasso"}, or \code{NULL}.
}
