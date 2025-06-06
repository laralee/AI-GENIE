% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_input_validation_helpers.R
\name{validate_openai}
\alias{validate_openai}
\title{Validate OpenAI API Key}
\usage{
validate_openai(openai.API)
}
\arguments{
\item{openai.API}{A required character string of your OpenAI API key.}
}
\value{
A trimmed version of the OpenAI API key.
}
\description{
Validates the \code{openai.API} parameter to ensure it is a non-empty string.
}
