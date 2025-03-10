% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_helpers.R
\name{gemma_format}
\alias{gemma_format}
\title{Gemma 2 Output Cleaning}
\usage{
gemma_format(response, split_content)
}
\arguments{
\item{response}{A string that contains the output of the LLM model}

\item{split_content}{A vector of the item attributes stemmed (not currently used)}
}
\value{
A list of the characteristics and items extracted from the model output
}
\description{
Cleans the output text based on the expected typical output of the Gemma 2 model.
}
