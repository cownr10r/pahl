% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corpsum.R
\name{corpsum}
\alias{corpsum}
\title{Summary Statistics}
\usage{
corpsum(doc = doc)
}
\arguments{
\item{doc}{A character object returned from the essay function}
}
\value{
Text number, Token Types, Sentence numbers, and Title. An abbreviated version of summary in quanteda
}
\description{
Return the summary statistics on text found in an .xlsx file.
This requires the previous essay function to be written like this:
doc <- essay(data_dir, file, 1)
}
\examples{
corpsum(doc)
}
