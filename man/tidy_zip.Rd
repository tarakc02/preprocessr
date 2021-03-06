% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/preprocessr.R
\name{tidy_zip}
\alias{tidy_zip}
\title{Clean up zip codes}
\usage{
tidy_zip(zip, length = 5)
}
\arguments{
\item{zip}{a vector of zip codes}

\item{length}{length of the output string. The zip code will be zero-padded
from the left until it is long enough}
}
\description{
Remove dashes etc. (including characters!) from zip codes, zero-pad
so that they are the correct length. Please note this function is only designed
to deal with American-style zip codes. You can adjust the \code(length) argument
for numeric zip codes that are longer/shorter than 5 digits, but any non-digit
will be dropped. Further, this function ignores/strips out the +4 part of
the zip code if it is there.
}

