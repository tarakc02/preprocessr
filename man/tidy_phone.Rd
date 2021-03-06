% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/preprocessr.R
\name{tidy_phone}
\alias{tidy_phone}
\title{Clean up phone numbers}
\usage{
tidy_phone(phone, length = 10, side = "right")
}
\arguments{
\item{phone}{character vector of phone numbers}

\item{length}{length to truncate number to (chopping off numerals from the
right)}

\item{side}{which side of the string do you want to take? if a lot of phone
numbers have country codes which you can remove, then taking the rightmost
digits (side = "right") will work}
}
\description{
Remove any non-numerals from a phone number and shorten it to a fixed length
}

