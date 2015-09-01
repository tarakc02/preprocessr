#' Truncate a string to a standard size
#' 
#' It's often useful in doing partial string comparisons (eg, for comparing 
#' street addresses) to remove non-standard characters and reduce the length.
#' 
#' @param string character vector of strings to truncate
#' @param length how long (at most) the resulting strings should be. -1 
#' (default) to not truncate. 
#' @param pattern regex pattern for characters that should be removed.
#' 
#' @details By default, only alpha-numeric characters are retained
#' @examples
#' address1 <- "123 N Washington way"
#' address2 <- "123 N. Washington"
#' identical(str_truncate(address1, 8), str_truncate(address2, 8))
#' @import stringr
#' @export
tidy_string <- function(string, length = -1, pattern = "[^A-Za-z0-9]", lowercase = TRUE) {
    s <- stringr::str_replace_all(string, pattern, "")
    s <- stringr::str_sub(s, 1, length)
    if (lowercase) s <- tolower(s)
    s
}

#' Clean up phone numbers
#' 
#' Remove any non-numerals from a phone number and shorten it to a fixed length
#' 
#' @param phone character vector of phone numbers
#' @param length length to truncate number to (chopping off numerals from the 
#' right)
#' @param side which side of the string do you want to take? if a lot of phone 
#' numbers have country codes which you can remove, then taking the rightmost 
#' digits (side = "right") will work
#' @import stringr
#' @export
tidy_phone <- function(phone, length = 10, side = "right") {
    s <- tidy_string(phone, length = -1, pattern = "[^0-9]", lowercase = FALSE)
    if (side == "right") s <- stringr::str_sub(s, -length)
    else                 s <- stringr::str_sub(s, 1, length)
    s
}


#' Clean up zip codes
#' 
#' Remove dashes etc. (including characters!) from zip codes, zero-pad 
#' so that they are the correct length. Please note this function is only designed 
#' to deal with American-style zip codes. You can adjust the \code(length) argument 
#' for numeric zip codes that are longer/shorter than 5 digits, but any non-digit 
#' will be dropped. Further, this function ignores/strips out the +4 part of 
#' the zip code if it is there. 
#' @import stringr
#' @param zip a vector of zip codes
#' @param length length of the output string. The zip code will be zero-padded 
#' from the left until it is long enough
#' @export
tidy_zip <- function(zip, length = 5) {
    isna <- is.na(zip)
    z <- zip[!isna]
    z <- stringr::str_replace(z, stringr::regex("([^-\\s]+)[-\\s](.*$)"), "\\1")
    z <- tidy_string(z, length, pattern = "[^0-9]", lowercase = TRUE)
    z <- stringr::str_pad(z, width=length, 
                          side = "left", pad="0")
    out <- rep(NA, length(zip))
    out[!isna] <- z
    out
}

#' Get soundex code for a given string
#' 
#' Since soundex is only defined on the letters a-z, this function will first
#' convert any input to ASCII before calculating the soundex.
#' 
#' @import stringdist
#' @export
soundex <- function(string, ...) {
    s <- iconv(string, to = "ASCII//TRANSLIT")
    stringdist::phonetic(s, method="soundex", ...)
}