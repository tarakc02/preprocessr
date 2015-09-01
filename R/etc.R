#' Convert camelCase to under_score
#' @import stringr
#' @export
camel_underscore <- function(strings) {
    ## deal with back-to-back caps
    s <- stringr::str_replace_all(strings, "([A-Z])([A-Z])", "\\1_\\2")
    s <- stringr::str_replace_all(s, "([a-z])([A-Z])", "\\1_\\2")
    tolower(s)
}

#' Convert under_score to camelCase
#' @import stringr
#' @export
underscore_camel <- function(strings) {
    s <- tolower(strings)
    base::gsub("_(.)", "\\U\\1\\E", s, perl = TRUE)
}

#' Convert blank values to NA
#' @import stringr
#' @export
blank_na <- function(text) {
    if (inherits(text, "factor")) text <- as.character(text)
    if (!inherits(text, "character")) return(text)
    ifelse(stringr::str_trim(text) == "", NA, text)
}

#' Convert NA values to blanks
#' @import assertthat
#' @export
na_blank <- function(text) {
    assertthat::assert_that(inherits(text, "character") | inherits(text, "factor"))
    if(inherits(text, "factor")) text <- as.character(text)
    ifelse(is.na(text), "", text)
}
