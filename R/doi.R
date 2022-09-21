#
# A set of functions for handling DOIs (digital object identifiers)
#






# global regular expression that identifies a DOI
pkg.env <- new.env()
pkg.env$doi_regex <- "10\\.\\d{4,9}/[-._;()/:a-z0-9A-Z]+"





#' Set the regular expression used to find a DOI
#'
#' The doi package uses a regular expression to find or extract DOIs.
#' Its standard value is "10\\\\.\\\\d{4,9}/\[-._;()/:a-z0-9A-Z\]+".
#' Use this function to set a custom regular expression of your choice, valid for the current session.
#' Omit the argument to set the regular expression back to standard.
#'
#' @param rg_expr A single string containing the regular expression to use further on during a session.
#' @export
doi_set_regex <- function(rg_expr = "10\\.\\d{4,9}/[-._;()/:a-z0-9A-Z]+") {

    if (!is.character(rg_expr)) stop("Argument rg_expr must be of type character.")
    if (length(rg_expr) == 0)   stop("Argument rg_expr must not be of length 0.")
    if (length(rg_expr) > 1)    warning("Length of argument rg_expr greater than 1. Only first element used.")
    if (is.na(rg_expr[1])) {
        warning("Attempt to assing NA as regular expression rejected. Regular expression to find DOIs unchanged.")
        invisible(NULL)
    }
    pkg.env$doi_regex <- rg_expr[1]
    invisible(rg_expr[1])

}





#' Turn DOIs into URLs
#'
#' Turns DOIs into URLs by placing the prefix "https://www.doi.org/" in front of them.
#'
#' @param doi_vec A character vector containing DOIs.
#' @return A character vector containing URLs.
#' @export
doi_to_url <- function(doi_vec) {

    if (!is.character(doi_vec)) stop("Argument doi_vec must be of type character.")
    if (length(doi_vec) == 0)   return(character(0))
    stringr::str_c("https://www.doi.org/", doi_vec)

}





#' Detect the presence or absence of a DOI in a string
#'
#' Returns TRUE if a given string contains a valid DOI. Vectorised over doi_vec.
#'
#' @param doi_vec A character vector containing DOIs.
#' @param na_yields_false A single logical value stating how to deal with NAs in the input vector.
#' Set to FALSE, doi_detect will return NA. Set to TRUE, it will return FALSE.
#' @return A logical vector with TRUE indicating whether a DOI was found.
#' @export
doi_detect <- function(doi_vec, na_yields_false = FALSE) {

    if (!is.character(doi_vec))       stop("Argument doi_vec must be a character vector.")
    if (!is.logical(na_yields_false)) stop("Argument na_yields_false must be of type logical.")
    if (length(na_yields_false) == 0) stop("Argument na_yields_false must not be of length 0.")
    if (length(na_yields_false) > 1)  warning("Length of argument na_yields_false greater than 1. Only first element used.")
    doi_vec <- tidyr::replace_na(doi_vec, NA_character_)

    a   <- stringr::str_detect(doi_vec, pkg.env$doi_regex)
    if (na_yields_false[1]) tidyr::replace_na(a, replace = FALSE) else a

}





#' Check if a string is a DOI
#'
#' Returns TRUE if a given string is a valid DOI. Vectorised over doi_vec.
#'
#' @param doi_vec A character vector containing DOIs.
#' @param na_yields_false A single logical value stating how to deal with NAs in the input vector.
#' Set to FALSE, doi_check will return NA. Set to TRUE, it will return FALSE.
#' @return A logical vector with TRUE indicating whether an element of the input vector is a valid DOI.
#' @export
doi_check <- function(doi_vec, na_yields_false = FALSE) {

    if (!is.character(doi_vec))       stop("Argument doi_vec must be a character vector.")
    if (!is.logical(na_yields_false)) stop("Argument na_yields_false must be of type logical.")
    if (length(na_yields_false) == 0) stop("Argument na_yields_false must not be of length 0.")
    if (length(na_yields_false) > 1)  warning("Length of argument na_yields_false greater than 1. Only first element used.")
    doi_vec <- tidyr::replace_na(doi_vec, NA_character_)

    rgx <- stringr::str_c("^", pkg.env$doi_regex, "$")
    a   <- stringr::str_detect(doi_vec, rgx)
    if (na_yields_false[1]) tidyr::replace_na(a, replace = FALSE) else a

}





#' Extracts the first DOI from a string
#'
#' Extracts valid DOIs from a given character vector. Vectorised over doi_vec.
#' Only returns the first match for each element.
#'
#' @param doi_vec A character vector.
#' @param lower_case A single logical value stating whether or not to put the whole DOI in lower case.
#' As DOIs are case insensitive, using lower case for all may help to identify duplicates more easily.
#' @return A character vector containing DOIs (if found) or NAs otherwise.
#' @export
doi_extract_first <- function(doi_vec, lower_case = FALSE) {

    if (!is.character(doi_vec))  stop("Argument doi_vec must be a character vector.")
    if (!is.logical(lower_case)) stop("Argument lower_case must be of type logical.")
    if (length(lower_case) == 0) stop("Argument lower_case must not be of length 0.")
    if (length(lower_case) > 1)  warning("Length of argument lower_case greater than 1. Only first element used.")
    doi_vec <- tidyr::replace_na(doi_vec, NA_character_)

    a <- stringr::str_extract(doi_vec, pkg.env$doi_regex)
    if (lower_case[1]) tolower(a) else a

}





#' Extracts all DOIs from a string
#'
#' Extracts all valid DOIs from a given character vector. Vectorised over doi_vec.
#'
#' @param doi_vec A character vector.
#' @param lower_case A single logical value stating whether or not to put the whole DOI in lower case.
#' As DOIs are case insensitive, using lower case for all may help to identify duplicates more easily.
#' @param simplify A single logical value stating whether or not to flatten the resulting list into
#' a character vector with unique DOIs only. This is useful if you just want to retrieve all unique
#' DOIs from a text and you do not mind loosing the mapping from the input vector to the output.
#' @return A list of character vectors containing DOIs (if found) or NAs otherwise. The length of the
#' list equals the length of the input vector. In case simplify = TRUE, a single character vector is
#' returned, whose length is not related to the length of the input vector.
#' @export
doi_extract_all <- function(doi_vec, lower_case = FALSE, simplify = FALSE) {

    if (!is.character(doi_vec))  stop("Argument doi_vec must be a character vector.")
    if (!is.logical(lower_case)) stop("Argument lower_case must be of type logical.")
    if (length(lower_case) == 0) stop("Argument lower_case must not be of length 0.")
    if (length(lower_case) > 1)  warning("Length of argument lower_case greater than 1. Only first element used.")
    if (!is.logical(simplify))    stop("Argument simplify must be of type logical.")
    if (length(simplify) == 0)    stop("Argument simplify must not be of length 0.")
    if (length(simplify) > 1)     warning("Length of argument simplify greater than 1. Only first element used.")
    doi_vec <- tidyr::replace_na(doi_vec, NA_character_)

    a <- stringr::str_extract_all(doi_vec, pkg.env$doi_regex)
    if (lower_case[1]) a <- purrr::map(a, ~ tolower(.x))
    if (simplify[1]) unique(purrr::flatten_chr(a)) else a

}


