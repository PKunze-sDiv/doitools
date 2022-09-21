#
# A set of functions providing easy access to Altmetric publication data
#





argument_validity_check <- function(doi_vec, api_key = NULL, wait_for = 1L) {

    if (!is.character(doi_vec)) stop("Argument doi_vec must be a character vector.")
    if (length(doi_vec) == 0)   stop("Argument doi_vec must not be of length 0.")
    if (!is.null(api_key) &&
        !is.character(api_key)) stop("Argument api_key must be NULL or a character vector.")
    if (!is.null(api_key) &&
        length(api_key) == 0)   stop("Argument api_key must be either NULL or of length greater than 0.")
    if (!is.null(api_key) &&
        length(api_key) > 1)    warning("Length of argument api_key greater than 1. Only first element used.")
    if (!is.numeric(wait_for))  stop("Argument wait_for must be numeric.")
    if (length(wait_for) == 0)  stop("Argument wait_for must not be of length 0.")
    if (length(wait_for) > 1)   warning("Length of argument wait_for greater than 1. Only first element used.")

}




# Prints a message indicating the remaining calls before the rate limit is reached
msg_limit_remainder <- function(api_info) {

  message(
      "API calls remaining:\n  this hour ",
      (dplyr::pull(api_info, .data$remaining_calls_this_hour))[1],
      "\n  today    ",
      (dplyr::pull(api_info, .data$remaining_calls_today))[1]
  )

}




# Returns the description for a given Altmetric API response status code
altmetric_status_code_description <- function(status_code) {

  if (!is.integer(status_code)) stop("Argument status_code must be of type integer.")
  if (length(status_code) == 0) stop("Argument status_code must not be of length 0.")
  if (length(status_code) > 1)  warning("Length of argument status_code greater than 1. Only first element used.")

  switch(
      as.character(status_code[1]),
      "0" = "DOI was NA. Never called API.",
      "401" = "Invalid API key used.",
      "403" = stringr::str_c("You aren't authorized for this call. Some calls and query types can only be ",
                    "made by holders of an API key and/or a license for the full access version of the API."),
      "404" = "Altmetric doesn't have any details for the DOI you provided.",
      "429" = "Maximum number of calls exceeded. You are being rate limited.",
      "502" = "The Altmetric Details Page API version you are using is currently down for maintenance.",
      "Unkown status code"
  )

}




# Turns the server response of the Altmetric API from a list into a tibble
#' @importFrom rlang .data
altmetric_response_to_tibble <- function(alt_list) {

  if (!is.list(alt_list))    stop("Argument alt_list must be a list returned by the Altmetric API.")
  if (length(alt_list) == 0) stop("Argument alt_list must not be of length 0.")

  # the API response is a list that includes sublists which prevents the simple usage of as_tibble
  # find out sublists and single elements
  sublist_finder        <- purrr::map_lgl(alt_list, is.list)
  names_sublists        <- names(sublist_finder[sublist_finder == TRUE])
  names_single_elememts <- names(sublist_finder[sublist_finder == FALSE])

  # put sublists in list columns inside of a tibble
  sublists_tibble <- tibble::as_tibble(t(alt_list[names_sublists]))

  # add residual single elements in individual columns
  result_tibble <- tibble::as_tibble(alt_list[names_single_elememts])
  result_tibble <- dplyr::bind_cols(result_tibble, sublists_tibble)

  # refinement, needed due to inconsistency of Altmetric API responses
  dplyr::mutate(result_tibble, last_updated = ifelse(is.character(.data$last_updated),
                                                     readr::parse_integer(.data$last_updated),
                                                     .data$last_updated))

}




#' Fetch publication data from the Altmetric API
#'
#' @description The Altmetric API provides a set of publication meta data including attention scores in social networks
#' for a large number of publications. See [https://api.altmetric.com](https://api.altmetric.com) for more
#' information. Use these functions to retrieve Altmetric data by supplying the DOI (digital object identifier)
#' of a publication.
#'
#' @details
#' \itemize{
#'   \item `doi_get_altmetrics` is vectorized over doi_vec and allows downloading metrics of multiple publications. It
#' provides a simple text progress bar.
#'   \item `doi_get_altmetrics_single` fetches metrics for a single publication only. It is suited for non-interactive
#' sessions like a shiny app that implements a custom progress indicator.
#'   \item `doi_get_altmetrics_background` is suited for background jobs. It allows downloading a large number of
#' publications that usually would exceed the API's rate limits. If the rate limit is reached, it waits
#' until retrieval is possible again and then continues downloading until completion. Intermediate results will be
#' saved in a file and can be backed up if the execution is interrupted.
#' }
#' @param doi_vec A character vector containing publication DOIs. doi_get_altmetrics_single only uses the
#' first element.
#' @param api_key The Altmetric api key to be used for the query.
#' If no api key is provided, the access is currently limited to 720 calls per hour and 1200 calls per day.
#' @param wait_for Time to wait between each api call in seconds. The default value is 1 which is what Altmetric
#' urges you not to undercut. doi_get_altmetrics_background uses this as a fixed value.
#' @return A list of two tibbles with the same number of rows. The first, called metrics, contains the downloaded
#' publication meta data for the supplied DOIs. The second one, called api_info, provides data on the status of each
#' API call. It has four columns:
#' \itemize{
#'   \item `api_response_code` The response code of the API
#'   \item `api_response_msg` The description of the response code, which can either be an error message or the word 'Success'
#'   \item `remaining_calls_this_hour` The number of remaining API calls beeing until the hourly rate limit is reached
#'   \item `remaining_calls_today` The number of remaining API calls beeing until the daily rate limit is reached
#' }
#' @examples my_dois <- c("10.7191/jeslib.2021.1180", "10.1108/02640470610689151", ",,,not,a,doi,,,")
#'
#' result <- doi_get_altmetrics(my_dois)
#' result
#'
#' # successful calls only
#' result$metrics[result$api_info$api_response_msg == "Success", ]
#'
#' # failed calls only
#' result$metrics[result$api_info$api_response_msg != "Success", ]
#'
#' # mean Altmetric score of all DOIs
#' mean(result$metrics$score, na.rm = TRUE)
#' @export
doi_get_altmetrics <- function(doi_vec, api_key = NULL, wait_for = 1L) {

    # check validity of function arguments
    argument_validity_check(doi_vec, api_key, wait_for)

    # initialize progress bar
    pb     <- utils::txtProgressBar(min = 0, max = length(doi_vec), style = 3)
    # initialize return value
    result <- list(list(metrics = tibble::tibble(), api_info = tibble::tibble()))

    # loop over DOIs
    for (i in seq_along(doi_vec)) {

        # read metrics from API
        result[[i]] <- doi_get_altmetrics_single(doi_vec[i], api_key[1])

        # is a rate limit reached?
        if ((dplyr::pull(result[[i]]$api_info, .data$api_response_code))[1] == 429L) {

            # is the daily or hourly limit reached? Warn accordingly
            if ((dplyr::pull(result[[i]]$api_info, .data$remaining_calls_today))[1] == 0L) {
                warning("Daily rate limit reached. ", appendLF = FALSE)
            } else {
                warning("Hourly rate limit reached. ", appendLF = FALSE)
            }

            warning("Interrupting at this point. You only have a subset of the data you tried to retrieve.",
                    "\nSee doi_get_altmetrics_background for easily handling the rate limit.")

            # close the progress bar
            close(pb)

            # returning the intermediate result until this point
            result[[i]] <- NULL
            return(
                list(metrics  = purrr::map_dfr(result, "metrics"),
                     api_info = purrr::map_dfr(result, "api_info"))
            )

        }

      # update progress bar
      utils::setTxtProgressBar(pb, i)

      # wait for the time specified in function argument
      Sys.sleep(wait_for[1])

    }

    # close the progress bar
    close(pb)

    # message remaining API calls
    if (is.null(api_key)) msg_limit_remainder(result[[i]]$api_info)

    # return result
    list(metrics  = purrr::map_dfr(result, "metrics"),
         api_info = purrr::map_dfr(result, "api_info"))

}




#' @rdname doi_get_altmetrics
#' @export
#' @importFrom rlang .data
doi_get_altmetrics_single <- function(doi_vec, api_key = NULL) {

    # check validity of function arguments
    argument_validity_check(doi_vec, api_key)
    if (is.na(doi_vec[1])) {
        warning("NA in argument doi_vec. This results in a row of NAs. Better filter NAs bevor calling doi_get_altmetrics.")
        metrics  <- tibble::tibble(doi = doi_vec[1])
        api_info <- tibble::tibble(api_response_msg          = altmetric_status_code_description(0L),
                                   api_response_code         = 0L,
                                   remaining_calls_this_hour = NA,
                                   remaining_calls_today     = NA
                    )
        return(list(metrics = metrics, api_info = api_info))
    }

    # create API call url and perform the request
    ApiUrl   <- "https://api.altmetric.com/v1/doi/"
    url      <- stringr::str_c(ApiUrl, doi_vec[1], ifelse(is.null(api_key[1]), "", stringr::str_c("?key=", api_key[1])))
    response <- httr::GET(url)

    # is the response a success or not? build metrics tibble and api_info tibble accordingly
    if (response$status_code == 200L) {
        metrics  <- httr::content(response)
        metrics  <- altmetric_response_to_tibble(metrics)
        metrics  <- dplyr::rename(metrics, altmetric_doi = .data$doi)
        metrics  <- dplyr::mutate(metrics, doi = doi_vec[1])
        metrics  <- dplyr::select(metrics, .data$doi,dplyr::everything())
        api_info <- tibble::tibble(api_response_msg  = "Success")
    } else {
        metrics  <- tibble::tibble(doi = doi_vec[1])
        api_info <- tibble::tibble(api_response_msg  = altmetric_status_code_description(response$status_code))
    }

    # attach further info to api_tibble (response status code and remainig calls)
    api_info <- dplyr::mutate(
                    api_info,
                    api_response_code         = response$status_code,
                    remaining_calls_this_hour = response$headers$`x-hourlyratelimit-remaining`,
                    remaining_calls_today     = response$headers$`x-dailyratelimit-remaining`
    )

    # return result
    return(list(metrics = metrics, api_info = api_info))

}








#' @rdname doi_get_altmetrics
#' @export
doi_get_altmetrics_background <- function(doi_vec, api_key = NULL) {

    # check validity of function arguments
    argument_validity_check(doi_vec, api_key)

    # initialize return value
    result         <- list(list(metrics = tibble::tibble(), api_info = tibble::tibble()))
    # backup file name for intermediate result in case process is being interrupted
    backup_file    <- "altmetrics_backup.rds"
    # status of the backup functionality
    backup_working <- TRUE

    # loop over DOIs
    for (i in seq_along(doi_vec)) {

        repeat {

            message("Trying DOI ", i, "/", length(doi_vec), " [ ", doi_vec[i], " ] ...")

            # read metrics from API
            result[[i]] <- doi_get_altmetrics_single(doi_vec[i], api_key[1])

            # is a rate limit reached?
            if ((dplyr::pull(result[[i]]$api_info, .data$api_response_code))[1] == 429L) {

                message("API rate limit reached. ", appendLF = FALSE)

                # attempt to save intermediate result to file
                if (backup_working) {
                    message("Saving intermediate result to ", backup_file, "... ", appendLF = FALSE)
                    a <- tryCatch(readr::write_rds(result, backup_file), error = function(e) a)
                    if (inherits(a, "error")) {
                        message("\n   --->>> FAIL! <<<---\nCannot save intermediate result to file. Proceeding without backup.")
                        backup_working <- FALSE
                    } else {
                        message("Success.")
                    }
                }

                # is the daily or hourly limit reached? Wait accordingly
                if ((dplyr::pull(result[[i]]$api_info, .data$remaining_calls_today))[1] == 0L) {
                    message("Waiting a day to proceed. Continue download at ", Sys.time() + 60L*60L*24L, "...")
                    Sys.sleep(60L*60L*24L)
                } else {
                    message("Waiting an hour to proceed. Continue download at ", Sys.time() + 60L*60L, "...")
                    Sys.sleep(60L*60L)
                }

                # attempt to restore intermediate result from file
                if (backup_working) {
                    message("Loading intermediate result from ", backup_file, "... ", appendLF = FALSE)
                    a <- tryCatch(readr::read_rds(backup_file), error = function(e) e)
                    if (inherits(a, "error")) {
                        message("\n   --->>> FAIL! <<<---\nCannot load intermediate result from file. Proceeding without backup.")
                        backup_working <- FALSE
                    } else {
                        message("Success.")
                        result <- a
                    }
                }

            } else {

                # no rate limit reached
                break

            }

        }

        # wait one second between each call
        Sys.sleep(1L)

    }

    # message remaining API calls
    if (is.null(api_key)) msg_limit_remainder(result[[i]]$api_info)

    # return result
    list(metrics  = purrr::map_dfr(result, "metrics"),
         api_info = purrr::map_dfr(result, "api_info"))

}


