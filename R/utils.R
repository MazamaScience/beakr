#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' Parse the parameters passed by the request
#'
#' @param request
#' @param body
#' @param query
#' @param type
#'
#' @return parsed parameters list
#' @export
#'
parseParameters <-
  function(request, body, query, type) {
    parameters <- list()
    parameters <- c(parameters, webutils::parse_query(query))

    if ( is.null(type) ) {
      return(parameters)
    }

    if ( grepl("json", type) && nchar(body) > 0 ) {
      parameters <- c( parameters,
                       jsonlite::fromJSON(body, simplifyDataFrame = FALSE) )
    }

    if ( grepl("multipart", type) ) {
      parameters <- c( parameters,
                       mime::parse_multipart(request) )
    }

    if ( grepl("form-urlencoded", type) ) {
      parameters <- c( parameters,
                       webutils::parse_query(body) )
    }
    return(parameters)
  }

#' Start a new beakr instance
#'
#' @return a beakr class instance
#' @export
#'
beakr <- function() {
  Beakr$new()
}

#' Title
#'
#' @param beakr
#' @param host
#' @param port
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
serve <-
  function(beakr, host = "127.0.0.1", port = 8080, verbose = FALSE ) {
    options("beakr.verbose" = verbose)
    message(paste0( "Serving the beakr at http://",
                    host,
                    ":",
                    port ))
    beakr$start(host, port)
    return(beakr)
}


#' Title
#'
#' @param beakr
#'
#' @return
#' @export
#'
#' @examples
stopServer <- function(beakr) {
  httpuv::stopServer(beakr$serverObject)
  return(beakr)
}

#' Title
#'
#' @param string
#' @param path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
matchPath <- function(string, path, ...) {
  # Result init
  result <- list(match = FALSE, src = path, params = list())

  if ( !is.null(string) ) {
    if ( !(grepl(pattern = "^\\^", x = string) ||
           grepl(pattern = "\\$$", x = string)) ) {
      pattern <- paste0("^", pattern, "$")
    }

    rex <- regexpr(pattern = pattern, text = path, perl = TRUE)

    for ( n in attr(x = rex, which = "capture.name") ) {
      result$params[[n]] <- substr( x = result$src,
                                    start = attr(rex, "capture.start")[,n],
                                    stop = (attr(rex, "capture.start")[,n] +
                                      attr(rex, "capture.length")[,n] - 1) )

    }
    result$match <- ifelse(rex[[1]] > -1, TRUE, FALSE)
  }

  return(result)
}

