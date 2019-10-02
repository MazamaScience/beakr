#' Internal function to add listeners
#'
#' Internal function.
#'
#' @param beakr a beakr instance.
#' @param FUN listener function
#' @param event the event to listen for, such as "start" or "finish".
addListener <- function(beakr, FUN, event) {
  mw <- Listener$new(FUN, event)
  beakr$route$addListener(mw)
  return(beakr)
}

#' Internal function to add middleware
#'
#' Internal function.
#'
#' @param beakr a beakr instance.
#' @param FUN the function to route middleware
#' @param path the path to rouet the middleware
#' @param method the HTTP method to employ
#' @param websocket boolean, TRUE if websocket.
routeMiddleware <- function( beakr, FUN, path = NULL,
                             method = NULL, websocket = FALSE ) {

  if ( !is.null(method) ) {
    method <- toupper(method)
  } else {
    method <- NULL
  }

  # Create new middleware
  mw <- Middleware$new(FUN, path, method, websocket)
  # Add the middleware
  beakr$route$addMiddleware(mw)
  return(beakr)
}

#' Internal regex path query.
#'
#' @param pattern the string pattern to parse
#' @param path the path to match to
#' @param ... additional parameters
matchPath <- function(pattern, path, ...) {
  # Result init
  result <- list(match = FALSE, src = path, params = list())

  if ( !is.null(pattern) ) {
    if ( !(grepl("^\\^", pattern) ||
           grepl("\\$$", pattern)) ) {
      pattern <- paste0("^", pattern, "$")
    }

    rex <- regexpr(pattern, path, perl = TRUE, ...)

    for ( n in attr(x = rex, which = "capture.name") ) {
      result$params[[n]] <- substr( x     = result$src,
                                    start = attr(rex, "capture.start")[,n],
                                    stop  = (attr(rex, "capture.start")[,n] +
                                               attr(rex, "capture.length")[,n] - 1) )

    }
    result$match <- ifelse(rex[[1]] > -1, TRUE, FALSE)
  } else {
    result$match <- TRUE
  }

  return(result)
}

#' Parse the parameters passed by the req
#'
#' @param req an HTTP req
#' @param body a body text string
#' @param query a url-encoded query string
#' @param type a media mime type (a.k.a. Multipurpose Internet Mail Extensions
#' or MIME type).
parseParameters <- function(req, body, query, type) {
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
                     mime::parse_multipart(req) )
  }

  if ( grepl("form-urlencoded", type) ) {
    parameters <- c( parameters,
                     webutils::parse_query(body) )
  }
  return(parameters)
}
