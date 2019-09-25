#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' Parse the parameters passed by the request
#'
#' @param request an HTTP request
#' @param body a body text string
#' @param query a url-encoded query string
#' @param type a media type (a.k.a. Multipurpose Internet Mail Extensions or MIME type)
#'
#' @return parsed parameters list
#' @export
#'
parseParameters <- function(request, body, query, type) {
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
#' @description Creates a new beakr instance that can be used to build on with
#' other functions (middleware).
#' @return a beakr class instance
#' @export
#'
beakr <- function() {
  Beakr$new()
}

#' Internal function to add listeners
#'
#' @param beakr
#' @param FUN
#' @param event
addListener <- function(beakr, FUN, event) {
  mw <- Listener$new(FUN, event)
  beakr$requestHandler$addListener(mw)
  return(beakr)
}

#' Title
#'
#' @return
#' @export
#'
newError <- function() {
  Error$new()
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
      pattern <- paste0("^", string, "$")
    }

    rex <- regexpr(pattern = pattern, text = path, perl = TRUE, ...)

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

#' Internal function to add middleware
#'
#' @param beakr
#' @param FUN
#' @param path
#' @param method
#' @param websocket
#'
#' @return
addMiddleware <- function( beakr, FUN, path = NULL,
                           method = NULL, websocket = FALSE ) {
  method <- ifelse(!is.null(method), toupper(method), NULL)
  # Create new middleware
  mw <- Middleware$new(FUN, path, method, websocket)
  # Add the middleware
  beakr$requestHandler$addMiddleware(mw)
  return(beakr)
}

#' Adding HTTP Method Overrides
#'
#' @param middleware_FUN
#' @param path
#' @param method
#'
#' @return
httpMethod <- function(middleware_FUN, path, method) {
  addMiddleware( beakr  = beakr,
                 FUN    = middleware_FUN,
                 path   = path,
                 method = method )
}

#' GET-binding middleware
#'
#' @param object
#' @param ...
#'
#' @return
#' @export
#'
get <- function(beakr, ...) {
  return(UseMethod("get"))
}
#' Default GET-binding
#' @export
get.default <- function(beakr, ...) {
  return(base::get(beakr, ...))
}
#' beakr GET-binding middleware
#' @describeIn GET-binding middleware
#' @export
get.Beakr <- function(beakr, ...) {
  FUNS <- list(...)
  path <- FUNS[[1]]
  FUNS <- FUNS[-1]

  lapply(
    X   = FUNS,
    FUN = function(middleware_FUN) {
      beakr:::addMiddleware( beakr  = beakr,
                     FUN    = middleware_FUN,
                     path   = path,
                     method = "GET" )
    }
  )
  return(beakr)
}

#' POST-binding middleware
#'
#' @param beakr
#' @param path
#' @param ...
#'
#' @return
#' @export
post <- function(beakr, path, ...) {
  lapply(
    X = list(...),
    FUN = function(middleware_FUN) {
      beakr:::addMiddleware( beakr  = beakr,
                     FUN    = middleware_FUN,
                     path   = path,
                     method = "POST" )
    }
  )
  return(beakr)
}

#' PUT-binding middleware
#'
#' @param beakr
#' @param path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
put <- function(beakr, path, ...) {
  lapply(
    X = list(...),
    FUN = function(middleware_FUN) {
      beakr:::addMiddleware( beakr  = beakr,
                     FUN    = middleware_FUN,
                     path   = path,
                     method = "PUT" )
    }
  )
  return(beakr)
}

#' DELETE-binding middleware
#'
#' @param beakr
#' @param path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
delete <- function(beakr, path, ...) {
  lapply(
    X = list(...),
    FUN = function(middleware_FUN) {
      beakr:::addMiddleware( beakr  = beakr,
                     FUN    = middleware_FUN,
                     path   = path,
                     method = "DELETE" )
    }
  )
  return(beakr)
}

#' Title
#'
#' @param beakr
#' @param path
#' @param method
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
use <- function(beakr, path, method = NULL, ...) {
  lapply(
    X = list(...),
    FUN = function(middleware_FUN) {
      beakr:::addMiddleware( beakr  = beakr,
                     FUN    = middleware_FUN,
                     path   = path,
                     method = method )
    }
  )
  return(beakr)
}

#' Title
#'
#' @param beakr
#' @param path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
webSocket <- function(beakr, path, ...) {
  lapply(
    X = list(...),
    FUN = function(middleware_FUN) {
     beakr:::addMiddleware( beakr  = beakr,
                    FUN    = middleware_FUN,
                    path   = path,
                    method = NULL )
    }
  )
  return(beakr)
}

#' Title
#'
#' @param beakr
#' @param path
#'
#' @return
#' @export
#'
#' @examples
errorHandler <- function(beakr, path = NULL) {
  jsoner <- function(request, response, error) {
    response$contentType("application/json")
    if ( error$occured ) {
      response$status <- 500L
      error_str <- paste(error$errors, collapse = "\n")
      if ( getOption("beakr.verbose") ) {
        cat("ERROR:\n", error_str, "\n")
        response$json(
          list(status = "error", status_code = 500L, error = error_str)
        )
      }
    } else {
      response$status = 404L
      response$json(
        list(status = "Page not found.", status_code = 404L)
      )
    }
  }
  beakr::use(beakr = beakr, path = path, jsoner)
  return(beakr)
}

#' Title
#'
#' @param beakr
#' @param path
#' @param root
#'
#' @return
#' @export
#'
#' @examples
static <- function(beakr, path = NULL, root = NULL) {
  root <- ifelse( test = is.null(root),
                  yes  = getwd(),
                  no   = root )
  filer <- function(response, request, error) {
    if ( substring(text = request$path, first = nchar(request$path)) == "/" ) {
      request$path <- paste0(request$path, "index.html")
    }

    if ( is.null(path) ) {
      fpath <- paste0(root, "/", request$path)
    } else {
      ppath <- gsub(paste0(".*", path, "(.*)"), "\\1", request$path)
      fpath <- paste0(root, "/", ppath)
    }

    bound <- ifelse( test = is.null(path),
                     yes  = TRUE,
                     no   = substr(request$path, 2, nchar(path) + 1) == path )

    if ( file.exists(fpath) & bound ) {
      mime_type <- mime::guess_type(fpath)
      response$contentType(mime_type)
      data <- readBin( con  = fpath,
                       what = "raw",
                       n    = file.info(fpath)$size )
      if( grepl("image|octect|pdf", mime_type) ) { # Assumptions...
        return(data)
      } else {
        return(rawToChar(data))
      }
    } else {
      response$setStatus(404L)
      return(NULL)
    }
  }
  beakr::get(beakr = beakr, path = NULL, filer)
  return(beakr)
}
