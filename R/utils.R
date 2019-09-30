#' Parse the parameters passed by the req
#'
#' @param req an HTTP req
#' @param body a body text string
#' @param query a url-encoded query string
#' @param type a media type (a.k.a. Multipurpose Internet Mail Extensions or MIME type)
#'
#' @return parsed parameters list
#' @export
#'
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
  beakr$route$addListener(mw)
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
listen <-
  function(beakr, host = "127.0.0.1", port = 8080, daemonized = FALSE, verbose = FALSE ) {
    options("beakr.verbose" = verbose)
    message(paste0("Serving beakr instance at http://", host, ":", port))
    beakr$start(host, port, daemonized)
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
kill <- function(beakr) {
  httpuv::stopServer(beakr$serverObject)
  cat("\nStopped ")
  beakr$print()
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
killall <- function() {
  httpuv::stopAllServers()
  cat("\nStopped All Instances")
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
active <- function() {
  lapply(
    X = httpuv::listServers() ,
    FUN = function(s) {
      paste( paste0("Host: ", s$getHost()),
             paste0("Port: ", s$getPort()),
             paste0("Active: ", "TRUE"),
             sep = " | " )
    }
  )
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
      addMiddleware( beakr  = beakr,
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
      addMiddleware( beakr  = beakr,
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
      addMiddleware( beakr  = beakr,
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
      addMiddleware( beakr  = beakr,
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
use <- function(beakr, path, ..., method = NULL) {
  lapply(
    X = list(...),
    FUN = function(middleware_FUN) {
      addMiddleware( beakr  = beakr,
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
     addMiddleware( beakr  = beakr,
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
  jsoner <- function(req, res, err) {
    res$contentType("application/json")
    if ( err$occurred ) {
      error_str <- paste(err$errors, collapse = "\n")
      res$status <- 500L
      res$json(list( status = "err",
                          status_code = 500L,
                          errors = error_str ))
      if ( getOption("beakr.verbose") ) {
        cat("ERROR:\n", error_str, "\n")
      }

    } else {
      res$status = 404L
      res$json(list( status = "Page not found.",
                          status_code = 404L ))
    }
  }

  return(use(beakr = beakr, path = path, method = NULL, jsoner))

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
  filer <- function(req, res, err) {
    if ( substring(text = req$path, first = nchar(req$path)) == "/" ) {
      req$path <- paste0(req$path, "index.html")
    }

    if ( is.null(path) ) {
      fpath <- paste0(root, "/", req$path)
    } else {
      ppath <- gsub(paste0(".*", path, "(.*)"), "\\1", req$path)
      fpath <- paste0(root, "/", ppath)
    }

    bound <- ifelse( test = is.null(path),
                     yes  = TRUE,
                     no   = substr(req$path, 2, nchar(path) + 1) == path )

    if ( file.exists(fpath) & bound ) {
      mime_type <- mime::guess_type(fpath)
      res$contentType(mime_type)
      data <- readBin( con  = fpath,
                       what = "raw",
                       n    = file.info(fpath)$size )
      if( grepl("image|octect|pdf", mime_type) ) { # Assumptions...
        return(data)
      } else {
        return(rawToChar(data))
      }
    } else {
      res$setStatus(404L)
      return(NULL)
    }
  }
  beakr::get(beakr = beakr, path = NULL, filer)
  return(beakr)
}

#' Title
#'
#' @param beakr
#' @param path
#' @param with_methods
#' @param with_origin
#' @param with_headers
#' @param with_credentials
#' @param max_age
#' @param expose_headers
#'
#' @return
#' @export
#'
#' @examples
cors <-
  function(
    beakr,
    path = NULL,
    with_methods = c("POST", "GET", "PUT", "OPTIONS", "DELETE", "PATCH"),
    with_origin = "*",
    with_headers = NULL,
    with_credentials = NULL,
    max_age = NULL,
    expose_headers = NULL
  ) {

    if ( !is.null(with_headers) ) {
      with_headers <- paste0(with_headers, collapse = ",")
    }
    if ( !is.null(with_methods) ) {
      with_methods <- paste0(with_methods, collapse = ",")
    }

    headers <- list( `Access-Control-Allow-Origin`      = with_origin,
                     `Access-Control-Expose-Headers`    = expose_headers,
                     `Access-Control-Max-Age`           = max_age,
                     `Access-Control-Allow-Credentials` = with_credentials,
                     `Access-Control-Allow-Methods`     = with_methods,
                     `Access-Control-Allow-Headers`     = with_headers )

    headers <- Filter(f = function(x) { !is.null(x) }, x = headers)

    FUN <- function(req, res, err) {

      if ( req$method == "OPTIONS" ) {
        res$setHeader("Access-Control-Allow-Methods", with_methods)
        res$setHeader("Access-Control-Allow-Origin", with_origin)
        res$setHeader("Access-Control-Allow-Headers", with_headers)
        # Return empty string stops process
        return("")
      }

      lapply(
        X = names(headers),
        FUN = function(header_name) {
          res$setHeader(header_name, headers[[header_name]])
        }
      )

      return(NULL)
    }

    addMiddleware(beakr = beakr, FUN = FUN, path = path, method = NULL)

    return(beakr)
  }
