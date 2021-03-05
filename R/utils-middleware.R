# ----- File serving -----------------------------------------------------------

#' @export
#' @title File-serving middleware
#'
#' @description Binds to GET requests that aren't handled by specified paths.
#' The result is to return files that are found on the host machine at the
#' requested path. Binary file types like \code{.png}, \code{.gif} or
#' \code{.pdf} are returned as raw bytes. All others are returned as characters.
#'
#' Mime types are guessed using the \pkg{mime} package. The \code{rawTypesPattern}
#' parameter is used to match mime types that should be returned as raw bytes.
#'
#' @details All files to be served in this manner must exist underneath the
#' host machine directory specified with \code{rootPath}. The directory
#' structure underneath \code{rootPath} will be mapped onto URLs underneath
#' \code{urlPath}. This helps when deploying web services at preordained URLs.
#'
#' The example below presents files underneath host machine directory
#' \code{hostDir/} to be accessed at URLS under \code{test/}.
#'
#' @param beakr \code{Beakr} instance or \code{NULL}.
#' @param urlPath String representing the URL directory underneath which static
#' file paths will appear.
#' @param rootPath String representing the absolute path used as the root
#' directory when searching for files on host machine. Defaults to the directory
#' in which the script is running.
#' @param rawTypesPattern String pattern identifying mime types to be returned
#' as raw bytes.
#' @param verbose Boolean to show a verbose static file information.
#'
#' @note If you run the example in the console, be sure to
#' \code{stopServer(bekar)} when you are done.
#'
#' @return A \code{Beakr} instance with added middleware.
#'
#' @examples
#' \donttest{
#' library(beakr)
#'
#' # Create a .txt file in temp directory
#' hostDir <- tempdir()
#' file <- paste0(hostDir, "/my_file.txt")
#' cat("I am a text file.", file = file)
#'
#' # Create an new beakr instance
#' beakr <- newBeakr()
#'
#' # beakr pipeline
#' beakr %>%
#'
#'   # Respond to GET requests at the "/hi" route
#'   httpGET(path = "/hi", function(req, res, err) {
#'     print("Hello, World!")
#'   }) %>%
#'
#'   # Respond to GET requests at the "/bye" route
#'   httpGET(path = "/bye", function(req, res, err) {
#'     print("Farewell, my friends.")
#'   }) %>%
#'
#'  # Host the directory of static files
#'  serveStaticFiles("/test", hostDir, verbose = TRUE) %>%
#'
#'  # Start the server on port 25118
#'  listen(host = "127.0.0.1", port = 25118, daemon = TRUE)
#'
#' # ------------------------------------------------------------
#' # POINT YOUR BROWSER AT:
#' # * http://127.0.0.1:25118/test/my_file.txt
#' #
#' # THEN, STOP THE SERVER WITH stopServer(beakr)
#' # ------------------------------------------------------------
#'
#' # Stop the beakr instance server
#' stopServer(beakr)
#' }

serveStaticFiles <- function(
  beakr = NULL,
  urlPath = NULL,
  rootPath = getwd(),
  rawTypesPattern = "image|json|octet|pdf|video",
  verbose = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(beakr) )
    stop("'beakr' is not defined")

  if ( is.null(rootPath) )
    stop("'rootPath' is not defined")

  # Guarantee that urlPath exists and begins with "/"
  if ( is.null(urlPath) ) {
    urlPath <- "/"
  } else if ( substr(urlPath, 1, 1) != "/" ) {
    urlPath <- paste0("/", urlPath)
  }

  if ( is.null(rawTypesPattern) )
    rawTypesPattern <- "image|json|octet|pdf|video"

  # ----- Create middleware function -------------------------------------------

  # # == BEGIN debugging, requires MazamaCoreUtils logging to be set up ==
  #
  # if ( !is.null(urlPath) )
  #   logger.trace("urlPath = %s", urlPath)
  #
  # if ( !is.null(rootPath) )
  #   logger.trace("rootPath = %s", rootPath)
  #
  # # == END debugging ==

  if ( verbose ) {
    cat(paste0("Hosting static directory ", rootPath, " @ ", urlPath))
  }

  beakr <-
    httpGET(
      beakr = beakr,
      path = NULL,
      FUN = function(req, res, err) {

        # Return Not Found if req$path doesn't inlcude urlPath
        if ( !stringr::str_detect(req$path, urlPath) ) {
          res$setStatus(404L)
          return(NULL)
        }

        # Get the URL-relative path
        relativePath <- stringr::str_replace(req$path, urlPath, "")
        relativePath <- stringr::str_replace(relativePath, "^/", "")
        filePath <- file.path(rootPath, relativePath)

        if ( !file.exists(filePath) ) {

          res$setStatus(404L)
          return(NULL)

        } else {

          # Set the response content type
          mimeType <- mime::guess_type(filePath)
          res$setContentType(mimeType)

          # Read raw bytes no matter what the mimeType
          data <- readBin( con  = filePath,
                           what = "raw",
                           n    = file.info(filePath)$size )

          # Return either raw bytes or characters depending on the mimeType
          if ( grepl(rawTypesPattern, mimeType) ) {
            return(data)
          } else {
            return(rawToChar(data))
          }

        }

      } # END FUN
    ) # END httpGET()

  return(beakr)

}


# ----- Error handling ---------------------------------------------------------

#' @export
#' @title Error-handling middleware
#'
#' @description This default error handler should be added
#' at the end of the beakr pipeline, right before \code{listen()}. Errors
#' generated by any previous step will be returned within a JSON wrapper.
#'
#' @note If you run the example in the console, be sure to
#' \code{stopServer(bekar)} when you are done.
#'
#' @param beakr Beakr instance
#' @param FUN a function to handle the error response
#'
#' @return A \code{Beakr} instance with added middleware.
#'
#' @examples
#' \donttest{
#' library(beakr)
#'
#' # Create an new beakr instance
#' beakr <- newBeakr()
#'
#' # beakr pipeline
#' beakr %>%
#'
#'   # Respond to GET requests at the "/hi" route
#'   httpGET(path = "/hi", function(req, res, err) {
#'     print("Hello, World!")
#'   }) %>%
#'
#'   # Respond to GET requests at the "/bye" route
#'   httpGET(path = "/bye", function(req, res, err) {
#'     print("Farewell, my friends.")
#'   }) %>%
#'
#'   handleErrors() %>%
#'
#'   # Start the server on port 25118
#'   listen(host = "127.0.0.1", port = 25118, daemon = TRUE)
#'
#' # ------------------------------------------------------------
#' # POINT YOUR BROWSER AT:
#' # * http://127.0.0.1:25118/NOT_A_ROUTE
#' #
#' # THEN, STOP THE SERVER WITH stopServer(beakr)
#' # ------------------------------------------------------------
#'
#' # Stop the beakr instance server
#' stopServer(beakr)
#' }

handleErrors <- function(
  beakr = NULL,
  FUN = jsonError
) {

  if ( is.null(beakr) )
    stop("'beakr' is not defined")

  beakr <-
    .routeMiddleware(
      beakr = beakr,
      FUN = FUN,
      path = NULL,
      method = NULL,
      websocket = FALSE
    )

  return(beakr)

}


#' @export
#' @title JSON error function
#'
#' @description This function is used to add a JSON error response to the
#' \code{res} object. It is called by the \code{handleErrors()} utility
#' function.
#'
#' @param req \code{Request} object.
#' @param res \code{Response} object.
#' @param err \code{Error} Error object.
#'
#' @return The incoming \code{res} object is modified.
#'
#' @seealso \link{Request}, \link{Response}, \link{Error}

jsonError <- function(req, res, err) {

  res$setContentType("application/json")

  if ( err$occurred ) {
    res$status <- 500L
    error_str <- paste(err$errors, collapse = "\n")

    cat("ERROR:\n", error_str, "\n")

    res$json(list( status = "error",
                   status_code = 500L,
                   errors = error_str ))

  } else {
    res$status = 404L
    res$json(list( status = "Page not found.",
                   status_code = 404L ))
  }

}

#' @export
#' @title Allow Cross-Origin-Requests
#'
#' @description Allow Cross-Origin Resource Sharing headers as described in
#' \href{https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS}{MDN Web Docs}.
#' Cross-origin resource sharing is a mechanism that allows restricted resources
#' on a web page to be requested from another domain(origin) outside the domain
#' from which the first resource was served.
#'
#' @param beakr \code{Beakr} instance object.
#' @param path String representing a path for which to specify a CORS policy.
#' Default \code{NULL} applies a single policy for all URL routes.
#' @param methods A vector of the request methods to allow. i.e
#' \code{Access-Control-Allow-Methods} parameter, e.g \code{GET, POST}.
#' @param origin A vector of the request origin(s) for which resource sharing
#' is enabled. i.e \code{Access-Control-Allow-Origin} response header parameter.
#' @param credentials A boolean to enable/disable credentialed requests. i.e
#' \code{Access-Control-Allow-Credentials} response header parameter.
#' @param headers A vector of the allowed headers. i.e
#' \code{Access-Control-Allow-Headers} response header parameter.
#' @param maxAge The max age, in seconds. i.e \code{Access-Control-Max-Age}
#' response header parameter.
#' @param expose The headers to expose. i.e \code{Access-Control-Expose-Headers}
#' response header parameter.
#'
#' @note You can verify that CORS is enabled by using the Chrome browser and
#' opening up the Developer Tools. The "Network" tab allows you to inspect
#' response headers and see where the \code{Cross-Origin} policy is specified.
#'
#' @note If you run the example in the console, be sure to
#' \code{stopServer(bekar)} when you are done.
#'
#' @return A \code{Beakr} instance with CORS enabled
#'
#' @seealso \link{Request}, \link{Response}, \link{Error}
#'
#' @examples
#' \donttest{
#' library(beakr)
#'
#' # Create an new beakr instance
#' beakr <- newBeakr()
#'
#' # beakr pipeline
#' beakr %>%
#'
#'   # Enable CORS
#'   cors() %>%
#'
#'   # Respond to GET requests at the "/hi" route
#'   httpGET(path = "/hi", function(req, res, err) {
#'     print("Hello, World!")
#'   }) %>%
#'
#'   # Respond to GET requests at the "/bye" route
#'   httpGET(path = "/bye", function(req, res, err) {
#'     print("Farewell, my friends.")
#'   }) %>%
#'
#'   # Start the server on port 25118
#'   listen(host = "127.0.0.1", port = 25118, daemon = TRUE)
#'
#' # ------------------------------------------------------------
#' # POINT YOUR BROWSER AT:
#' # * http://127.0.0.1:25118/hi
#' # * http://127.0.0.1:25118/bye
#' #
#' # THEN, STOP THE SERVER WITH stopServer(beakr)
#' # ------------------------------------------------------------
#'
#' # Stop the beakr instance server
#' stopServer(beakr)
#' }

cors <- function(
  beakr,
  path = NULL,
  methods = c('GET', 'POST', 'PUT', 'DELETE', 'OPTIONS', 'PATCH'),
  origin = "*",
  credentials = NULL,
  headers = NULL,
  maxAge = NULL,
  expose = NULL
) {

  if(!is.null(headers)) headers <- paste0(headers, collapse = ",")
  if(!is.null(methods)) methods <- paste0(methods, collapse = ",")

  resHeaders <- Filter(
    function(i) { !is.null(i) },
    list(
      "Access-Control-Allow-Origin" = origin,
      "Access-Control-Expose-Headers" = expose,
      "Access-Control-Max-Age" = maxAge,
      "Access-Control-Allow-Credentials" = credentials,
      "Access-Control-Allow-Methods" = methods,
      "Access-Control-Allow-Headers" = headers
    )
  )

  .routeMiddleware(
    beakr,
    path = path,
    method = NULL,
    FUN = function(req, res, err) {

      if ( req$method == "OPTIONS" ) {
        res$setHeader("Access-Control-Allow-Methods", methods)
        res$setHeader("Access-Control-Allow-Origin", origin)
        res$setHeader("Access-Control-Allow-Headers", headers)
      }

      lapply(
        names(resHeaders),
        function(header) { res$setHeader(header, resHeaders[[header]]) }
      )

      return(NULL)
  })


  return(beakr)

}
