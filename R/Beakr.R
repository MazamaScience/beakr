#' @keywords internal
"_PACKAGE"
#'
#' @title A minimalist web framework.
#'
#' @description
#' The \pkg{beakr} package provides a minimalist web framework for for
#' developing application programming interfaces in R. The package includes
#' basic functionality for handling common \code{HTTP} requests.
#'
#' \pkg{beakr} allows R code to listen for and respond to HTTP requests, so
#' you can serve web traffic directly from an R process. \pkg{beakr} relies heavily
#' on the \href{https://github.com/rstudio/httpuv}{httpuv} package, and therefore
#' the lower level \href{https://github.com/joyent/libuv}{libuv} and
#' \href{https://github.com/nodejs/http-parser}{http-parser} C libraries.
#' \pkg{beakr} is a ground-up rewrite and
#' continuation of the \pkg{jug} package developed by Bart Smeets. The
#' \pkg{beakr} package is supported and maintained by
#' \href{http://www.mazamascience.com/}{Mazama Science}.
#'
#' @seealso \code{\link{newBeakr}}
#'
#' @name beakr-package
#' @aliases Beakr-Package
#' @title A minimalist web framework
#' @author Hans Martin \email{hans@mazamascience.com}
#' @keywords package
NULL

#' Beakr Application Class
#'
#' @description
#' A `Beakr` object defines a web server instance using the [`httpuv`] package.
#' It provides the main entry point for creating and starting a Beakr
#' application, wrapping a [`Router`] and exposing lifecycle methods.
#'
#' @docType class
#' @name Beakr
#' @export
#' @importFrom R6 R6Class
#'
#' @format An [`R6::R6Class`] generator for `Beakr` objects.
#'
#' @seealso [Router], [Middleware], [httpuv::startServer], [httpuv::runServer]

Beakr <-
  R6::R6Class(
    classname = "Beakr",
    public = list(
      #' @field name Application name. If `NULL`, a random name is set in `$initialize()`.
      name = NULL,
      #' @field router The `Router` instance used to handle requests.
      router = NULL,
      #' @field server The underlying `httpuv` server object (once started).
      server = NULL,

      #' @description
      #' Build the application definition passed to **httpuv** (request & WS handlers).
      appDefinition = function() {
        list(
          call = function(req) {
            self$router$invoke(req)
          },
          onWSOpen = function(websocket) {
            websocket$onMessage(function(binary, message) {
              websocket$send(self$router$invoke(
                req              = websocket$req,
                websocket_msg    = message,
                websocket_binary = binary
              ))
            })
          }
        )
      },

      #' @description
      #' Initialize the app: create a `Router` and assign a random `name` if missing.
      initialize = function() {
        self$router <- Router$new()
        if ( is.null(self$name) ) {
          self$name <- .randomName()
        }
      },

      #' @description
      #' Start the HTTP server via **httpuv**.
      #' @param host Hostname or IP to bind.
      #' @param port Integer port to listen on.
      #' @param daemon If `TRUE`, run in background with `httpuv::startServer()`;
      #'   otherwise run foreground with `httpuv::runServer()`.
      start = function(host, port, daemon) {
        if ( daemon ) {
          self$server <- httpuv::startServer(host = host, port = port, app = self$appDefinition())
        } else {
          self$server <- httpuv::runServer(host = host, port = port, app = self$appDefinition())
        }
      },

      #' @description
      #' Print a one-line summary (name, state, host, port, #middlewares).
      print = function() {
        if ( !is.null(self$server) ) {
          name <- self$name
          st <- ifelse(self$server$isRunning(), "Active", "Inactive")
          hst <- self$server$getHost()
          prt <- self$server$getPort()
          mws <- length(self$router$middleware)
        } else {
          name <- self$name
          st <- "Inactive"
          hst <- "..."
          prt <- "..."
          mws <- length(self$router$middleware)
        }
        cat("Beakr Instance:", name, "\n",
            "State:", st, "|", "Host:", hst, "|", "Port:", prt, "|", "Middlewares:", mws, "\n")
        invisible(self)
      }
    )
  )


