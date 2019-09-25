#' Middleware Class

Middleware <-
  R6::R6Class(
    classname = "Middleware",
    public = list(
      path = NULL,
      FUN = NULL,
      method = NULL,
      protocol = NULL,
      # Initialize
      initialize = function(FUN, path, method, websocket) {
        self$FUN = FUN
        self$path = path
        self$method = method
        self$protocol = if ( websocket ) {"websocket"} else {"http"}#ifelse(websocket, "websocket", "http")
      }
    )
  )

#' #' Internal function to add middleware
#' #'
#' #' @param beakr
#' #' @param FUN
#' #' @param path
#' #' @param method
#' #' @param websocket
#' #'
#' #' @return
#'
#' addMiddleware <-
#'   function(beakr, FUN, path = NULL, method = NULL, websocket = FALSE) {
#'     method <- ifelse(!is.null(method), toupper(method), NULL)
#'     # Create new middleware
#'     mw <- Middleware$new(FUN, path, method, websocket)
#'     # Add the middleware
#'     beakr$requestHandler$addMiddleware(mw)
#'     return(beakr)
#'   }
#'
#' #' GET-binding middleware
#' #'
#' #' @param object
#' #' @param ...
#' #'
#' #' @return
#' #' @export
#' #'
#' get <- function(beakr, ...) {
#'   return(UseMethod("get"))
#' }
#'
#' #' @export
#' get.default <- function(beakr, ...) {
#'   return(base::get(beakr, ...))
#' }
#'
#'
#' #' @describeIn GET-binding middleware
#' #' @export
#' get.Beakr <- function(beakr, ...) {
#'   FUNS <- list(...)
#'   path <- FUNS[[1]]
#'   FUNS <- FUNS[-1]
#'
#'   lapply(
#'     X   = FUNS,
#'     FUN = function(mw_FUN) {
#'       addMiddleware(beakr, mw_FUN, path, method = "GET")
#'     }
#'   )
#'
#'   return(beakr)
#' }
