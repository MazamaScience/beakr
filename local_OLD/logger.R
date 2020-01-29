#' @title Instance logging
#'
#' @description A wrapper for \emph{futile.logger} to log events.
#' Currently supported logged events: \emph{'start', 'finish', 'error'}.
#'
#' @param beakr a beakr instance.
#' @param level the log level (i.e. 'DEBUG', 'INFO', etc.).
#' @param file if provided, a file to write log output to.
#' @param echo if TRUE will print the log to console.
#'
#' @export
#' @return A \code{Beakr} object with added middleware.
#' @examples
#' \dontrun{
#' newBeakr() %>%
#'   httpGET('/', function(req, res, err) 'LOG TEST') %>%
#'   logger() %>%
#'   listen()
#' ## In browser at 127.0.0.1:25118
#' # DEBUG -datestamp- HTTP | / - GET - REQUEST RECEIVED
#' # INFO -datestamp- HTTP | / - GET - 200
#' }

logger <- function(
  beakr,
  level = 'DEBUG',
  file = NULL,
  echo = TRUE
) {

  if ( is.null(beakr) ) {
    beakr <- newBeakr(name = "NULL")
  }

  if ( level == 'TRACE' ) {
    thresh <- futile.logger::TRACE
  } else if ( level == 'ERROR' ) {
    thresh <- futile.logger::ERROR
  } else if ( level == 'FATAL' ) {
    thresh <- futile.logger::FATAL
  } else if ( level == 'WARN' ) {
    thresh <- futile.logger::WARN
  } else if ( level == 'INFO' ) {
    thresh <- futile.logger::INFO
  } else {
    thresh <- futile.logger::DEBUG
  }


  if ( is.null(file) & echo ) {
    appdr <- futile.logger::appender.console()
  } else if ( !is.null(file) & echo ) {
    appdr <- futile.logger::appender.tee(file)
  } else if ( !is.null(file) & !echo ) {
    appdr <- futile.logger::appender.file(file)
  } else {
    stop("logger requires  a file and/or console output to write to.")
  }

  try(futile.logger::flog.logger( name = "beakr",
                                  threshold = thresh,
                                  appender = appdr ), silent = TRUE)
  on(
    beakr = beakr,
    event = 'start',
    FUN = function(event, req, res, err) {
      msg <- paste( toupper(req$protocol),
                    "|",
                    req$path,
                    "-",
                    req$method,
                    "- REQUEST RECEIVED",
                    "\n",
                    sep = " " )
      return(futile.logger::flog.debug(msg, name = "beakr"))
    }
  )
  on(
    beakr = beakr,
    event = 'finish',
    FUN = function(event, req, res, err) {
      msg <- paste( toupper(req$protocol),
                    "|",
                    req$path,
                    "-",
                    req$method,
                    "-",
                    res$status,
                    "\n",
                    sep = " " )
      return(futile.logger::flog.info(msg, name = "beakr"))
    }
  )
  on(
    beakr = beakr,
    event = 'error',
    FUN = function(event, req, res, err, err_msg) {
      msg <- paste( toupper(req$protocol),
                    "|",
                    req$path,
                    "-",
                    req$method,
                    "- ERROR:",
                    "\n" ,
                    err_msg,
                    sep = " " )
      return(futile.logger::flog.error(msg, name = "beakr"))
    }
  )

  return(beakr)

}

#' @export
#' @title Beakr Event Listener
#'
#' @description Add an event listener to a \code{Beakr} instance. Currently
#' supported events are \code{"start", "finish", "error"}. The events
#' \code{"start"} and \code{"finish"} will pass the current state of the
#' \code{req}, \code{res} and \code{err} objects to the Listener. The
#' \code{"error"} event will pass string error message.
#'
#' @param beakr a beakr instance.
#' @param event the event to listen for, (\emph{"start", "finish", "error"}).
#' @param FUN the response middleware function.
#'
on <- function(beakr, event, FUN) {
  l <- Listener$new(event = event, FUN)
  beakr$router$addListener(l)
  # .addListener(beakr = beakr, event = event, FUN = FUN)
  return(beakr)
}

