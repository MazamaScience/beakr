#' @title Instance logging
#'
#' @description A wrapper for \emph{futile.logger} to log events.
#' Currently supported logged events: \emph{'start', 'finish', error'}.
#'
#' @param beakr a beakr instance.
#' @param level the log level (i.e. 'DEBUG', 'INFO', etc.).
#' @param file if provided, a file to write log output to.
#' @param echo if TRUE will print the log to console.
#'
#' @export
logger <- function(beakr, level = 'DEBUG', file = NULL, echo = TRUE) {

  if ( is.null(beakr) ) {
    beakr <- createBeakr(name = "NULL")
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

  futile.logger::flog.logger( name = "beakr",
                              threshold = thresh,
                              appender = appdr )


  onEvent(
    beakr = beakr,
    event = 'start',
    FUN = function(event, req, res, err) {
      msg <- paste( toupper(req$protocol),
                    "|",
                    req$path,
                    "-",
                    req$method,
                    "- Request Received",
                    "\n",
                    sep = " " )
      return(futile.logger::flog.debug(msg, name = "beakr"))
    }
  )
  onEvent(
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
  onEvent(
    beakr = beakr,
    event = 'error',
    FUN = function(event, req, res, err, err_msg) {
      msg <- paste( toupper(req$protocol),
                    "|",
                    req$path,
                    "-",
                    req$method,
                    "- error encountered:",
                    "\n" ,
                    err_msg,
                    sep = " " )
      return(futile.logger::flog.error(msg, name = "beakr"))
    }
  )

  return(beakr)

}
