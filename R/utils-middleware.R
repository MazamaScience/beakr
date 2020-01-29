# Middleware functions that accept (req, res, err)


# ----- Error handling functions -----------------------------------------------

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
#' @seealso \link{Request}, \link{Response}, \link{ERROR}

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
