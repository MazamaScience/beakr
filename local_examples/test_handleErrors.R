#
# An example of how to call user-defined error handling methods when beakr
# encounters an error.
#
# Output will be available at http://127.0.0.1:25118 or http://localhost:25118
#

library(MazamaCoreUtils)
library(beakr)

logger.setup()
logger.setLevel(TRACE)

myErrorFunction <- function(req, res, err) {

  res$setContentType("application/json")

  if ( err$occurred ) {
    res$status <- 500L
    error_str <- paste(err$errors, collapse = "\n")

    cat("Your error was:\n", error_str, "\n")

    res$json(list( status = "error",
                   status_code = 500L,
                   errors = error_str ))

  } else {
    res$status = 404L
    res$json(list( status = "A custom 'Page not found' error.",
                   status_code = 404L ))
  }

}

newBeakr() %>%

  # ----- Welcome --------------------------------------------------------------

httpGET("/", function(req, res, err) {

  # Cause an error
  response <- stop("My special error")

  return(response)

}) %>%

  handleErrors(FUN = myErrorFunction) %>%

  listen(daemon = FALSE)
