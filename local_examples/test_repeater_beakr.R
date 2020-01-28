#
# A more thorough test of beakr to see if it can be used by someone with basic
# R skills who wants their entire function written as a verbose, single script.
#
# Output will be available at http://127.0.0.1:8080 or http://localhost:8080
#

library(beakr)
library(MazamaCoreUtils)

newBeakr() %>%

  # ----- Welcome --------------------------------------------------------------

  http_get("/", function(req, res, err) {

    response <-
"
<html>
<body>
<h1>Welcome to repeater!</h1>
<p>URL paths look like <code>/repeater?text=...&times=...&responseType=...</code></p>
<p>The following <code>responseTypes</code> are supported:</p>
<ul>
<li><code>txt</code></li>
<li><code>json</code></li>
</ul>
</body>
</html>
"
    return(response)

  }) %>%

  # ----- Repeater -------------------------------------------------------------

  http_get("/repeater", function(req, res, err) {

    text <- setIfNull(req$parameters$text, "Howdy")
    times <- setIfNull(req$parameters$times, 8)
    responseType <- setIfNull(req$parameters$responseType, "txt")

    if ( times > 10 )
      stop("Parameter 'times' must be < 10")

    res$setContentType(mime::mimemap[responseType])

    if ( responseType == "txt" ) {

      response <- paste(rep(text, times), collapse = "\n")

    } else if ( responseType == "json" ) {

      responseList <- list(
        status = "success",
        output = paste(rep(text, times), collapse = "\n")
      )

      response <- jsonlite::toJSON(
        responseList,
        na = "null",
        pretty = TRUE,
        auto_unbox = TRUE
      )

    } else if ( responseType == "png" ) {

      pngFile <- tempfile(pattern = "repeater", fileext = "png")
      png(pngFile)
      plot(0:11,0:11, col = "transparent", axes = FALSE, xlab = "", ylab = "")
      for ( i in 1:times ) {
        text(1, 10 - i, text)
      }
      dev.off()
      response <- readr::read_file_raw(pngFile)

    } else {

      stop(paste0("responseType 'responseType' is not recognized"))

    }

    return(response)

  }) %>%

  # ----- Handle errors --------------------------------------------------------

  error_handler() %>%

  # ----- Start Beakr ----------------------------------------------------------

  listen(daemon = FALSE)

