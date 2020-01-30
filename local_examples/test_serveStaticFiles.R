#
# A more thorough test of beakr to see if it can be used by someone with basic
# R skill who wants their entire function written as a verbose, single script.
#
# Output will be available at http://127.0.0.1:25118 or http://localhost:25118
#

library(MazamaCoreUtils)
library(beakr)

logger.setup()
logger.setLevel(TRACE)

newBeakr() %>%

  # ----- Welcome --------------------------------------------------------------

  httpGET("/", function(req, res, err) {

    response <-
"
<html>
<body>
<h1>Welcome to serveStaticFiles!</h1>
<p>URL paths look like <code>/serveStaticFiles/local_examples/example.txt</code></p>
</body>
</html>
"
    return(response)

  }) %>%


  # Use "/serveStaticFiles" as the URL root directory
  serveStaticFiles("/serveStaticFiles") %>%

  handleErrors() %>%

  listen(daemon = FALSE)

