#
# ----- App #1:  Successful GET ------------------------------------------------

library(beakr)

beakr <- newBeakr()

beakr %>%
  httpGET("/", function(req, res, err) {
    return("Successful GET request!\n")
  }) %>%
  listen(host = '127.0.0.1', port = 12518, daemon = TRUE)


# In terminal:
#  curl http://127.0.0.1:12518
# > Successful GET request!

# ----- App #2:  Successful POST -----------------------------------------------

library(beakr)

newBeakr() %>%
  httpPOST("/", function(req, res, err) {
    return("Successful POST request!\n")
  }) %>%
  listen()

# In terminal:
#  curl -X POST http://127.0.0.1:12518/
# > Successful POST request!

# ----- App #3:  Successful PUT ------------------------------------------------

library(beakr)

newBeakr() %>%
  httpPUT("/", function(req, res, err) {
    return("Successful PUT request!\n")
  }) %>%
  listen()

# In terminal:
#  curl -X PUT http://127.0.0.1:12518/
# > Successful PUT request!
