#
# ----- App #1:  Successful GET ------------------------------------------------

library(beakr)

createBeakr() %>%
  GET("/", function(req, res, err) {
    return("Successful GET request!\n")
  }) %>%
  listen()

# In browser:
#   http://127.0.0.1:8080
# > Successful GET request!

# ----- App #2:  Successful POST -----------------------------------------------

library(beakr)

createBeakr() %>%
  POST("/", function(req, res, err) {
    return("Successful POST request!\n")
  }) %>%
  listen()

# In terminal:
#  curl -X POST http://127.0.0.1:8080/
# > Successful POST request!

# ----- App #3:  Successful PUT ------------------------------------------------

library(beakr)

createBeakr() %>%
  PUT("/", function(req, res, err) {
    return("Successful PUT request!\n")
  }) %>%
  listen()

# In terminal:
#  curl -X PUT http://127.0.0.1:8080/
# > Successful PUT request!
