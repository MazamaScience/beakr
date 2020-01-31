
# TODO:  decorate() example doesn't work

# Import
library(beakr)
library(caret)

# Load the Iris data set
data("iris")

# Train using KNN
knn_model <- train(
  Species ~ .,
  data = iris,
  method = "knn",
  trControl = trainControl(method = "cv", number = 10),
  metric = "Accuracy"
)

# Function to predict the species using the trained model.
predict_species <- function(sl, sw, pl, pw) {
  test <- data.frame(
    Sepal.Length = as.numeric(sl),
    Sepal.Width = as.numeric(sw),
    Petal.Length = as.numeric(pl),
    Petal.Width = as.numeric(pw),
    Species = NA
  )

  return(predict(knn_model, test))
}

# Create the beakr instance
beakr <- newBeakr()

# Use beakr to expose the model in the "/predict-species" url path.
#   See help("decorate") for more info about decorating functions.
beakr %>%
  httpGET("/", function(req, res, err) { print("Hi") } ) %>%
  httpGET(path = "/predict-species", decorate(predict_species)) %>%
  handleErrors() %>%
  listen(host = "127.0.0.1", port = 25118, daemon = TRUE)

# Try:
# http://127.0.0.1:25118/predict-species?sl=4&sw=3&pl=1.5&pw=0.5
#  http://127.0.0.1:25118/predict-species?sl=6&sw=2&pl=5&pw=1
#  http://127.0.0.1:25118/predict-species?sl=6&sw=2&pl=5&pw=2
