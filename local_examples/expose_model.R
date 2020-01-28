
# TODO:  decorate() example doesn't work

# Import
library(beakr)
library(caret)

# Load the Iris data set
data('iris')
# Train using KNN
knn_model <- train( Species ~ .,
                    data = iris,
                    method = 'knn',
                    trControl = trainControl(method='cv', number=10),
                    metric = 'Accuracy' )

# Function to predict the species using the trained model.
predict_species <- function(sl, sw, pl, pw) {
  test <- data.frame( Sepal.Length = sl,
                      Sepal.Width = sw,
                      Petal.Length = pl,
                      Petal.Width = pw,
                      Species = NA )

  return(predict(knn_model, test))
}

# Create the beakr instance
beakr <- newBeakr()

# Use beakr to expose the model in the '/predict-species' url path.
#   See help('decorate') for more info about decorating functions.
beakr %>%
  http_get('/', function(req, res, err) "Hi") %>%
  http_put(path = '/predict-species', decorate(predict_species)) %>%
  error_handler() %>%
  listen(host = '127.0.0.1', port = 1234, daemon = TRUE)

