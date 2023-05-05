# Installation and introduction of workin with auto-encoders in R
# installing necessary packages
install.packages("tensorflow")

# creating and loading python environment
library(reticulate)
#path_to_python <- install_python() # did not work so I hardcoded the path
path_to_python <- "C:/Users/emmel/AppData/Local/Programs/Python/Python311/python.exe"

# Option 1: with virtualenv 
virtualenv_create("r-reticulate", python = path_to_python)
library(tensorflow)
install_tensorflow(envname = "r-reticulate")

install.packages("keras")
use_virtualenv("r-reticulate")
library(keras)
install_keras(envname = "r-reticulate")
# R will ask to restart, so restart
# load tensor library and use the virtualenv and install keras should work

# Option 2: with conda (did not work for me)
# conda_create(envname = "r-reticulate")
# install_tensorflow(method = 'conda', envname = 'r-reticulate')
# library(reticulate)
# use_condaenv('r-reticulate')

# checking if the package is successfully installed
library(tensorflow)
tf$constant("Hello Tensorflow!")

# Getting started with an example (MNIST data)
# start by using the created env and loading the packages
library(tensorflow)
use_virtualenv("r-reticulate", required = TRUE)
library(keras)
library(tensorflow)

# example from mnist dataset
c(c(x_train, y_train), c(x_test, y_test)) %<-% keras::dataset_mnist()
x_train <- x_train / 255
x_test <-  x_test / 255

# building a sequential model
model <- keras_model_sequential(input_shape = c(28, 28)) %>%
  layer_flatten() %>%
  layer_dense(128, activation = "relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(10)

# For each example, the model returns a vector of log-odds scores, one for each class.
predictions <- predict(model, x_train[1:2, , ])
predictions

# convert log-odd scores to probabilities
tf$nn$softmax(predictions)

# define loss-function
loss_fn <- loss_sparse_categorical_crossentropy(from_logits = TRUE)

loss_fn(y_train[1:2], predictions)

# before training the model needs to be configured and compiled
model %>% compile(
  optimizer = "adam",
  loss = loss_fn,
  metrics = "accuracy"
)

# training the model
model %>% fit(x_train, y_train, epochs = 5)

# evaluating the model
model %>% evaluate(x_test,  y_test, verbose = 2)

