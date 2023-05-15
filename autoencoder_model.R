# second autoencoder model
# loading packages
library(tensorflow)
use_virtualenv("r-reticulate")
library(tensorflow)
library(keras)
library(data.table)
library(caret)
library(tfruns)

# files and variables ---------------------------------------------------------#
ESV_ab_count <- "data/abundance-table.Soil (non-saline).txt"
ESV_rel_count <- "data/rel-abundance-table.Soil (non-saline).txt"

# reading in data -------------------------------------------------------------#
Obs_A <- fread(ESV_ab_count)
Obs_R <- fread(ESV_rel_count)

Taxa_labels <- Obs_A[,1]
Obs1 <- as.matrix(Obs_A[,-1])
Obs2 <- as.matrix(Obs_R)[,-1]

# converting integers and characters to float
Obs1 <- apply(Obs1, MARGIN = c(1,2), FUN = as.numeric)
Obs2 <- apply(Obs2, MARGIN = c(1,2), FUN = as.numeric)

# transposed version
Obs2t <- t(Obs2)

# -----------------------------------------------------------------------------#
# internal validation (splitting up training set)
# Set the proportion of the validation set
validation_prop <- 0.2
# Get the number of rows in the data
n <- nrow(Obs2t)
# Create a vector of indices
indices <- 1:n
# Use createDataPartition to split the indices into a training and validation set
set.seed(123) # set seed for reproducibility

validation_indices <- createDataPartition(Obs2t, times = 1, p = validation_prop, list = FALSE)
training_indices <- setdiff(indices, validation_indices)

# Create the training and validation sets from the indices
x_train <- Obs2t[training_indices, ]
x_test <- Obs2t[-training_indices, ]

# building model --------------------------------------------------------------#

# Define the input shape of your data
input_shape <- dim(Obs2t)[2]

# setting flags for hyperparameter tuning
FLAGS <- flags(
  flag_numeric("dropout1", 0.4),
  flag_numeric("dropout2", 0.3),
  flag_integer("units1", 256),
  flag_integer("units2", 128),
  flag_string("activation1", "relu"),
  flag_string("activation2", "sigmoid")
)

# Define the encoder part of your autoencoder using a Sequential model
encoder <- keras_model_sequential() %>%
  layer_dense(units = FLAGS$units1, activation = FLAGS$activation1, input_shape = input_shape) %>%
  layer_dropout(rate = FLAGS$dropout1) %>%
  layer_dense(units = FLAGS$units2, activation = FLAGS$activation1) %>%
  layer_dropout(rate = FLAGS$dropout2) %>%
  layer_dense(units = 64, activation = FLAGS$activation1) %>%
  layer_dense(units = 32, activation = FLAGS$activation1) 

# Define the decoder part of your autoencoder using a Sequential model
decoder <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = FLAGS$activation1, input_shape = c(32)) %>%
  layer_dense(units = FLAGS$units2, activation = FLAGS$activation1) %>%
  layer_dense(units = FLAGS$units1, activation = FLAGS$activation1) %>%
  layer_dense(units = input_shape, activation = FLAGS$activation2)

# Combine the encoder and decoder models into an autoencoder model
autoencoder <- keras_model_sequential() %>%
  encoder %>%
  decoder

# Compile the autoencoder model
autoencoder %>% compile(optimizer = "adam", 
                        loss = "binary_crossentropy",
                        metrics = c('accuracy'))

# Train the autoencoder model on your data
autoencoder %>% fit(x_train, x_train, epochs = 20, batch_size = 32)
autoencoder %>% evaluate(x_test,  x_test, verbose = 2)

# Use the encoder part of the model to create the latent space representation of your data
latent_space <- encoder %>% predict(Obs2t)

