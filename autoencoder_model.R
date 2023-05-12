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
ESV_ab_count <- "training/abundance-table.Soil (non-saline).txt"

# reading in data -------------------------------------------------------------#
# absolute abundance
Obs_A <- fread(ESV_ab_count)

# relative abundance
Obs_R <- apply(Obs_A[,-1], 2, function(x) {
  
  return(x/ sum(x))
  
})
Taxa_labels <- Obs_A[,1]
Obs1 <- as.matrix(Obs_A[,-1])

# converting integers and characters to float
Obs1 <- apply(Obs1, MARGIN=c(1,2), FUN=as.numeric)

# -----------------------------------------------------------------------------#
# internal validation (splitting up training set)
# Set the proportion of the validation set
validation_prop <- 0.2
# Get the number of rows in the data
n <- nrow(Obs_R)
# Create a vector of indices
indices <- 1:n
# Use createDataPartition to split the indices into a training and validation set
set.seed(123) # set seed for reproducibility

validation_indices <- createDataPartition(Obs_R, times = 1, p = validation_prop, list = FALSE)
training_indices <- setdiff(indices, validation_indices)

# Create the training and validation sets from the indices
x_train <- Obs_R[training_indices, ]
x_test <- Obs_R[-training_indices, ]

# building model --------------------------------------------------------------#

# Define the input shape of your data
input_shape <- dim(Obs_R)[2]

# setting flags for hyperparameter tuning
FLAGS <- flags(
  flag_numeric("dropout1", 0.5),
  flag_numeric("dropout2", 0.4),
  flag_integer("units1", 256)
)

# Define the encoder part of your autoencoder using a Sequential model
encoder <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = input_shape) %>%
  layer_dropout(rate = FLAGS$dropout1) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = FLAGS$dropout1) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dropout(rate = FLAGS$dropout2) %>%
  layer_dense(units = 16, activation = "relu") 

# Define the decoder part of your autoencoder using a Sequential model
decoder <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", input_shape = c(16)) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 125, activation = "sigmoid")

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
latent_space <- encoder %>% predict(Obs1)