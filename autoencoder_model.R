# second autoencoder model
# loading packages
library(tensorflow)
use_virtualenv("r-reticulate")
library(tensorflow)
library(keras)
library(data.table)
library(tfruns)

# files and variables ---------------------------------------------------------#
ESV_rel_count <- "data/rel-abundance-table.Soil (non-saline).txt"
train_set <- "train.txt"
test_set <- "test.txt"

# reading in data -------------------------------------------------------------#
# full dataset
Obs_R <- fread(ESV_rel_count)
Obs2 <- as.matrix(Obs_R)[,-1]
Obs2 <- apply(Obs2, MARGIN = c(1,2), FUN = as.numeric)

# transposed version
Obs2t <- t(Obs2)
dimnames(Obs2t)[2] <- Obs_R[ ,1] 

# test and train
x_train <- fread(train_set)
x_test <- fread(test_set)

x_train <- as.matrix(x_train)[,-1] %>% apply(x_train, MARGIN = c(1,2), FUN = as.numeric)
x_test <- as.matrix(x_test)[,-1] %>% apply(x_test, MARGIN = c(1,2), FUN = as.numeric)
# building model --------------------------------------------------------------#

# Define the input shape of your data
input_shape <- dim(Obs2t)[2]

# setting flags for hyperparameter tuning
FLAGS <- flags(
  flag_numeric("dropout1", 0.4),
  flag_numeric("dropout2", 0.3),
  flag_numeric("learning1", 0.001),
  flag_numeric("weight1", 0.0005),
  flag_integer("units1", 256),
  flag_integer("units2", 64),
  flag_integer("units3", 64),
  flag_integer("epoch1", 30),
  flag_string("activation1", "relu"),
  flag_string("activation2", "softmax")
)

# Define the encoder part of your autoencoder using  Sequential model
encoder <- keras_model_sequential() %>%
  layer_dense(units = FLAGS$units1, activation = FLAGS$activation1, input_shape = input_shape) %>%
  layer_dense(units = FLAGS$units2, activation = FLAGS$activation1) %>%
  layer_dense(units = FLAGS$units2, activation = FLAGS$activation1) %>%
  layer_dense(units = FLAGS$units3, activation = FLAGS$activation1) %>%
  layer_dense(units = 32, activation = FLAGS$activation1) %>%
  layer_dense(units = 16, activation = FLAGS$activation1)

# Define the decoder part of your autoencoder using a Sequential model
decoder <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = FLAGS$activation1, input_shape = c(16))%>%
  layer_dense(units = FLAGS$units3, activation = FLAGS$activation1) %>%
  layer_dense(units = FLAGS$units2, activation = FLAGS$activation1) %>%
  layer_dense(units = FLAGS$units2, activation = FLAGS$activation1) %>%
  layer_dense(units = FLAGS$units1, activation = FLAGS$activation1) %>%
  layer_dense(units = input_shape, activation = FLAGS$activation2)

# Combine the encoder and decoder models into an autoencoder model
autoencoder <- keras_model_sequential() %>%
  encoder %>%
  decoder

# set optimizer
optimizer <- optimizer_adam(learning_rate = FLAGS$learning1,
                            weight_decay = FLAGS$weight1)

# Compile the autoencoder model
autoencoder %>% compile(optimizer = optimizer, 
                        loss = "binary_crossentropy",
                        metrics = c('accuracy'))

# Train the autoencoder model on your data
autoencoder %>% fit(x_train, x_train, epochs = 20, batch_size = 32)

#autoencoder %>% fit(Obs2t, Obs2t, epochs = 50, batch_size = 32)

# evaluate the model on test data and train data
autoencoder %>% evaluate(x_test,  x_test, verbose = 2)
autoencoder %>% evaluate(x_train, x_train, verbose = 2)

#autoencoder %>% evaluate(Obs2t, Obs2t, verbose = 2)

# Use the encoder part of the model to create the latent space representation of your data
latent_space <- encoder %>% predict(Obs2t)

