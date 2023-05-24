# Autoencoder model
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

# data augmentation: resampling of train set
num_resamples <- 200
resampled_indices <- sample(nrow(x_train), num_resamples, replace = TRUE)
resampled_data <- x_train[resampled_indices, ]

# Check the dimensions of the resampled dataset
dim(resampled_data)

# building model --------------------------------------------------------------#

# Define the input shape of your data
input_shape <- dim(Obs2t)[2]

# setting flags for hyperparameter tuning
FLAGS <- flags(
  flag_numeric("dropout1", 0.4),
  flag_numeric("dropout2", 0.3),
  flag_numeric("learning1", 0.005),
  flag_numeric("weight1", 0.001),
  flag_integer("units1", 512),
  flag_integer("units2", 128),
  flag_integer("units3", 64),
  flag_integer("units4", 32),
  flag_integer("latent", 8),
  flag_integer("epoch1", 100),
  flag_string("activation1", "relu"),
  flag_string("activation2", "softmax")
)

# Define the encoder part of your autoencoder using  Sequential model
encoder <- keras_model_sequential() %>%
  layer_dense(units = FLAGS$units1, activation = FLAGS$activation1, input_shape = input_shape) %>%
  layer_dense(units = FLAGS$units2, activation = FLAGS$activation1) %>%
  layer_dense(units = FLAGS$units3, activation = FLAGS$activation1) %>%
  layer_dense(units = FLAGS$units4, activation = FLAGS$activation1) %>%
  layer_dense(units = FLAGS$latent, activation = FLAGS$activation1)

# Define the decoder part of your autoencoder using a Sequential model
decoder <- keras_model_sequential() %>%
  layer_dense(units = FLAGS$units4, activation = FLAGS$activation1, input_shape = c(FLAGS$latent))%>%
  layer_dense(units = FLAGS$units3, activation = FLAGS$activation1) %>%
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
                        metrics = c('accuracy', 'mse'))

# Train the autoencoder model on your data
num_train_iterations <- 5

for (i in 1:num_train_iterations) {
  # Train the model
  history <- autoencoder %>% fit(resampled_data, resampled_data, epochs = FLAGS$epoch1, batch_size = 32)
  
  # Optional: Print training progress or other information
  print(paste("Training iteration", i))
  print(history)
}

# evaluate the model on test data and train data
autoencoder %>% evaluate(x_test,  x_test, verbose = 2)
autoencoder %>% evaluate(x_train, x_train, verbose = 2)
autoencoder %>% evaluate(Obs2t, Obs2t, verbose = 2)

# Use the encoder part of the model to create the latent space representation of your data
latent_space <- encoder %>% predict(Obs2t)

# set as data.table and writ to file
latent_space_DT <- as.data.frame(latent_space)
latent_space_DT <- setDT(latent_space_DT)
fwrite(
  latent_space_DT, "latent-space.txt",
  row.names = FALSE, quote = FALSE, sep = "\t"
)


