# Autoencoder model
# loading packages
library(tensorflow)
use_virtualenv("r-reticulate")
library(tensorflow)
library(keras)
library(data.table)
library(tfruns)

# files and variables ---------------------------------------------------------#
ESV_rel_count <- "rel-abundance-table_10k.Soil (non-saline).txt"
train_set <- "train_10k.txt"
test_set <- "test_10k.txt"

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

# data augmentation: resampling of train set ----------------------------------#
#num_resamples <- 200
#resampled_indices <- sample(nrow(x_train), num_resamples, replace = TRUE)
#resampled_data <- x_train[resampled_indices, ]


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
  flag_integer("units5", 16),
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
  layer_dense(units = FLAGS$units5, activation = FLAGS$activation1) %>%
  layer_dense(units = FLAGS$latent, activation = FLAGS$activation1)

# Define the decoder part of your autoencoder using a Sequential model
decoder <- keras_model_sequential() %>%
  layer_dense(units = FLAGS$units5, activation = FLAGS$activation1, input_shape = c(FLAGS$latent))%>%
  layer_dense(units = FLAGS$units4, activation = FLAGS$activation1) %>%
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
num_train_iterations <- 6

for (i in 1:num_train_iterations) {
  # Train the model
  history <- autoencoder %>% fit(x_train, x_train, epochs = FLAGS$epoch1, batch_size = 32)
  
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
  latent_space_DT, "latent-space_10k.txt",
  row.names = FALSE, quote = FALSE, sep = "\t"
)

# Identify original taxa of latent variables-----------------------------------#

# -------------------------- ATTEMP 1 -----------------------------------------#
# Get the weights of the encoder model
encoder_weights <- encoder$get_weights()

# Extract the weights for the latent space layer
latent_weights <- encoder_weights[[11]]

# Analyze the weights for each latent space variable
num_latent_variables <- dim(latent_weights)[2]  # Get the number of latent space variables
num_features <- dim(latent_weights)[1]  # Get the number of features

latent_variable_list <- list()

for (latent_index in 1:num_latent_variables) {
  feature_weights <- latent_weights[, latent_index]
  sorted_features <- order(feature_weights, decreasing = TRUE)
  
  feature_list <- list()
  for (feature_index in 1:num_features) {
    feature_weight <- feature_weights[sorted_features[feature_index]]
    if (feature_weight != 0) {
      feature_list[[as.character(sorted_features[feature_index])]] <- feature_weight
    }
  }
  
  latent_variable_list[[paste0("Latent variable", latent_index)]] <- feature_list
}

# Print the latent variable list
str(latent_variable_list)

# -------------------------- ATTEMP 2 -----------------------------------------#
# Get the weights of the encoder model
encoder_weights <- encoder$get_weights()

# Calculate the number of encoder layers
num_encoder_layers <- length(encoder_weights)

# Create a list to store the feature weights
feature_weights_list <- list()

# Loop through the encoder layers in reverse order
num_features <- dim(encoder_weights[[1]])[1]  # Get the number of features

for (layer_index in seq(num_encoder_layers, 1, -1)) {
  layer_weights <- encoder_weights[[layer_index]]  # Weights of the current layer
  
  if (layer_index > 1) {
    prev_layer_weights <- encoder_weights[[layer_index - 2]]  # Weights of the previous layer
  }
  
  # Analyze the weights for each latent space variable in the current layer
  if (is.matrix(layer_weights) && !anyNA(layer_weights)) {
    num_latent_variables <- dim(layer_weights)[2]  # Get the number of latent space variables
    
    for (latent_index in 1:num_latent_variables) {
      feature_weights <- layer_weights[, latent_index]
      sorted_features <- order(feature_weights, decreasing = TRUE)
      
      latent_variable_name <- paste0("Latent variable", latent_index, " in Layer", layer_index)
      feature_weights_list[[latent_variable_name]] <- setNames(feature_weights[sorted_features], sorted_features)
    }
  } else {
    cat("Skipping Layer", layer_index, "as it has no valid weights\n\n")
  }
}

str(feature_weights_list)

# selecting the variables with highest weight
highest_weight_features <- list()

for (latent_variable_name in names(feature_weights_list)) {
  feature_weights <- feature_weights_list[[latent_variable_name]]
  highest_weight_feature <- names(feature_weights)[which.max(feature_weights)]
  
  highest_weight_features[[latent_variable_name]] <- highest_weight_feature
}

# Print the input feature with the highest weight for each latent variable
for (latent_variable_name in names(highest_weight_features)) {
  cat(latent_variable_name, ":\n")
  cat("Input variable with the highest weight:", highest_weight_features[[latent_variable_name]], "\n")
  cat("\n")
}


# Trace back to the original 8 latent variables in layer 11 (doesn't work yet)
original_latent_variables <- highest_weight_features[grepl("Latent variable\\d+ in Layer11", names(highest_weight_features))]
latent_variable_names <- names(highest_weight_features)[grepl(paste0("Latent variable\\d+ in Layer", prev_layer_index), names(highest_weight_features))]

latent_variable_names <- character()
for (index in prev_layer_index) {
  pattern <- paste0("Latent variable\\d+ in Layer", index)
  matching_names <- names(highest_weight_features)[sapply(names(highest_weight_features), function(x) grepl(pattern, x))]
  latent_variable_names <- c(latent_variable_names, matching_names)
}

while (length(latent_variable_names) > 0) {
  prev_layer_index <- as.integer(gsub(".*Layer(\\d+)", "\\1", latent_variable_names))
  prev_latent_variables <- character()
  
  for (index in prev_layer_index) {
    pattern <- paste0("Latent variable\\d+ in Layer", index)
    matching_names <- names(highest_weight_features)[grepl(pattern, names(highest_weight_features))]
    prev_latent_variables <- c(prev_latent_variables, matching_names)
  }
  
  if (length(prev_latent_variables) == 0) {
    break
  }
  
  original_latent_variables <- c(prev_latent_variables, original_latent_variables)
  latent_variable_names <- prev_latent_variables
}

# Print the input features with the highest weights from the original latent variables
for (latent_variable_name in names(original_latent_variables)) {
  feature_index <- original_latent_variables[[latent_variable_name]]
  cat(latent_variable_name, ":\n")
  cat("Input feature with the highest weight:", feature_index, "\n")
  cat("\n")
}

# -----------------------------------------------------------------------------#




