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
taxon <- "taxonomy-table_10k.Soil (non-saline).txt"
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
  flag_numeric("learning1", 0.005),
  flag_numeric("weight1", 0.001),
  flag_integer("units1", 256),
  flag_integer("units2", 64),
  flag_integer("latent", 32),
  flag_integer("epoch1", 100),
  flag_string("activation1", "relu"),
  flag_string("activation2", "softmax")
)

# Define the encoder part of your autoencoder using  Sequential model
encoder <- keras_model_sequential() %>%
  layer_dense(units = FLAGS$units1, activation = FLAGS$activation1, input_shape = input_shape) %>%
  layer_dense(units = FLAGS$units2, activation = FLAGS$activation1) %>%
  layer_dense(units = FLAGS$latent, activation = FLAGS$activation1)

# Define the decoder part of your autoencoder using a Sequential model
decoder <- keras_model_sequential() %>%
  layer_dense(units = FLAGS$units2, activation = FLAGS$activation1, input_shape = c(FLAGS$latent))%>%
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

# set as data.table and write to file
latent_space_DT <- as.data.frame(latent_space)
latent_space_DT <- setDT(latent_space_DT)
fwrite(
  latent_space_DT, "latent-space_10k-1layer.txt",
  row.names = FALSE, quote = FALSE, sep = "\t"
)

# Identify original taxa of latent variables-----------------------------------#

# Get the weights of the decoder model
decoder_weights <- get_weights(decoder)

# Calculate the number of decoder layers
num_decoder_layers <- length(decoder_weights)

# Create a data.table to store the feature weights
feature_weights_dt <- data.table(latent_variable = character(), layer_index = integer(), feature_index = integer(), weight = numeric())

# Loop through the decoder layers
for (layer_index in 1:num_decoder_layers) {
  layer_weights <- decoder_weights[[layer_index]]  # Weights of the current layer
  
  # Analyze the weights for each latent space variable in the current layer
  if (is.matrix(layer_weights) && !anyNA(layer_weights)) {
    num_latent_variables <- dim(layer_weights)[1]  # Get the number of latent space variables
    
    for (latent_index in 1:num_latent_variables) {
      feature_weights <- layer_weights[latent_index, ]
      sorted_features <- order(feature_weights, decreasing = TRUE)
      
      latent_variable_name <- paste0("V", latent_index)
      
      # Create a data.table with the feature weights and append it to feature_weights_dt
      feature_weights_dt_temp <- data.table(latent_variable = latent_variable_name,
                                            layer_index = layer_index,
                                            feature_index = paste0("V", sorted_features),
                                            weight = feature_weights[sorted_features])
      
      feature_weights_dt <- rbindlist(list(feature_weights_dt, feature_weights_dt_temp))
    }
  } else {
    cat("Skipping Layer", layer_index, "as it has no valid weights\n\n")
  }
}

# Select the top 5 variables with the highest weight for each variable in each layer
highest_weight_features_dt <- feature_weights_dt[, .SD[order(-weight)][1:5], by = .(latent_variable, layer_index)]

# Create a separate data table for each layer ---------------------------------#
layer_indices <- unique(highest_weight_features_dt$layer_index)

layer_list <- lapply(layer_indices, function(index) {
  highest_weight_features_dt[layer_index == index, .(latent_variable, feature_index, weight)]
})

names(layer_list) <- paste0("layer", layer_indices)
list2env(layer_list, envir = .GlobalEnv)

#---------------------------(under construction )------------------------------#
# First + second layer
merged_dt <- merge(layer1, layer3, by.x = "feature_index", by.y = "latent_variable", allow.cartesian = TRUE)
merged_dt <- merged_dt[order(-weight.y)]
top5_variables <- merged_dt[, .SD[1:5], by = latent_variable]
merged_dt <- top5_variables[, .(latent_variable, "layer1"=feature_index, "layer3" = feature_index.y )][order(latent_variable)]

# second + third layer
merged_dt <- merge(merged_dt, layer5, by.x = "layer3", by.y = "latent_variable", allow.cartesian = TRUE)
merged_dt <- merged_dt[order(-weight)]
top5_variables <- merged_dt[, .SD[1:5], by = latent_variable]
merged_dt <- top5_variables[, .(latent_variable, layer1, layer3, "layer5" = feature_index)][order(latent_variable)]

# extracting the taxa from input layer ---------------------------------------------#
taxonomy <- fread(taxon)

# matching up input variables and TaxaIDabv in taxonomy table
merged_dt$layer5_numeric <- as.numeric(substr(merged_dt$layer5, 2, nchar(merged_dt$layer5)))
taxonomy$taxa_numeric <- as.numeric(substr(taxonomy$TaxaIDabv, 5, nchar(taxonomy$TaxaIDabv)))
result <- merge(merged_dt, taxonomy, by.x = "layer5_numeric", by.y = "taxa_numeric", all.x = TRUE)

# Remove the temporary numeric columns
result$layer5_numeric <- NULL
result$taxa_numeric <- NULL

final <- result[, .(latent_variable, TaxaIDabv, TaxaID, Kingdom, Phylum, Class, Order, Family, Genus, Species)][order(latent_variable)]

# write to file
fwrite(
  final, "taxa-latent-variable.txt",
  row.names = FALSE, quote = FALSE, sep = "\t"
)
