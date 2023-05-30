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
  flag_numeric("dropout1", 0.4),
  flag_numeric("dropout2", 0.3),
  flag_numeric("learning1", 0.005),
  flag_numeric("weight1", 0.001),
  flag_integer("units1", 512),
  flag_integer("units2", 256),
  flag_integer("units3", 128),
  flag_integer("units4", 64),
  flag_integer("units5", 32),
  flag_integer("latent", 16),
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
  latent_space_DT, "latent-space_10k.txt",
  row.names = FALSE, quote = FALSE, sep = "\t"
)

# Identify original taxa of latent variables-----------------------------------#

# Get the weights of the encoder model
encoder_weights <- encoder$get_weights()

# Calculate the number of encoder layers
num_encoder_layers <- length(encoder_weights)

# Loop through the encoder layers in reverse order
num_features <- dim(encoder_weights[[1]])[1]

# Create a data.table to store the feature weights
feature_weights_dt <- data.table(latent_variable = character(), layer_index = integer(), feature_index = integer(), weight = numeric())

# Loop through the encoder layers in reverse order
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
      
      latent_variable_name <- latent_index
      
      # Create a data.table with the feature weights and append it to feature_weights_dt
      feature_weights_dt_temp <- data.table(latent_variable = latent_variable_name,
                                            layer_index = layer_index,
                                            feature_index = sorted_features,
                                            weight = feature_weights[sorted_features])
      
      feature_weights_dt <- rbindlist(list(feature_weights_dt, feature_weights_dt_temp))
    }
  } else {
    cat("Skipping Layer", layer_index, "as it has no valid weights\n\n")
  }
}

# Print the resulting data.table
print(feature_weights_dt)

# Select the variables with the highest weight for each latent variable and layer
highest_weight_features_dt <- feature_weights_dt[, .SD[which.max(weight)], by = .(latent_variable, layer_index)]
highest_weight_features_dt$latent_variable <- as.integer(highest_weight_features_dt$latent_variable)

# Print the resulting data.table
print(highest_weight_features_dt)

# Create a separate data table for each layer
layer11 <- highest_weight_features_dt[layer_index=="11", .(latent_variable, feature_index) ]
layer9 <- highest_weight_features_dt[layer_index=="9", .(latent_variable, feature_index) ]
layer7 <- highest_weight_features_dt[layer_index=="7", .(latent_variable, feature_index) ]
layer5 <- highest_weight_features_dt[layer_index=="5", .(latent_variable, feature_index) ]
layer3 <- highest_weight_features_dt[layer_index=="3", .(latent_variable, feature_index) ]

# Merge the data tables based on latent variables
merged_dt <- merge(layer11, layer9, by.x = "feature_index", by.y = "latent_variable")
merged_dt <- merged_dt[, .(latent_variable, "layer11"=feature_index, "layer9" = feature_index.y )][order(latent_variable)]

merged_dt <- merge(merged_dt, layer7, by.x = "layer9", by.y = "latent_variable")
merged_dt <- merged_dt[, .(latent_variable, `layer11`, layer9, "layer7"=feature_index )][order(latent_variable)]

merged_dt <- merge(merged_dt, layer5, by.x = "layer7", by.y = "latent_variable")
merged_dt <- merged_dt[, .(latent_variable, `layer11`, layer9, layer7, "layer5"=feature_index )][order(latent_variable)]

merged_dt <- merge(merged_dt, layer3, by.x = "layer5", by.y = "latent_variable")
merged_dt <- merged_dt[, .(latent_variable, `layer11`, layer9, layer7, layer5, "layer3"=feature_index )][order(latent_variable)]
merged_dt$layer3 <- paste0("V", merged_dt$layer3)

# extracting the taxa from input layer -----------(fail)-----------------------------#
input_weight <- as.data.frame(encoder_weights[[1]])

highest_column <- as.data.frame(apply(input_weight, 1, function(x) which.max(x)))
highest_column$taxa <- seq_len(nrow(highest_column))
highest_column$taxa <- paste0("Taxa", highest_column$taxa)

# merge taxa with corresponding variables
merged_dt <- merge(merged_dt, highest_column, by.x = "layer3", by.y = "apply(input_weight, 1, function(x) which.max(x))")
merged_dt <- merged_dt[, .(latent_variable, layer3, "Taxa"=taxa )][order(latent_variable)]

# extracting the taxa from input layer ---------------------------------------------#
input_weight <- as.data.frame(encoder_weights[[1]])

# restrict to only variables from latent space
selected_columns <- unique(merged_dt$layer3)
result <- input_weight[, selected_columns, drop = FALSE]

# max weight for each column
max_values <- apply(result, 2, function(x) {
  max_value <- max(x)
  max_row_index <- which(x == max_value)
  data.frame(max_value = max_value, Taxa = paste0("Taxa", max_row_index))
})

result <- do.call(rbind, max_values)
result <- rownames_to_column(result, var = "row_names")

# getting the full taxonomy from taxonomy table
taxonomy <- fread(taxon)

result <- merge(result, taxonomy, by.x = "Taxa", by.y = "TaxaIDabv")
final <- merge(result, merged_dt, by.x = "row_names", by.y = "layer3")
final <- final[, .(latent_variable, Taxa, TaxaID, Kingdom, Phylum, Class, Order, Family, Genus, Species)][order(latent_variable)]


# write to file
fwrite(
  final, "taxa-latent-variable.txt",
  row.names = FALSE, quote = FALSE, sep = "\t"
)
