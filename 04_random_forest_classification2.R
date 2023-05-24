# classification of micro-organismes
# loading packages
library(data.table)
library(randomForest)
library(caret)

# files and variables ---------------------------------------------------------#
latentSpace <- "latent-space_10k.txt"
sample_meta <- "sample-metadata_10k.Soil (non-saline).txt"

# read in files ---------------------------------------------------------------#
ls <- fread(latentSpace)
sam01 <- fread(sample_meta)

# random forest classifier ----------------------------------------------------#

# Combine the latent space with the target variable
combined_data <- cbind(ls, target = sam01$ClimateZ) 

# Split the combined dataset into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(combined_data), 0.7 * nrow(combined_data))
train_data <- combined_data[train_index, ]
test_data <- combined_data[-train_index, ]

# set target as a factor 
train_data$target <- as.factor(train_data$target)
target_levels <- levels(train_data$target)

# Create a list to store the random forest models
rf_models <- list()

# Iterate over each class
for (level in target_levels) {
  # Create a binary target variable for the current class
  binary_target <- ifelse(train_data$target == level, "Yes", "No")
  binary_target <- factor(binary_target, levels = c("Yes", "No"))
  
  # Train the random forest model for the current class
  model <- randomForest(x = train_data[, -9], y = binary_target, ntree = 100)
  
  # Store the model in the list
  rf_models[[level]] <- model
}

# Function to predict the class for a single observation using the models
predict_class <- function(observation) {
  # Initialize a vector to store the class probabilities
  class_probs <- rep(0, length(target_levels))
  
  # Iterate over each class
  for (i in 1:length(target_levels)) {
    # Get the model for the current class
    model <- rf_models[[target_levels[i]]]
    
    # Predict the probability for the current class
    class_probs[i] <- predict(model, newdata = observation, type = "prob")[2]
  }
  
  # Determine the class with the highest probability
  predicted_class <- target_levels[which.max(class_probs)]
  
  return(predicted_class)
}

# Predict the classes for the test data
predictions <- apply(test_data[, -9], 1, predict_class)

# getting the most informative variable for each class
importance(rf_models$Af)

# hyperparameter tuning ------------under construction-------------------------#

# Define the hyperparameter grid
hyperparameters <- expand.grid(
  mtry = c(2, 4, 6, 8),         # Number of variables randomly sampled as candidates at each split
  ntree = c(50, 100, 200, 300)    # Number of trees in the random forest
)

# Get the levels of the target variable
target_levels <- levels(train_data$target)

rf_models <- list()

# Iterate over each class
for (level in target_levels) {
  # Create a binary target variable for the current class
  binary_target <- ifelse(train_data$target == level, "Yes", "No")
  binary_target <- factor(binary_target, levels = c("Yes", "No"))
  
  # Create the tuning grid for the current class
  tuning_grid <- data.frame(mtry = hyperparameters$mtry)
  
  # Train the random forest model with hyperparameter tuning for the current class
  model <- train(x = train_data[, -9], y = binary_target, method = "rf",
                 tuneGrid = tuning_grid, trControl = trainControl(method = "cv", number = 5))
  
  # Store the best model in the list
  rf_models[[level]] <- model$finalModel
}

# Function to predict the class for a single observation using the models
predict_class <- function(observation) {
  # Initialize a vector to store the class probabilities
  class_probs <- rep(0, length(target_levels))
  
  # Iterate over each class
  for (i in 1:length(target_levels)) {
    # Get the model for the current class
    model <- rf_models[[target_levels[i]]]
    
    # Predict the probability for the current class
    class_probs[i] <- predict(model, newdata = observation, type = "prob")[2]
  }
  
  # Determine the class with the highest probability
  predicted_class <- target_levels[which.max(class_probs)]
  
  return(predicted_class)
}

# Predict the classes for the test data
predictions <- apply(test_data[, -9], 1, predict_class)

