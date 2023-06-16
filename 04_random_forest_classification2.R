# classification of climate zones
# loading packages
pkg <- installed.packages()[, "Package"]
if(!('randomForest' %in% pkg)) {install.packages("randomForest")}
if(!('caret' %in% pkg)) {install.packages("caret")}

library(data.table)
library(randomForest)
library(caret)

# files and variables ---------------------------------------------------------#
latentSpace <- "latent-space.txt"
sample_meta <- "sample-metadata.Soil (non-saline).txt"
ls_taxa <- "taxa-latent-variable.txt"

# read in files ---------------------------------------------------------------#
ls <- fread(latentSpace)
sam01 <- fread(sample_meta)
taxa <- fread(ls_taxa)

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
  model <- randomForest(x = train_data[, -33], y = binary_target, ntree = 100)
  
  # Store the model in the list
  rf_models[[level]] <- model
  
}

# Predict the class for a single observation using rf_models
predict_class <- function(observation) {
  class_probs <- rep(0, length(target_levels))
  
  # Iterate over each class
  for (i in 1:length(target_levels)) {

    model <- rf_models[[target_levels[i]]]
  
    class_probs[i] <- predict(model, newdata = observation, type = "prob")[,1]
  }
  
  # Determine the class with the highest probability
  predicted_class <- target_levels[which.max(class_probs)]
  
  return(predicted_class)
}

# Predict the classes for the test data
predictions <- apply(test_data[, -33], 1, predict_class)

# Calculate the accuracy
accuracy <- mean(predictions == test_data$target)
print(accuracy)

# Iterate over each class and get top 2 variables with highest mdg ------------#

# Create an empty data frame to store the key variables
key_variable_df <- data.frame(ClimateZ = character(), Key_Variable = character(), stringsAsFactors = FALSE)

# iterate over each class
for (level in target_levels) {
  
  model <- rf_models[[level]]
  
  mdg <- model$importance[, "MeanDecreaseGini"]
  
  top_indices <- order(mdg, decreasing = TRUE)[1:2]
  
  # Get the variable names corresponding to the top indices
  top_variables <- colnames(train_data[, -9])[top_indices]
  
  # Create a data frame with the class and the top variables
  class_df <- data.frame(ClimateZ = level, Key_Variable = top_variables, stringsAsFactors = FALSE)
  
  # Append the class_df to key_variable_df
  key_variable_df <- rbind(key_variable_df, class_df)
}

# Print the dataframe with the top 2 key features for each class
print(key_variable_df)

# assigning key taxa to climate zones based on key variable -------------------#
res <- merge(key_variable_df, taxa, by.x = "Key_Variable", by.y = "latent_variable", all.x = TRUE)

# write to file
fwrite(
  res, "taxa-climatezone.txt",
  row.names = FALSE, quote = FALSE, sep = "\t"
)
