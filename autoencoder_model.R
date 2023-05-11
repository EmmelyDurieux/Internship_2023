# first autoencoder model
# loading packages
use_virtualenv("r-reticulate")
library(tensorflow)
library(keras)
library(data.table)
library(dplyr)
library(caret)

# reading in data -------------------------------------------------------------#
ab_count <- fread("abundance-table.Soil (non-saline).txt")
sample_metadata <- fread("sample-metadata.Soil (non-saline).txt")


# transpose and merge data ----------------------------------------------------#
ab_count_t <- as.data.frame(t(ab_count))
colnames(ab_count_t) <- ab_count_t[1,]
ab_count_t <- ab_count_t[-1,]


# OneHot encoding format for climatezones--------------------------------------#
subset_metadata <- sample_metadata[, .(SampleIDabv, ClimateZ)]
levels(subset_metadata$SampleIDabv) <- as.list(unique(subset_metadata$SampleIDabv))
levels(subset_metadata$ClimateZ) <- as.list(unique(subset_metadata$ClimateZ))
target <- model.matrix(~ClimateZ, subset_metadata)
target <- target[,-1]



# splitting data --------------------------------------------------------------#
# features and target
x <- as.matrix(ab_count_t)  
y <- target 

# converting integers and characters to float
x <- apply(x, MARGIN=c(1,2), FUN=as.numeric)
x <- x/1.0
y <- y/1.0

# merged version cuz why not
dataset <- cbind(x, y)

# Now Selecting 75% of data as train data  
#dataset$id <- 1:nrow(dataset)
#train <- dataset %>% dplyr::sample_frac(.75)
#test  <- dplyr::anti_join(datase, train, by = 'id')


# converting text to integers ----------(under construction)-------------------#
# Vocabulary size and number of words in a sequence.
#vocab_size = 30

# Use the text vectorization layer to normalize, split, and map strings to
# integers. Note that the layer uses the custom standardization defined above.
# Set maximum_sequence length as all samples are not of the same length.
#vectorize_layer = tf$keras$layers$TextVectorization(
#  max_tokens=vocab_size,
#  output_mode='int')

# Make a text-only dataset (without labels), then call adapt
#target_text <- subset_metadata 
#vectorize_layer %>% adapt(target_text)


# -----------------------------------------------------------------------------#
# internal validation (splitting up training set)
# Set the proportion of the validation set
validation_prop <- 0.2
# Get the number of rows in the data
n <- nrow(x)
# Create a vector of indices
indices <- 1:n
# Use createDataPartition to split the indices into a training and validation set
set.seed(123) # set seed for reproducibility

validation_indices <- createDataPartition(x, times = 1, p = validation_prop, list = FALSE)
training_indices <- setdiff(indices, validation_indices)

# Create the training and validation sets from the indices
x_train <- x[training_indices, ]
y_train <- y[training_indices,]
x_val <- x[-training_indices, ]
y_val <- y[-training_indices,]

# building model (with climatezones) ------ did not work ----------------------#
model <- keras_model_sequential() %>%
  layer_flatten(input_shape = c(1635)) %>%
  layer_dense(128, activation = "relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(14)

# compiling model
model %>% compile(
  optimizer = 'adam',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

# training model
epochs <- 5
history <- model %>% fit(
  x_train, y_train,
  validation_data = list(x_val, y_val),
  epochs = epochs
)

