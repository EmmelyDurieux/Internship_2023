# splitting data 
# loading packages
pkg <- installed.packages()[, "Package"]
if(!('caret' %in% pkg)) {install.packages("caret")}
library(data.table)
library(caret)

# files and variables ---------------------------------------------------------#
ESV_ab_count <- "data/abundance-table.Soil (non-saline).txt"
ESV_rel_count <- "data/rel-abundance-table.Soil (non-saline).txt"
workdir <- getwd()

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
dimnames(Obs2t)[2] <- Obs_R[ ,1] 
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

# write to file
dt_train <- as.data.table(x_train, keep.rownames = TRUE)
dt_test <- as.data.table(x_test, keep.rownames = TRUE)

fwrite(
  dt_train, paste0(workdir, "/train.txt"),
  row.names = FALSE, quote = FALSE, sep = "\t"
)

fwrite(
  dt_test, paste0(workdir, "/test.txt"),
  row.names = FALSE, quote = FALSE, sep = "\t"
)
