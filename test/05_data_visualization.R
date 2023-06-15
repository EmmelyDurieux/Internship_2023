# Data visualization and comparison
# loading packages
if (!require(devtools)) install.packages("devtools")
devtools::install_github("gaospecial/ggVennDiagram")
library("ggVennDiagram")
library(data.table)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(RColorBrewer)
library(GGally)
library(viridis)
library(hrbrthemes)


# files and variables ---------------------------------------------------------#
ESV_counts <- "abundance-table_10k.Soil (non-saline).txt"
sample_meta <- "sample-metadata_10k.Soil (non-saline).txt"
clim_taxa <- "taxa-climatezone-V2.txt"
control <- "hubs-bootstrap.txt"

# read in files ---------------------------------------------------------------#
counts <- fread(ESV_counts)
sam <- fread(sample_meta)
taxa <- fread(clim_taxa)
control <- fread(control)

# Merging and filtering data --------------------------------------------------#
# filtering counts for the key taxa

filterAndTransformCounts <- function(counts, taxa) {
  # Filter counts based on matching TaxaIDabv
  filtered_counts <- counts[counts$TaxaIDabv %in% taxa$TaxaIDabv, ]
  
  # Transpose the filtered counts
  filtered_counts <- t(filtered_counts)
  
  # Set the column names from the first row
  colnames(filtered_counts) <- filtered_counts[1, ]
  filtered_counts <- filtered_counts[-1, ]
  
  # Convert to data frame and set row names
  filtered_counts <- as.data.frame(filtered_counts)
  row_names <- row.names(filtered_counts)
  rownames(filtered_counts) <- row_names
  
  # Convert columns to numeric
  filtered_counts <- as.data.frame(lapply(filtered_counts, as.numeric))
  
  return(filtered_counts)
}

filtered_counts <- filterAndTransformCounts(counts, taxa)
print(filtered_counts)


# adding climate zones
filtered_counts$RowID <- row.names(filtered_counts)
merged_data <- merge(filtered_counts, sam[, c("SampleID", "ClimateZ")], by.x = "RowID", by.y = "SampleID")
merged_data$RowID <- NULL

# Create a separate data table for each climate zone
zone_indices <- unique(merged_data$ClimateZ)

zone_list <- lapply(zone_indices, function(zone) {
  merged_data[merged_data$ClimateZ == zone, ]
})

names(zone_list) <- zone_indices
list2env(zone_list, envir = .GlobalEnv)

# descriptive statistics ------------------------------------------------------#
# distribution phylum each taxa per climate zone
my_result <- by(taxa$Phylum, taxa$ClimateZ, FUN = function(x) table(x))

nikos_result <- by(control$Phylum, control$ClimateZone, FUN = function(x) table(x))

# Create separate data frames for each climate zone
my_result_df_list <- lapply(my_result, as.data.frame)
nikos_result_df_list <- lapply(nikos_result, as.data.frame)

# Calculate relative frequencies for each data frame
my_result_freq <- lapply(my_result_df_list, function(df) {
  if (nrow(df) > 0) {
    total <- sum(df[, -1])
    df[, -1] <- df[, -1] / total * 100
  }
  return(df)
})

nikos_result_freq <- lapply(nikos_result_df_list, function(df) {
  if (nrow(df) > 0) {
    total <- sum(df[, -1])
    df[, -1] <- df[, -1] / total * 100
  }
  return(df)
})


# pie chart
par(mfrow = c(1,2))

# Get unique levels from both datasets
all_labels <- unique(c(my_result_freq$Af$x, nikos_result_freq$Af$x))

# Generate a distinct color for each label
label_colors <- rainbow(length(all_labels))

# Af
pie(my_result_freq$Af$Freq, 
    labels = my_result_freq$Af$x,
    main = "autoencoder",
    col = label_colors)

pie(nikos_result_freq$Af$Freq, 
    labels = nikos_result_freq$Af$x,
    main = "co-occurence network",
    col = label_colors)

# Am

par(mfrow = c(1,2))

pie(my_result_freq$Am$Freq, 
    labels = my_result_freq$Am$x,
    main = "autoencoder")

pie(nikos_result_freq$Am$Freq, 
    labels = nikos_result_freq$Am$x,
    main = "co-occurence network")

# ET

par(mfrow = c(1,2))

pie(my_result_freq$ET$Freq, 
    labels = my_result_freq$ET$x,
    main = "autoencoder")

pie(nikos_result_freq$ET$Freq, 
    labels = nikos_result_freq$ET$x,
    main = "co-occurence network")

# Dfa 
par(mfrow = c(1,2))

pie(my_result_freq$Dfa$Freq, 
    labels = my_result_freq$Dfa$x,
    main = "autoencoder")

pie(nikos_result_freq$Dfa$Freq, 
    labels = nikos_result_freq$Dfa$x,
    main = "co-occurence network")

# statistical similarity with Jaccard similarity coefficient ------------------#
# Function to calculate Jaccard similarity coefficient
calculate_jaccard <- function(df1, df2) {
  phyla_df1 <- df1$x
  phyla_df2 <- df2$x
  
  common_phyla <- intersect(phyla_df1, phyla_df2)
  jaccard_sim <- length(common_phyla) / (length(phyla_df1) + length(phyla_df2) - length(common_phyla))
  
  return(jaccard_sim)
}

# Calculate Jaccard similarity for each climate zone
jaccard_results <- sapply(names(nikos_result_freq), function(climate_zone) {
  jaccard_sim <- calculate_jaccard(nikos_result_freq[[climate_zone]], my_result_freq[[climate_zone]])
  return(jaccard_sim)
})

# Print the Jaccard similarity results
print(jaccard_results)


# Function to calculate Jaccard similarity and frequency comparison -----------#
calculate_similarity <- function(df1, df2) {
  phyla_df1 <- df1$x
  phyla_df2 <- df2$x
  
  common_phyla <- intersect(phyla_df1, phyla_df2)
  
  # Calculate Jaccard similarity
  jaccard_sim <- length(common_phyla) / (length(phyla_df1) + length(phyla_df2) - length(common_phyla))
  
  # Calculate frequency comparison
  freq_diff <- abs(df1$Freq[df1$x %in% common_phyla] - df2$Freq[df2$x %in% common_phyla])
  avg_freq_diff <- mean(freq_diff)
  avg_freq_sim_percent <- 1 - (avg_freq_diff / 100) 
  
  return(list(jaccard_sim = jaccard_sim, avg_freq_sim_percent = avg_freq_sim_percent))
}

# Calculate similarity and frequency comparison for each climate zone
similarity_results <- lapply(names(nikos_result_freq), function(climate_zone) {
  result <- calculate_similarity(nikos_result_freq[[climate_zone]], my_result_freq[[climate_zone]])
  return(result)
})

# Print the similarity and frequency comparison results
for (i in seq_along(similarity_results)) {
  climate_zone <- names(nikos_result_freq)[i]
  jaccard_sim <- similarity_results[[i]]$jaccard_sim
  avg_freq_sim_percent <- similarity_results[[i]]$avg_freq_sim_percent
  
  cat("Climate Zone:", climate_zone, "\n")
  cat("Jaccard Similarity:", jaccard_sim, "\n")
  cat("Average Frequency simularity (%):", avg_freq_sim_percent, "\n")
  cat("\n")
}


# Extract the similarity and frequency comparison values
jaccard_sim_values <- sapply(similarity_results, function(result) result$jaccard_sim)
avg_freq_sim_percent_values <- sapply(similarity_results, function(result) result$avg_freq_sim_percent)

# Create a data frame for visualization
similarity_data <- data.frame(ClimateZone = names(nikos_result_freq),
                              JaccardSimilarity = jaccard_sim_values,
                              AvgFreqSimPercent = avg_freq_sim_percent_values)


# Set up the theme for the plot
theme_set(theme_bw())  # Use a clean white background
font_size <- 12  # Specify the font size


# Plotting
num_zones <- length(unique(similarity_data$ClimateZone))
zone_colors <- viridis(num_zones)



# Bar plot for Jaccard similarity
plot_jaccard <- ggplot(similarity_data, aes(x = ClimateZone, y = JaccardSimilarity, fill = ClimateZone)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(JaccardSimilarity, 2)), vjust = -0.5, size = 3.5, color = "black") +  # Add text labels
  labs(title = "Jaccard Similarity informative phylum per climate zone (autoencoder vs co-occurence)",
       x = "Climate Zone",
       y = "Similarity") +
  scale_fill_manual(values = zone_colors) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme(plot.title = element_text(size = font_size + 2, face = "bold"),
        axis.title.x = element_text(size = font_size),
        axis.title.y = element_text(size = font_size),
        axis.text.x = element_text(size = font_size),
        axis.text.y = element_text(size = font_size))

plot_jaccard


# Bar plot for average frequency difference
plot_avg_freq_diff <- ggplot(similarity_data, aes(x = ClimateZone, y = AvgFreqSimPercent, fill = ClimateZone)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(AvgFreqSimPercent, 2)), vjust = -0.5, size = 3.5, color = "black") + 
  labs(title = "Average Frequency similarity for matching informative phylum",
       x = "Climate Zone",
       y = "Similarity ") +
  scale_fill_manual(values = zone_colors) + 
  scale_y_continuous(limits = c(0, 1)) + 
  theme(plot.title = element_text(size = font_size + 2, face = "bold"),
        axis.title.x = element_text(size = font_size),
        axis.title.y = element_text(size = font_size),
        axis.text.x = element_text(size = font_size),
        axis.text.y = element_text(size = font_size))

plot_avg_freq_diff

# Arrange the plots using grid.arrange from the gridExtra package
#grid.arrange(plot_jaccard, plot_avg_freq_diff, nrow = 2)

# weighted difference score
# Calculate weighted average difference score
weight_jaccard <- 0.6  # Weight for Jaccard similarity
weight_avg_diff <- 0.4  # Weight for average frequency difference

overall_diff_score <- (weight_jaccard * mean(similarity_data$JaccardSimilarity)) +
  (weight_avg_diff * mean(similarity_data$AvgFreqDiffPercent/100))

# Print the overall difference score
print(overall_diff_score)

# percentage of overlap unique phylum -----------------------------------------#
# Initialize variables
total_overlap <- 0
total_unique <- length(unique(unlist(my_result)))
total_zones <- length(my_result)
overlap_percentages_unique <- data.frame(ClimateZone = character(total_zones), PercentageOverlap = numeric(total_zones))

# Iterate over each climate zone
for (zone in 1:total_zones) {
  # Extract the unique values for the current climate zone from each data set
  my_result_unique <- unique(unlist(my_result[[zone]]))
  nikos_result_unique <- unique(unlist(nikos_result[[zone]]))
  
  # Calculate the overlapping values
  overlap <- intersect(my_result_unique, nikos_result_unique)
  
  # Calculate the percentage of overlap for the current climate zone
  percentage_overlap <- length(overlap) / length(unique(c(my_result_unique, nikos_result_unique)))
  
  # Add the percentage of overlap to the total overlap percentage
  total_overlap <- total_overlap + percentage_overlap
  
  overlap_percentages_unique[zone, "ClimateZone"] <- names(my_result)[zone]
  overlap_percentages_unique[zone, "PercentageOverlap"] <- percentage_overlap
}

# Calculate the overall percentage of overlap
overall_percentage_overlap <- (total_overlap / total_zones) * 100

# Print the overall percentage of overlap
print(overall_percentage_overlap)



# total overlap whole results -------------------------------------------------#
# Initialize variables
total_overlap <- 0
total_unique <- length((unlist(my_result)))
total_zones <- length(my_result)
overlap_percentages <- data.frame(ClimateZone = character(total_zones), PercentageOverlap = numeric(total_zones))

# Iterate over each climate zone
for (zone in 1:total_zones) {
  # Extract the non-unique values for the current climate zone from each data set
  my_result_values <- unlist(my_result[[zone]])
  nikos_result_values <- unlist(nikos_result[[zone]])
  
  # Calculate the overlapping values
  overlap <- intersect(my_result_values, nikos_result_values)
  
  # Calculate the percentage of overlap for the current climate zone
  percentage_overlap <- length(overlap) / length((c(my_result_values, nikos_result_values)))
  
  # Add the percentage of overlap to the total overlap percentage
  total_overlap <- total_overlap + percentage_overlap
  
  # Store the individual overlap percentage in the data frame
  overlap_percentages[zone, "ClimateZone"] <- names(my_result)[zone]
  overlap_percentages[zone, "PercentageOverlap"] <- percentage_overlap
}

# Calculate the overall percentage of overlap
overall_percentage_overlap <- (total_overlap / total_zones) * 100

# Print the overall percentage of overlap
print(overall_percentage_overlap)
overlap_percentages

# plot overlap percentage 
plot_overlap_per <- ggplot(overlap_percentages, aes(x = ClimateZone, y = PercentageOverlap*100, fill = ClimateZone)) +
       geom_bar(stat = "identity") +
       geom_text(aes(label = round(PercentageOverlap*100, 1)), vjust = -0.5, size = 3.5, color = "black") + 
       labs(title = "Overlap informative phylum compared between autoencoder and co-occurence based approach",
                       x = "Climate Zone",
                       y = "Overlap (%)") +
       scale_fill_manual(values = zone_colors) + 
       scale_y_continuous(limits = c(0, 100)) + 
       theme(plot.title = element_text(size = font_size + 2, face = "bold"),
                         axis.title.x = element_text(size = font_size),
                         axis.title.y = element_text(size = font_size),
                         axis.text.x = element_text(size = font_size),
                         axis.text.y = element_text(size = font_size))
plot_overlap_per



# trying the same but with Class instead of Phylum ----------------------------#
my_result_class <- by(taxa$Class, taxa$ClimateZ, FUN = function(x) table(x))

nikos_result_class <- by(control$Class, control$ClimateZone, FUN = function(x) table(x))

# Create separate data frames for each climate zone
my_result_df_list_class <- lapply(my_result_class, as.data.frame)
nikos_result_df_list_class <- lapply(nikos_result_class, as.data.frame)

# overlap class/climate zone whole dataset
total_overlap <- 0
total_unique <- length((unlist(my_result_class)))
total_zones <- length(my_result_class)

# Iterate over each climate zone
for (zone in 1:total_zones) {
  # Extract the non-unique values for the current climate zone from each data set
  my_result_values <- unlist(my_result_class[[zone]])
  nikos_result_values <- unlist(nikos_result_class[[zone]])
  
  # Calculate the overlapping values
  overlap <- intersect(my_result_values, nikos_result_values)
  
  # Calculate the percentage of overlap for the current climate zone
  percentage_overlap <- length(overlap) / length((c(my_result_values, nikos_result_values)))
  
  # Add the percentage of overlap to the total overlap percentage
  total_overlap <- total_overlap + percentage_overlap
}

# Calculate the overall percentage of overlap
overall_percentage_overlap <- (total_overlap / total_zones) * 100

# Print the overall percentage of overlap
print(overall_percentage_overlap)

# abundance counts linked to autoencoder phylum -------------------------------#
# test with Af 
# Create the boxplot

# Extract the phylum information from taxa data frame
taxa_phylum <- taxa[, c("TaxaIDabv", "Phylum")]

# Get the matching index
matching_index <- match(colnames(zone_list$Af[, -29]), taxa_phylum$TaxaIDabv)

# Subset taxa_phylum using the matching index
matching_phylum <- taxa_phylum$Phylum[matching_index]

# Create a color palette for each unique phylum
phylum_colors <- viridis(length(unique(taxa_phylum$Phylum)))

# Create a default color for taxa without a phylum
default_color <- "gray"

# Match taxa names with phylum colors
taxa_colors <- ifelse(is.na(matching_phylum), default_color, phylum_colors)

par(las = 2)
boxplot(zone_list$Af[,-29], 
        main = "Boxplot - Af Climate Zone", 
        ylab = "Abs abundance count",
        col = taxa_colors,
        ylim = c(0, 350))

unique_phylum <- unique(taxa_phylum$Phylum)

# Create a color palette for the legend
legend_colors <- phylum_colors[!is.na(unique_phylum)]

# Create the legend
legend("topright", 
       legend = unique_phylum, 
       fill = legend_colors, 
       title = "Phylum")

# least represented phylum

getSelectedPhylum <- function(climate_zone) {
 
  zone_df <- zone_list[[climate_zone]]
  
  # calculate colsum and order
  column_sums <- colSums(zone_df[, -29])
  
  lowest_sum_indices <- order(column_sums)[1:5]
  
  # Select the columns with the lowest sums
  lowest_sum_columns <- colnames(zone_df[, lowest_sum_indices])

  selected_phylum <- taxa[taxa$TaxaIDabv %in% lowest_sum_columns, .(TaxaIDabv, Phylum)]
  
  return(unique(selected_phylum))
}

selected_phylum_Af <- getSelectedPhylum("Af")
print(selected_phylum_Af)


# Venn plot -------------------------------------------------------------------#
set.seed(20190708)

unique_climateZ <- unique(taxa$ClimateZ)[1:4]

climate <- lapply(unique_climateZ, function(climate_value) {
  taxa_subset <- as.character(taxa$Phylum[taxa$ClimateZ == climate_value])
  return(taxa_subset)
})

# 4D
ggVennDiagram(
  climate, label_alpha = 0,
  category.names = c("EF","Csb","Cfa", "Cfb")
) +
  ggplot2::scale_fill_gradient(low="blue",high = "yellow")

# 3D
ggVennDiagram(climate[1:3], 
              label_alpha = 0,
              category.names = c("EF", "Csb", "Cfa"))+
  ggplot2::scale_fill_gradient(low="yellow",high = "red")


# Boxplots --------------------------------------------------------------------#
# Create a box plot for Taxa
ggplot(merged_data, aes(x = ClimateZ, y = Taxa2)) +
  geom_boxplot() +
  xlab("Climate zone") +
  ylab("Abundance count") +
  ggtitle("Box Plot of Taxa2 by ClimateZ")

ggplot(merged_data, aes(x = ClimateZ, y = Taxa58)) +
  geom_boxplot() +
  xlab("Climate zone") +
  ylab("Abundance count") +
  ggtitle("Box Plot of Taxa58 by ClimateZ")


# parallell coordinate chart --------------------------------------------------#
subset_data <- merged_data[merged_data$ClimateZ == "Af" |
                             merged_data$ClimateZ == "Am" |
                             merged_data$ClimateZ == "BSk" |
                             merged_data$ClimateZ == "BWk", ]

ggparcoord(subset_data,
           columns = 1:6, groupColumn = 29,
           scale="std",
           showPoints = TRUE, 
           title = "Parallell coordinate chart abundance counts univariately normalized",
           alphaLines = 0.8
) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  xlab("Microorganism")


# climate zones separately########
# color palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Af 

Af_long <- Af[, -29] %>%
  gather(column, value)

# Create a box plot for each column
ggplot(Af_long, aes(x = column, y = value)) +
  geom_boxplot() +
  scale_fill_hue() + 
  xlab("Microorganism") +
  ylab("Abundance counts") +
  ggtitle("Distribution counts mo in Af climate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Am

Am_long <- Am[, -29] %>%
  gather(column, value)

# Create a box plot for each column
ggplot(Am_long, aes(x = column, y = value)) +
  geom_boxplot() +
  scale_color_hue() + 
  xlab("Microorganism") +
  ylab("Abundance counts") +
  ggtitle("Distribution counts mo in Af climate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# Create a density plot for the current column
ggplot(Am_long, aes(x = column, y = value)) +
  geom_density(fill = "skyblue", color = "black") +
  xlab(col) +
  ylab("Density") +
  ggtitle(paste("Density Plot of mo in Am climate")) +
  theme_bw()


