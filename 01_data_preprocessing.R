# ### DATA PREPROCESSING OF BIOM TABLE AND METADATA FOR AUTOENCODER ###########
# getting the climate zone from kgc based on latitude and longitude
# performing descriptive statistics to filter out samples

# loading and/or installing packages
pkg <- installed.packages()[, "Package"]
if(!('kgc' %in% pkg)) {install.packages("kgc")}
if(!('data.table' %in% pkg)) {install.packages("data.table")}
if(!('stringr' %in% pkg)) {install.packages("stringr")}

library(data.table)
library(kgc)
library(rbiom)
library(stringr)

# files and variables ---------------------------------------------------------#
# metadata: https://github.com/biocore/emp/tree/master/data/mapping-files
# biomtable: (https://github.com/biocore/emp/tree/master/data/biom-tables)

sample_meta <- "data/emp_qiime_mapping_subset_2k.tsv"
biom_table <- "data/emp_deblur_90bp.subset_2k.biom"
empo3_ontology <- "Soil (non-saline)"
abundance_threshold <- 1e-04
prevalence_threshold <- 0.1

# read in EMP meta data + assign climate zones --------------------------------#
sam <- fread(sample_meta)
sam_subset <- sam[which(empo_3 == empo3_ontology), ]

sam_subset <- data.table(sam_subset, 
                   rndCoord.lon = RoundCoordinates(sam_subset$longitude_deg),
                   rndCoord.lat = RoundCoordinates(sam_subset$latitude_deg))
sam_subset <- data.table(sam_subset, ClimateZ=LookupCZ(sam_subset))


# read and filter biom file ---------------------------------------------------#
biom2 <- read_biom(biom_table, tree = FALSE)

# limiting data to only soil (non-saline samples)
subset_biom <- select(biom2, samples = sam_subset$`#SampleID`)

# obtain counts and taxonomy biom file
taxonomy_table  <- taxonomy(subset_biom)
ab_counts <- counts(subset_biom)
ab_counts = setDT(as.data.frame(ab_counts), keep.rownames = "TaxaIDabv")


# filtering ESV's -------------------------------------------------------------#
# relative abundances (threshold : >0.01%) + presenting in more than 10% of samples

# relative abundances 

rel_abundance_table <- ab_counts[, lapply(.SD, function(x) x/sum(x, na.rm = TRUE)),
                      .SDcols = colnames(ab_counts[,-1])]

# adding back the taxa sequence
rel_abundance_table <- data.table(ab_counts$TaxaIDabv, rel_abundance_table)

# removing noise (rel_abundance < 0.01)
rel_abundance_table[rel_abundance_table < abundance_threshold] <- 0


# prevalence
row_filter <- rowSums(rel_abundance_table > 0) > floor(prevalence_threshold * nsamples(subset_biom))
rel_abundance_table <- rel_abundance_table[row_filter, ]
abs_abundance_table <- ab_counts[row_filter,]


# filtering taxonomy table ----------------------------------------------------#
taxonomy_table <- setDT(
  as.data.frame(taxonomy_table),
  keep.rownames = "TaxaID"
)

taxonomy_table <- taxonomy_table[row_filter,]

taxonomy_table$TaxaIDabv = rleid(taxonomy_table$TaxaID, prefix = "Taxa")

taxonomy_table = taxonomy_table[, c(
  "TaxaID",
  "TaxaIDabv",
  "Kingdom",
  "Phylum",
  "Class",
  "Order",
  "Family",
  "Genus",
  "Species"
), with = FALSE]

index <- match(rel_abundance_table$TaxaIDabv, taxonomy_table$TaxaID)

rel_abundance_table$TaxaIDabv <- taxonomy_table[index, ]$TaxaIDabv

abs_abundance_table$TaxaIDabv <- taxonomy_table[index, ]$TaxaIDabv

taxonomy_table <- taxonomy_table[which(Kingdom == "k__Bacteria"), ]
abundance_table <- abs_abundance_table[which(TaxaIDabv %in% taxonomy_table$TaxaIDabv), ]
rel_abundance_table <- rel_abundance_table[which(TaxaIDabv %in% taxonomy_table$TaxaIDabv)]

# filter samples based on representation --------------------------------------#

# drop missing values
sam_subset = sam_subset[which(ClimateZ != "" & !is.na(ClimateZ)), ]


index = colSums(obs0[, sam_subset$`#SampleID`, with = FALSE])

drop = names(which(index == 0))
keep = names(which(index != 0))

sam_subset = sam_subset[which(sam_subset$`#SampleID` %in% keep), ]


# filter out climate zones with less then representing 3 samples  

sam_subset[, by = ClimateZ, N_climatezone := .N]

sam_subset = sam_subset[which(N_climatezone >= 3), ]

sam_subset$N_climatezone = NULL

# creating mapping files ----------------------

sam_mapping <- data.table(
  "SampleIDabv" = paste0("S", 1:nrow(sam_subset)),
  "SampleID"    = sam_subset$`#SampleID`
)


sam_subset = merge(sam_mapping, sam_subset, by.x = "SampleID", by.y = "#SampleID")
sam_subset = sam_subset[str_order(sam_subset$SampleIDabv, numeric = TRUE), ]

# relative abundance table
rel_abundance_table = rel_abundance_table[, c("TaxaIDabv", sam_subset$SampleID), with = FALSE]

colnames(rel_abundance_table) = c("TaxaIDabv", sam_subset$SampleIDabv)

# absolute abundance table
abundance_table = abundance_table[, c("TaxaIDabv", sam_subset$SampleID), with = FALSE]

colnames(abundance_table) = c("TaxaIDabv", sam_subset$SampleIDabv)

# write to to file ------------------------------------------------------------#

fwrite(
  sam_subset, paste0(workdir, "/sample-metadata.", empo3_ontology, ".txt"),
  row.names = FALSE, quote = FALSE, sep = "\t"
)

fwrite(
  abundance_table, paste0(workdir, "/abundance-table.", empo3_ontology, ".txt"),
  row.names = FALSE, quote = FALSE, sep = "\t"
)

fwrite(
  rel_abundance_table, paste0(workdir, "/rel-abundance-table.", empo3_ontology, ".txt"),
  row.names = FALSE, quote = FALSE, sep = "\t"
)

fwrite(
  taxonomy_table, paste0(workdir, "/taxonomy-table.", empo3_ontology, ".txt"),
  row.names = FALSE, quote = FALSE, sep = "\t"
)

