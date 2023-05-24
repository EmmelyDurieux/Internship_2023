# getting the climate zone from kgc based on latitude and longitude
# performing descriptive statistics to filter out samples
# loading and/or installing packages
pkg <- installed.packages()[, "Package"]
if(!('kgc' %in% pkg)) {install.packages("kgc")}
library(fastverse)
library(data.table)
library(kgc)
library(rbiom)

# read in EMP data
# data: https://github.com/biocore/emp/tree/master/data/mapping-files
df<-read.delim("emp_qiime_mapping_subset_2k.tsv", sep="\t")
DT <- setDT(df)

# lookup the climate zones
zonesDT <- data.table(DT, 
                      rndCoord.lon = RoundCoordinates(DT$longitude_deg),
                      rndCoord.lat = RoundCoordinates(DT$latitude_deg))
zonesDT <- data.table(zonesDT, ClimateZ=LookupCZ(zonesDT))
zonesDT

# filtering and grouping data
# limit to only soil (non-saline) data
zonesDT_soil <- zonesDT[empo_3=="Soil (non-saline)"]

# how many samples in each climate zone
zonesDT_soil[,count(empo_3), by=ClimateZ][order(freq)]

# which samples belong to the less represented climatezone
zonesDT_soil[ClimateZ=="BWh"| ClimateZ=="BSh" | ClimateZ=="Cwb", .(X.SampleID)]

# dropping climate zones with less than 3 representing samples
zonesDT_soil <- zonesDT_soil[ClimateZ!="BWh"& ClimateZ!="BSh" & ClimateZ!="Cwb"]

# filter on type of sample
zonesDT_soil[, count(sample_taxid), by=sample_scientific_name]
zonesDT_soil[sample_scientific_name=="rhizosphere metagenome", .(X.SampleID)]

# looking at the country
zonesDT_soil[,count(country)]
zonesDT_soil[, count(country), by=ClimateZ][order(x)]

# looking at sequence depth
zonesDT_soil[, count(depth_m)]

# amount of observed OTU's per zone
zonesDT_soil[, sum(adiv_observed_otus), by=ClimateZ]
zonesDT_soil[, .(sum=sum(adiv_observed_otus)), by=ClimateZ][order(sum)]

# filter for samples that have 0 observations_deblur
zonesDT_soil[observations_deblur_90bp==0, .(X.SampleID)]
zonesDT_soil[observations_deblur_100bp==0, .(X.SampleID)]
zonesDT_soil[observations_deblur_150bp==0, .(X.SampleID)]

# sum observations_deblur per climate zone
zonesDT_soil[observations_deblur_90bp!=0,.(sum=sum(observations_deblur_90bp)), 
             by=ClimateZ][order(sum)]
zonesDT_soil[observations_deblur_100bp!=0,.(sum=sum(observations_deblur_100bp)), 
             by=ClimateZ][order(sum)]
zonesDT_soil[observations_deblur_150bp!=0,.(sum=sum(observations_deblur_150bp)), 
             by=ClimateZ][order(sum)]

# write filtered data.table to file
fwrite(zonesDT_soil, file = "Kgc_soil_filtered.tsv", sep = "\t")

# looking at ESVs in the biom file
# reading EMP data (https://github.com/biocore/emp/tree/master/data/biom-tables)
biom2 <- read.biom("emp_deblur_90bp.subset_2k.biom")
print(biom2)

# store biom file as data.table file 
# DTbiom = counts(biom2) %>% as.data.frame(biom2) %>% setDT(biom2, keep.rownames = TRUE)
biom_counts = counts(biom2)
DFbiom = as.data.frame(biom_counts)
DTbiom = setDT(DFbiom, keep.rownames = TRUE) # Taxa will become an extra column

# checking the data.table
str(DTbiom)
dim(DTbiom)

# limiting data to only soil (non-saline samples)
subset_DTbiom <- DTbiom[, colnames(DTbiom) %in% zonesDT_soil$X.SampleID, with=FALSE]

# checking if it matches up to the zonesDT_soil sampleIDs
str(subset_DTbiom)
colnames(subset_DTbiom)

# looking at amount of ESV's per sample
# for example the first sample in the data.table
DTbiom[, sum(DTbiom$"1883.2008.269.Crump.Artic.LTREB.main.lane2.NoIndex")]
# 77249 ESV's

# now for every sample in the subset biom data.table
# Apply sum function to every column of subset_DTbiom

# create empty data.frame
df_sum <- data.frame(X.SampleID = character(), ESV_sum = numeric())

# loop over columns and calculate sum
for (i in colnames(subset_DTbiom)){
  col_sum <- sum(subset_DTbiom[[i]])
  new_row <- data.frame(X.SampleID = i, ESV_sum = col_sum)
  df_sum <- rbind(df_sum, new_row)
}

# print result
print(df_sum)
df_sum <- setDT(df_sum)
# note: the sampleID are not sorted the way they are in zonesDT_soil
df_sum_ordered <- df_sum[][order(df_sum$X.SampleID)]

# checking if these numbers make sense
subset_DTbiom[subset_DTbiom$"808.FL.3.16a.s.4.1.sequences"!=0]
subset_DTbiom[subset_DTbiom$"808.FL.3.16a.s.4.1.sequences"!=0, sum(subset_DTbiom$"808.FL.3.16a.s.4.1.sequences")]

# combining the ESV sum with the climatezones
combi <- merge(zonesDT_soil, df_sum_ordered, by="X.SampleID")

# looking at the amount of ESV's per climate zone
combi[,.(sum=sum(ESV_sum)), by=ClimateZ][order(sum)]

# which ESV's are not present in any sample
# taking a look at the samples
subset_DTbiom$"808.FL.3.16a.s.4.1.sequences"
# The dataset is parse and there are a lot of EVs that aren't present
# select rows with sum equal to 0
zero_rows <- subset_DTbiom[rowSums(subset_DTbiom) == 0]

# print result
print(zero_rows)
# according to this, there are 251468 rows where rowSum = 0

# add column indicating whether each row has sum equal to 0
subset_DTbiom[, has_zero_sum := rowSums(subset_DTbiom) == 0]

str(subset_DTbiom)

# adding back the taxa sequence 
subset_DTbiom <- data.table(DTbiom$rn, subset_DTbiom)

# select rows with sum not equal to 0
no_zero_rows <- subset_DTbiom[has_zero_sum==FALSE]

# other option of filtering ESV's
# relative abundances (threshold : >0.001%) + presenting in more than 10% of samples
# calculating relative abundances

# calculate column sums
col_sums_except_first <- colSums(subset_DTbiom[, -1, with = FALSE])

# calculate proportions and scaling factor
prop <- subset_DTbiom[, -1]/ col_sums_except_first
scaling_factor <- 100

# calculate relative abundances
rel_abundances <- prop * scaling_factor

# adding back the taxa sequence
rel_abundances <- data.table(subset_DTbiom$V1, rel_abundances)

# filtering out rows
# 10% samples: >= 13 samples need to have rel_abundance >0.001 
filter_rows <- rel_abundances[rowSums(rel_abundances >= 0.001) >= 13]

# write to file
fwrite(filter_rows, file = "Rel_abundance_BIOM_filtered.tsv", sep = "\t")
