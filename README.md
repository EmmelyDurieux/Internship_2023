# Autoencoder-based identification of key microorganisms within climate zones

This repository contains the code developed during my internship in 2023 at INAB, Thessaloniki. The main objective of the internship was to investigate and identify key microorganisms in Köppen-Geiger defined climate zones. The analysis relied on publicly available data from the Earth Microbiome Project, specifically utilizing the BIOM tables and sample metadata files.

The overarching approach of the project involved the implementation of an autoencoder to reduce the dimensionality of the input features, generating a compressed representation known as the latent space. This latent space was then utilized as input for a random forest classifier, allowing the identification of significant latent variables associated with each climate zone. By examining the weights within the autoencoder, it was possible to pinpoint the most informative features contributing to the composition of the latent variable, thereby establishing a connection between these features and the respective climate zone.

Every script in this workflow is written in R (version 4.3.0) and run in a Windows based system (Windows 10 Pro 22H2, 64-bits).
The scripts are numbered in order of execution, with "01" being the first. 

### Necessary packages, used version and installation guide/documentation
The following R packages are used in the workflow:
- kgc (1.0.0.2)
- data.table (1.14.8)
- caret (6.0.94)
- randomForest (4.7.1.1)
- rbiom (1.0.3.9087)
- tensorflow and keras (2.11.0)  

I recommend installing rbiom, tensorflow and keras before running the scripts. 
- rbiom: https://cmmr.github.io/rbiom/index.html 
- tensorflow and keras: https://tensorflow.rstudio.com/install/index.html 
note: I had some troubles installing tensorflow and keras, I put my code that fixed my problems in the unnumbered script "installation-Tensorflow-Keras.R"

## 01_data_preprocessing.R
This script:
- processes the BIOM files: obtains abundance counts for each taxa 
- processes the sample metadata files: assigns climate zone to each sample
- selects only samples that belong to "Soil (non-saline)" empo_3 ontology
- filters data on sample/climate representation and taxa prevalence :
-       data filter threshold:
            - relative abundance of taxa > 0.01%
            - taxa prevalence > 10% samples
            - samples/climate zone > 3


Packages:
- rbiom : to process biom files
- kgc : to assign Köppen-Geiger climate zone to each sample


### output
- relative abudance table
- absolute abundace table
- taxonomy table
- sample meta data

## 02_splitting_train_test.R
This script:
splits the retained relative abundance counts into a train and test set and stores it in a .txt file

### output
- train.txt
- test.txt

## 03_autoencoder_model.R
This script:
- trains a 1 hidden layer autoencoder with the training data
- stores the latent space in a .txt file
- retraces top 5 features for each latent variable based on highest absolute weight, which is also stored in a .txt file

Packages: 
- tensorflow
- keras

note: in this script you will see that I load tensorflow before and after I activate and use my virtualenv. This is on purpose, if I don't do it this way my virtualenv and/or tensorflow simply don't want to work. This is not a universal problem, so when running this script on another system, it's possible that this won't be necessary. If there are other problems working with virtualenv it can help to restart R and clean the environment.

### output
- latent-space.txt : latent space variable representation (32) for each sample
- taxa-latent-variable.txt : each latent variable with their top 5 most weighted features

## 04_random_forest_classification2.R
This script:
- trains a random forest classification model for each climate zone separatly on a subset of the latent space data.
- extracts key latent variables for each climate zone by selecting variables with highest mean decrease in gini impurity
- combines top 5 weighted features and key latent variable for each climate zone

Packages:
- randomForest
- caret

### output
- taxa-climatezone.txt
