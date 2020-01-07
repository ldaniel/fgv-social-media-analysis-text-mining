# loading required libraries --------------------------------------------------

# libraries for data prep
library(readr)
library(dplyr)
library(stringr)

# load auxiliary functions ----------------------------------------------------
source("./src/util/auxiliary_functions.R")

# sync kaggle kernel ----------------------------------------------------------
#source("./src/util/sync_kaggle.R")

# executing data preparation steps --------------------------------------------
source("./src/datapreparation/step_01_config_environment.R")
#source("./src/datapreparation/step_02_data_download.R")
source("./src/datapreparation/step_03_data_ingestion.R")
source("./src/datapreparation/step_04_data_cleaning.R")
source("./src/datapreparation/step_05_data_enhancement.R")
source("./src/datapreparation/step_06_dataset_preparation.R")