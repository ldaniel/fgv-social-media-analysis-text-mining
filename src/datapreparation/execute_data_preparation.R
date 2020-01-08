# loading required libraries --------------------------------------------------

# data preparation
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(readr)
library(stringr)
library(tidytext)
library(forcats)

# wordcloud
library(ggwordcloud)

# network
library(visNetwork)
library(IRdisplay)
library(igraph, warn.conflicts = FALSE)

# topic modeling
library(topicmodels)

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