# loading required libraries --------------------------------------------------

# data preparation
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(readr)
library(stringr)
library(tidytext)
library(forcats)
library(tictoc)

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
source("./src/01_data_preparation/step_01_config_environment.R")
#source("./src/01_data_preparation/step_02_data_download.R")
source("./src/01_data_preparation/step_03_data_ingestion.R")
source("./src/01_data_preparation/step_04_data_cleaning.R")
source("./src/01_data_preparation/step_05_data_enhancement.R")
source("./src/01_data_preparation/step_06_dataset_preparation.R")