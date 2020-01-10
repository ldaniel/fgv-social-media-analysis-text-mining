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

# executing data preparation steps --------------------------------------------
source("./src/02_data_analysis/step_01_config_environment.R")
source("./src/02_data_analysis/step_02_data_tokenization.R")
