## ---- step_03_data_ingestion.R

# performing data loading -----------------------------------------------------
dataDirectory <- "../data/raw/"

target_data <- read_xlsx(paste(dataDirectory, "base_casamentos.xlsx", sep = ""), 
                         sheet = "CASAMENTOS", 
                         skip = 1, 
                         col_names = TRUE)