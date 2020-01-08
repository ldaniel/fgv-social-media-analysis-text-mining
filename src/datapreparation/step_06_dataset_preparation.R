## ---- step_06_dataset_preparation.R

# dataset preparation ---------------------------------------------------------

# The objective of this step is to return a DataFrame to be used in the
# predictive modeling.

start_time = Sys.time()

# saving the final dataset to evaluate
rdsFileName <- paste(directoryPath, "/data/processed/lyrics.rds", sep = "")
saveRDS(lyrics, rdsFileName)

end_time = Sys.time()

# call logging function
WriteLog(TaskName = "step_06_data_preparation.R", 
         TotalTime = end_time - start_time, 
         AdditionalInfo = rdsFileName)
