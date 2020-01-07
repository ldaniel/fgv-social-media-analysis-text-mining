## ---- step_06_dataset_preparation.R

# dataset preparation ---------------------------------------------------------

# The objective of this step is to return a DataFrame to be used in the
# predictive modeling.

start_time = Sys.time()

# saving the final dataset to evaluate
saveRDS(lyrics, '../data/processed/lyrics.rds')

end_time = Sys.time()

log  <- paste('[', Sys.time(), '] ',
              'Task: step_06_data_preparation.R | ', 
              'Total time : ', end_time - start_time, ' | ',
              'Memory used: ', pryr::mem_used(), ' bytes',
              sep = "")

write(log, file = "log.txt", append = TRUE)
