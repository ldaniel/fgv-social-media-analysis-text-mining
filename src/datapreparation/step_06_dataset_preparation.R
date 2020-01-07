## ---- step_05_dataset_preparation.R

# dataset preparation ---------------------------------------------------------

# The objective of this step is to return a DataFrame to be used in the
# predictive modeling.

# saving the final dataset to evaluate
saveRDS(target_data, '../data/processed/target_dataset.rds')