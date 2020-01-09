## ---- step_02_data_tokenization.R

# performing data tokenization from Kaggle ----------------------------------------

# applying tokenization in the lirycs ----
start_time = Sys.time()

lyrics <- readRDS('./data/processed/lyrics.rds')

lyrics_token <- unnest_tokens(lyrics,
                              input = lyrics,
                              output = word,
                              token = 'words',
                              drop = TRUE,
                              to_lower = TRUE)

total_words <- paste('Total words after tokenization: ', length(lyrics_token$index), sep = "")
end_time = Sys.time()

# call logging function
WriteLog(TaskName = "step_02_data_tokenization.R", 
         StartTime = start_time,
         EndTime = end_time, 
         AdditionalInfo = total_words)