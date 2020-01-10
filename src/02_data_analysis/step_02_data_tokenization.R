## ---- step_02_data_tokenization.R

# performing data tokenization ------------------------------------------------

# applying tokenization in the lyrics ----
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


# eliminating stop words ----
start_time = Sys.time()

custom_stop_words <- c(tm::stopwords("german"), tm::stopwords("spanish"), 
                       tm::stopwords("portuguese"), tm::stopwords("french"),
                       stop_words$word, 'chorus', 'repeat', 'versus', 'chorus:repeat', 
                       'instrumental')

lyrics_token <- filter(lyrics_token,
                       str_detect(word, '^[a-z]') &
                         !(word %in% custom_stop_words) &
                         nchar(word) >= 3)

bing = get_sentiments('bing')
lyrics_token$sentiment = plyr::mapvalues(lyrics_token$word, 
                                         bing$word, bing$sentiment, 
                                         warn_missing = FALSE)

lyrics_token$sentiment = if_else(!(lyrics_token$sentiment %in% c('positive', 'negative')), 
                                 'neutral', lyrics_token$sentiment)

count_words <- count(lyrics_token, word, sentiment, sort = TRUE)

total_observations_after_stop_words <- paste('Total observations after stop words: ', length(lyrics_token$index), sep = "")

end_time = Sys.time()

# call logging function
WriteLog(TaskName = "step_02_data_tokenization.R", 
         StartTime = start_time,
         EndTime = end_time, 
         AdditionalInfo = total_words)

# saving datasets for later use ----
start_time = Sys.time()

rdsFileName <- paste(directoryPath, "/data/processed/lyrics_token.rds", sep = "")
saveRDS(lyrics_token, rdsFileName)

end_time = Sys.time()

# call logging function
WriteLog(TaskName = "step_02_data_tokenization.R", 
         StartTime = start_time,
         EndTime = end_time, 
         AdditionalInfo = rdsFileName)


start_time = Sys.time()

rdsFileName <- paste(directoryPath, "/data/processed/count_words.rds", sep = "")
saveRDS(count_words, rdsFileName)

end_time = Sys.time()

# call logging function
WriteLog(TaskName = "step_02_data_tokenization.R", 
         StartTime = start_time,
         EndTime = end_time, 
         AdditionalInfo = rdsFileName)

