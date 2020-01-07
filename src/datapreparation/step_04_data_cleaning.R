## ---- step_04_data_cleaning.R

# analysing missing values and other strange conditions -----------------------

start_time = Sys.time()

lyrics <- filter(lyrics, 
                 !is.na(lyrics), 
                 !(genre %in% c('Not Available', 'Other')),
                 as.integer(year) >= 1970)

# uncomment for fast prototyping
# lyrics  <- sample_n(lyrics, size = 150000)
# invisible(gc())

total_lyrics_loaded <- paste('Total records: ', length(lyrics$index), sep = "")
end_time = Sys.time()

# call logging function
WriteLog(TaskName = "step_04_data_clearing.R", 
         TotalTime = end_time - start_time, 
         AdditionalInfo = total_lyrics_loaded)
