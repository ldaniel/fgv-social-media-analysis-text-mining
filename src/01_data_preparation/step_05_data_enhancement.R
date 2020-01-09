## ---- step_05_data_enhancement.R

# enhancement of data ---------------------------------------------------------

start_time = Sys.time()

lyrics$decade <- paste(str_sub(lyrics$year, 1, 3), '0', sep = '')
lyrics$genre  <- trimws(lyrics$genre)

total_lyrics_loaded <- paste('Total records: ', length(lyrics$index), sep = "")
end_time = Sys.time()

# call logging function
WriteLog(TaskName = "step_05_data_enhancement.R", 
         StartTime = start_time,
         EndTime = end_time, 
         AdditionalInfo = total_lyrics_loaded)
