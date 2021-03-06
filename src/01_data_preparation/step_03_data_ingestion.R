## ---- step_03_data_ingestion.R

# performing data loading -----------------------------------------------------

# remarks:
#   read_csv is from the readr package;
#   read_csv is faster than read.csv for big files. 

# loading lyrics ----
start_time = Sys.time()

dataDirectory <- paste(directoryPath, "/data/raw/", sep = "")
lyrics <- read_csv(paste(dataDirectory, "lyrics.csv", sep = ""),
                   col_types = cols(
                     index = col_integer(),
                     song = col_character(),
                     year = col_integer(),
                     artist = col_factor(),
                     genre = col_factor(),
                     lyrics = col_character()),
                   locale = locale(encoding = 'UTF-8'))

total_lyrics_loaded <- paste('Total records: ', length(lyrics$index), sep = "")
end_time = Sys.time()

# call logging function
WriteLog(TaskName = "step_03_data_ingestion.R", 
         StartTime = start_time,
         EndTime = end_time, 
         AdditionalInfo = total_lyrics_loaded)
