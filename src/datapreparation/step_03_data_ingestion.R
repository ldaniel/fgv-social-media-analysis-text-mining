## ---- step_03_data_ingestion.R

# performing data loading -----------------------------------------------------
start = Sys.time()

dataDirectory <- "./data/raw/"
lyrics <- read_csv(paste(dataDirectory, "lyrics.csv", sep = ""),
                   col_types = cols(
                     index = col_integer(),
                     song = col_character(),
                     year = col_integer(),
                     artist = col_factor(),
                     genre = col_factor(),
                     lyrics = col_character()),
                   locale = locale(encoding = 'UTF-8'))

total_lyrics_loaded <- length(lyrics$index)
end = Sys.time()

log  <- paste('[', Sys.time(), '] ',
              'Task: step_03_data_ingestion.R | ', 
              'Total time : ', end - start, ' | ',
              'Total records: ', total_lyrics_loaded, ' | ',
              'Memory used: ', pryr::mem_used(), ' bytes',
              sep = "")

write(log, file = "log.txt", append = TRUE)
