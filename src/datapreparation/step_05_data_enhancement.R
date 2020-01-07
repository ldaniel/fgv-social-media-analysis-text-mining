## ---- step_05_data_enhancement.R

# enhancement of data ---------------------------------------------------------

start_time = Sys.time()

lyrics$decade <- paste(str_sub(lyrics$year, 1, 3), '0', sep = '')
lyrics$genre  <- trimws(lyrics$genre)

total_lyrics_loaded <- length(lyrics$index)
end_time = Sys.time()

log  <- paste('[', Sys.time(), '] ',
              'Task: step_05_data_enhancement.R | ', 
              'Total time : ', end_time - start_time, ' | ',
              'Total records: ', total_lyrics_loaded, ' | ',
              'Memory used: ', pryr::mem_used(), ' bytes',
              sep = "") 

write(log, file = "log.txt", append = TRUE)
