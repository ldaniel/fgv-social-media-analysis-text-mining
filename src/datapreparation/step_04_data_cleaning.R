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

total_lyrics_loaded <- length(lyrics$index)
end_time = Sys.time()

log  <- paste('[', Sys.time(), '] ',
              'Task: step_04_data_clearing.R | ', 
              'Total time : ', end_time - start_time, ' | ',
              'Total records: ', total_lyrics_loaded, ' | ',
              'Memory used: ', pryr::mem_used(), ' bytes',
              sep = "")

write(log, file = "log.txt", append = TRUE)