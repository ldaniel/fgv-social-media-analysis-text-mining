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

print(paste('Número de observações: ', length(lyrics$index)))

end   = Sys.time()
end - start
pryr::mem_used()

log  <- paste(end - start, ' - ', 'import lyrics', ' - memory: ', pryr::mem_used())
write(log, file = "log.txt", append = TRUE)