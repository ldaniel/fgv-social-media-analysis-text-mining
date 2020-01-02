# Data wrangling
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(readr)
library(stringr)
library(tidytext)
library(ggplot2)
library(forcats)

# wordclod
library(wordcloud2)

# network
library(visNetwork)
library(igraph, warn.conflicts = FALSE)


# read data ----

lyrics <- read_csv('data/raw/lyrics.csv',
                   col_types = cols(
                     index = col_double(),
                     song = col_character(),
                     year = col_double(),
                     artist = col_character(),
                     genre = col_character(),
                     lyrics = col_character()),
                   locale = locale(encoding = 'UTF-8'))

print(paste('Número de observações: ', length(lyrics$index)))

# efetuar a limpeza dos dados
lyrics <- filter(lyrics, 
                 !is.na(lyrics), 
                 !(genre %in% c('Not Available', 'Other')),
                 as.integer(year) >= 1970)

print(paste('Número de observações: ', length(lyrics$index)))

# Vamos incluir a identificação da década de lançamento da música para podermos avaliar alterações ao longo do tempo
lyrics$decade <- paste(str_sub(lyrics$year, 1, 3), '0', sep = '')
lyrics$genre  <- trimws(lyrics$genre)

# Uma amostra do dataset.
sample_n(lyrics, 5)

# EDA ----

# per genre ----

group_by(lyrics, genre) %>%
  summarise(songs = n(),
            artists = length(unique(artist))) %>% 
  arrange(desc(songs))

# per decade ----

group_by(lyrics, decade) %>% 
  summarise(songs = n(),
            artists = length(unique(artist))) %>% 
  arrange(desc(decade))

# top artists ----

count(lyrics, genre, artist, sort = TRUE) %>% 
  group_by(genre) %>% 
  arrange(desc(n)) %>% 
  filter(row_number() <= 3) %>% 
  arrange(desc(genre), desc(n))

# tokens ----

custom_stop_words <- c(tm::stopwords("german"), 
                       tm::stopwords("spanish"), 
                       tm::stopwords("portuguese"), 
                       tm::stopwords("french"))
# 
# lyrics_token <- unnest_tokens(lyrics,
#                               input = lyrics,
#                               output = word,
#                               token = 'words',
#                               drop = TRUE,
#                               to_lower = TRUE) %>%
#   anti_join(stop_words, by = 'word') %>% 
#   filter(str_detect(word, '^[a-z]') &
#            !(word %in% custom_stop_words))
# 
# saveRDS(lyrics_token, 'data/processed/lyrics_token.rds')

lyrics_token <- readRDS('data/processed/lyrics_token.rds')

gaw <- group_by(lyrics_token, genre, artist, word) %>%
  summarise(gaw_c = n()) %>% 
  ungroup() %>%
  group_by(genre, artist) %>%
  mutate(gaw_p = gaw_c / sum(gaw_c)) %>% 
  ungroup() %>%
  arrange(artist, desc(gaw_p))

gw <- group_by(lyrics_token, genre, word) %>%
  summarise(gw_c = n()) %>% 
  ungroup() %>% 
  group_by(genre) %>%
  mutate(gw_p = gw_c / sum(gw_c)) %>% 
  ungroup() %>%
  arrange(genre, desc(gw_p)) %>% 
  group_by(word) %>% 
  mutate(w_c = sum(gw_c))

w <- group_by(lyrics_token, word) %>%
  summarise(w_c = n()) %>% 
  ungroup() %>%
  mutate(w_p = w_c / sum(w_c)) %>%
  arrange(desc(w_c))

arrange(w, desc(w_c)) %>% 
  filter(row_number() < 201) %>% 
  wordcloud2(size = 1, 
           color='random-dark',
           backgroundColor = 'white')

gw %>%
  group_by(genre) %>% 
  arrange(desc(gw_p)) %>% 
  filter(row_number() <= 10) %>%
  mutate(rank = row_number()) %>% 
  ggplot() +
  geom_bar(stat = 'identity',
           aes(y = gw_p,
               x = fct_reorder(word, w_c),
               fill = genre)) +
  geom_text(aes(label = as.character(rank),
                 x = fct_reorder(word, w_c),
                 y = 0.002)) +
  facet_wrap(vars(genre), nrow = 1) +
  coord_flip() +
  xlab('Word') +
  ylab('Relative frequency') +
  labs(title = 'Top 10 words by music genre') +
  theme(legend.position = 0,
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


# DTM ----

dtm <- group_by(lyrics_token, index, word) %>% 
  summarise(count = n())

dtm <- cast_dtm(data = dtm, document = index, term = word, count)

# Network --

