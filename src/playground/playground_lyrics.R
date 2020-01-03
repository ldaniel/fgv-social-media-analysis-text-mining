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
library(ggwordcloud)

# network
library(visNetwork)
library(igraph, warn.conflicts = FALSE)

# initial configuration
set.seed(42)

# read data and apply some filter ----

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

# exploratory data analysis ----

# per genre ----

# Quantidade de músicas e artistas por gênero musical.

temp <- group_by(lyrics, genre) %>%
  summarise(songs = n(),
            artists = length(unique(artist))) %>% 
  arrange(desc(songs))

ggplot(data = temp, aes(x = fct_reorder(genre, songs), y = songs)) +
  geom_bar(stat = 'identity', aes(fill = genre)) +
  geom_label(aes(label = paste('songs:', songs, '\nartists: ', artists, sep = ''),
                 y = 1000, fill = genre), size = 8, hjust = 'left', alpha = 0.25) +
  coord_flip() +
  xlab('Genre') +
  ylab('') +
  labs(title = 'Songs and Artists by Genre') +
  theme(legend.position = 0,
        text = element_text(size = 20),
        axis.text.x = element_blank())

# per decade ----

# Quantidade de músicas e artistas por década.

temp  <- group_by(lyrics, decade) %>% 
  summarise(songs = n(),
            artists = length(unique(artist))) %>% 
  arrange(desc(decade))

ggplot(data = temp, aes(x = decade, y = songs)) +
  geom_bar(stat = 'identity', aes(fill = decade)) +
  geom_label(aes(label = paste('songs:', songs, '\nartists: ', artists, sep = ''),
                 y = 1000, fill = decade), size = 8, hjust = 'left', alpha = 0.25) +
  coord_flip() +
  xlab('Decade') +
  ylab('') +
  labs(title = 'Songs and Artists by Decade') +
  theme(legend.position = 0,
        text = element_text(size = 20),
        axis.text.x = element_blank())

# top artists ----

# Top 3 artistas, em relação a quantidade de músicas, por gênero.

count(lyrics, genre, artist, sort = TRUE) %>% 
  group_by(genre) %>% 
  arrange(desc(n)) %>% 
  filter(row_number() <= 3) %>% 
  arrange(desc(genre), desc(n))

# tokens ----

# Análise de palavras mais comuns.
# Vamos utilizar o pacote tidytext para tokenizar os termos em um dataframe contendo uma palavra por linha.
# Também vamos excluir as stop words em inglês inicialmente.

lyrics_token <- unnest_tokens(lyrics,
                              input = lyrics,
                              output = word,
                              token = 'words',
                              drop = TRUE,
                              to_lower = TRUE)

print(paste('Número de termos: ', length(lyrics_token$index)))


# Algumas músicas, ou mesmo parte da letra, estão em outros idiomas além do inglês.
# Para resolver isso vamos aplicar mais dois filtros, onde eliminamos termos que não iniciam 
# com letras {a- z} e stop words em outros idiomas de origem latina.

custom_stop_words <- c(tm::stopwords("german"), 
                       tm::stopwords("spanish"), 
                       tm::stopwords("portuguese"), 
                       tm::stopwords("french"),
                       stop_words$word, 'chorus')

lyrics_token <- filter(lyrics_token,
                       str_detect(word, '^[a-z]') &
                         !(word %in% custom_stop_words))

lyrics_token <- inner_join(lyrics_token, get_sentiments('bing'), by = 'word')

print(paste('Número de termos após a eleminação das stop words: ', length(lyrics_token$index)))

head(lyrics_token, n = 5)


# De posse do dataset organizado, vamos iniciar com a contagem relativa dos termos por artista e gênero musical.

# contagem por gênero musical
gw <- group_by(lyrics_token, genre, sentiment, word) %>%
  summarise(gw_c = n()) %>% 
  ungroup() %>% 
  group_by(genre) %>%
  mutate(gw_p = gw_c / sum(gw_c)) %>% 
  ungroup() %>%
  arrange(genre, desc(gw_p)) %>% 
  group_by(word) %>% 
  mutate(w_c = sum(gw_c))

# contagem por palavra
w <- group_by(lyrics_token, sentiment, word) %>%
  summarise(w_c = n()) %>% 
  ungroup() %>%
  mutate(w_p = w_c / sum(w_c)) %>%
  arrange(desc(w_c))

# Vamos iniciar a exploração observando as 200 palavras mais comuns encontradas no dataset.
temp <- group_by(w, sentiment) %>% 
  arrange(desc(w_c)) %>% 
  filter(row_number() < 101) %>% 
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(70, 30)),
         freq_sd = (w_c - min(w_c)) / (max(w_c) - min(w_c))) %>% 
  ungroup()

options(repr.plot.width = 20, repr.plot.height = 6)

plt  <- ggplot(data = temp,
               aes(label = word, 
                   size = w_c,
                   color = factor(sentiment), 
                   angle = angle)) +
  geom_text_wordcloud_area(eccentricity = 0.65) +
  facet_wrap(vars(sentiment), nrow = 1) +
  scale_size_area(max_size = 50) +
  theme_minimal() +
  theme(text = element_text(size = 20))

suppressWarnings(print(plt))

temp <- group_by(gw, genre, ) %>%  
  arrange(desc(gw_p)) %>% 
  filter(row_number() < 51) %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(70, 30)), 
         freq_sd = (gw_c - min(gw_c)) / (max(gw_c) - min(gw_c))) %>% 
  ungroup()

options(repr.plot.width = 20, repr.plot.height = 48)

plt <- ggplot(data = temp,
              aes(label = word, 
                  size = freq_sd,
                  color = factor(sample.int(20, nrow(temp), replace = TRUE)), 
                  angle = angle)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 35) +
  facet_wrap(genre ~ sentiment, nrow = 10)

suppressWarnings(print(plt))

# Na sequência vamos verificar se as 10 palavras mais comuns são uniformes entre os gêneros musicais presentes no dataset.

options(repr.plot.width = 20, repr.plot.height = 12)

gw %>%
  group_by(genre) %>% 
  arrange(desc(gw_p)) %>% 
  filter(row_number() <= 10) %>%
  mutate(rank = row_number()) %>% 
  ggplot() +
  geom_bar(stat = 'identity',
           aes(y = gw_p, x = fct_reorder(word, w_c), fill = genre)) +
  geom_text(aes(label = as.character(rank), x = fct_reorder(word, w_c), y = 0.002)) +
  facet_wrap(vars(genre), nrow = 1) +
  coord_flip() +
  xlab('Word') +
  ylab('Relative frequency') +
  labs(title = 'Top 10 words by music genre') +
  theme(legend.position = 0,
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Como podemos observar pelo gráfico acima em geral as top 10 palavras são as mesmas entre os gêneros, porém, 
# os gêneros Metal e Hip-Hop se destacam com diferentes temas em relação aos demais gêneros.
# Também observamos que Country, Folk e Indie abordam com maior frequência o tema Home. 

# análise de ngramas ----

# Além da análise das palavras mais comuns podemos explorar quais são os Bigramas e Trigramas mais comuns utilizados.

# Bigramas
lyrics_token_bi <- unnest_tokens(lyrics,
                                 input = lyrics,
                                 output = term,
                                 token = 'ngrams',
                                 drop = TRUE,
                                 to_lower = TRUE,
                                 n = 2)

print(paste('Número de termos: ', length(lyrics_token_bi$index)))


# Agora separamos o bigrama em duas palavras para eliminarmos os bigramas que contem 
# stop words assim como fizemos com o a análise de palavras individuais.
# Iremos eliminar as linhas onde ao menos uma das palavras do bigrama são stop words.

lyrics_token_bi <- separate(lyrics_token_bi, term, sep = ' ', into = c('w1', 'w2'), remove = FALSE)
lyrics_token_bi <- filter(lyrics_token_bi, !(w1 %in% custom_stop_words) & str_detect(w1, '^[a-z]'))
lyrics_token_bi <- filter(lyrics_token_bi, !(w2 %in% custom_stop_words) & str_detect(w2, '^[a-z]'))
lyrics_token_bi <- filter(lyrics_token_bi, w1 != w2)

print(paste('Número de termos apos eliminação de stop words: ', length(lyrics_token_bi$index)))

# A exemplo do que fizemos com a contagem individual de palavras vamos verificar 
# quais os bigramas mais utilizados por gênero musical.

# contagem por gênero musical
gw <- group_by(lyrics_token_bi, genre, term) %>%
  summarise(gw_c = n()) %>% 
  ungroup() %>% 
  group_by(genre) %>%
  mutate(gw_p = gw_c / sum(gw_c)) %>% 
  ungroup() %>%
  arrange(genre, desc(gw_p)) %>% 
  group_by(term) %>% 
  mutate(w_c = sum(gw_c))

# contagem por palavra
w <- group_by(lyrics_token_bi, term) %>%
  summarise(w_c = n()) %>% 
  ungroup() %>%
  mutate(w_p = w_c / sum(w_c)) %>%
  arrange(desc(w_c))

# Vamos verificar a nuvem de termos de bigramas.
temp <- arrange(w, desc(w_c)) %>% 
  filter(row_number() < 101) %>% 
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(70, 30)))

plt <- ggplot(data = temp,
              aes(label = term, 
                  size = w_c,
                  color = factor(sample.int(20, nrow(temp), replace = TRUE)), 
                  angle = angle)) +
  geom_text_wordcloud_area(eccentricity = 1.1) +
  scale_size_area(max_size = 25) +
  theme_minimal()

suppressWarnings(print(plt))

temp <- group_by(gw, genre) %>%  
  arrange(desc(gw_p)) %>% 
  filter(row_number() < 26) %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(100, 0)), 
         freq_sd = (gw_c - min(gw_c)) / (max(gw_c) - min(gw_c))) %>% 
  ungroup()

options(repr.plot.width = 20, repr.plot.height = 24)

plt <- ggplot(data = temp,
              aes(label = term, 
                  size = freq_sd,
                  color = factor(sample.int(20, nrow(temp), replace = TRUE)), 
                  angle = angle)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  facet_wrap(vars(genre), nrow = 5)

suppressWarnings(print(plt))

# Agora verificamos os top 5 bigramas por gênero musical.
options(repr.plot.width = 20, repr.plot.height = 12)

gw %>%
  group_by(genre) %>% 
  arrange(desc(gw_p)) %>% 
  filter(row_number() <= 5) %>%
  mutate(rank = row_number()) %>% 
  ggplot() +
  geom_bar(stat = 'identity',
           aes(y = gw_p, x = fct_reorder(term, w_c), fill = genre)) +
  geom_text(aes(label = as.character(rank), x = fct_reorder(term, w_c), y = 0.0002)) +
  facet_wrap(vars(genre), nrow = 1) +
  coord_flip() +
  xlab('Term') +
  ylab('Relative frequency') +
  labs(title = 'Top 5 bigrams by music genre') +
  theme(legend.position = 0,
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# análise de sentimentos ----
# Utilizando a LEXICON Bing vamos trentar clasificar as músicas entre sentimentos positivos e negativos.