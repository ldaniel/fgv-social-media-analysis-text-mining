# config
set.seed(123456)
options(repr.plot.width = 20, repr.plot.height = 12)
options(encoding = 'UTF-8')

list.of.packages <- c("ggwordcloud")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Data wrangling
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(readr)
library(stringr)
library(tidytext)
library(ggplot2)
library(forcats)

# wordcloud
library(ggwordcloud)

# network
library(visNetwork)
library(IRdisplay)
library(igraph, warn.conflicts = FALSE)

# topic modeling
library(topicmodels)

# data ingestion
lyrics <- read_csv('../input/380000-lyrics-from-metrolyrics/lyrics.csv',
                   col_types = cols(
                     index = col_double(),
                     song = col_character(),
                     year = col_double(),
                     artist = col_character(),
                     genre = col_character(),
                     lyrics = col_character()),
                  locale = locale(encoding = 'UTF-8'))

print(paste('Número de observações: ', length(lyrics$index)))

# data filter
lyrics <- filter(lyrics, 
                 !is.na(lyrics), 
                 !(genre %in% c('Not Available', 'Other')),
                 as.integer(year) >= 1970)

# uncomment for fast prototyping
lyrics  <- sample_n(lyrics, size = 100000)
# invisible(gc())

print(paste('Número de observações: ', length(lyrics$index)))

# data enhance
lyrics$decade <- paste(str_sub(lyrics$year, 1, 3), '0', sep = '')
lyrics$genre  <- trimws(lyrics$genre)

saveRDS(lyrics, 'lyrics.rds')

# view sample
sample_n(lyrics, 3)

# view number of observations per genre
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

# view number of observations per decade
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

# view top artists
count(lyrics, genre, artist, sort = TRUE) %>% 
  group_by(genre) %>% 
  arrange(desc(n)) %>% 
  filter(row_number() <= 3) %>% 
  arrange(desc(genre), desc(n))

# tokens per word
lyrics_token <- unnest_tokens(lyrics,
                              input = lyrics,
                              output = word,
                              token = 'words',
                              drop = TRUE,
                              to_lower = TRUE)

print(paste('Número de observações: ', length(lyrics_token$index)))

rm(lyrics)
invisible(gc())
# eliminating stop words
custom_stop_words <- c(tm::stopwords("german"), tm::stopwords("spanish"), 
                       tm::stopwords("portuguese"), tm::stopwords("french"),
                       stop_words$word, 'chorus', 'repeat', 'versus', 'chorus:repeat')

lyrics_token <- filter(lyrics_token,
                       str_detect(word, '^[a-z]') &
                       !(word %in% custom_stop_words))

bing = get_sentiments('bing')
lyrics_token$sentiment = plyr::mapvalues(lyrics_token$word, 
                                         bing$word, bing$sentiment, 
                                         warn_missing = FALSE)

lyrics_token$sentiment = if_else(!(lyrics_token$sentiment %in% c('positive', 'negative')), 
                                    'neutral', lyrics_token$sentiment)

print(paste('Número de observações após a eleminação das stop words: ', 
            length(lyrics_token$index)))

sample_n(lyrics_token, size = 15)

count_words <- count(lyrics_token, word, sentiment, sort = TRUE)

# saving datasets for later use.
saveRDS(lyrics_token, 'lyrics_token.rds')
saveRDS(count_words, 'count_words.rds')

rm(bing, count_words)
invisible(gc)

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

rm(lyrics_token)
invisible(gc)

# view word cloud
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
  scale_size_area(max_size = 35) +
  theme_minimal() +
  theme(text = element_text(size = 20))

suppressWarnings(print(plt))

rm(w)
invisible(gc)
# view word cloud per genre
temp <- group_by(gw, genre, sentiment) %>%  
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
  scale_size_area(max_size = 25) +
  facet_wrap(genre ~ sentiment, nrow = 10) +
  theme(text = element_text(size = 15))

suppressWarnings(print(plt))

# view most common words
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
        axis.ticks.x = element_blank(),
        text = element_text(size = 20))

rm(gw)
invisible(gc)

# tokens bigramas
lyrics <- readRDS('lyrics.rds')

lyrics_token_bi <- unnest_tokens(lyrics,
                                 input = lyrics,
                                 output = term,
                                 token = 'ngrams',
                                 drop = TRUE,
                                 to_lower = TRUE,
                                 n = 2)

print(paste('Número de termos: ', length(lyrics_token_bi$index)))

rm(lyrics)
invisible(gc())
# clean bigrams
lyrics_token_bi <- separate(lyrics_token_bi, term, 
                            sep = ' ', 
                            into = c('w1', 'w2'), 
                            remove = FALSE)
lyrics_token_bi <- filter(lyrics_token_bi, 
                          !(w1 %in% custom_stop_words) & str_detect(w1, '^[a-z]'))
lyrics_token_bi <- filter(lyrics_token_bi, 
                          !(w2 %in% custom_stop_words) & str_detect(w2, '^[a-z]'))
invisible(gc())
lyrics_token_bi <- filter(lyrics_token_bi, w1 != w2)
saveRDS(lyrics_token_bi, 'lyrics_token_bi.rds')

print(paste('Número de termos apos eliminação de stop words: ', 
            length(lyrics_token_bi$index)))

rm(lyrics_token_bi)
invisible(gc)

# custom function to plot network diagram
plot_network <- function(top_words = 25, 
                         artist_filter = NULL, 
                         genre_filter = NULL) {
    # load required data
    count_words  <- readRDS('lyrics_token.rds')
    data         <- readRDS('lyrics_token_bi.rds')
    
    # apply filters

    if (!is.null(artist_filter)) {count_words  <- filter(count_words, 
                                                         artist %in% artist_filter)}
    if (!is.null(genre_filter)) {count_words   <- filter(count_words, 
                                                         genre %in% genre_filter)}
    count_words  <- count(count_words, word, sentiment, sort = TRUE)
    top_words <- filter(count_words, row_number() <= top_words)
    if (!is.null(artist_filter)) {data  <- filter(data, 
                                                  artist %in% artist_filter)}
    if (!is.null(genre_filter)) {data  <- filter(data, 
                                                 genre %in% genre_filter)}
    data <- group_by(data, w1, w2) %>% 
               summarise(count = n()) %>%
               ungroup() %>%
               mutate(percent = count / sum(count)) %>%
               group_by(w1) %>%
               arrange(desc(count)) %>%
               filter(row_number() <= 5) %>% 
               ungroup() %>% 
               filter(w1 %in% top_words$word)

    # set nodes
    nodes = tibble(label = unique(c(data$w1, data$w2)))
    nodes = tibble::rowid_to_column(nodes, "id")
    nodes$value = plyr::mapvalues(nodes$label, count_words$word, 
                                  count_words$n, warn_missing = FALSE)
    nodes$value = as.numeric(nodes$value)
    nodes$value = (nodes$value - min(nodes$value)) / 
                                    (max(nodes$value) - min(nodes$value))
    nodes$value = nodes$value * 100
    nodes$group = plyr::mapvalues(nodes$label, count_words$word, 
                                  count_words$sentiment, warn_missing = FALSE)

    # set edges
    edges  <- tibble(from   = data$w1,
                     to     = data$w2,
                     weight = data$percent)
    edges$from = plyr::mapvalues(edges$from, nodes$label, 
                                 nodes$id, warn_missing = FALSE)
    edges$to = plyr::mapvalues(edges$to, nodes$label, 
                               nodes$id, warn_missing = FALSE)

    net_graph <- visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
                     visNodes(scaling = list(min = 10, max = 50), 
                              physics = TRUE, mass = 1.25) %>% 
                     visEdges(arrows = "to") %>% 
                     visOptions(highlightNearest = TRUE, 
                                nodesIdSelection = TRUE,
                                selectedBy = "group") %>%
                     visGroups(groupname = "positive", color = "green")  %>% 
                     visGroups(groupname = "neutral") %>% 
                     visGroups(groupname = "negative", color = "red") %>% 
                     visLegend(width = 0.1)
    
    rm(count_words, data)
    invisible(gc)
    
    # return graph
    return (net_graph)
}

# bigrams graph for all dataset
net_graph <- plot_network(top_words = 25)

htmlwidgets::saveWidget(net_graph, "net_graph.html")

display_html('<iframe src="net_graph.html" width=100% height=600></iframe>')

# bigrams graph for one artist
net_graph <- plot_network(top_words = 50, artist_filter = 'bob-dylan')

htmlwidgets::saveWidget(net_graph, "net_graph_artist.html")

display_html('<iframe src="net_graph_artist.html" width=100% height=600></iframe>')

# bigrams graph for one genre
net_graph <- plot_network(top_words = 25, genre_filter = 'Metal')

htmlwidgets::saveWidget(net_graph, "net_graph_genre.html")

display_html('<iframe src="net_graph_genre.html" width=100% height=600></iframe>')

# bigrams graph for one genre
net_graph <- plot_network(top_words = 25, genre_filter = 'Rock')

htmlwidgets::saveWidget(net_graph, "net_graph_genre.html")

display_html('<iframe src="net_graph_genre.html" width=100% height=600></iframe>')

# contagem por gênero musical
lyrics_token_bi <- readRDS('lyrics_token_bi.rds')

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

rm(lyrics_token_bi)
invisible(gc)

# wordcloud commom bigrams
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

rm(w)
invisible(gc)

suppressWarnings(print(plt))

# word cloud commom bigrams per genre
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
  facet_wrap(vars(genre), nrow = 5) +
  theme(text = element_text(size = 20))  

suppressWarnings(print(plt))

# most commom bigrams per genre
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
        axis.ticks.x = element_blank(),
        text = element_text(size = 20))

rm(gw)
invisible(gc)

# load lyrics dataset back to memory.
lyrics <- readRDS('lyrics.rds')

# tokennize dataset but do not treat for stopwords.
lyrics_token <- unnest_tokens(lyrics,
                              input = lyrics,
                              output = word,
                              token = 'words',
                              drop = TRUE,
                              to_lower = TRUE)

# calculate the term frequency by gere.
lyrics_token <- lyrics_token[, c('genre','word')] %>% 
    filter(str_detect(word, "^[a-z']")) %>% 
    group_by(genre, word) %>% 
    summarise(n = n()) %>% 
    group_by(genre) %>% 
    mutate(total = sum(n)) %>% 
    mutate(tf = n / total) %>% 
    group_by(genre) %>% 
    arrange(desc(n)) %>%
    mutate(rank_tf = row_number()) %>%
    ungroup()

# display the most commom terms based on its term frequency.
    filter(lyrics_token, rank_tf <= 3) %>% 
    arrange(genre, rank_tf)

rm(lyrics)
invisible(gc)
# histogram of term frequency
options(repr.plot.width = 20, repr.plot.height = 24)

ggplot(lyrics_token, aes(x = n / total, fill = genre)) +
  geom_histogram(show.legend = FALSE, bins = 100) + 
  facet_wrap( ~ genre, ncol = 2, scales = 'free') +
  theme(text = element_text(size = 20))

# term frequency vs rank
options(repr.plot.width = 20, repr.plot.height = 12)

ggplot(lyrics_token, aes(x = rank_tf, y = tf, color = genre)) +
  geom_line(alpha = 0.8, size = 1.1, show.legend = FALSE) +
  scale_x_log10() + scale_y_log10() +
  xlab("Rank - Term frequency") + ylab("Term frequency") +
  theme(text = element_text(size = 20))

# augment the term frequency data frame with inverse document frequency data.
lyrics_token <- bind_tf_idf(tbl = lyrics_token,  
                            term = word, 
                            document = genre, 
                            n = n) %>% 
                arrange(desc(tf_idf)) %>% 
                group_by(genre) %>% 
                mutate(rank_tf_idf  = row_number()) %>% 
                arrange(genre, rank_tf_idf) %>% 
                ungroup()

filter(lyrics_token, rank_tf_idf  <= 3)

# top words by genre using TF-IDF
options(repr.plot.width = 20, repr.plot.height = 24)

    group_by(lyrics_token, genre) %>%
    top_n(10, tf_idf) %>%
    ungroup() %>% 
    mutate(word = reorder(word, desc(tf_idf))) %>% 
    ggplot(aes(x = fct_reorder(word, tf_idf), 
               y = tf_idf, 
               fill = genre)) +
        geom_col(show.legend = FALSE) +
        facet_wrap( ~ genre, ncol = 2, scales = "free") +
        coord_flip() +
        labs(x = NULL, y = "TF-IDF") +
        theme(text = element_text(size = 20), 
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank())

rm(lyrics_token)
invisible(gc)
# create DTM
lyrics_token <- readRDS('lyrics_token.rds')

dtm <- cast_dtm(data = count(lyrics_token, index, word, sort = TRUE),
                index, word, n)
dtm

saveRDS(dtm, 'dtm.rds')
# calculating model with 10 topics
tpm <- LDA(dtm, k = 10, control = list(seed = 123456))

tpm
summary(tpm)

saveRDS(tpm, 'tpm.rds')

rm(dtm)
invisible(gc)
# visualizing top words per topic
options(repr.plot.width = 20, repr.plot.height = 12)

term_topics <- tidy(tpm, matrix = "beta")

term_top_terms <- term_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

term_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", nrow = 2) +
  coord_flip() +
  theme(text = element_text(size = 20), 
        axis.text.x = element_blank(), 
        axis.ticks = element_blank())

rm(term_topics)
invisible(gc)
# visualizing topic classification per genre
options(repr.plot.width = 20, repr.plot.height = 24)

lyrics <- readRDS('lyrics.rds')
songs_topics <- tidy(tpm, matrix = "gamma")

lyrics$index <- as.character(lyrics$index)

inner_join(lyrics, songs_topics, by = c('index' = 'document')) %>% 
    select(index, song, year, artist, genre, decade, topic, gamma) %>% 
        ggplot(aes(x = factor(topic), y = gamma, fill = genre)) +
            geom_boxplot(show.legend = FALSE) +
            facet_wrap(~ genre, nrow = 5) +
            xlab('Topic') + ylab('gamma') +
            labs(title = 'Topic classification per Genre') +
            theme(text = element_text(size = 20), 
                  axis.text.y = element_blank(), 
                  axis.ticks.y = element_blank())

rm(lyrics, songs_topics)
invisible(gc)

