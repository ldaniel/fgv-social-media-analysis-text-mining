## ---- config----

start  <- Sys.time()

set.seed(123456)
options(repr.plot.width = 20, repr.plot.height = 12)
options(encoding = 'UTF-8')

list.of.packages <- c("ggwordcloud")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

end  <- Sys.time()
mem  <- pryr::mem_used()

WriteLog <- function(TaskName, StartTime, EndTime, AdditionalInfo) {

  TotalTime <- difftime(EndTime, StartTime, tz, 
                        units = c("auto", "secs", "mins", "hours",
                                  "days", "weeks"))
  
  log  <- paste('[', Sys.time(), '] ',
                'Task: ', TaskName, ' | ', 
                'Time elapsed: ', format(round(TotalTime, 3), format = '%H:%M:%S'), ' | ',
                'Memory used: ', round(pryr::mem_used() / 1000 / 1000, 0), ' MB | ',
                'Top Memory used: ', round(AdditionalInfo / 1000 / 1000, 0), ' MB | ',
                sep = "")

  write(log, file = "log.txt", append = TRUE)
  #print(log)
}

WriteLog('config', start, end, mem)

## ---- end-of-config----

## ---- load_libraries----

start = Sys.time()

#data_wrangling

library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(readr)
library(stringr)
library(tidytext)
library(ggplot2)
library(forcats)

#wordcloud

library(ggwordcloud)

#network_visuallization

library(visNetwork)
library(IRdisplay)
library(igraph, warn.conflicts = FALSE)

#topic_modeling

library(topicmodels)

end = Sys.time()
mem = pryr::mem_used()

WriteLog('load library', start, end, mem)

## ---- end-of-load_libraries----

## ---- data_ingestion----

start = Sys.time()

lyrics <- read_csv('../input/380000-lyrics-from-metrolyrics/lyrics.csv',
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
mem   = pryr::mem_used()

WriteLog('import lyrics', start, end, mem)

## ---- end-of-data_ingestion----

## ---- data_filter----

start = Sys.time()

lyrics <- filter(lyrics, 
                 !is.na(lyrics), 
                 !(genre %in% c('Not Available', 'Other')),
                 as.integer(year) >= 1970)

# uncomment for fast prototyping
# lyrics  <- sample_n(lyrics, size = 1000)
# invisible(gc())

print(paste('Número de observações: ', length(lyrics$index)))

end = Sys.time()
mem = pryr::mem_used()

WriteLog('filter missing values', start, end, mem)

## ---- en-of-data_filter----

## ---- data_enhance----

start  <- Sys.time()

lyrics$decade <- paste(str_sub(lyrics$year, 1, 3), '0', sep = '')
lyrics$genre  <- trimws(lyrics$genre)

saveRDS(lyrics, 'lyrics.rds')

end  <- Sys.time()
mem = pryr::mem_used()

WriteLog('data enhance', start, end, mem)

## ---- end-of-data_enhance----

## ---- view_sample----

start = Sys.time()

sample_n(lyrics, 25) %>% 
    mutate(lyrics_preview = str_sub(lyrics, 1, 140)) %>% 
    select(-lyrics)

end = Sys.time()
mem = pryr::mem_used()

WriteLog('view sample', start, end, mem)

## ---- end-of-view_sample----

## ---- number_of_observations_per_genre----

start = Sys.time()

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

end = Sys.time()
mem = pryr::mem_used()

WriteLog('plot obs per genre', start, end, mem)

## ---- end-of-number_of_observations_per_genre----

## ---- number_of_observations_per_decade----

start = Sys.time()

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

end = Sys.time()
mem = pryr::mem_used()

WriteLog('plot obs per decade', start, end, mem)

## ---- end-of-number_of_observations_per_decade----

## ---- top_artists----

start = Sys.time()

count(lyrics, genre, artist, sort = TRUE) %>% 
  group_by(genre) %>% 
  arrange(desc(n)) %>% 
  filter(row_number() <= 3) %>% 
  arrange(desc(genre), desc(n))

end = Sys.time()
mem = pryr::mem_used()

WriteLog('plot top artists', start, end, mem)

## ---- end-of-top_artists----

## ---- get_tokens----

start  <- Sys.time()

lyrics_token <- unnest_tokens(lyrics,
                              input = lyrics,
                              output = word,
                              token = 'words',
                              drop = TRUE,
                              to_lower = TRUE)

print(paste('Número de observações: ', length(lyrics_token$index)))

end  <- Sys.time()
mem = pryr::mem_used()

WriteLog('get tokens', start, end, mem)

## ---- end-of-get_tokens----

rm(lyrics)
invisible(gc())

## ---- eliminating_stopwords----

start  <- Sys.time()

custom_stop_words <- c(tm::stopwords("german"), tm::stopwords("spanish"), 
                       tm::stopwords("portuguese"), tm::stopwords("french"),
                       stop_words$word, 'chorus', 'repeat', 'versus', 'chorus:repeat', 
                       'instrumental')

lyrics_token <- filter(lyrics_token,
                       str_detect(word, '^[a-z]') &
                       !(word %in% custom_stop_words) &
                       nchar(word) >= 3)

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

mem  <- pryr::mem_used()
rm(bing, count_words)
invisible(gc)

end = Sys.time()

WriteLog('save token lyrics', start, end, mem)

## ---- end-of-eliminating_stopwords----

## ---- count_tokens----

start  <- Sys.time()

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

mem  <- pryr::mem_used()
rm(lyrics_token)
invisible(gc)

end = Sys.time()

WriteLog('count tokens', start, end, mem)

## ---- end-of-count_tokens----

## ---- view_wordcloud_token----

start  <- Sys.time()

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

mem  <- pryr::mem_used()
rm(w)
invisible(gc)

end = Sys.time()

WriteLog('token lyrics wordcloud', start, end, mem)

## ---- end-of-view_wordcloud_token----

## ---- wordcloud_per_genre_token----

start <- Sys.time()

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

end = Sys.time()
mem = pryr::mem_used()

WriteLog('token lyrics wordcloud per genre', start, end, mem)

## ---- end-of-wordcloud_per_genre_token----

## ---- top_10_words_token----

start = Sys.time()

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

end = Sys.time()
mem = pryr::mem_used()

WriteLog('plot top words', start, end, mem)

## ---- end-of-top_10_words_token----

## ---- get_tokens_bigrams----

start  <- Sys.time()

lyrics <- readRDS('lyrics.rds')

lyrics_token_bi <- unnest_tokens(lyrics,
                                 input = lyrics,
                                 output = term,
                                 token = 'ngrams',
                                 drop = TRUE,
                                 to_lower = TRUE,
                                 n = 2)

print(paste('Número de termos: ', nrow(lyrics_token_bi)))

end = Sys.time()
mem = pryr::mem_used()

WriteLog('get tokens bigrams', start, end, mem)

## ---- end-of-get_tokens_bigrams----

rm(lyrics, plt, temp)
invisible(gc())

## ---- clean_bigrams----

start  <- Sys.time()

lyrics_token_bi <- separate(lyrics_token_bi, term, 
                            sep = ' ',
                            into = c('w1', 'w2'), 
                            remove = FALSE)

lyrics_token_bi <- filter(lyrics_token_bi, w1 != w2)

lyrics_token_bi <- filter(lyrics_token_bi, nchar(w1) >= 3, nchar(w2) >= 3)

invisible(gc())

lyrics_token_bi <- filter(lyrics_token_bi, 
                          !(w1 %in% custom_stop_words) & str_detect(w1, '^[a-z]'))

lyrics_token_bi <- filter(lyrics_token_bi, 
                          !(w2 %in% custom_stop_words) & str_detect(w2, '^[a-z]'))

saveRDS(lyrics_token_bi, 'lyrics_token_bi.rds')

print(paste('Número de termos apos eliminação de stop words: ', 
            nrow(lyrics_token_bi)))

mem  <- pryr::mem_used()

rm(lyrics_token_bi)
invisible(gc)

end = Sys.time()

WriteLog('save tokens lyrics bigrams', start, end, mem)

## ---- end-of-clean_bigrams----

## ---- plot_network----

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

## ---- end-of-plot_network----

## ---- plot_network_all----

start  <- Sys.time()

net_graph <- plot_network(top_words = 25)

htmlwidgets::saveWidget(net_graph, "net_graph.html")

display_html('<iframe src="net_graph.html" width=100% height=600></iframe>')

end = Sys.time()
mem = pryr::mem_used()

WriteLog('plot net 1', start, end, mem)

## ---- end-of-plot_network_all----

## ---- plot_network_artist----

start  <- Sys.time()

net_graph <- plot_network(top_words = 50, artist_filter = 'bob-dylan')

htmlwidgets::saveWidget(net_graph, "net_graph_artist.html")

display_html('<iframe src="net_graph_artist.html" width=100% height=600></iframe>')

end = Sys.time()
mem = pryr::mem_used()

WriteLog('plot net 2', start, end, mem)

## ---- end-of-plot_network_artist----

## ---- plot_network_genre_1----

start  <- Sys.time()

net_graph <- plot_network(top_words = 25, genre_filter = 'Metal')

htmlwidgets::saveWidget(net_graph, "net_graph_genre_1.html")

display_html('<iframe src="net_graph_genre_1.html" width=100% height=600></iframe>')

end = Sys.time()
mem = pryr::mem_used()

WriteLog('plot net 3', start, end, mem)

## ---- end-of-plot_network_genre_1----

## ---- plot_network_genre_2----

start  <- Sys.time()

net_graph <- plot_network(top_words = 25, genre_filter = 'Rock')

htmlwidgets::saveWidget(net_graph, "net_graph_genre_2.html")

display_html('<iframe src="net_graph_genre_2.html" width=100% height=600></iframe>')

end = Sys.time()
mem = pryr::mem_used()

WriteLog('plot net 4', start, end, mem)

## ---- end-of-plot_network_genre_2----

## ---- top_bigrams_token----

start  <- Sys.time()

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

mem  <- pryr::mem_used()

rm(lyrics_token_bi)
invisible(gc)

end = Sys.time()
mem = pryr::mem_used()

WriteLog('calculate tokens lyrics bigram count', start, end, mem)

## ---- end-of-top_bigrams_token----

## ---- view_wordcloud_token_bigrams----

start  <- Sys.time()

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

mem  <- pryr::mem_used()

rm(w)
invisible(gc)

suppressWarnings(print(plt))

end = Sys.time()
mem = pryr::mem_used()

WriteLog('plot tokens lyrics bigrams', start, end, mem)

## ---- end-of-view_wordcloud_token_bigrams----

## ---- view_wordcloud_token_bigrams_per_genre----

start  <- Sys.time()

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

end = Sys.time()
mem = pryr::mem_used()

WriteLog('plot tokens lyrics bigrams per genre', start, end, mem)

## ---- end-of-view_wordcloud_token_bigrams_per_genre----

## ---- top_5_words_token_bigrams----

start  <- Sys.time()

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

mem  <- pryr::mem_used()

rm(gw)
invisible(gc)

end = Sys.time()

WriteLog('plot top words bigrams', start, end, mem)

## ---- end-of-top_5_words_token_bigrams----

## ---- calculate_term_frequency----

start  <- Sys.time()

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

mem  <- pryr::mem_used()

rm(lyrics)
invisible(gc)

end = Sys.time()
mem = pryr::mem_used()

WriteLog('calculate tf', start, end, mem)

## ---- end-of-calculate_term_frequency----

## ---- histogram_term_frequency----

start  <- Sys.time()

options(repr.plot.width = 20, repr.plot.height = 24)

ggplot(lyrics_token, aes(x = n / total, fill = genre)) +
  geom_histogram(show.legend = FALSE, bins = 60) + 
  facet_wrap( ~ genre, ncol = 2, scales = 'free') +
  theme(text = element_text(size = 20))

end <- Sys.time()

WriteLog('plot histogram of tf', start, end, pryr::mem_used())

## ---- end-of-histogram_term_frequency----

## ---- term_frequency_vs_rank----

start  <- Sys.time()

options(repr.plot.width = 20, repr.plot.height = 12)

ggplot(lyrics_token, aes(x = rank_tf, y = tf, color = genre)) +
  geom_line(alpha = 0.8, size = 1.1, show.legend = FALSE) +
  scale_x_log10() + scale_y_log10() +
  xlab("Rank - Term frequency") + ylab("Term frequency") + 
  labs(title = 'Term frequency vs Rank - Log Scale') +
  theme(text = element_text(size = 20))

end  <- Sys.time()

WriteLog('plot term frequency vs rank', start, end, pryr::mem_used())

## ---- end-of-term_frequency_vs_rank----

## ---- calculate_idf----

start  <- Sys.time()

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

end = Sys.time()
mem = pryr::mem_used()

WriteLog('calculate idf', start, end, mem)

## ---- end-of-calculate_idf----

## ---- top_words_per_genre_tdidf----

start  <- Sys.time()

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

end  <- Sys.time()

WriteLog('print tf idf', start, end, pryr::mem_used())

## ---- end-of-calculate_idf----

rm(lyrics_token)
invisible(gc)

## ---- filter_tokens_LDA----

start  <- Sys.time()

lyrics_token <- readRDS('lyrics_token.rds')
distinct_words <- distinct(lyrics_token, index, word)

song_count <- length(unique(distinct_words$index))

reject_words <- count(distinct_words, word, sort = TRUE) %>% 
                    mutate(prop = n / song_count * 100) %>% 
                    arrange(desc(prop)) %>% 
                    filter(n <= 2)

reject_words <- reject_words$word

print(paste('Número de termos que aparecem em apenas uma música: ', 
            length(reject_words)))

lyrics_token <- filter(lyrics_token, !(word %in% reject_words))

print(paste('Número de tokens: ', 
            nrow(lyrics_token)))

print(paste('Número de termos restante: ', 
            length(unique(lyrics_token$word))))

mem  <- pryr::mem_used()

rm(reject_words, distinct_words, song_count)
invisible(gc)

end = Sys.time()

WriteLog('filter tokens LDA', start, end, mem)

## ---- end-of-filter_tokens_LDA----

## ---- sampling_tokens_LDA----

start  <- Sys.time()

print(paste('Número total de músicas: ', length(unique(lyrics_token$index))))

songs_sample  <- sample(unique(lyrics_token$index), 50000, replace = FALSE)

print(paste('Número de músicas selecionadas para o LDA: ', length(songs_sample)))

lyrics_token <- filter(lyrics_token, index %in% songs_sample)

mem  <- pryr::mem_used()
rm(songs_sample)

end = Sys.time()

WriteLog('sampling tokens LDA', start, end, mem)

## ---- end-of-sampling_tokens_LDA----

## ---- create_dtm----

start  <- Sys.time()

dtm <- cast_dtm(data = count(lyrics_token, index, word, sort = TRUE),
                weighting = tm::weightTf,
                document = index, 
                term = word, 
                value = n)

dtm

saveRDS(dtm, 'dtm.rds')

mem  <- pryr::mem_used()

end = Sys.time()

WriteLog('calculate dtm', start, end, mem)

## ---- end-of-create_dtm----

## ---- LDA_10_topics----

start  <- Sys.time()

tpm <- LDA(dtm, k = 10, control = list(seed = 123456))

tpm
summary(tpm)

saveRDS(tpm, 'tpm_10.rds')

mem  <- pryr::mem_used()
invisible(gc)

end = Sys.time()

WriteLog('calculate tpm 10', start, end, mem)

## ---- end-of-LDA_10_topics----

## ---- top_words_per_topic_LDA_10----

start  <- Sys.time()

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

mem  <- pryr::mem_used()

rm(term_topics)
invisible(gc)

end <- Sys.time()

WriteLog('plot top words lda 10', start, end, mem)

## ---- end-of-top_words_per_topic_LDA_10----

## ---- classification_per_genre_LDA_10----

start  <- Sys.time()

options(repr.plot.width = 20, repr.plot.height = 8)

lyrics <- readRDS('lyrics.rds')
songs_topics <- tidy(tpm, matrix = "gamma")

lyrics$index <- as.character(lyrics$index)

inner_join(lyrics, songs_topics, by = c('index' = 'document')) %>% 
    select(index, song, year, artist, genre, decade, topic, gamma) %>% 
        ggplot(aes(x = factor(topic), y = gamma, fill = genre)) +
            geom_boxplot(show.legend = FALSE, outlier.size = 0.1) +
            facet_wrap(~ genre, nrow = 2) +
            ylim(0, 1) +
            xlab('Topic') + ylab('gamma') +
            labs(title = 'Topic classification per Genre') +
            theme(text = element_text(size = 20), 
                  axis.text.y = element_blank(), 
                  axis.ticks.y = element_blank())

mem  <- pryr::mem_used()

rm(lyrics, songs_topics)
invisible(gc)

end  <- Sys.time()

WriteLog('plot classification per genre lda 10', start, end, mem)

## ---- end-of-classification_per_genre_LDA_10----

## ---- classification_per_genre_LDA_10_avg_gamma----

start  <- Sys.time()

options(repr.plot.width = 20, repr.plot.height = 8)

lyrics <- readRDS('lyrics.rds')
songs_topics <- tidy(tpm, matrix = "gamma")

lyrics$index <- as.character(lyrics$index)

inner_join(lyrics, songs_topics, by = c('index' = 'document')) %>% 
    select(index, song, year, artist, genre, decade, topic, gamma) %>% 
    group_by(genre, topic) %>% 
    summarize(mean_gamma = mean(gamma, na.rm = TRUE)) %>% 
    arrange(topic) %>%
        ggplot(aes(x = factor(topic), y = genre, fill = mean_gamma)) +
            geom_bin2d(stat = 'identity', show.legend = FALSE) +
            geom_text(aes(label = round(mean_gamma, 4)), color = 'white', size = 6) +
            scale_fill_gradient(low = "#E53935", high = "#196F3D") +
            theme_minimal() + ylab('Genre') + xlab('Topic') + 
            labs(title = 'Average Gamma by Genre vs Topic') +
            theme(text = element_text(size = 20), 
                  panel.grid = element_blank())

mem  <- pryr::mem_used()

end  <- Sys.time()

WriteLog('plot classification per gere lda 10 avg gamma', start, end, mem)

## ---- end-of-classification_per_genre_LDA_10_avg_gamma----

## ---- LDA_5_topics----

start  <- Sys.time()

tpm <- LDA(dtm, k = 5, control = list(seed = 123456))

tpm
summary(tpm)

saveRDS(tpm, 'tpm_5.rds')

mem  <- pryr::mem_used()

rm(dtm)
invisible(gc)

end = Sys.time()

WriteLog('calculate tpm 5', start, end, mem)

## ---- end-of-LDA_5_topics----

## ---- top_words_per_topic_LDA_5----

start  <- Sys.time()

options(repr.plot.width = 20, repr.plot.height = 12)

term_topics <- tidy(tpm, matrix = "beta")

term_top_terms <- term_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

term_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", nrow = 1) +
  coord_flip() +
  theme(text = element_text(size = 20), 
        axis.text.x = element_blank(), 
        axis.ticks = element_blank())

mem  <- pryr::mem_used()

rm(term_topics)
invisible(gc)

end <- Sys.time()

WriteLog('plot top words lda 5', start, end, mem)

## ---- end-of-top_words_per_topic_LDA_5----

## ---- classification_per_genre_LDA_5----

start  <- Sys.time()

options(repr.plot.width = 20, repr.plot.height = 8)

lyrics <- readRDS('lyrics.rds')
songs_topics <- tidy(tpm, matrix = "gamma")

lyrics$index <- as.character(lyrics$index)

inner_join(lyrics, songs_topics, by = c('index' = 'document')) %>% 
    select(index, song, year, artist, genre, decade, topic, gamma) %>% 
        ggplot(aes(x = factor(topic), y = gamma, fill = genre)) +
            geom_boxplot(show.legend = FALSE, outlier.size = 0.1) +
            facet_wrap(~ genre, nrow = 2) +
            ylim(0, 1) +
            xlab('Topic') + ylab('gamma') +
            labs(title = 'Topic classification per Genre') +
            theme(text = element_text(size = 20), 
                  axis.text.y = element_blank(), 
                  axis.ticks.y = element_blank())

mem  <- pryr::mem_used()
invisible(gc)

end  <- Sys.time()

WriteLog('plot classification per genre lda 5', start, end, mem)

## ---- end-of-classification_per_genre_LDA_5----

## ---- classification_per_genre_LDA_5_avg_gamma----

start  <- Sys.time()

options(repr.plot.width = 20, repr.plot.height = 10)

lyrics$index <- as.character(lyrics$index)

inner_join(lyrics, songs_topics, by = c('index' = 'document')) %>% 
    select(index, song, year, artist, genre, decade, topic, gamma) %>% 
    group_by(genre, topic) %>% 
    summarize(mean_gamma = mean(gamma, na.rm = TRUE)) %>% 
    arrange(topic) %>%
        ggplot(aes(x = factor(topic), y = genre, fill = mean_gamma)) +
            geom_bin2d(stat = 'identity', show.legend = FALSE) +
            geom_text(aes(label = round(mean_gamma, 4)), color = 'white', size = 6) +
            scale_fill_gradient(low = "#E53935", high = "#196F3D") +
            theme_minimal() + ylab('Genre') + xlab('Topic') + 
            labs(title = 'Average Gamma by Genre vs Topic') +
            theme(text = element_text(size = 20), 
                  panel.grid = element_blank())

mem  <- pryr::mem_used()
invisible(gc)

end  <- Sys.time()

WriteLog('plot classification per genre lda 5 avg gamma', start, end, mem)

## ---- end-of-classification_per_genre_LDA_5_avg_gamma----

## ---- classification_non_english----

start = Sys.time()

group_by(songs_topics, document) %>% 
    arrange(desc(gamma)) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    filter(topic == 2) %>% 
    inner_join(lyrics, by = c('document' = 'index')) %>% 
    select(artist, song, genre, decade, gamma, lyrics) %>% 
    mutate(lyrics_preview = str_sub(lyrics, 1, 140)) %>% 
    select(-lyrics) %>% 
    arrange(desc(gamma)) %>% 
    head(25)

end = Sys.time()
mem = pryr::mem_used()

WriteLog('finish', start, end, mem)

## ---- end-of-classification_non_english----

