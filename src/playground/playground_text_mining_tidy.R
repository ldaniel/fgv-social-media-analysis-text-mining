# loading required libraries --------------------------------------------------

# general libraries
library(dplyr)
library(ggplot2)
library(stringr)

# libraries for text mining
library(tidytext)
library(janeaustenr)


# loading other scripts do be used here ---------------------------------------
source("./src/util/auxiliary_functions.R")
source("./src/datapreparation/step_01_config_environment.R")

# create corpus ---------------------------------------------------------------
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text

text_df <- tibble(line = 1:4, text = text)

text_df

# The unnest_tokens function -------------------

text_df %>% unnest_tokens(word, text)

# Tidying the works of Jane Austen -------------

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books

tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE) 

# ploting -------------

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# The gutenbergr package ----------------------------- 

library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells %>%
  count(word, sort = TRUE)


bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))


# https://www.tidytextmining.com/tidytext.html
