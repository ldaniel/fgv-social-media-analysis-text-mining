# loading required libraries --------------------------------------------------

# general libraries
library(dplyr)

# libraries for sentimental analysis
library(tidytext)
library(janeaustenr)
library(stringr)


# doing some stuff #1 ---------------------------------------------------------
sentiments_afinn <- get_sentiments("afinn")
sentiments_bing <- get_sentiments("bing")
sentiments_nrc <- get_sentiments("nrc")

# doing some stuff #2 ---------------------------------------------------------
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)