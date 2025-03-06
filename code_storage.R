#' Get the stored text of the code
#'
#' This function returns the text of the code as a string, so users can copy and paste it.
#'
#' @return A character vector containing the code as text
#' @export
textme_code <- function() {
  return("
install.packages(c(
  'tidyverse', 'tidytext', 'tm', 'textstem', 'widyr', 'ggplot2',
  'udpipe', 'topicmodels', 'wordcloud', 'RColorBrewer', 'readr', 'readxl', 'dplyr'
))

library(tidyverse)
library(tidytext)
library(tm)
library(textstem)
library(widyr)
library(dplyr)
library(ggplot2)
library(udpipe)
library(topicmodels)
library(wordcloud)
library(RColorBrewer)
library(readr)
library(readxl)

text_data <- tibble(id=1:3, text=c(
  'The camera is amazing and the battery lasts all day.',
  'I love the sleek design and lightweight body.',
  'The battery life is terrible but the screen resolution is fantastic.'
))

# Preprocessing
niggasingh <- Corpus(VectorSource(text_data$text))
niggasingh <- tm_map(niggasingh, content_transformer(tolower))
niggasingh <- tm_map(niggasingh, removePunctuation)
niggasingh <- tm_map(niggasingh, removeWords, stopwords('english'))
niggasingh <- tm_map(niggasingh, stripWhitespace)

clean <- sapply(niggasingh, as.character)
clean

# TDM and DTM
tdm <- TermDocumentMatrix(niggasingh)
tdm_matrix <- as.matrix(tdm)
term_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)
wordcloud(names(term_freq), term_freq, min.freq=2, color='black')

dtm <- DocumentTermMatrix(niggasingh)
lda_model <- LDA(dtm, k=2)
topics <- terms(lda_model, 5)
topics

# TF-IDF
tokens <- text_data %>%
  unnest_tokens(word, text)

tf_idf <- tokens %>%
  count(id, word) %>%
  bind_tf_idf(term=word, document=id, n=n)

tf_idf

# Zipf's Law
word_freq <- tokens %>%
  count(word, sort = TRUE) %>%
  mutate(rank=row_number())

ggplot(word_freq, aes(x=rank, y=n)) +
  geom_line() +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = 'Zipf Law Visualization')

# Sentiment Analysis
sentiments <- get_sentiments('bing')

sentiment_analysis <- tokens %>%
  inner_join(sentiments, by='word') %>%
  count(sentiment, sort=TRUE)

sentiment_analysis

# Lexicon-Based Sentiment Analysis
positive_words <- c('good', 'great', 'amazing', 'cute')
negative_words <- c('bad', 'terrible', 'horrible')

sentimentanalysis <- tokens %>%
  mutate(sentiment=case_when(
    word %in% positive_words ~ 1,
    word %in% negative_words ~ -1,
    TRUE ~ 0))

sentiment_score <- sentimentanalysis %>%
  summarise(score=sum(sentiment))

sentiment_score

# Stemming & Lemmatization
stemmed <- tokens %>%
  mutate(stemmed=stem_words(word))

lemmatized <- tokens %>%
  mutate(lemmatized=lemmatize_words(word))

lemmatized
stemmed

# POS Tagging
udpipe_model <- udpipe_download_model('english')
udpipe_model <- udpipe_load_model(udpipe_model$file_model)

annotated_df <- udpipe_annotate(udpipe_model, x=text_data$text)
pos_df <- as.data.frame(annotated_df)

print(pos_df[,c('token', 'upos')])

# Word Co-occurrence
cooccurrence <- tokens %>%
  pairwise_count(word, id)

cooccurrence
")
}
