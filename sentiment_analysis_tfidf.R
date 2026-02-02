library(readr)
library(dplyr)
library(stringr)
library(tibble)

setwd("/Users/supriyanannapaneni/Desktop/Text_As_Data/assignment1/Text_As_Data_Assignment1")

# Files
commerce_file  <- "A07594__Circle_of_Commerce.txt"
freetrade_file <- "B14801__Free_Trade.txt"

commerce_raw  <- read_file(commerce_file)
freetrade_raw <- read_file(freetrade_file)

# document table

texts <- tibble(
  doc_title = c("Circle of Commerce", "Free Trade"),
  text = c(commerce_raw, freetrade_raw)
) %>%
  mutate(
    text = str_replace_all(text, "ſ", "s"),
    text = str_replace_all(text, "\\s+", " "),
    text = str_to_lower(text)
  )

### --- raw count sentiment

tokens <- texts %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "^[a-z]+$")) %>%  
  anti_join(stop_words, by = "word")

sentiment_words <- tokens %>%
  inner_join(get_sentiments("bing"), by = "word")

raw_sentiment <- sentiment_words %>%
  count(doc_title, sentiment) %>%
  pivot_wider(
    names_from = sentiment,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    net_sentiment_raw = positive - negative
)

### --- TF-IDF sentiment analysis

word_counts <- tokens %>%
  count(doc_title, word)

tfidf_tbl <- word_counts %>%
  bind_tf_idf(word, doc_title, n)

sentiment_tfidf_words <- tfidf_tbl %>%
  inner_join(get_sentiments("bing"), by = "word")

tfidf_sentiment <- sentiment_tfidf_words %>%
  group_by(doc_title) %>%
  summarise(
    tfidf_positive = sum(tf_idf[sentiment == "positive"], na.rm = TRUE),
    tfidf_negative = sum(tf_idf[sentiment == "negative"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    tfidf_positive = replace_na(tfidf_positive, 0),
    tfidf_negative = replace_na(tfidf_negative, 0),
    net_sentiment_tfidf = tfidf_positive - tfidf_negative
  )

### --- final table

final_sentiment <- raw_sentiment %>%
  left_join(tfidf_sentiment, by = "doc_title")

# which words dominate sentiment when frequency = importance
top_raw_sentiment_words <- sentiment_words %>%
  count(doc_title, word, sentiment, sort = TRUE) %>%
  group_by(doc_title, sentiment) %>%
  slice_head(n = 10) %>%
  ungroup()

# which distinctive sentiment words matter
top_tfidf_sentiment_words <- sentiment_tfidf_words %>%
  arrange(desc(tf_idf)) %>%
  group_by(doc_title, sentiment) %>%
  slice_head(n = 10) %>%
  ungroup()

# raw vs TF-IDF word importance
raw_vs_tfidf_words <- sentiment_words %>%
  count(doc_title, word, sentiment) %>%
  left_join(
    sentiment_tfidf_words %>%
      select(doc_title, word, tf_idf),
    by = c("doc_title", "word")
  ) %>%
  replace_na(list(tf_idf = 0)) %>%
  arrange(doc_title,desc(tf_idf), desc(n))

write_csv(final_sentiment, "sentiment_comparison.csv")


### ---- additional challenge

# 1. Total TF-IDF sentiment per document

total_tfidf_sentiment <- sentiment_tfidf_words %>%
  group_by(doc_title) %>%
  summarise(
    total_tfidf_sentiment = sum(tf_idf),
    .groups = "drop"
  )

# 2. Top 5 sentiment words per document

top5_tfidf_words <- sentiment_tfidf_words %>%
  arrange(desc(tf_idf)) %>%
  group_by(doc_title) %>%
  slice_head(n = 5) %>%
  ungroup()

# 3. Proportion of sentiment carried by top 5 words

tfidf_concentration <- top5_tfidf_words %>%
  group_by(doc_title) %>%
  summarise(
    top5_tfidf_sentiment = sum(tf_idf),
    .groups = "drop"
  ) %>%
  left_join(total_tfidf_sentiment, by = "doc_title") %>%
  mutate(
    proportion_top5 = top5_tfidf_sentiment / total_tfidf_sentiment
  )





