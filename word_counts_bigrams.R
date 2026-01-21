# Loading required packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(forcats)
library(tibble)
library(scales)

# Setting working directory and loading text files
getwd()
setwd("/Users/supriyanannapaneni/Desktop/Text_As_Data/assignment1/Text_As_Data_Assignment1")
list.files(recursive = TRUE)

filepath <- "/Users/supriyanannapaneni/Desktop/Text_As_Data/assignment1/Text_As_Data_Assignment1"
file.exists(filepath)

commerce_file <- "A07594__Circle_of_Commerce.txt"
freetrade_file <- "B14801__Free_Trade.txt"

list.files()

commerce_raw <- readLines(commerce_file)
freetrade_raw <- readLines(freetrade_file)

# Combining into single tibble
corpus_tbl <- tibble(
  doc_title = c("text a", "text b"),
  text = c(commerce_raw, freetrade_raw)
)

corpus_tbl

# PART I: Creating diagnostics table (before stopword removal)
corpus_diagnostics <- corpus_tbl %>% 
  mutate(n_chars = str_length(text)) %>% 
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word)) %>%
  group_by(doc_title) %>%
  summarize(
    n_chars = first(n_chars),
    n_word_tokens = n(),
    n_word_types = n_distinct(word)
  )

print(corpus_diagnostics)

# Defining and applying stopwords
data("stop_words")

additional_stops <- tibble(word = c("vnto", "haue", "doo", "hath", "bee", "ye", "thee"))

combined_stopwords <- bind_rows(stop_words, additional_stops) %>%
  distinct(word)

combined_stopwords %>% slice(1:10)

# Calculating word frequencies after stopword removal
token_frequencies <- corpus_tbl %>%
  unnest_tokens(word, text) %>% 
  mutate(word = str_to_lower(word)) %>% 
  anti_join(combined_stopwords, by = "word") %>% 
  count(doc_title, word, sort = TRUE)

token_frequencies

# PART III: Calculating total words per document (after stopword removal)
document_totals <- token_frequencies %>%
  group_by(doc_title) %>% 
  summarize(total_words = sum(n))

print(document_totals)

# Adding relative frequency column
normalized_frequencies <- token_frequencies %>% 
  left_join(document_totals, by = "doc_title") %>% 
  mutate(
    n = as.numeric(n),
    total_words = as.numeric(total_words),
    relative_freq = n / total_words
  )

print(normalized_frequencies)

# Filtering for "trade" and comparing
trade_analysis <- normalized_frequencies %>% 
  filter(word == "trade") %>%
  select(doc_title, word, n, total_words, relative_freq)

print(trade_analysis)

# PART III: Visualization with relative frequencies
top_n_words <- 20

# Identifying top 20 words by maximum count across both texts
top_words_list <- token_frequencies %>%
  pivot_wider(
    names_from = doc_title,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(max_count = pmax(`text a`, `text b`)) %>%
  arrange(desc(max_count)) %>% 
  slice_head(n = top_n_words)

top_words_list

# Preparing data for plotting with relative frequencies
plot_data <- normalized_frequencies %>%
  filter(word %in% top_words_list$word) %>%
  select(word, doc_title, relative_freq) %>%
  mutate(word = fct_reorder(word, relative_freq, .fun = max))

plot_data

# Visualization
ggplot(plot_data, aes(x = relative_freq, y = word, fill = doc_title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_title, scales = "free_x") +
  scale_fill_manual(values = c("text a" = "#2E86AB", "text b" = "#A23B72")) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Normalized Word Frequencies Across Two Texts",
    subtitle = paste0(
      "Top ", top_n_words,
      " words by maximum raw frequency (shown as proportion of total words)"
    ),
    x = "Relative Frequency",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Verification
nrow(top_words_list)
dplyr::n_distinct(top_words_list$word)
plot_data %>%
  dplyr::distinct(word) %>%
  nrow()