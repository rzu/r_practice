library(dplyr)

# Read in the pre-cleaned review file (removed some special symbols that caused error upon opening)
file = read.csv('imdb_reviews.csv', stringsAsFactors = F, header = F)

# Assign names to columns
colnames(file) <- c("num_id", "id","rating","sentiment", "comment")
imdb <- tbl_df(file[2:5])

n_distinct(imdb$id) # 12500 unique users

# How many spoilers are there approximately?
imdb %>%
  filter(grepl('spoiler', comment, ignore.case = T)) %>%
  nrow() # 1297 reviews with spoilers

# How many reviews are per uid
imdb = imdb %>% group_by(id) %>% add_tally() # Max 4 reviews per user

# How many reviews per rating
hist(imdb$rating) # distribution of reviews per rating 1-4 and 7-10

# How many users have 1...4 reviews
count(imdb[imdb$n == 1,]) # Only 2 users with 1 review each
count(imdb[imdb$n == 2,]) # 115 users with 2 reviews each
count(imdb[imdb$n == 3,]) # 1767 userss with 3 reviews each
count(imdb[imdb$n == 4,]) # 10616 users with 4 reviews each

# Subsets for each rating
r1 = imdb[imdb$rating == 1,]
r2 = imdb[imdb$rating == 2,]
r3 = imdb[imdb$rating == 3,]
r4 = imdb[imdb$rating == 4,]
r7 = imdb[imdb$rating == 7,]
r8 = imdb[imdb$rating == 8,]
r9 = imdb[imdb$rating == 9,]
r10 = imdb[imdb$rating == 10,]

# Attempt 1: Word counts and n-grams

library(tm)
library(ngram)
library(SnowballC)
#install.packages('tm.plugin.webmining')
#library(tm.plugin.webmining)

# Some data cleaning
rev_tf = function(r) {
  x = paste(r$comment, collapse=" // ")
  x = Corpus(VectorSource(x))
  
  # These are self-explanatory
  x = tm_map(x, content_transformer(tolower))
  x = tm_map(x, removeNumbers)
  x = tm_map(x, removeWords, stopwords("english"))
  x = tm_map(x, removePunctuation)
  x = tm_map(x, stripWhitespace)
  x = tm_map(x, removeWords, c("<br />"))
  x = tm_map(x, stemDocument) # reduce words to their root form
  
  return(x)
}

rev_wfreq = function(x) {
  # Find individual word frequencies - wrap Brian's code into a function
  dtm <- TermDocumentMatrix(x)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing=TRUE)
  d <- data.frame(word = names(v), freq=v)
  
  return(d)
}

# N-grams - not very useful stuff, have to preprocess better?

r1_ngram = ngram(toString(r1$comment), n = 3, sep = " ")
r1_phrases = get.phrasetable(r1_ngram)

# Attempt n: Sentiment analysis

#install.packages('syuzhet')
library('syuzhet')
library("ggplot2")

rev_snmt = function(x) {
  d = get_nrc_sentiment(x$comment)
  td = data.frame(t(d))
  
  td_new <- data.frame(rowSums(td[]))
  #The function rowSums computes column sums across rows for each level of a grouping variable.
  
  #Transformation and  cleaning
  names(td_new)[1] <- "count"
  td_new <- cbind("sentiment" = rownames(td_new), td_new)
  rownames(td_new) <- NULL

  return(td_new[1:8,])
}

r7_snmt = rev_snmt(r7)

# Visualisation
# TODO: ggplot throws a device error if called from within a function
rev_plot = qplot(sentiment, data=r7_snmt, weight=count, geom="bar",fill=sentiment)+ggtitle("Emotions for reviews rated 7")
ggsave("emotions/r7.jpg", plot = rev_plot, width = 7, height = 5, units = "in", dpi = 300)


# Step n: Topic models using Quanteda

#install.packages('quanteda')
#install.packages('topicmodels')
#install.packages('tidytext')
library(quanteda)
library(topicmodels)

# Process each rating's comments to find relevant topics
rdfm <- dfm(r10$comment, remove_punct = TRUE, remove = stopwords('en')) %>% 
  dfm_trim(min_count = 0.95, max_docfreq = 0.1)
rdfm <- rdfm[ntoken(rdfm) > 0,]

dtm <- convert(rdfm, to = "topicmodels")
lda <- LDA(dtm, k = 10)

terms(lda, 20) # Garbage

# Capitalized pairs. Nothing interesting

# Top tokens per review
toks = tokens(imdb$comment, remove_punct = T, remove_numbers = T, remove_url = T)

# Get some capitalized pairs
cap_pairs = tokens_select(toks, '^[A-Z]', valuetype = 'regex', case_insensitive = FALSE, padding = TRUE) %>% 
  textstat_collocations(min_count = 100)
head(cap_pairs, 50)


# Per Steve's github example, pick out the most polarizing words
library(tidytext)
library(tidyr)

tidy <- imdb %>%
  unnest_tokens(word, comment) # Get tokens

word_freqs = tidy %>%
  group_by(word) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) # Get frequencies

word_freqs %>%
  top_n(50)

word_freqs %>%
  top_n(-50)

sentiment_words_count = tidy %>% 
  group_by(sentiment, word) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) # Get counts of positive and negative

log_ratios = sentiment_words_count %>% 
  spread(sentiment, count) %>%
  mutate(negative = ifelse(is.na(negative), 0, negative)) %>%
  mutate(positive = ifelse(is.na(positive), 0, positive)) %>%
  mutate(total=negative+positive) %>%
  mutate(log_ratio = log2(positive/negative))

# Get the most polarizing words
top_log_ratios = log_ratios %>%
  filter(total > 50) %>%
  group_by(log_ratio < 0) %>%
  top_n(40, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, log_ratio))

# Plot the most polarizing words

log_ratio_plot = log_ratios %>%
  filter(total > 50) %>%
  group_by(log_ratio < 0) %>%
  top_n(15, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, log_ratio)) %>%
  ggplot(aes(word, log_ratio, fill = log_ratio < 0)) +
  geom_col() +
  coord_flip() +
  ylab("log odds ratio") +
  scale_fill_discrete(name = "", labels = c("positive", "negative"))

# Save the plot to a file
ggsave("log_ratio_plot.jpg", plot = log_ratio_plot, width = 7, height = 5, units = "in", dpi = 300)

# Get most frequent words that follow words 'he' and 'she'

bigrams <- imdb %>%
  unnest_tokens(bigram, comment, token = "ngrams", n = 2) # Get n-grams of length 2

pronouns <- c("he", "she")

bigram_counts <- bigrams %>% # Get counts of 2-grams
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% pronouns) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  rename(total = nn)

word_ratios <- bigram_counts %>% # Get log-ratios of each 2-gram
  group_by(word2) %>%
  filter(sum(total) > 10) %>%
  ungroup() %>%
  spread(word1, total, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log2(she / he)) %>%
  arrange(desc(logratio)) 

word_ratio_plot = word_ratios %>% # Plot the top ones
  mutate(abslogratio = abs(logratio)) %>%
  group_by(logratio < 0) %>%
  top_n(15, abslogratio) %>%
  ungroup() %>%
  mutate(word = reorder(word2, logratio)) %>%
  ggplot(aes(word, logratio, color = logratio < 0)) +
  geom_segment(aes(x = word, xend = word,
                   y = 0, yend = logratio), 
               size = 1.1, alpha = 0.6) +
  geom_point(size = 3.5) +
  coord_flip() +
  labs(x = NULL, 
       y = "Relative appearance after 'she' compared to 'he'",
       title = "Words paired with 'he' and 'she' in IMDB reviews") +
  scale_color_discrete(name = "", labels = c("More 'she'", "More 'he'")) +
  scale_y_continuous(breaks = seq(-3, 3),
                     labels = c("0.125x", "0.25x", "0.5x", 
                                "Same", "2x", "4x", "8x"))

ggsave("he_she_ratio_plot.jpg", plot = word_ratio_plot, width = 8, height = 5, units = "in", dpi = 300)
