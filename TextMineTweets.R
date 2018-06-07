# load twitter library - the rtweet library is recommended now over twitteR

library(rtweet, quietly = TRUE)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)
# plotting packages
library(igraph)
library(ggraph)
library(tidyr)
library(reshape2)
library(wordcloud)

#for pairwise correlation
library(widyr)


####remember to set-up details so we can use Twitter API
setwd("~/Data-driven Projects")

source("TextMineTweets_Ch1.R")
source("TextMineTweets_Ch1_Ngram_2.R")
source("TextMineTweets_Ch2.R")
source("TextMineTweets_Ch4.R")


query = gsub(" ", "+", readline("Enter query:"))
filter_words = get_filter_words()
tweets = search_twitter_with(query)
tweets_simple = remove_extra_words(tweets)
tweet_words_per_line = tokenize_and_polish(tweets_simple, filter_words)


#CHAPTER 1

vizualize_top_20_words(query, tweet_words_per_line)

#CHAPTER 1 - N-grams

tweets_separated_words_pw = make_pair_words(tweets_simple)
tweets_filtered_pw = remove_extra_words_pair_words(tweets_separated_words_pw, filter_words)
vizualize_top_2_grams(query, tweets_filtered_pw)

#CHAPTER 2

sentimented_tweets = inner_join_sentiments(tweet_words_per_line)

positive_words = get_words_sentiment(sentimented_tweets, "positive")
pos_words_cumsum = get_words_for_cumsum(positive_words)

negative_words = get_words_sentiment(sentimented_tweets, "negative")
neg_words_cumsum = get_words_for_cumsum(negative_words)

pos_neg_df = rbind(pos_words_cumsum, neg_words_cumsum)

vizualize_cumsum_pos_neg_words(query, pos_neg_df)

top_10_pos = get_top_10(positive_words, "positive")
top_10_neg = get_top_10(negative_words, "negative")

vizualize_top_pos_neg_words(query, top_10_pos, top_10_neg)

vizualize_wordcloud_pos_neg(query, sentimented_tweets)


# CHAPTER 4

bigrams_separated = unnest_bigrams(tweets_simple)

bigrams_filtered = filter_bigrams(bigrams_separated)

vizualize_correlated_words(bigrams_filtered, top_10_pos)

vizualize_correlated_words(bigrams_filtered, top_10_neg)

vizualize_pairwise_correlation(query, tweet_words_per_line)

#add the format(Sys.time(), "%a %b %d %X") in the filename or something
#if you really want to distinguish the files
write.csv(tweets_simple, file = paste(query, " tweets.csv", sep = ""))

