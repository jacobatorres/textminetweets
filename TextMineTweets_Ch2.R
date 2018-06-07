
#inner join the words from tweets to sentiments

inner_join_sentiments <- function(tweets){
  sentimented_tweets = tweets %>%
    inner_join(get_sentiments("bing"))
  
  return(sentimented_tweets)
}

# get cumulative value for running total plot
get_words_for_cumsum <- function(tweets){
  return(tweets %>%
           count(line, sentiment) %>% 
           mutate(running_total = cumsum(n)))
}

get_words_sentiment <- function(tweets, senti) {
  val = tweets %>%
    filter(sentiment == senti)
  
  return(val)
}

#know how much positive words and negative words in each tweet
vizualize_cumsum_pos_neg_words <- function (query, pos_neg_df){
  title = paste("Running Total of Negative and Positive Words in '", query, "' Tweets", sep = '')
  png(filename = paste("~/Data-driven Projects/graphs/",title, ".png", sep = ''))
  
  print(ggplot(pos_neg_df, aes(x = line, y = running_total, color = sentiment)) +
          geom_col(show.legend = FALSE) + 
          facet_wrap(~sentiment, ncol = 2) + 
          labs(title = title,
               x = "tweets", y = "count"))
  dev.off()
  
}


#show top positive and top negative words
get_top_10 <- function(tweets, senti){
  return(tweets %>% 
           count(word, sort = TRUE) %>%
           top_n(10, n) %>%
           mutate(sentiment = senti))
}

# get the top positive and negative words, displayed side by side
vizualize_top_pos_neg_words <- function(query, top_10_pos, top_10_neg){
  title_date = paste("Top 10 Positive & Negative Words in '", query, "' Tweets", sep = '')
  png(filename = paste("~/Data-driven Projects/graphs/",title_date, ".png", sep = ''))
  
  print(bind_rows(top_10_neg, top_10_pos)  %>% 
          mutate(word = reorder(word, n)) %>%
          ggplot(aes(x = word, y = n, fill = sentiment)) +
          geom_col(show.legend = FALSE) +
          facet_wrap(~sentiment, scales = "free_y") + 
          coord_flip() + 
          labs(title = title_date, x = "words", y = "count"))
  dev.off()
  
}


#for wordcloud of all words, with positive and negative words split
#not used in the blog post
vizualize_wordcloud_pos_neg <- function(query, tweets) {
  title_date = paste("Wordcloud of Negative and Positive Words in '", query, "' Tweets", sep = '')
  
  png(filename = paste("~/Data-driven Projects/graphs/",title_date, ".png", sep = ''))
  print(tweets %>%
          inner_join(get_sentiments("bing")) %>%
          count(word, sentiment, sort = TRUE) %>%
          acast(word ~ sentiment, value.var = "n", fill = 0) %>%
          comparison.cloud(colors = c("#b2281c", "#50b21c"),
                           max.words = 100) %>%
          do(layout(title = "asd")))
  dev.off()
  
  
  
}


