#get 2-grams 

# use the unnest_tokens function to remove punctuation, convert to lowercase, add id for each tweet
# also to create ngrams of n = 2 
make_pair_words <- function(tweets){
  tweets_clean_pairs = tweets %>%
    dplyr::select(text) %>%  
    unnest_tokens(pair_words, text, token = "ngrams", n = 2)
  
  tweets_separated_words = tweets_clean_pairs %>%
    separate(pair_words, c("word1", "word2"), sep = " ")
  
  return(tweets_separated_words)
}

# remove filtered words, stop words
remove_extra_words_pair_words <- function(tweets, unspoken_words){
  tweets_filtered = tweets %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>% 
    filter(!grepl("[^\x01-\x7F]+", word1)) %>%
    filter(!grepl("[^\x01-\x7F]+", word2)) %>%
    filter(!word1 %in% unspoken_words) %>%
    filter(!word2 %in% unspoken_words)
  
  return(tweets_filtered)
}

# plot the n-gram

vizualize_top_2_grams <- function(query, tweets) {
  title_date = paste("Top 20 Two-worded Phrases in '", query, "' Tweets", sep = '')
  png(filename = paste("~/Data-driven Projects/graphs/",title_date, ".png", sep = ''))
  
  print(tweets %>%
    count(word1, word2, sort = TRUE) %>%
    filter(n >= 150) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
    geom_node_point(color = "darkslategray4", size = 3) +
    geom_node_text(aes(label = name), vjust = 1.8, size = 4) +
    labs(title = title_date,
         x = "", y = ""))
  dev.off()
}