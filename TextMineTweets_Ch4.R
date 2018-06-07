
#unnest to bigram, removes unncessary tokens
unnest_bigrams <- function(tweets){
 return(tweets %>%
          unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
          separate(bigram, c("word1", "word2"), sep = " "))  
}

# filter bigrams, but dont include filter words because it tends to remove
# even words that are important, like 'himself'
# and we're dealing with > 1 words now, so it's a more contextual analysis
filter_bigrams <- function(bigrams){
  
  return(bigrams %>%
           filter(!word1 %in% stop_words$word) %>%
           filter(!word2 %in% stop_words$word) %>%
           filter(!grepl("[^\x01-\x7F]+", word1)) %>%
           filter(!grepl("[^\x01-\x7F]+", word2)) 
  )
    
}



#vizualize the correlated words with the positive words

vizualize_correlated_words <- function(bigrams_sep, top_10){
  
  top_3 = top_10 %>% top_n(3,n)
  top_3_words = gsub(" ", ", ", paste(top_3$word, collapse = " "))
  corr_1 = bigrams_sep %>%
    filter(word1 %in% top_3$word)
  
  corr_2 = bigrams_sep %>%
    filter(word2 %in% top_3$word)
  
  corr_12 = bind_rows(corr_1, corr_2)
  
  title_date = paste("Top Bigrams Associated with (", top_3_words, ")", sep = "")
  png(filename = paste("~/Data-driven Projects/graphs/",title_date, ".png", sep = ''))
  
  vizualize_top_bigrams(corr_12, title_date)
  dev.off()
}


# used in the function above, for easier readability
vizualize_top_bigrams <- function(corrs, title_date){
  
  print(
    corrs %>% 
      count(word1, word2, sort = TRUE) %>% 
      unite(bigram, word1, word2, sep = " ") %>%
      mutate(bigram = reorder(bigram,n)) %>%
      top_n(10, n) %>% 
      ggplot(aes(x = bigram, y = n)) +
      geom_col() + 
      coord_flip() + 
      labs(title = title_date, 
           x = "bigram", y = "count")
  )
  
}


# get the pairwise correlation of words, and graph them

vizualize_pairwise_correlation <- function(query, tweets){
  word_cors = tweets %>%
    group_by(word) %>%
    filter(n() >= 20) %>%
    filter(!grepl("[^\x01-\x7F]+", word)) %>%
    pairwise_cor(word, line, sort=TRUE)
  
  title_date = paste("Top Correlated Phrases in '", query, "' Tweets", sep = '')
  png(filename = paste("~/Data-driven Projects/graphs/",title_date, ".png", sep = ''))
  
  print(
    word_cors %>%
      filter(correlation > 0.85) %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
      geom_node_point(color = "lightblue", size = 4) + 
      geom_node_text(aes(label = name), repel = TRUE) +
      labs(title = title_date)
  )
  
  dev.off()
  
  
}
