#get query results from search entry 
search_twitter_with <- function(query){
  time_a = Sys.time()
  print(paste("Current time: ", format(time_a, "%a %b %d %X %Y"), sep =""))
  
  tweets <- search_tweets(q = query, n = 20000,
                                 lang = "en",
                                 include_rts = FALSE)
  print(difftime(Sys.time(), time_a))
  
  return(tweets)
}

#remove URLs, and convert newlines to spaces
remove_extra_words <- function(tweets) {
  
  tweets$text_no_url <- gsub("http.*","",  tweets$text)
  tweets$text_no_url <- gsub("https.*","", tweets$text_no_url)
  tweets$text_no_url <- gsub("\n"," ", tweets$text_no_url)
  tweets_per_line = data_frame(line = seq(1,length(tweets$text_no_url)), text = tweets$text_no_url)
  return(tweets_per_line)
  
}

#use unnest_tokens to tokenize values
tokenize_and_polish <- function(tweets, unspoken_words) {
  dt_df = tweets %>% unnest_tokens(word, text)
  
  dt_df = dt_df %>% anti_join(stop_words) %>% filter(!word %in% unspoken_words)
  return(dt_df)
  
}

#asks for filter words so we can remove them from results
get_filter_words <- function(){
  filter_words = c()
  query = ''
  while(query != "TIGIL"){
    filter_words = c(filter_words, query)
    query = readline("Word to be filtered out <'TIGIL' to stop>:")
  }
  return(filter_words)
}

#check the top words
#eliminate chinese words
#eliminate words having the string "donald" or "trump" or "president"
#get only the top 30

vizualize_top_20_words <- function(query, tweets){
  title_date = paste("Top 20 Words in '", query, "' Tweets", sep = '')
  png(filename = paste("~/Data-driven Projects/graphs/",title_date, ".png", sep = ''))
  print(tweets %>% 
          filter(!grepl("[^\x01-\x7F]+", word)) %>%
          count(word, sort = TRUE) %>%
          top_n(20) %>%
          mutate(word = reorder(word, n)) %>%
          ggplot(aes(x = word, y = n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          labs(x = "Word",
               y = "Count",
               title = title_date)
  )
  
  dev.off()
}
