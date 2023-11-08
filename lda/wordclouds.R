get_test_preprocessing_1 <- function(topic_res){
  topic_res1 <- topic_res[[1]] %>% tibble::as_tibble() %>% filter(adjusted_p.value < 0.05)
  topic_res2 <- topic_res1[,
                           c( "p.value","cohen_d","label_1", "adjusted_p.value", "top_terms" )]
  names(topic_res2)[3] <- c("topic")
  names(topic_res2)[4] <- c("p_value")
  return(topic_res)
}

get_test_preprocessing_2 <- function(topic_res){
  topic_res2 <- tibble::as_tibble(topic_res2)
  data <- tibble::as_tibble(list(category = topic_res2[["topic"]][1:2],
                                 cohen_d = topic_res2[["cohen_d"]][1:2],
                                 words = topic_res2[["top_terms"]][1:2]))
  names(data) <- c("category", "cohen_d", "words")
  data$category <- as.character(data$category)
  
  # Flatten the words data to create a similar weight for each word
  words_vector <- unlist(strsplit(as.character(data$words), ", "))
  # TODO 1: Add the loadings score of the top words under each topic
  words_df <- data.frame(words = words_vector, freq = seq(35, 36, length.out = length(words_vector)))
  return(list(data=data, words_df=words_df))
}

# Function to find category
find_category <- function(word) {
  for (category in names(words_list)) {
    if (word %in% words_list[[category]]) {
      return(category)
    }
  }
  return(NA)
}

get_test_preprocessing_3 <- function(topic_res){
  data <- topic_res$data
  words_df <- topic_res$words_df
  # Assuming your dataframes are named 'data' and 'words_df'
  data$words1 <- strsplit(data$words, ", ")
  
  # Create a named list of words for each category
  words_list <- setNames(data$words1, data$category) %>% tibble::as_tibble()
  # Add 'category' column to 'words_df'
  words_df <- words_df %>% mutate(category = sapply(words, find_category)) %>% tibble::as_tibble()
  
  # Reshape 'data' dataframe
  temp <- data %>% select(category, cohen_d) %>% tibble::as_tibble()
  temp$category <- as.character(temp$category)
  
  # Left join 'words_df' with 'data_reshaped'
  words_df <- words_df %>% dplyr:::left_join(temp, by = "category")
  
  # !!!! arbitrarily opposite color setting to meet the output logic
  words_df <- words_df %>% mutate(color = ifelse(category == "nothing", "red3","lightgreen"))
  return(list(data=data, words_df=words_df))
}


get_plot <- function(topic_res){
  data <- topic_res$data
  words_df <- topic_res$words_df
  
  
  # p1 - cohen_d
  p1 <- ggplot(data[,1:2], aes(x = category, y = cohen_d, 
                               #color = factor(category)
                               )) + 
    geom_col(aes(fill = category)) + 
    scale_fill_manual(values = c("red"  , "green")) +
    # geom_errorbar(aes(ymin = error_low, ymax = error_high), width = 0.1) +
    theme_bw() +
    #   Adjust the aesthetics inside geom_text_wordcloud()
    ggwordcloud::geom_text_wordcloud(data = words_df, 
                                     aes(label = words, 
                                         area = freq * 1200, 
                                         size = freq * 3,
                                         colour=color,
                                         y = -1 * sign(cohen_d) * min(abs(cohen_d)))) +
    coord_flip() +
    scale_x_discrete(limits = c("nothing", "ending")) +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(),
          legend.position = "top") + 
    labs(title = "Cohen's d of topics")  +
    theme(plot.title = element_text(size = 24),
          axis.title.x = element_text(size = 24),
          axis.title.y = element_text(size = 24),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          strip.text.x = element_text(size = 24),
          legend.text = element_text(size = 14)
    )
  # TODO 2, may need a discussion with Oscar: statistical testing result with asterisks, not implemented yet
  # + geom_text(aes(label = asterisks, vjust = ifelse(category == "Financial Security", 1, -1)), hjust = -0.6)
}