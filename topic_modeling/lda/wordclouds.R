library(ggwordcloud)
source("./topic_modeling/lda/main.R")
library(LDAvis)
library(servr)


assign_phi_to_words <- function(df_list, phi, model_type){
  for (i in 1:length(df_list)){
    df <- df_list[[i]] 
    phi_vector <- c()
    for (j in 1:nrow(df)){
      word <- df[j,]
      if (model_type=="mallet"){
        phi_vector <- c(phi_vector,phi[i,][word])
      } else {
        phi_vector <- c(phi_vector,phi[paste0("t_",i),][word])
      }
    }
    df$phi <- phi_vector
    df_list[[i]] <- df
  }
  return(df_list)
}


create_topic_words_dfs <- function(summary){
  n <- nrow(summary)
  df_list <- vector("list", n)
  # Create and name the dataframes in a loop
  for (i in 1:n) {
    word_vector <- unlist(strsplit(summary[paste0("t_",i),]$top_terms, ", "))
    df <- data.frame(Word = word_vector) # Create an empty dataframe
    df_list[[i]] <- df  # Add the dataframe to the list
    name <- paste("t", i, sep = "_")  # Create the name for the dataframe
    assign(name, df_list[[i]])  # Assign the dataframe to a variable with the specified name
  }
  return(df_list)
}



create_plots <- function(df_list, 
                         summary,
                         test, 
                         test_type,
                         cor_var,
                         color_negative_cor,
                         color_positive_cor,
                         scale_size=TRUE,
                         plot_topics_idx=NULL,
                         p_threshold=NULL,
                         save_dir="."){
  if (is.null(plot_topics_idx)){
    plot_topics_idx <- seq(1, length(df_list))
  } 
  for (i in plot_topics_idx){
  #for (i in 1:length(df_list)){
    #view(df_list[[i]])
    if (test_type == "linear_regression"){
      estimate_col <- paste0(cor_var,".estimate") # grep(partial_name, data_frame_names, value = TRUE)
      p_adjusted_col <- paste0(cor_var,".p_adjusted")
    } else if (test_type == "t-test"){
      estimate_col <- "cohens d" # probably doesnt work yet
    } else if (test_type == "logistic_regression"){
      estimate_col <- "estimate"
      estimate_col <- "p_adjustedfdr"
    }
    estimate <- test[i,][[estimate_col]]# $PHQtot.estimate
    p_adjusted <- test[i,][[p_adjusted_col]] # $PHQtot.p_adjustedfdr
    if (scale_size==TRUE){
      prevalence <- summary[paste0("t_",i),]$prevalence
    }
    #print(paste0("prevalence: ", prevalence))
    
    
    # this will ensure that all topics are plotted
    if (is.null(p_threshold) ){
      p_threshold <- p_adjusted +1 
    }
    
    #print(is.null(p_threshold))
    if (!is.nan(p_adjusted) & p_adjusted < p_threshold){
      
      
      #estimate <- test[i,][[grep(estimate_col, colnames(test), value=TRUE)]]# $PHQtot.estimate
      #p_adjusted <- test[i,][[grep("p_adjusted", colnames(test), value=TRUE)]] # $PHQtot.p_adjustedfdr
      if (estimate < 0){
        color_scheme <- color_negative_cor # scale_color_gradient(low = "darkgreen", high = "green")
      } else {
        color_scheme <- color_positive_cor # scale_color_gradient(low = "darkred", high = "red")
      }
      if (scale_size == TRUE){
        max_size <- 10*log(prevalence)
        y <- paste0("P = ", prevalence)
      } else {
        max_size <- 10
        y <- ""
      }
      plot <- ggplot(df_list[[i]], aes(label = Word, size = phi, color = phi))+#,x=estimate)) +
        geom_text_wordcloud() +
        scale_size_area(max_size = max_size) +
        theme_minimal() +
        #theme(plot.margin = margin(0,0,0,0, "cm")) +
        color_scheme + 
        labs(x = paste0("r = ", estimate),
             y= y)
        
      if (!dir.exists(save_dir)) {
          # Create the directory
        dir.create(save_dir)
        cat("Directory created successfully.\n")
      } 
      if(!dir.exists(paste0(save_dir, "/seed_", seed, "/wordclouds"))){
        dir.create(paste0(save_dir, "/seed_", seed, "/wordclouds"))
      }
      p_adjusted <- sprintf("%.2e", p_adjusted)
      ggsave(paste0(save_dir,"/seed_", seed, "/wordclouds/t_", i, "_r_", estimate, "_p_", p_adjusted,".png"), plot = plot, width = 10, height = 8, units = "in")
    }
  }
}

create_df_list_bert_topics <- function(save_dir, num_topics){
  df_list <- list()
  for (i in 1:num_topics){
    df_list[[i]] <- read_csv(paste0(save_dir,"/seed_",seed,"/df_list_term_phi/", i, "_top_words.csv"))
  }
  return(df_list)
}


plot_wordclouds <- function(model,
                            model_type,
                            test,
                            test_type,
                            cor_var,
                            color_negative_cor,
                            color_positive_cor,
                            scale_size=TRUE,
                            plot_topics_idx,
                            p_threshold,
                            save_dir,
                            seed){
  if (model_type=="bert_topic"){
    view(test)
    num_topics <- nrow(test)
    print(num_topics)
    #print(num_t_columns)
    df_list <- create_df_list_bert_topics(save_dir, num_topics)
  } else if (model_type=="mallet"){
    model <- name_cols_with_vocab(model, "phi", model$vocabulary)
    #view(model$phi)
    #view(model$phi)
    df_list <- create_topic_words_dfs(model$summary)
    #view(df_list)
    df_list <- assign_phi_to_words(df_list, model$phi, model_type)
  } else if (model_type =="textmineR"){
    df_list <- create_topic_words_dfs(model$summary)
    view(df_list)
    df_list <- assign_phi_to_words(df_list, model$phi, model_type)
    print(df_list)
  }
  create_plots(df_list = df_list, 
               summary=model$summary,
               test=test, 
               test_type="linear_regression",
               cor_var=cor_var,
               color_negative_cor = color_negative_cor,
               color_positive_cor = color_positive_cor,
               scale_size=TRUE,
               plot_topics_idx=plot_topics_idx,
               p_threshold=p_threshold,
               save_dir=save_dir)
  print(paste0("The plots are saved in ", save_dir, "/seed", seed, "/wordclouds"))
}

get_doc_length <- function(string){
  words <- strsplit(string, "\\s+")  # Split the string by spaces
  return(length(words[[1]]))  # Count the words and return the number
}

create_lda_vis <- function(model, data, data_col, save_dir){
  doc_length <- sapply(data[[data_col]], get_doc_length)
  json <- createJSON(phi = model$phi, 
                     theta = model$theta, 
                     doc.length = doc_length, 
                     vocab = model$vocabulary, 
                     term.frequency = model$frequencies$word.freq)
  vis <- serVis(json, out.dir = paste0(save_dir, "/lda_vis"), open.browser = TRUE)
  return(vis)
}