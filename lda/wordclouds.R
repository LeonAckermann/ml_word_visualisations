library(ggwordcloud)


assign_phi_to_words <- function(df_list, phi){
  for (i in 1:length(df_list)){
    df <- df_list[[i]] 
    phi_vector <- c()
    for (j in 1:nrow(df)){
      word <- df[j,]
      phi_vector <- c(phi_vector,phi[paste0("t_",i),][word])
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
                         test, 
                         test_type,
                         color_negative_cor,
                         color_positive_cor,
                         save_dir="."){
  for (i in 1:length(df_list)){
    #view(df_list[[i]])
    if (test_type == "linear_regression"){
      estimate_col <- "estimate" # grep(partial_name, data_frame_names, value = TRUE)
    } else if (test_type == "t-test"){
      estimate_col <- "cohens d" # probably doesnt work yet
    }
    estimate <- test[i,][[grep(estimate_col, colnames(test), value=TRUE)]]# $PHQtot.estimate
    p_adjusted <- test[i,][[grep("p_adjusted", colnames(test), value=TRUE)]] # $PHQtot.p_adjustedfdr
    if (estimate < 0){
      color_scheme <- color_negative_cor # scale_color_gradient(low = "darkgreen", high = "green")
    } else {
      color_scheme <- color_positive_cor # scale_color_gradient(low = "darkred", high = "red")
    }
    plot <- ggplot(df_list[[i]], aes(label = Word, size = phi, color = phi)) +
      geom_text_wordcloud() +
      scale_size_area(max_size = 10) +
      theme_minimal() +
      color_scheme
    if (!dir.exists(save_dir)) {
        # Create the directory
      dir.create(save_dir)
      cat("Directory created successfully.\n")
    } 
    if(!dir.exists(paste0(save_dir, "/seed_", seed, "/wordclouds"))){
      dir.create(paste0(save_dir, "/seed_", seed, "/wordclouds"))
    }
    ggsave(paste0(save_dir,"/seed_", seed, "/wordclouds/t_", i, "_r_", estimate, "_p_", p_adjusted,".png"), plot = plot, width = 10, height = 8, units = "in")
      

  }
}


plot_wordclouds <- function(model,
                            test,
                            test_type,
                            color_negative_cor,
                            color_positive_cor,
                            save_dir,
                            seed){
  df_list <- create_topic_words_dfs(model$summary)
  df_list <- assign_phi_to_words(df_list, model$phi)
  create_plots(df_list = df_list, 
               test=test, 
               test_type="linear_regression",
               color_negative_cor = color_negative_cor,
               color_positive_cor = color_positive_cor,
               save_dir=save_dir)
  print(paste0("The plots are saved in ", save_dir, "/seed", seed, "/wordclouds"))
}