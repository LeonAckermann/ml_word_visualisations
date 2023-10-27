library(textmineR)
library(tidyverse)
library(mallet)
library(rJava)
library(tokenizers)
source("./lda/utils.R")
library(text2vec)
library(dplyr)
library(quanteda)



get_dtm <- function(data_dir, # provide relative directory path to data
                    id_col,
                    data_col,
                    group_var,
                    ngram_window,
                    stopwords,
                    removalword,
                    split,
                    seed){
  set.seed(seed)
  
  # Data
  text <- readRDS(data_dir) #load data
  text_cols= text[c(id_col,data_col,group_var,"Sex", "age", "current_score")] #, "minidep_scale", "miniGAD_scale")] # select columns
  text_cols <- text_cols[complete.cases(text_cols), ] # remove rows without values
  text_cols = text_cols[sample(1:nrow(text_cols)), ] # shuffle
  
  split_index <- round(nrow(text_cols) * split) 
  train <- text_cols[1:split_index, ] # split training set
  if (split<1){
    test <- text_cols[split_index:nrow(text_cols), ] # split test set
  } else {
    test <- train
  }
  
  if (removalword != ""){
    train[[data_col]] <- gsub(paste0("\\b", removalword, "\\b"), "", train[[data_col]]) 
    
  }
  
  # Calculate counts and ratios for the training set
  train_counts <- table(train[c(group_var)])
  train_ratio_0 <- train_counts[1] / sum(train_counts)
  train_ratio_1 <- train_counts[2] / sum(train_counts)
  
  # Calculate counts and ratios for the test set
  test_counts <- table(test[c(group_var)])
  test_ratio_0 <- test_counts[1] / sum(test_counts)
  test_ratio_1 <- test_counts[2] / sum(test_counts)
  
  # Create a DataFrame
  result_df <- data.frame(
    Set = c("Training", "Test"),
    Count_0 = c(train_counts[1], test_counts[1]),
    Count_1 = c(train_counts[2], test_counts[2]),
    Ratio_0 = c(train_ratio_0, test_ratio_0),
    Ratio_1 = c(train_ratio_1, test_ratio_1)
  )
  
  # create a document term matrix for training set
  train_dtm <- CreateDtm(doc_vec = train[[data_col]], # character vector of documents
                   doc_names = train[[id_col]], # document names
                   ngram_window = ngram_window, # minimum and maximum n-gram length
                   stopword_vec = stopwords::stopwords("en", source = "snowball"),
                   lower = TRUE, # lowercase - this is the default value
                   remove_punctuation = TRUE, # punctuation - this is the default
                   remove_numbers = TRUE, # numbers - this is the default
                   verbose = FALSE, # Turn off status bar for this demo
                   cpus = 4) # default is all available cpus on the system

  
  train_dtm <- train_dtm[,colSums(train_dtm) > 2] # remove words with occurences < 2
  #train_dtm <- dfm_trim(train_dtm, c("family", "families"))
  # create a document term matrix for test set
  test_dtm <- CreateDtm(doc_vec = test[[data_col]], # character vector of documents
                         doc_names = test[[id_col]], # document names
                         ngram_window = ngram_window, # minimum and maximum n-gram length
                         stopword_vec = stopwords::stopwords("en", source = "snowball"),
                         lower = TRUE, # lowercase - this is the default value
                         remove_punctuation = TRUE, # punctuation - this is the default
                         remove_numbers = TRUE, # numbers - this is the default
                         verbose = FALSE, # Turn off status bar for this demo
                         cpus = 4) # default is all available cpus on the system
  test_dtm <- test_dtm[,colSums(test_dtm) > 2] # remove words with occurences < 2
  
  
  return(list(train_dtm=train_dtm, test_dtm=test_dtm, train_data=train, test_data=test,split_stats=result_df))
}

get_lda_model <- function(model_type,
                          dtm,
                          num_topics,
                          num_top_words,
                          num_iterations,
                          seed,
                          save_dir=NULL,
                          load_dir=NULL){
  set.seed(seed)
  if (!is.null(load_dir)){
    model <- readRDS(paste0(load_dir, "/seed_", seed, "/model.rds"))
  } else {
    if (model_type == "textmineR"){
      model <- get_textmineR_model(dtm = dtm,
                                   num_topics = num_topics,
                                   num_top_words = num_top_words,
                                   num_iterations = num_iterations)
    } else {
      model <- get_mallet_model(dtm = dtm,
                                num_topics = num_topics,
                                num_top_words = num_top_words,
                                num_iterations = num_iterations)
    }
    
    model$summary <- data.frame(topic = rownames(model$labels),
                                label = model$labels,
                                coherence = round(model$coherence, 3),
                                prevalence = round(model$prevalence,3),
                                top_terms = apply(model$top_terms, 
                                                  2, 
                                                  function(x){paste(x, collapse = ", ")}),
                                stringsAsFactors = FALSE)
    model$summary[order(model$summary$prevalence, decreasing = TRUE) , ][ 1:10 , ]
  }
  
  if (!is.null(save_dir)){
    if (!dir.exists(save_dir)) {
      # Create the directory
      dir.create(save_dir)
      cat("Directory created successfully.\n")
    } 
    if(!dir.exists(paste0(save_dir, "/seed_", seed))){
      dir.create(paste0(save_dir, "/seed_", seed))
    }
    saveRDS(model, paste0(save_dir, "/seed_", seed, "/model.rds"))
  }
  
  return(model)
}

get_lda_preds <- function(model, # only needed if load_dir==NULL 
                          num_iterations, # only needed if load_dir==NULL, 
                          dtm, # only needed if load_dir==NULL
                          data,
                          group_var, # only needed if load_dir==NULL
                          seed,
                          save_dir=NULL,
                          load_dir=NULL){
  set.seed(seed)
  if (!is.null(load_dir)){
    preds <- readRDS(paste0(load_dir, "/seed_", seed, "/preds.rds"))
  } else {
    #data <- read.csv(paste0("./data/", data_dir))
    preds <- predict(model$pred_model,
                     dtm,
                     method = "gibbs",
                     iterations = num_iterations,
                     burnin = 180,
                     cpus = 4)
    preds <- as_tibble(preds)
    colnames(preds) <- paste("t_", 1:ncol(preds), sep="")
    categories <- data[group_var]
    view(categories)
    preds <- bind_cols(categories, preds)
    preds <- preds %>% tibble()

  }
  
  if (!is.null(save_dir)){
    if (!dir.exists(save_dir)) {
      # Create the directory
      dir.create(save_dir)
      cat("Directory created successfully.\n")
    } 
    if(!dir.exists(paste0(save_dir, "/seed_", seed))){
      dir.create(paste0(save_dir, "/seed_", seed))
    }
    saveRDS(model, paste0(save_dir, "/seed_", seed, "/preds.rds"))
  }
  
  return(preds)
}

get_lda_test <- function(model,
                         preds, 
                         group_var,
                         control_vars,
                         test_method,
                         seed,
                         load_dir=NULL,
                         save_dir=NULL){
  if (!is.null(load_dir)){
    test <- readRDS(paste0(load_dir, "/seed_", seed, "/test.rds"))
  } else {
    test <- topic_test(topic_terms = model$summary,
                       topics_loadings = preds,
                       grouping_variable = preds[group_var],
                       control_vars = control_vars,
                       test_method = test_method,
                       split = "median",
                       n_min_max = 20,
                       multiple_comparison = "fdr")
  }
  
  if (!is.null(save_dir)){
    if (!dir.exists(save_dir)) {
      # Create the directory
      dir.create(save_dir)
      cat("Directory created successfully.\n")
    } else {
      cat("Directory already exists.\n")
    }
    if(!dir.exists(paste0(save_dir, "/seed_", seed))){
      dir.create(paste0(save_dir, "/seed_", seed))
    }
    saveRDS(preds, paste0(save_dir, "/seed_", seed, "/test.rds"))
  }
  return(test)
}

get_mallet_model <- function(dtm,
                             num_topics,
                             num_top_words, 
                             num_iterations){
  # still to complete
  docs <- Dtm2Docs(dtm)
  model <- MalletLDA(num.topics = num_topics,
                            alpha.sum = 5,
                            beta = 0.01)
  instances <- mallet.import(as.character(seq_along(docs)),
                             docs,
                             #"example_stoplist.csv",
                             preserve.case = FALSE,
                             token.regexp= "[\\p{L}\\p{N}_]+|[\\p{P}]+\ ")
  model$loadDocuments(instances)
  model$train(num_iterations)
  topic.words <- mallet.topic.words(model,
                                    smoothed=TRUE,
                                    normalized=TRUE)
  
  df_top_terms <- data.frame(matrix(NA, nrow = num_top_words, ncol = num_topics))
  colnames(df_top_terms) <- paste0("t_", 1:num_topics)
  for(i_topic in 1:num_topics){
    top_terms <- mallet.top.words(model,
                                  word.weights = topic.words[i_topic,],
                                  num.top.words = num_top_words)
    
    #top_terms <- paste(top_terms$term, collapse=" ")
    #topic <- paste("t_", i_topic, sep="")
    #df <- tibble(topic, top_terms)
    #list_top_terms[[i_topic]] <- df
    top_terms <- top_terms$term
    df_top_terms[paste0("t_", i_topic)] <- top_terms
  }
  
  
  return_model <- list()
  #return_model$top_terms_mallet <- bind_rows(list_top_terms)
  return_model$top_terms_mallet <- df_top_terms
  return_model$top_terms <- return_model$top_terms_mallet
  return_model$phi <- mallet.topic.words(model, smoothed=TRUE, normalized=TRUE)
  #return_model$top_terms <- GetTopTerms(phi = return_model$phi, M = num_top_words)
  
  model$prevalence_mallet <- colSums(mallet.doc.topics(model, smoothed=TRUE, normalized=TRUE)) /
    sum(mallet.doc.topics(model, smoothed=TRUE, normalized=TRUE)) * 100
  #sum(list_top_terms$prevalence)
  
  return_model$theta <- mallet.doc.topics(model, smoothed=TRUE, normalized=TRUE)
  return_model$prevalence <- colSums(return_model$theta) / sum(return_model$theta) * 100
  keys <- paste0("t_", 1:num_topics)
  return_model$prevalence <- setNames(return_model$prevalence, keys)
  #return_model$labels <- LabelTopics(assignments = return_model$theta > 0.05, dtm = dtm, M = 1)
  
  return_model$coherence <- return_model$prevalence
  
  # put theta into the right format
  df_theta <- data.frame(return_model$theta)
  df_theta <- setNames(df_theta, keys)
  return_model$theta <- df_theta
  
  # take the first word as dummy label, no other solution worked
  first_row <- return_model$top_terms_mallet[1,]
  return_model$labels <- matrix(first_row, nrow = num_topics, ncol = 1)
  rownames(return_model$labels) <- paste0("t_", 1:num_topics)
  colnames(return_model$labels) <- "label_1"
  #return(c(result1 = result1, result2 = result2))
  
  pred_model <- list()
  pred_model$phi <- mallet.topic.words(model, smoothed=TRUE, normalized=TRUE)
  pred_model$theta <- mallet.doc.topics(model, smoothed=TRUE, normalized=TRUE)
  pred_model$alpha <- model$alpha
  pred_model$data <- dtm
  names(pred_model)
  class(pred_model) <- "lda_topic_model"

  return_model$pred_model <- pred_model
  
  #return(list(pred_model = pred_model, return_model=return_model))
  return(return_model)
}

get_textmineR_model <- function(dtm,
                                num_topics,
                                num_top_words,
                                num_iterations){
  model <- FitLdaModel(dtm = dtm,
                       k = num_topics,
                       iterations = num_iterations, # I usually recommend at least 500 iterations or more
                       burnin = 180,
                       alpha = 0.1,
                       beta = 0.05,
                       optimize_alpha = TRUE,
                       calc_likelihood = TRUE,
                       calc_coherence = TRUE,
                       calc_r2 = F,
                       cpus = 16)
  
  model$top_terms <- GetTopTerms(phi = model$phi, M = num_top_words)
  model$prevalence <- colSums(model$theta) / sum(model$theta) * 100
  model$labels <- LabelTopics(assignments = model$theta > 0.05, dtm = dtm, M = 1)
  return(model)

} 



get_topic_test <- function(model_type,
                           num_topics,
                           num_top_words,
                           data_dir,
                           id_column,
                           data_column,
                           diagnose_column,
                           save=FALSE,
                           load=FALSE,
                           seed=1){
  
  set.seed(seed)
  save_dir <- paste0("./", data_column, "_", diagnose_column, "_", model_type, "_", num_topics)
  load_dir <- save_dir
  text <- read.csv(paste0("./data/", data_dir))
  #text <- read.csv('response_format_cleaned_ds1.csv') # load text
  text_columns = text[c(id_column,data_column,diagnose_column)] # select columns
  text_columns <- text_columns[complete.cases(text_columns), ] # remove rows without values
  
  # create a document term matrix
  dtm <- CreateDtm(doc_vec = text_columns[[data_column]], # character vector of documents
                   doc_names = text_columns$participant_id, # document names
                   ngram_window = c(1, 3), # minimum and maximum n-gram length
                   stopword_vec = stopwords::stopwords("en", source = "snowball"),
                   lower = TRUE, # lowercase - this is the default value
                   remove_punctuation = TRUE, # punctuation - this is the default
                   remove_numbers = TRUE, # numbers - this is the default
                   verbose = FALSE, # Turn off status bar for this demo
                   cpus = 4) # default is all available cpus on the system
  dtm <- dtm[,colSums(dtm) > 2] # remove words with occurences < 2
  view(dtm)
  if (load){
    if (model_type == "textmineR"){
      model <- readRDS(paste0(load_dir, "/seed_", seed, "/model.rds"))
      preds <- readRDS(paste0(load_dir, "/seed_", seed, "/preds.rds"))
    } else {
      model <- readRDS(paste0(load_dir, "/seed_", seed, "/model.rds"))
      pred_model <- readRDS(paste0(load_dir, "/seed_", seed, "/pred_model.rds"))
      preds <- readRDS(paste0(load_dir, "/seed_", seed, "/preds.rds"))
    }
  } else{
    if (model_type == "textmineR"){
      model <- get_textmineR_model(dtm = dtm,
                                   num_topics = num_topics,
                                   num_top_words = num_top_words)
    } else {
      models <- get_mallet_model(dtm = dtm,
                                num_topics = num_topics,
                                num_top_words = num_top_words)
      model <- models$return_model
      pred_model <- models$pred_model
    }
    model$summary <- data.frame(topic = rownames(model$labels),
                                label = model$labels,
                                coherence = round(model$coherence, 3),
                                prevalence = round(model$prevalence,3),
                                top_terms = apply(model$top_terms, 
                                                  2, 
                                                  function(x){paste(x, collapse = ", ")}),
                                stringsAsFactors = FALSE)
    
    model$summary[order(model$summary$prevalence, decreasing = TRUE) , ][ 1:10 , ]
    if (model_type == "textmineR"){
      preds <- predict(model,
                       dtm,
                       method = "gibbs",
                       iterations = 200,
                       burnin = 180,
                       cpus = 4)
      preds <- as_tibble(preds)
      colnames(preds) <- paste("t_", 1:ncol(preds), sep="")
      categories <- text_columns[c(diagnose_column)]
      preds <- bind_cols(categories, preds)
    } else {
      preds <- predict(pred_model,
                       dtm,
                       method = "gibbs",
                       iterations = 200,
                       burnin = 180,
                       cpus = 4)
      preds <- as_tibble(preds)
      colnames(preds) <- paste("t_", 1:ncol(preds), sep="")
      categories <- text_columns[c(diagnose_column)]
      preds <- bind_cols(categories, preds)
      #preds <- model$theta
      #categories <- text_columns[c(diagnose_column)]
      #preds <- bind_cols(categories, preds)
    }
    
  }
  preds <- preds %>% tibble()
  test <- topic_test(topic_terms = model$summary,
                     topics_loadings = preds,
                     grouping_variable = preds[diagnose_column],
                     test_method = "t-test",
                     split = "median",
                     n_min_max = 20,
                     multiple_comparison = "fdr")
  
  if (save){
    if (!dir.exists(save_dir)) {
      # Create the directory
      dir.create(save_dir)
      cat("Directory created successfully.\n")
    } else {
      cat("Directory already exists.\n")
    }
    if(!dir.exists(paste0(save_dir, "/seed_", seed))){
      dir.create(paste0(save_dir, "/seed_", seed))
    }
    saveRDS(model, paste0(save_dir, "/seed_", seed, "/model.rds"))
    saveRDS(preds, paste0(save_dir, "/seed_", seed, "/preds.rds"))
    saveRDS(preds, paste0(save_dir, "/seed_", seed, "/test.rds"))
    if (model_type == "mallet"){
      saveRDS(pred_model, paste0(save_dir, "/seed_", seed, "/pred_model.rds"))
    }
  }
  
  
  
  return(test)
}