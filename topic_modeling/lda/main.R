library(textmineR)
library(tidyverse)
library(mallet)
library(rJava)
library(tokenizers)
library(reticulate)
source("./topic_modeling/lda/utils.R")
source("./topic_modeling/lda/inferencer.R")
use_condaenv("bert_topic", required = TRUE)
source_python("./topic_modeling/bert_topic/bert_topic.py")
source_python("./topic_modeling/Neural_Topic_models/ETM_R_integration.py")
library(text2vec)
library(dplyr)
library(quanteda)

get_absolute_frequency_term_document <- function(dtm){
  word_counts <- colSums(dtms$train_dtm)
  sorted_word_counts <- word_counts[order(-word_counts)]
  sorted_word_df <- data.frame(word = names(sorted_word_counts), count = sorted_word_counts)
  return(sorted_word_df)
}

get_relative_frequency_term_document <- function(dtm){
  # Convert binary matrix to a count matrix
  count_matrix <- as.matrix(dtm)

  # Calculate the sum of term counts in each document
  total_term_counts <- colSums(count_matrix)
  #view(total_term_counts)
  
  # Calculate the relative frequency matrix
  relative_frequency_matrix <- count_matrix / total_term_counts
  
  relative_frequency_matrix[is.na(relative_frequency_matrix)] <- 0
  return(relative_frequency_matrix)
}

name_cols_with_vocab <- function(model, col, vocabulary){
  colnames(model[[col]]) <- as.character(unlist(vocabulary))
  return(model)
}

# returns matrix in shape num(topics)*num(documents)
# dim(topic_term_matrix) = num(topics)*num(terms)
# dim(term_document_matrix) = num(terms)*num(documents)
get_topic_per_document_dist <- function(topic_term_matrix, term_document_matrix, vocabulary){
  num_topics <- dim(topic_term_matrix)[1]
  num_documents <- dim(term_document_matrix)[1]
  num_words <- length(vocabulary)
  terms <- colnames(term_document_matrix)
  colnames(topic_term_matrix) <- as.character(unlist(vocabulary))
  
  topic_document_matrix <- array(0, dim = c(num_documents, num_topics))
  for (i in 1:num_documents){
    for (j in 1:num_topics){
      for (k in 1:num_words){
        topic_document_matrix[i,j] <- topic_document_matrix[i,j] + (topic_term_matrix[j,k] * term_document_matrix[i,k])# multiple topic_term_matrix[j][k]* term_document_matrix[i][k] and
      }
    }
  }
  return(topic_document_matrix)
}

get_occ_frequency <- function(dtm, occ_rate){
  term_frequencies <- colSums(as.matrix(dtm))
  df <- data.frame(as.matrix(dtm))
  
  # Create a data frame with term and frequency
  term_frequency_df <- data.frame(Term = colnames(dtm), Frequency = term_frequencies)
  removal_frequency <- round(nrow(term_frequency_df)*occ_rate)
  return(removal_frequency)
}

get_removal_columns <- function(dtm, n, type, mode="absolute"){
  terms <- get_removal_terms(dtm, n, type, mode)
  filtered_terms_test <- c()
  for (term in terms){
    filtered_terms_test <- c(filtered_terms_test, term)
  }
  #filtered_terms_test <- c("sad", "tired", "happy", "low")
  column_indices_to_remove <- which(colnames(dtm) %in% filtered_terms_test)
  return(column_indices_to_remove)
}

get_dtm_to_df <- function(dtm){
  term_frequencies <- colSums(as.matrix(dtm))
  df <- data.frame(as.matrix(dtm))
  
  # Create a data frame with term and frequency
  term_frequency_df <- data.frame(Term = colnames(dtm), Frequency = term_frequencies)
  return(term_frequency_df)
}

get_removal_terms <- function(dtm, n, type, mode="absolute"){
  term_frequencies <- colSums(as.matrix(dtm))
  df <- data.frame(as.matrix(dtm))
  
  # Create a data frame with term and frequency
  term_frequency_df <- data.frame(Term = colnames(dtm), Frequency = term_frequencies)
  
  # Sort the data frame in descending order of frequency
  term_frequency_df <- term_frequency_df[order(-term_frequency_df$Frequency), ]
  
  # Print the terms with the highest frequencies (e.g., top 10 terms)
  #top_terms <- head(term_frequency_df, n = 10)
  if (mode=="percent"){
    removal_index <- nrow(term_frequency_df)*n 
  } else if (mode == "absolute"){
    removal_index <- n-1
  }
  if (type=="most"){
    removal_index <- round(removal_index) 
    removal_words <- term_frequency_df[["Term"]][1:removal_index]
  } else {
    removal_index <- nrow(term_frequency_df) - round(removal_index) # calculates index of term that has highest freqency of all percent least frequent words
    removal_words <- term_frequency_df[["Term"]][nrow(term_frequency_df):removal_index]
  }

  return(removal_words)
}

get_removal_frequency <- function(dtm, percent, type){
  term_frequencies <- colSums(as.matrix(dtm))
  df <- data.frame(as.matrix(dtm))
  
  # Create a data frame with term and frequency
  term_frequency_df <- data.frame(Term = colnames(dtm), Frequency = term_frequencies)
  
  # Sort the data frame in descending order of frequency
  term_frequency_df <- term_frequency_df[order(-term_frequency_df$Frequency), ]
  
  # Print the terms with the highest frequencies (e.g., top 10 terms)
  #top_terms <- head(term_frequency_df, n = 10)
  removal_index <- nrow(term_frequency_df)*percent
  # print(nrow(term_frequency_df))
  if (type=="most"){
    removal_index <- round(removal_index) 
    # print(removal_index)
  } else {
    removal_index <- nrow(term_frequency_df) - round(removal_index) # calculates index of term that has highest freqency of all percent least frequent words
  }
  #removal_index <- round(removal_index) 
  removal_frequency <- term_frequency_df[["Frequency"]][removal_index] 
  return(removal_frequency)
}

get_dtm <- function(data_dir, # provide relative directory path to data
                    id_col,
                    data_col,
                    group_var,
                    cor_var,
                    ngram_window,
                    stopwords,
                    removalword,
                    occ_rate,
                    removal_mode,
                    removal_rate_most,
                    removal_rate_least,
                    split,
                    seed,
                    save_dir=NULL){
  set.seed(seed)
  
  # Data
  #text <- readRDS(data_dir) #load data
  text <- read_csv(data_dir)
  #if (is.null(group_var)){
  #  text_cols= text[c(id_col,data_col,cor_var)] #, "minidep_scale", "miniGAD_scale")] # select columns
  #} else {
  #  text_cols= text[c(id_col,data_col,cor_var, group_var)] #, "minidep_scale", "miniGAD_scale")] # select columns
  #}
  text_cols <- text
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
  
  if (!is.null(group_var)){
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
  } else {
    result_df <- NULL
  }
  
  
  # create a document term matrix for training set
  train_dtm <- CreateDtm(doc_vec = train[[data_col]], # character vector of documents
                   doc_names = train[[id_col]], # document names
                   ngram_window = ngram_window, # minimum and maximum n-gram length
                   stopword_vec = stopwords, #::stopwords("en", source = "snowball"),
                   lower = TRUE, # lowercase - this is the default value
                   remove_punctuation = TRUE, # punctuation - this is the default
                   remove_numbers = TRUE, # numbers - this is the default
                   verbose = FALSE, # Turn off status bar for this demo
                   cpus = 4) # default is all available cpus on the system
  if (occ_rate>0){
    #print("rows in train")
    #print(nrow(train))
    removal_frequency <- round(nrow(train)*occ_rate) -1
    #print("removal frequency")
    #print(removal_frequency)
    train_dtm <- train_dtm[,colSums(train_dtm) > removal_frequency]
  }
  if (removal_mode != "threshold"){
    if (removal_rate_least > 0){
      removal_columns <- get_removal_columns(train_dtm, removal_rate_least, "least", removal_mode)
      if (removal_rate_most > 0){
        removal_columns_most <- get_removal_columns(train_dtm, removal_rate_most, "most", removal_mode)
        removal_columns <- c(removal_columns, removal_columns_most)
      }
      train_dtm <- train_dtm[,-removal_columns]
    } else if (removal_rate_most > 0){
      removal_columns <- get_removal_columns(train_dtm, removal_rate_most, "most", removal_mode)
      train_dtm <- train_dtm[,-removal_columns]
    }
  } else if (removal_mode == "threshold"){
    if (!is.null(removal_rate_least)){
      train_dtm <- train_dtm[,colSums(train_dtm) > removal_rate_least]
    }
    if (!is.null(removal_rate_most)){
      train_dtm <- train_dtm[,colSums(train_dtm) < removal_rate_most]
    }
  }
  
  #if (removal_rate_least > 0 | removal_rate_most > 0){
  #  print("removal columns")
  #  print(removal_columns)
  #}
  
  
  
  
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
  
  if (occ_rate>0){
    #print("rows in train")
    #print(nrow(train))
    removal_frequency <- round(nrow(test)*occ_rate) -1
    #print("removal frequency")
    #print(removal_frequency)
    test_dtm <- test_dtm[,colSums(test_dtm) > removal_frequency]
  }
  #removal_frequency <- get_occ_frequency(test_dtm, occ_rate)
  #test_dtm <- test_dtm[,colSums(test_dtm) > removal_frequency]
  
  if (removal_mode != "threshold"){
    if (removal_rate_least > 0){
      removal_columns <- get_removal_columns(test_dtm, removal_rate_least, "least", removal_mode)
      if (removal_rate_most > 0){
        removal_columns_most <- get_removal_columns(test_dtm, removal_rate_most, "most", removal_mode)
        removal_columns <- c(removal_columns, removal_columns_most)
      }
      test_dtm <- test_dtm[,-removal_columns]
    } else if (removal_rate_most > 0){
      removal_columns <- get_removal_columns(test_dtm, removal_rate_most, "most", removal_mode)
      test_dtm <- test_dtm[,-removal_columns]
    }
  } else if (removal_mode == "threshold"){
    if (!is.null(removal_rate_least)){
      test_dtm <- test_dtm[,colSums(test_dtm) > removal_rate_least]
    }
    if (!is.null(removal_rate_most)){
      test_dtm <- test_dtm[,colSums(test_dtm) < removal_rate_most]
    }
  }
  
  
  
  dtms <- list(train_dtm=train_dtm, test_dtm=test_dtm, train_data=train, test_data=test,split_stats=result_df)

  if (!is.null(save_dir)){
    if (!dir.exists(save_dir)) {
      # Create the directory
      dir.create(save_dir)
      cat("Directory created successfully.\n")
    } 
    if(!dir.exists(paste0(save_dir, "/seed_", seed))){
      dir.create(paste0(save_dir, "/seed_", seed))
    }
    print(paste0("The Dtm, data, and summary are saved in", save_dir,"/seed_", seed,"/dtms.rds"))
    saveRDS(dtms, paste0(save_dir, "/seed_", seed, "/dtms.rds"))
  }
  return(dtms)
}

get_bertopic_model <- function(data,
                               data_var,
                               embedding_model="default",
                               umap_model="default",
                               hdbscan_model="default",
                               vectorizer_model="default",
                               representation_model="default",
                               num_top_words=10,
                               n_gram_range=c(1,3),
                               stop_words="english",
                               min_df=min_df,
                               bm25_weighting=bm25_weighting,
                               reduce_frequent_words=reduce_frequent_words,
                               seed=1234,
                               save_dir="./results"){
  
  model <- create_bertopic_model(data=data,
                                 data_var=data_var,
                                 embedding_model=embedding_model,  # provide a value for embedding_model
                                 umap_model=umap_model,       # provide a value for umap_model
                                 hdbscan_model=hdbscan_model,    # provide a value for hdbscan_model
                                 vectorizer_model=vectorizer_model,  # provide a value for vectorizer_model
                                 representation_model=representation_model,  # provide a value for representation_model
                                 top_n_words=num_top_words,  # provide a value for top_n_words
                                 n_gram_range=n_gram_range,
                                 min_df=min_df,
                                 bm25_weighting=bm25_weighting,
                                 reduce_frequent_words=reduce_frequent_words,
                                 stop_words = stop_words,  # provide a value for n_gram_range
                                 seed=seed,
                                 save_dir=save_dir  # provide a value for save_dir
                                 )
  model$summary <- data.frame(model[2])
  preds <- read.csv(paste0(save_dir,"/seed_",seed,"/topic_distr.csv"))
  train_data <- read.csv(paste0(save_dir,"/seed_",seed,"/data.csv"))
  return(list(model=model, preds=preds, train_data=train_data))
}

get_neuralTopic_model <- function(taskname,
                                  no_below=5,
                                  no_above=0.005,
                                  num_epochs=100,
                                  n_topic=20,
                                  bkpt_continue=FALSE,
                                  use_tfidf=FALSE,
                                  rebuild=FALSE,
                                  batch_size=512,
                                  criterion='cross_entropy',
                                  emb_dim=300,
                                  auto_adj=FALSE,
                                  ckpt=None,
                                  lang="en"){
  create_ETM_model(taskname,
                   no_below,
                   no_above,
                   num_epochs,
                   n_topic,
                   bkpt_continue,
                   use_tfidf,
                   rebuild,
                   batch_size,
                   criterion,
                   emb_dim,
                   auto_adj,
                   ckpt,
                   lang)
  preds <- read_csv("./topic_modeling/Neural_Topic_Models/doc_topic_df.csv")
  return(preds)
}

get_lda_model <- function(model_type="mallet",
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
    } else if (model_type == "mallet"){
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
  
  # print(save_dir)
  
  if (!is.null(save_dir)){
    if (!dir.exists(save_dir)) {
      # Create the directory
      dir.create(save_dir)
      cat("Directory created successfully.\n")
    } 
    if(!dir.exists(paste0(save_dir, "/seed_", seed))){
      dir.create(paste0(save_dir, "/seed_", seed))
    }
    print(paste0("The Model is saved in", save_dir,"/seed_", seed,"/model.rds"))
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
                          mode="mallet",
                          save_dir=NULL,
                          load_dir=NULL){
  set.seed(seed)
  if (!is.null(load_dir)){
    preds <- readRDS(paste0(load_dir, "/seed_", seed, "/preds.rds"))
  } else {
    #data <- read.csv(paste0("./data/", data_dir))
    if (mode=="function"){
      if (is.null(model$pred_model)){
        model$pred_model <- model
      }
      preds <- predict(model$pred_model,  #model$pred_model, if mallet model
                       dtm,
                       method = "gibbs",
                       iterations = num_iterations,
                       burnin = 180,
                       cpus = 4)
    } else if (mode=="custom"){
      #print("custom")
      preds <- get_topic_per_document_dist(topic_term_matrix = model$pred_model$phi,
                                           term_document_matrix = get_relative_frequency_term_document(dtms$train_dtm),
                                           vocabulary = model$vocabulary)
    } else if (mode == "mallet"){
      
      inf_model <- model$inferencer
      #print(inf_model)
      preds <- infer_topics(
        inferencer = inf_model,
        instances = model$instances,
        n_iterations= 200,
        sampling_interval = 10, # aka "thinning"
        burn_in = 10,
        random_seed = seed
      )
    }
    
    #preds <- model$topic_docs
    preds <- as_tibble(preds)
    colnames(preds) <- paste("t_", 1:ncol(preds), sep="")
    if (!is.null(group_var)){
      categories <- data[group_var]
      preds <- bind_cols(categories, preds)
    }
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
    print(paste0("Predictions are saved in", save_dir,"/seed_", seed,"/preds.rds"))
    saveRDS(preds, paste0(save_dir, "/seed_", seed, "/preds.rds"))
  }
  
  return(preds)
}

get_lda_test <- function(model,
                         preds, 
                         data,
                         group_var, # only one in the case of t-test
                         control_vars,
                         test_method,
                         seed,
                         load_dir=NULL,
                         save_dir=NULL){
  if (!is.null(load_dir)){
    test <- readRDS(paste0(load_dir, "/seed_", seed, "/test.rds"))
  } else {
    #view(preds)
    if (!(group_var %in% names(preds))){
      print(paste0("add group_var: ", group_var))
      preds <- bind_cols(data[group_var], preds)
    }
    #view(preds)
    for (control_var in control_vars){
      if (!(control_var %in% names(preds))){
        print(paste0("added control var: ", control_var))
        preds <- bind_cols(data[control_var], preds)
      }
    }
    preds <- preds %>% tibble()
    #view(preds)
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
    if (test_method=="textTrain_regression"){
      df <- list(variable = group_var,
                estimate = test$estimate,
                t_value = test$statistic,
                p_value = test$p.value)
      write_csv(data.frame(df), paste0(save_dir, "/seed_", seed, "/textTrain_regression.csv"))
    }
    saveRDS(preds, paste0(save_dir, "/seed_", seed, "/test_",test_method, ".rds"))
    print(paste0("The test was saved in: ", save_dir,"/seed_", seed, "/test_",test_method, ".rds"))
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
  return_model$instances <- instances #model$instances()
  return_model$inferencer <- model$getInferencer()
  #view(return_model$instances)
  return_model$top_terms_mallet <- df_top_terms
  return_model$top_terms <- return_model$top_terms_mallet
  #return_model$phi <- mallet.topic.w
  return_model$phi <- mallet.topic.words(model, smoothed=TRUE, normalized=TRUE)
  return_model$topic_docs <- mallet.doc.topics(model, smoothed=TRUE, normalized=TRUE)
  #return_model$top_terms <- GetTopTerms(phi = return_model$phi, M = num_top_words)
  return_model$frequencies <- mallet.word.freqs(model)
  return_model$vocabulary <- model$getVocabulary()
  model$prevalence_mallet <- colSums(mallet.doc.topics(model, smoothed=TRUE, normalized=TRUE)) /
    sum(mallet.doc.topics(model, smoothed=TRUE, normalized=TRUE)) * 100
  #sum(list_top_terms$prevalence)
  return_model$labels <- mallet.topic.labels(model)
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
  colnames(pred_model$phi) <- as.character(unlist(return_model$vocabulary))
  pred_model$theta <- mallet.doc.topics(model, smoothed=TRUE, normalized=TRUE)
  k <- ncol(pred_model$theta) # Specify the value of k  
  new_col_names <- paste("t", 1:k, sep = "_") # Generate new column names
  colnames(pred_model$theta) <- new_col_names # Assign new column names to the dataframe
  pred_model$alpha <- model$alpha
  pred_model$gamma <- CalcGamma(phi = pred_model$phi, 
                                theta = pred_model$theta)
  
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
