library(textmineR)
library(tidyverse)
library(mallet)
library(rJava)
library(tokenizers)
source("./0_topicTest.R")
library(text2vec)

# Calculate coherence score

get_mallet_model <- function(dtm,
                             num_topics,
                             num_top_words){
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
  model$train(500)
  topic.words <- mallet.topic.words(model,
                                    smoothed=TRUE,
                                    normalized=TRUE)
  
  list_top_terms <- list()
  for(i_topic in 1:num_topics){
    top_terms <- mallet.top.words(model,
                                  word.weights = topic.words[i_topic,],
                                  num.top.words = num_top_words)
    
    top_terms <- paste(top_terms$term, collapse=" ")
    topic <- paste("t_", i_topic, sep="")
    df <- tibble(topic, top_terms)
    list_top_terms[[i_topic]] <- df
  }
  model$top_terms_mallet <- bind_rows(list_top_terms)
  
  return_model <- list()
  return_model$phi <- mallet.topic.words(model, smoothed=TRUE, normalized=TRUE)
  return_model$top_terms <- GetTopTerms(phi = return_model$phi, M = num_top_words)
  
  model$prevalence_mallet <- colSums(mallet.doc.topics(model, smoothed=TRUE, normalized=TRUE)) /
    sum(mallet.doc.topics(model, smoothed=TRUE, normalized=TRUE)) * 100
  sum(list_top_terms$prevalence)
  
  return_model$theta <- mallet.doc.topics(model, smoothed=TRUE, normalized=TRUE)
  return_model$prevalence <- colSums(return_model$theta) / sum(return_model$theta) * 100
  #return_model$labels <- LabelTopics(assignments = return_model$theta > 0.05, dtm = dtm, M = 1)
  
  return_model$coherence <- NA
  return_model$labels <- NA
  return(return_model)
}

get_textmineR_model <- function(dtm,
                                num_topics,
                                num_top_words){
  set.seed(12345)
  model <- FitLdaModel(dtm = dtm,
                       k = num_topics,
                       iterations = 500, # I usually recommend at least 500 iterations or more
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
                           data_column,
                           diagnose_column,
                           save_dir=NULL,
                           load_dir=NULL){
  
  text <- read.csv('response_format_cleaned_ds1.csv') # load text
  text_columns = text[c("participant_id",data_column,diagnose_column)] # select columns
  text_columns <- text_columns[complete.cases(text_columns), ] # remove rows without values
  
  # create a document term matrix
  dtm <- CreateDtm(doc_vec = text_columns$wor_text, # character vector of documents
                   doc_names = text_columns$participant_id, # document names
                   ngram_window = c(1, 3), # minimum and maximum n-gram length
                   stopword_vec = stopwords::stopwords("en", source = "snowball"),
                   lower = TRUE, # lowercase - this is the default value
                   remove_punctuation = TRUE, # punctuation - this is the default
                   remove_numbers = TRUE, # numbers - this is the default
                   verbose = FALSE, # Turn off status bar for this demo
                   cpus = 4) # default is all available cpus on the system
  dtm <- dtm[,colSums(dtm) > 2] # remove words with occurences < 2
  
  if (is.null(load_dir) == FALSE){
    model <- readRDS(paste0(load_dir, "/model.rds"))
    preds <- readRDS(paste0(load_dir, "/preds.rds"))
  } else{
    if (model_type == "textmineR"){
      model <- get_textmineR_model(dtm = dtm,
                                   num_topics = num_topics,
                                   num_top_words = num_top_words)
    } else {
      model <- get_mallet_model(dtm = dtm,
                                num_topics = num_topics,
                                num_top_words = num_top_words)
    }
    model$summary <- data.frame(topic = rownames(model$phi),
                                label = model$labels,
                                coherence = round(model$coherence, 3),
                                prevalence = round(model$prevalence,3),
                                top_terms = apply(model$top_terms, 
                                                  2, 
                                                  function(x){paste(x, collapse = ", ")}),
                                stringsAsFactors = FALSE)
    
    model$summary[order(model$summary$prevalence, decreasing = TRUE) , ][ 1:10 , ]
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
  }
  
  if (is.null(save_dir) == FALSE){
    if (!dir.exists(save_dir)) {
      # Create the directory
      dir.create(save_dir)
      cat("Directory created successfully.\n")
    } else {
      cat("Directory already exists.\n")
    }
    saveRDS(model, paste0(save_dir, "/model.rds"))
    saveRDS(preds, paste0(save_dir, "/preds.rds"))
  }
  
  preds <- preds %>% tibble()
  test <- topic_test(topic_terms = model$summary,
                     topics_loadings = preds,
                     grouping_variable = preds[diagnose_column],
                     test_method = "t-test",
                     split = "median",
                     n_min_max = 20,
                     multiple_comparison = "fdr")
  
  return(test)
}