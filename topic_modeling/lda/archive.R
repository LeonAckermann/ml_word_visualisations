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
  #view(dtm)
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