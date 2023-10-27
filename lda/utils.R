#mallet, the javascript for textmining
# @param topic_model (list object) LDA topic from textmineR with model$summary (see: https://cran.r-project.org/web/packages/textmineR/vignettes/c_topic_modeling.html)

# rearragne order of column starting with
# topic_name, p-value, adjusted-p-values, cohen's d or r;  terms ; label1; label2; prevalence; coherence; [the include the rest in any order]

#### func topic testing ####
# p.adjust https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/p.adjust

set.seed(42)

#' The function to determine a continuous variable from a threshold
#' @param df (tibble) The initial tibble
#' @param column_name (string) The name of the grouping variable
#' @param threshold (integer) The threshold for determine the continous variable
#' @return boolean value
#' @noRd
is_continuous_variable <- function(df, threshold) {
  numbers_unique_values <- length(unique(df[[1]]))
  return (numbers_unique_values > threshold)
}

#' The function to calculate median split on a single column
#' @param topic_loadings (tibble) The initial tibble
#' @param grouping1 (string) The name of the grouping variable
#' @param colname (string) The name of the topic variable
#' @importFrom dplyr mutate sym
#' @return RObj of the testing result of a single topic
#' @noRd
median_split_test_topic_wise <- function(topic_loadings,
                                         grouping1,
                                         colname) {
  median_grouping <- median(topic_loadings[[grouping1]])
  topic_loadings <- topic_loadings %>%
    dplyr::mutate(group = ifelse(!!dplyr::sym(grouping1) >= median_grouping, 'Group1', 'Group2'))
  t_test_result <- t.test(topic_loadings[[colname]] ~ group,
                          var.equal = FALSE,
                          data = topic_loadings)
  return(t_test_result)
}

#' The function to calculate median split on columns
#' @param topic_loadings (tibble) The initial tibble
#' @param grouping1 (string) The name of the grouping variable
#' @param colnames (vector) The vector of the topic names
#' @importFrom purrr pmap
#' @return a named list of testing results
#' @noRd
median_split_test_topics <- function(topic_loadings,
                                     grouping1,
                                     colnames) {
  results <- purrr::pmap(list(
    list(topic_loadings),
    grouping1,
    colnames),
    median_split_test_topic_wise)
  names(results) <- colnames
  return(results)
}

#' The function to calculate the cor on a single column
#' @param topic_loadings (tibble) The initial tibble
#' @param grouping1 (string) The name of the grouping variable
#' @param colname (string) The name of the topic variable
#' @importFrom dplyr mutate sym
#' @return RObj of the testing result of a single topic
#' @noRd
corr_topic_wise <- function(topics_loadings,
                            grouping1,
                            colname) {
  corr_results <- cor.test(topics_loadings[[grouping1]],
                           topics_loadings[[colname]])
  return(corr_results)
}


#' The function to calculate correlation on columns
#' @param topic_loadings (tibble) The initial tibble
#' @param grouping1 (string) The name of the grouping variable
#' @param colnames (vector) The vector of the topic names
#' @param method1 (string) The method to adjust the p value
#' @importFrom purrr pmap
#' @return a named list of testing results
#' @noRd
topics_corr_grouping <- function(topics_loadings,
                                 grouping1,
                                 colnames1,
                                 method1="bonferroni") {
  topics_stats <- purrr::pmap(list(
    list(topics_loadings),
    grouping1,
    colnames1),
    corr_topic_wise)

  names(topics_stats) <- colnames1

  topics_stats <- map(topics_stats,
                      ~ c(.x, adjust.p.value = stats::p.adjust(
                        .x$p.value,
                        method = method1,
                        n = length(topics_loadings))))
  return(topics_stats)
}

#' The function to extract key stats for a tibble
#' @param topics_stats (list) The output from the previous topic stats
#' @param cal_cohen_d (boolean) Whether to calculate cohen's d, not applicable for correlation results
#' @importFrom purrr pmap_dfr
#' @importFrom tibble tibble
#' @importFrom effsize cohen.d
#' @return a named list of testing results
#' @noRd
extract_topic_stats_corr <- function(topics_stats, cal_cohen_d = FALSE) {
  require(tibble)
  require(effsize)

  # Define a function to extract the required information from a single topic
  extract_single_topic <- function(name, topic) {
    df <- topic$parameter["df"]
    p_value <- topic$p.value
    estimate <- topic$estimate["cor"]
    adjust.p_value <- topic$adjust.p.value
    conf_int_lower <- topic$conf.int[1]
    conf_int_higher <- topic$conf.int[2]

    # Calculate Cohen's d if cal_cohen_d is TRUE
    if (cal_cohen_d) {
      effect_size <- cohen.d(topic$statistic, df)$estimate
    } else {
      effect_size <- "not_available"
    }

    return(tibble::tibble("topic_name" = name, "df" = df, "p.value" = p_value,
                          "adjust.p_value" = adjust.p_value,
                          "estimate_corr" = estimate,
                          "conf.int_lower" = conf_int_lower, "conf.int_higher" = conf_int_higher,
                          "cohen_d" = effect_size))
  }

  # Use purrr::pmap_dfr to apply the function to each topic in the list and return a tibble
  output <- purrr::pmap_dfr(list(names(topics_stats), topics_stats),
                            extract_single_topic)

  return(output)
}

#' The function to t_test on topics
#' @param topics_loadings (list) The topic loadings from input
#' @param method1 (string) Method to adjust p value.
#' @param calc_cohen_d (boolean) To calculate cohen'd else return "NA"
#' @importFrom purrr pmap
#' @importFrom utils combn
#' @importFrom dplyr filter
#' @importFrom stats t.test p.adjust
#' @importFrom effsize cohen.d
#' @NoRd
topics_t_test_grouping <- function(topics_loadings,
                                   method1,
                                   calc_cohen_d = TRUE) {
  # Get unique combinations of 'value' column
  view(topics_loadings)
  combinations <- utils::combn(unique(topics_loadings$value), 2, simplify = FALSE)
  view(combinations)

  # Function to perform t-test and calculate Cohen's d
  t_test_func <- function(combination) {
    df1 <- topics_loadings %>% dplyr::filter(value == combination[1])
    df2 <- topics_loadings %>% dplyr::filter(value == combination[2])
    #df1 <- df1[, 2:ncol(df1)]
    #df2 <- df2[, 2:ncol(df2)]
    #view(df1)
    #view(t)

    results <- map(3:ncol(topics_loadings), ~ list(
      #view(df1[[.]]),
      t_test = stats::t.test(df1[[.]], df2[[.]]),
      cohen_d = if (calc_cohen_d) effsize::cohen.d(df1[[.]], df2[[.]]) else "NA"
    ))
    #View(results)

    # Adjust p-value if more than two categories
    groupings <- unique(topics_loadings$value)
    view(length(groupings))
    if (length(groupings) > 1) {
      results <- map(results, ~ c(.x, adjust.p.value = stats::p.adjust(
        .x$t_test$p.value,
        method = method1,
        n = length(groupings))))
    } else {
      # Return "not_available" if a group has only one observation
      results <- map(results, ~ c(.x, adjust.p.value = "not_available"))
    }

    names(results) <- paste0("t_", 1:(ncol(topics_loadings)-2))

    return(results)
  }

  # Apply function to each combination
  results <- map(combinations, t_test_func)

  # Name each element in the list with the combination it represents
  names(results) <- map(combinations, paste, collapse = "_")

  return(results)
}


#' The function to extract key stats for a tibble
#' @param topics_stats (list) The output from the previous topic stats
#' @param cal_cohen_d (boolean) Whether to calculate cohen's d, not applicable for correlation results
#' @importFrom purrr pmap_dfr compact
#' @importFrom tibble tibble
#' @importFrom effsize cohen.d
#' @return a named list of testing results
#' @noRd
extract_topic_stats_cate <- function(topics_stats, cal_cohen_d = TRUE) {
  require(tibble)
  require(effsize)

  # Define a function to extract the required information from a single topic
  extract_single_topic <- function(name1, topic1, cal_cohen_d=TRUE) {
    # Extract the non-NULL htest object / flatten
    htest <- purrr::compact(unlist(topic1, recursive = FALSE))

    t_stats <- htest[["t_test.statistic"]]
    df <- htest[["t_test.parameter"]][["df"]]
    adjust_p_value <- htest[["adjust.p.value"]]
    p_value <- htest[["t_test.p.value"]]
    mean_1 <- htest[["t_test.estimate"]][["mean of x"]]
    mean_2 <- htest[["t_test.estimate"]][["mean of y"]]
    conf_int_lower <- htest[["t_test.conf.int"]][[1]]
    conf_int_higher <- htest[["t_test.conf.int"]][[2]]

    # Calculate Cohen's d if cal_cohen_d is TRUE and df is sufficient
    #cal_cohen_d && df > 2
    if (!cal_cohen_d) {  # Adjust this threshold as needed
      # This is problematic to have NA values. Backup, no use.
      #eff_size <- effsize::cohen.d(htest$statistic[["t"]], df)$estimate
      eff_size <- "NA"
      eff_size_conf_lower <- "NA"
      eff_size_conf_higher <- "NA"
    } else {
      eff_size <- htest[["cohen_d.estimate"]]
      eff_size_conf_lower <- htest[["cohen_d.conf.int"]][["lower"]]
      eff_size_conf_higher <- htest[["cohen_d.conf.int"]][["upper"]]
    }

    return(tibble::tibble("topic_name" = name1,
                          "p.value" = p_value,
                          "adjusted_p.value" = adjust_p_value,
                          "cohen_d" = eff_size,
                          "mean_group_1" = mean_1,
                          "mean_group_2" = mean_2,
                          "t_stats" = t_stats,
                          "df" = df,
                          "p_conf.int_lower" = conf_int_lower,
                          "p_conf.int_higher" = conf_int_higher,
                          "cohen_d_conf.int_lower" = eff_size_conf_lower,
                          "cohen_d_conf.int_higher" = eff_size_conf_higher))
  }

  # Use purrr::pmap_dfr to apply the function to each topic in the list and return a tibble
  output <- purrr::pmap_dfr(list(names(topics_stats),
                                 topics_stats, cal_cohen_d),
                            extract_single_topic)

  return(output)
}


#' The function to sort the output
#' @param output (tibble) The output from the function extract stats
#' @importFrom dplyr arrange
#' @return a sorted tibble
#' @noRd
sort_stats_tibble <- function(df) {
  # Check if 'adjusted_p.value' column exists
  if ("adjusted_p.value" %in% colnames(df)) {
    # If it exists, check if it's numeric
    if (is.numeric(df$adjusted_p.value)) {
      # Sort by 'p.value' and 'adjusted_p.value'
      df <- df %>% dplyr::arrange(p.value, adjusted_p.value)
    } else {
      # If it's not numeric, sort by 'p.value' only
      df <- df %>% dplyr::arrange(p.value)
    }
  } else {
    # If 'adjusted_p.value' column doesn't exist, sort by 'p.value' only
    df <- df %>% dplyr::arrange(p.value)
  }
  return(df)
}

#' The function for topic testing
#' @param topic_loadings (tibble) The predicted loadings of topics including the grouping variable.
#' @param grouping_variable (tibble) The variable for grouping
#' @param topic_terms (R_obj) The object from model$summary in textmineR package vignette topic_modeling
#' @param split (string) How to split the CONTINUOUS test_values for testing
#' @param n_min_max (integer) If split = "min_max", the number of records to test per group.
#' @param multiple_comparison (string) The p-correction method
#' @importFrom dplyr select everything
#' @return Results
topic_test <- function(topic_terms,
                       topics_loadings,
                       grouping_variable,
                       control_vars,
                       test_method = "correlation",
                       split = "median",
                       n_min_max = 20,
                       multiple_comparison = "bonferroni"){
  require(stats)
  require(purrr)
  require(dplyr)
  require(tidyr)
  #require(textmineR)
  colnames(grouping_variable) <- "value"
  topics_groupings <- bind_cols(topics_loadings,
                                grouping_variable)

  topics_loadings <- topics_loadings[complete.cases(topics_groupings), ]
  grouping_variable <- grouping_variable[complete.cases(topics_groupings), ]


  if (TRUE){
    # format checker
    if (!tibble::is_tibble(topics_loadings)) {
      stop("Parameter `topics_loadings` must be a tibble.")
    }
    if (!tibble::is_tibble(grouping_variable)) {
      stop("Parameter `grouping_variable` must be a tibble.")
    }
    if (nrow(topics_loadings) != nrow(grouping_variable)){
      stop("Parameters `topics_loadings` & `grouping_variable`
           should have the same length.")
    }
    if (!is.character(split)) {
      stop("Parameter `split` must be a string.")
    }
    if (!split %in% c("median", "quartile", "min_max")) {
      stop("Parameter `split` must be one of 'median', 'quartile', or 'min_max'.")
    }
    if (!is.numeric(n_min_max)) {
      stop("Parameter `n_min_max` must be numeric.")
    }
    if (!is.character(multiple_comparison)) {
      stop("Parameter `multiple_comparison` must be a string.")
    }
    if (!multiple_comparison %in% c("holm", "hochberg", "hommel", "bonferroni",
                                    "BH", "BY","fdr", "none")) {
      stop("Variable `multiple_comparison` must be one of `holm`, `hochberg`,
      `hommel`, `bonferroni`, `BH`, `BY`,`fdr`, or `none`.")
    }
  }


  if (FALSE){
    model1$prevalence <- colSums(model1$theta) / sum(model1$theta) * 100
    model1$top_terms <- textmineR::GetTopTerms(phi = model1$phi, M = 5)
    model1$summary <- data.frame(topic = rownames(model1$phi),
                                 label = model1$labels,
                                 coherence = round(model1$coherence, 3),
                                 prevalence = round(model1$prevalence,3),
                                 top_terms = apply(model1$top_terms, 2, function(x){
                                   paste(x, collapse = ", ")
                                 }),
                                 stringsAsFactors = FALSE)
  }


#  if (TRUE){
#    # continuous or categorical grouping variable
#    threshold <- 50
#    if (grouping_variable %>% tibble::as_tibble() %>%
#        is_continuous_variable(
#          ., threshold
#        )){
#      con_cat_grouping <- "con"}else{con_cat_grouping <- "cat"}
#  }
#


  if (test_method == "correlation"){

#    # median split
#    if (FALSE){
#      output <- median_split_test_topics(topics_tibble,
#                                         grouping_variable,
#                                         colnames(dplyr::select(topics_tibble,
#                                                                -grouping_variable)))
#    }
    # corr
    if (TRUE){
      temp <- cbind(grouping_variable, topics_loadings)
      colnames(temp)[1] <- colnames(grouping_variable)[1]
      colnames(temp)[2:ncol(temp)] <- colnames(topics_loadings)
      topics_loadings <- temp
      temp <- NULL
      result <- topics_corr_grouping(
        topics_loadings,
        grouping1 = colnames(topics_loadings)[1],
        colnames1 = colnames(topics_loadings)[2:ncol(topics_loadings)],
        method1 = multiple_comparison)
      # Change the output of a list to a tibble. For corr only now.
      output <- extract_topic_stats_corr(result)
      names(output)[1] <- c("topic_name")
      output <- dplyr::left_join(output, topic_terms,
                                 by = join_by(topic_name == topic))

      output <- output %>%
        dplyr::select(
          topic_name,
          p.value,
          adjust.p_value,
          top_terms,
          prevalence,
          coherence,
          dplyr::everything()  # this will include the rest of the columns in their original order
        )
    }

    return (output %>% sort_stats_tibble())
  }
  if (test_method == "t-test"){
    temp <- cbind(grouping_variable, topics_loadings)
    colnames(temp)[1] <- colnames(grouping_variable)[1]
    colnames(temp)[2:ncol(temp)] <- colnames(topics_loadings)
    topics_loadings <- temp
    temp <- NULL
    result <- topics_t_test_grouping(topics_loadings,
                                     method1 = multiple_comparison)

    # Produce the topic list through the pairs of categories
    output_list <- purrr::map(names(result), function(name) {
      output <- extract_topic_stats_cate(result[[name]])
      #view(output)
      names(output)[1] <- c("topic_name")
      #names(output)[1] <- "topic_name"
      view(output)
      view(topic_terms)
      print(class(output$topic_name))
      print(class(topic_terms$topic))
      #output <- dplyr::left_join(output, topic_terms, by = join_by(topic_name == topic))
      output <- dplyr::left_join(output, topic_terms, by = join_by(topic_name == topic))
      view(output)
      #output <- dplyr::left_join(output, topic_terms, by = join_by(topic))
      
      output <- output %>%
        dplyr::select(
          topic_name,
          p.value,
          adjusted_p.value,
          cohen_d,
          top_terms,
          label_1,
          #label.label_2,
          prevalence,
          coherence,
          mean_group_1,
          mean_group_2,
          dplyr::everything()  # this will include the rest of the columns in their original order
        )
      output <- sort_stats_tibble(output)
      return(output)
    })
    names(output_list) <- names(result)

    return (output_list)
  }
  if (test_method == "linear_regression" | test_method == "logistic_regression"){
    # still get number of topics automatically
    num_topics <- nrow(topic_terms)
    lda_topics <- character(num_topics)
    # Create the list of LDA topics
    for (i in 1:num_topics) {
      lda_topics[i] <- paste("t_", i, sep = "")
    }
    
    view(preds)
    
    preds <- topics_loadings # load topics_loading into different variable to reduce naming errors
    for (topic in lda_topics) {
      preds[[paste0("z_",topic)]] <- scale(preds[[topic]])
    }
    
    control_variables <- control_vars
    for (variable in control_variables){
      preds[[paste0("z_",variable)]] <- scale(preds[[variable]])
    }
    
    # Initialize an empty list to store the topic names
    z_lda_topics <- character(num_topics)
    for (i in 1:num_topics) {
      z_lda_topics[i] <- paste0("z_t_", i)
    }
    # Loop through each LDA topic and create a linear model
    multi_models <- list()
   
    preds[is.na(preds)] <- 0
    
    view(preds)
    
    if (test_method == "linear_regression"){
      
      formula_tail <- "~"
      for (variable in control_variables){
        formula_tail <- paste0(formula_tail, " + z_", variable)
      }
      
      
      for (topic in z_lda_topics) {
        formula <- as.formula(paste0(topic, formula_tail))
        multi_models[[paste0("t_",topic)]] <- lm(formula, data = preds)
      }
    } 
    
    print(preds)
    print(test_method)
    if (test_method == "logistic_regression"){
      print(control_variables[1])
      print(z_lda_topics[1])
      for (topic in z_lda_topics){
        
        multi_models[[paste0("t_", topic)]] <- glm(paste0("z_",control_variables[1], " ~ ", topic), data = preds)
      }
    }
    
    
    #print(multi_models)
    print(length(multi_models))
    
    control_variable_summary <- list()
    topics <- c()
    if (test_method=="linear_regression"){
      for (variable in control_variables){
        control_variable_summary[[variable]] <- list()
        control_variable_summary[[variable]][["estimate"]] <- c()
        control_variable_summary[[variable]][["t"]] <- c()
        control_variable_summary[[variable]][["p"]] <- c()
        control_variable_summary[[variable]][["p_adjusted"]] <- c()
      }
    }
    if (test_method=="logistic_regression"){
      control_variable_summary[["estimate"]] <- c()
      control_variable_summary[["t"]] <- c()
      control_variable_summary[["p"]] <- c()
      control_variable_summary[["p_adjusted"]] <- c()
    }
    
    for (i in 1:length(multi_models)){
      temp <- multi_models[[i]]
      p_values <- summary(temp)$coefficients[, "Pr(>|t|)"]
      t_values <- summary(temp)$coefficients[, "t value"]
      estimate_values <- summary(temp)$coefficients[, "Estimate"]
      topics <- c(topics, paste0("t",i))
      if (test_method == "linear_regression"){
        for (variable in control_variables){
          control_variable_summary[[variable]][["estimate"]] <- c(control_variable_summary[[variable]][["estimate"]],
                                                                  estimate_values[[paste0("z_", variable)]])
          control_variable_summary[[variable]][["t"]] <- c(control_variable_summary[[variable]][["t"]],
                                                           t_values[[paste0("z_",variable)]])
          control_variable_summary[[variable]][["p"]] <- c(control_variable_summary[[variable]][["p"]],
                                                           p_values[[paste0("z_", variable)]])
        }
      }
      if (test_method == "logistic_regression"){
        control_variable_summary[["estimate"]] <- c(control_variable_summary[["estimate"]],
                                                                  estimate_values[[paste0("z_t_",i )]])
        control_variable_summary[["t"]] <- c(control_variable_summary[["t"]],
                                                           t_values[[paste0("z_t_",i)]])
        control_variable_summary[["p"]] <- c(control_variable_summary[["p"]],
                                                           p_values[[paste0("z_t_",i )]])
      }
    
    }
    
    if (test_method == "linear_regression"){
      for (variable in control_variables){
        p_adjusted <- stats::p.adjust(control_variable_summary[[variable]][["p"]],
                                      "bonferroni",
                                      length(multi_models))
        control_variable_summary[[variable]][["p_adjusted"]] <- c(control_variable_summary[[variable]][["p_adjusted"]],
                                                                  p_adjusted)
      }
    }
    if (test_method == "logistic_regression"){
      p_adjusted <- stats::p.adjust(control_variable_summary[["p"]],
                                    "bonferroni",
                                    length(multi_models))
      control_variable_summary[["p_adjusted"]] <- c(control_variable_summary[["p_adjusted"]],
                                                                p_adjusted)
    }
    
    #return (control_variable_summary)
    control_variable_summary$topic <- lda_topics

    output <- right_join(topic_terms[c("topic", "top_terms")], data.frame(control_variable_summary), by = join_by(topic))
    # add the adjustment for bonferroni
    return(output)    
    
    
  }
  
}






#if (FALSE){
#  get_stopwords <- function(noPara="1"){
#    ini_stopword_list <- stopwords::stopwords("en", source="snowball")
#    stopword_list <- c(ini_stopword_list,
#                       "it", "me", "and", "in", "the",
#                       "https", "www", "com", "to", "a", "my", "but", "for",
#                       "have", "is", "of", "you", "don", "not", "he",
#                       # time-related words
#                       "last", "two", "weeks", "years", "ago", "day")
#    return (stopword_list)
#
#  }
#  library(tidyverse)
#  library(textmineR)
#  dat <- readRDS("dat2Oscar.rds")
#  mod1 <- readRDS("mod_50.rds")
#  sumOutput <- readRDS("topicTerms.rds")
  #names(sumOutput)[9] <- c("topic_name")
#  tempDTM <- CreateDtm(doc_vec = dat$suicide$suicide, # character vector of documents
#                       doc_names = dat$suicide$procID, # document names
#                       ngram_window = c(1, 3), # minimum and maximum n-gram length
#                       stopword_vec = get_stopwords(),
#                       lower = TRUE, # lowercase - this is the default value
#                       remove_punctuation = TRUE, # punctuation - this is the default
#                       remove_numbers = TRUE, # numbers - this is the default
#                       verbose = FALSE, # Turn off status bar for this demo
#                       cpus = 4)
#  lowColsumDtm <- 0.05 # Seems 0.05 is better than 0.025
#  highColsumDtm <- 1 - lowColsumDtm
#  thresholds1 <- round(quantile(unique(unname(colSums(tempDTM))),
#                                probs = c(lowColsumDtm, highColsumDtm)),digits=0)
#  tempDTM1 <- tempDTM[ ,colSums(tempDTM) > thresholds1[1]] # Zipf law?
#  tempDTM1 <- tempDTM1[ ,colSums(tempDTM1) < thresholds1[2]]
#  topics1 <- predict(mod1,
#                     tempDTM1,
#                     method = "gibbs",
#                     iterations = 200,
#                     burnin = 180,
#                     cpus = 6) %>% tibble::as_tibble()
#  toTestTopics <- topics1[,]
#  topics_tibble <- cbind(dat$suicide$predIDAS,toTestTopics) %>% tibble::as_tibble()
#  names(topics_tibble)[1] <- "value"
#  if (FALSE){
#    topics_tibble <- topics_tibble %>%
#      dplyr::mutate(value = cut(value,
#                                breaks = quantile(value, probs=0:4/4, na.rm = TRUE),
#                                labels = c("Q1", "Q2", "Q3", "Q4"),  
#                                include.lowest = TRUE))
#  }
#  output <- topic_test(tibble::tibble(topics_tibble[,2:51]),
#                       tibble::tibble(topics_tibble[,"value"]),
#                       sumOutput) # tibble::tibble(aaa[[1]])
#
#
#}
