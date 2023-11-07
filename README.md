# ml_word_visualisations

## Structure of repository

Folder [lda](./lda) consists of the files to run lda experiments

1.  [utils.R](./lda/utils.R) which includes functions for testing lda topics

2.  [main.R](./lda/main.R) which includes methods for creating and testing lda models from different libraries

Folder [data](./data) is there to store your data. <br>

Folder [results](./results) stores the results of your experiment

## Example

### LDA

#### 0. Preparation

#### 0.1 Set (hyper)parameters

**Data**

```R
data_dir <- "./data/depression_anxiety_cleaned.csv"
data_col <- "dep_all_words"
id_col <- "unique_id"
group_col <- "minidep_diagnose" # now necessary, but only used for t-test
```

**Document Term Matrix**

```R
ngram_window <- c(1,3)
stopwords <- stopwords::stopwords("en", source = "snowball")
removalword <- "" # just possible with one word
occ_rate <- 0
removal_num_most <- 1
removal_num_least <- 0
removal_mode <- "absolute" # "relative"
split <- 1
```

**LDA**

```R
model_type <- "textmineR" # or "mallet"
num_topics <- 20
num_top_words <- 10
num_train_iterations <- 2000
num_pred_iterations <- 200
pred_mode <- "function" # or "custom" for mallet
```

**Analysis**

```R
cor_var <- "PHQtot" # grouping variable for t-test, to be predicted variable for other
control_vars <- c("PHQtot") # vector of variables to control analysis with if test_method is linear_regression
test_method <- "textTrain_regression" # linear_regression, logistic_regression, t-test
```

**Miscellaneous**

```R
seed <- 1234
```

##### 0.2 Create directory to save all computations

All objects created within the pipeline are created in the directory below. These include

-   Document Term Matrix

-   model

-   predictions

-   analysis results

```R
save_dir <- paste0("./results/",
            model_type,"_",
            data_col, "_",
            num_topics, 
            "_most_",removal_num_most, 
            "_least_", removal_num_least, 
            "_occ_", occ_rate, 
            "_pred_", mode)
```

##### 0.3 Imports

```R
library(textmineR)
library(tidyverse)
library(dplyr)
library(textmineR)
library(mallet)
library(rJava)
library(tokenizers)
library(text2vec)
library(quanteda)
source("./lda/main.R")
```

#### 1. Compute Document Term Matrix

```R
dtms <- get_dtm(data_dir = data_dir,
                id_col = id_col,
                data_col = data_col,
                group_var = group_col,
                ngram_window = ngram_window,
                stopwords = stopwords,
                removalword = removalword,
                occ_rate = occ_rate,
                removal_mode = removal_mode,
                removal_rate_most = removal_num_most,
                removal_rate_least = removal_num_least,
                split=split,
                seed=seed,
                save_dir=save_dir)
```

#### 2. Create LDA Model

```R
model <- get_lda_model(model_type=model_type,
                        dtm=dtms$train_dtm,
                        num_topics=num_topics,
                        num_top_words=num_top_words,
                        num_iterations = num_train_iterations,
                        seed=seed,
                        save_dir=save_dir)
```

#### 3. Create Predictions

```R
preds <- get_lda_preds(model = model,
                        num_iterations=num_pred_iterations,
                        data = dtms$train_data,
                        dtm = dtms$train_dtm,
                        group_var = c(cor_var),
                        seed=seed,
                        mode=mode,
                        save_dir = save_dir)
```

#### 4. Analysis

##### 4.1 textTrain_regression

```R
test <- get_lda_test(model=model,
                    preds=preds,
                    group_var = cor_var,
                    control_vars = control_vars,
                    test_method = "textTrain_regression",
                    seed=seed,
                    save_dir=save_dir)
```

##### 4.2 Linear Regression

```R
test <- get_lda_test(model=model,
                    preds=preds,
                    group_var = cor_var,
                    control_vars = control_vars,
                    test_method = "linear_regression",
                    seed=seed,
                    save_dir=save_dir)
```
