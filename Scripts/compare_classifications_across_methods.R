library(magrittr)
library(tidyverse)
library(readr)
library(readxl)

source("Scripts/load_dataviz_themes.R")

# Load all of the tweet classifications and the ML classifier ----

  ##_ Load the set of 800 tweets handcoded by the group
    handcoded_tweets <- read_rds("Data/Combined_Handcoded_Tweets.RDS")
  
  ##_ Load the full corpus of tokenized tweets with the dictionary-based sentiment scores
    tokenized_tweets_with_sentiment <- read_rds("Data/tokenized_relevant_tweets_sentiment.rds")
    tokenized_tweets <- read_rds("Data/tokenized_relevant_tweets.rds")
  
  ##_ Load the final trained model with its train and test sets
    load("Analysis Outputs/Trained Sentiment Classification Model.RData")
    
  
# Obtain all of the sentiment classifications for the test set from model evaluation -----
    
  test_set_classifications <- test_set %>%
    # Get the predictions from the model
    add_column('model_classification' = predict(trained_model, newdata = test_set)) %>%
    select(status_id, human_code = code, model_classification) %>%
    # Add the sentiment from the dictionary method
    left_join(y = tokenized_tweets_with_sentiment %>%
                select(status_id, SentimentGI) %>%
                mutate(status_id = as.numeric(status_id)),
              by = "status_id") %>%
    mutate(SentimentGI = case_when(
      SentimentGI > 0 ~ "Positive",
      SentimentGI == 0 ~ "Neutral",
      SentimentGI < 0 ~ "Negative"
    ))
    
    overall_accuracy <- test_set_classifications %>%
      summarize(
        model_accuracy = mean(human_code == model_classification),
        dictionary_accuracy = mean(human_code == SentimentGI)
      )
    
    category_recall_rates <- test_set_classifications %>%
      gather(key = 'classifier', value = 'classification',
             one_of(c("model_classification", "SentimentGI"))) %>%
      group_by(classifier, human_code) %>%
      summarize(
        recall = mean(human_code == classification)
      ) %>%
      ungroup()
    
    category_precision_rates <- test_set_classifications %>%
      gather(key = 'classifier', value = 'classification',
             one_of(c("model_classification", "SentimentGI"))) %>%
      group_by(classifier, classification) %>%
      summarize(
        precision = mean(human_code == classification)
      ) %>%
      ungroup()
    
    precision_recall_plot <- category_precision_rates %>%
      rename(category = classification) %>%
      left_join(y = category_recall_rates %>%
                  rename(category = human_code),
                by = c("classifier", "category")) %>%
      mutate(classifier = case_when(
        str_detect(classifier, "model") ~ "Random Forest Classifier",
        str_detect(classifier, "SentimentGI") ~ "Dictionary-based Method"
      )) %>%
      gather(key = "metric", value = "value",
             one_of(c("precision", "recall"))) %>%
      mutate(metric = str_to_title(metric)) %>%
      ggplot(aes(x = category, y = value, group = classifier, fill = classifier)) +
      coord_flip() +
      facet_wrap(~ metric) +
      geom_col(position = position_dodge(width = .8), width = 0.75) +
      geom_text(aes(label = scales::percent(value)), position = position_dodge(width = 0.8), hjust = -0.25) +
      scale_y_continuous(name = NULL, limits = c(0,1), breaks = seq(0,1,0.2), labels = scales::percent) +
      scale_x_discrete(name = "Sentiment Category") +
      scale_fill_manual(name = "Sentiment Classifier:", values = c("Random Forest Classifier" = "#000033",
                                   "Dictionary-based Method" = "#FF9933")) +
      labs(
        subtitle = glue::glue("Precision and recall of the two classification methods on the test set of {nrow(test_set)} hand-classified tweets not used for training models."),
        title = paste("The random forest model outperforms the dictionary-based method in terms of precision",
                      "and in terms of recall for 'neutral' tweets. However, recall for 'negative' tweets is comparatively poor.",
                      sep = "\n")
      )
    
    ggsave(filename = "Graphics/Precision-Recall by Category - Dictionary vs Random Forest.png",
           dpi = 550, units = 'in', width = 6.25, height = 3, scale = 1.5)
    
    file.
