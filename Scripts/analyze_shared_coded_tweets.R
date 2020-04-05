library(magrittr)
library(tidyverse)
library(withr)

# 01 - Import the original handcodings and get the tweet text ----
  handcoded_tweets <- readRDS("Data/Combined_Handcoded_Tweets.RDS")
  relevant_tweets <- read_rds("Data/tokenized_relevant_tweets_sentiment.rds")

  # Add the tweet text and context information to handcoded tweets 
    handcoded_tweets %<>%
      mutate(status_id = as.character(status_id)) %>%
      left_join(
        y = relevant_tweets %>% select(status_id, text,
                                       is_quote, is_retweet, quoted_text, retweet_text),
        by = "status_id"
      )
  
# 02 - Import the recoded tweets ----
    recodings <- list.files('Data/Recodings', full.names = TRUE) %>% 
      setNames(., str_extract(., "Sofi|Rachael|Ben|Victoria")) %>%
      map(read_csv, col_types = cols(.default = col_character())) %>%
      map_dfr(function(df) {
        if ("Sentiment" %in% colnames(df)) {
          select(df, -code) %>% rename(code = Sentiment)
        } else {
          df
        }
      },
      .id = "coder") %>%
      # Switch coder names to coder numbers, in order to align with formatting in other data
      mutate(coder = case_when(coder == "Sofi" ~ 1, coder == "Rachael" ~ 2, coder == "Ben" ~ 3, coder == "Victoria" ~ 4) %>%
               as.character()) %>%
      # Remove status ID prefix meant to prevent Excel corrupting the IDs
      mutate(status_id = str_remove(status_id, "ID_")) %>%
      select(status_id, coder, code)
  
# 03 - Combined with the original codings of tweets done by the original assignees ----
  shared_coded_tweets <- semi_join(x = handcoded_tweets,
            y = recodings,
            by = "status_id") %>%
    anti_join(y = recodings,
              by = c("status_id", "coder")) %>%
    select(one_of(colnames(recodings))) %>%
    bind_rows(recodings)
    
# 04 - Basic counts ----
    shared_coded_tweets %>%
      mutate(coder = paste0("coder_", coder)) %>%
      spread(key = "coder", value = "code")
    
  shared_coded_tweets %>%
      filter(!is.na(code))
  
# 05 - Inferential stats ----
  
  # Fit a multinomial logistic regression
  # and use a likelihood ratio test
  # to test whether any of the coefficients for coders are nonzero
    mnom_logit_model <- nnet::multinom(
      data = shared_coded_tweets,
      formula = code ~ -1 + coder
    )
    
    lmtest::lrtest(mnom_logit_model)
    
  # Estimate standard reliability metrics
    library(irr)
    
    coded_tweet_matrix <- shared_coded_tweets %>%
      mutate(coder = paste0("coder_", coder),
             code = paste0("code_", code)) %>%
      spread(key = "coder", value = "code") %>%
      {m <- as.matrix(select(., -status_id))
       rownames(m) <- .[['status_id']]
       m}
    
    # Fleiss' Kappa
     irr::kappam.fleiss(coded_tweet_matrix, exact = FALSE, detail = TRUE)
     
    # Light's Kappa
     irr::kappam.light(coded_tweet_matrix)
     