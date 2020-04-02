library(magrittr)
library(tidyverse)
library(withr)

# Import the original handcodings and get the tweet text
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

  
  handcoded_tweets %<>%
    # Add a text prefix to the status ID
    # to keep Excel from converting the IDs to lossy abbreviations
    # (shakes fist angrily at the sky)
    mutate(status_id = paste0("ID_", status_id)) %>%
    rename(original_coder = coder) %>%
    mutate(original_coder = case_when(
      original_coder == 1 ~ "Sofi", original_coder == 2 ~ "Rachael",
      original_coder == 3 ~ "Ben", original_coder == 4 ~ "Victoria"
    ))

# Sample 50 tweets from each set
  recoding_sample <- with_seed(
    seed = 1999,
    handcoded_tweets %>%
      group_by(original_coder) %>%
      sample_n(size = 50) %>%
      ungroup()
  )
  
  # For each coder, create a copy of the recoding sample
  # and remove from that sample the 50 tweets that they already coded
  recoding_sets <- map(.x = c("Sofi", "Rachael", "Ben", "Victoria") %>% setNames(., .),
      .f = function(coder) {
        filter(recoding_sample, original_coder != coder) %>%
          select(-original_coder) %>%
          mutate(code = NA_integer_)
      })
  
# Write the recoding sets to separate CSV files
  if (!dir.exists("Data/Recodings")) {dir.create("Data/Recodings")}
  
  map2(.x = recoding_sets, .y = names(recoding_sets),
       .f = ~ write_csv(.x, path = glue::glue("Data/Recodings/Recoding_{.y}.csv")))
  