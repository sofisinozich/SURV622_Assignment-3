library(tidyverse)
library(withr)
library(magrittr)

relevant_tweets <- read_rds("Data/tokenized_relevant_tweets_sentiment.rds")
# Do we want to do further cleaning of these tweets or stick with these?

# Split tweets for hand-coding --------------------------------------------
# Randomly select four sets of 200
totaltweets<-with_seed(seed=30820383,sample.int(dim(relevant_tweets)[1], size=800))
# relevant_tweets %>% slice(totaltweets[1:200]) %>% select(status_id,text) %>% write_csv("Data/raw1.csv")
# relevant_tweets %>% slice(totaltweets[201:400]) %>% select(status_id,text) %>% write_csv("Data/raw2.csv")
# relevant_tweets %>% slice(totaltweets[401:600]) %>% select(status_id,text) %>% write_csv("Data/raw3.csv")
# relevant_tweets %>% slice(totaltweets[601:800]) %>% select(status_id,text) %>% write_csv("Data/raw4.csv")

# Recombine coded tweets --------------------------------------------------
# Once the sets are coded, reimport them for merging (don't want to keep text from the CSV due to encoding issues)

selected_ids <- relevant_tweets$status_id[totaltweets] %>% as.numeric

# Fix status_id values corrupted by Excel for coder 1 and coder 2
# Fortunately, this can be done by matching the text of a tweet in the assignment file
# to the text of a tweet in the handcoding files. 
# There are duplicate matches (some tweets have identical text), but this works out ok
# because dulicate text was always given the same code (good work us)

  raw_handcoding_assignments <- list.files("Data", "raw\\d\\.csv", full.names = TRUE) %>% 
    sort %>%
    map_dfr(read_csv, .id = 'coder', col_types = cols(.default = col_character()))
  
  handcodings <- list.files("Data", "handcode_\\d(plus|).csv", full.names = TRUE)  %>% 
    sort %>%
    map(read_csv, col_types = cols(.default = col_character())) %>%
    map( ~ rename_all(.x, list(~ str_replace(., "^(Sentiment|coded)$", "code")))) %>%
    bind_rows(.id = 'coder') %>%
    select(status_id, text, code, coder) %>%
    mutate(status_id = str_remove_all(status_id, "\\."))
  
  fixed_status_id_data <- list()
  for (coder_index in c(1,2)) {
    fixed_status_id_data[[paste0("coder_", coder_index)]] <- inner_join(
      x = handcodings %>% filter(coder == coder_index),
      y = raw_handcoding_assignments %>% filter(coder == coder_index) %>%
        select(text, status_id_assignment = status_id),
      by = "text") %>%
      distinct(status_id_assignment, text, .keep_all = TRUE) %>%
      mutate(status_id = status_id_assignment) %>%
      select(-status_id_assignment)
  }
  
  fixed_status_id_data <- reduce(fixed_status_id_data, bind_rows)
  
  handcodings %<>% filter(!coder %in% c(1,2)) %>%
    bind_rows(fixed_status_id_data)

  handcodings %<>% select(status_id, code, coder)

# Save to an RDS file 
  
  saveRDS(handcodings, "Data/Combined_Handcoded_Tweets.RDS")

# Create features and set up train/test sets ------------------------------
library(tidytext)

tweet_features <- relevant_tweets[totaltweets,] %>% 
  unnest_tokens(word, 'text', token="tweets", strip_punct=TRUE, strip_url=TRUE) %>% 
  anti_join(stop_words) %>% 
  group_by(status_id) %>% 
  count(word) %>% 
  spread(word,n) %>% 
  map_df(replace_na,replace=0)

# Remove features with fewer than 10 appearances
feature_counts <- tweet_features %>% select(-status_id) %>% colSums >= 10
tweet_features <- tweet_features[,c(TRUE,feature_counts)]
tweet_features %<>% select(-2) %>% mutate(status_id = as.numeric(status_id)) # Remove one column with an unreadable character and switch status_id to numeric

dim(tweet_features)
# Because some of the hand-coded tweets are just URLs (or something to that effect) we have fewer in the set. 
# Think that's alright.

tweet_features %<>% left_join(handcodings %>% mutate(status_id = as.numeric(status_id)),
                              by="status_id")

test_set <- with_seed(seed=9983,tweet_features %>% sample_frac(.2))
write_rds(test_set,"Data/test_set.rds")
# Please reserve this for the "final" test after you have done your crossvalidation and etc using the training data!

train_set <- tweet_features %>% filter(not(status_id %in% test_set$status_id))
write_rds(train_set,"Data/train_set.rds")