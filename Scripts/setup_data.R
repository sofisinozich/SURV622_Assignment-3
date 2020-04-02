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

# Some fixing because Rachael and I have messed up status IDs... :| 
handcode1 <- read_csv("Data/handcode_1.csv") %>% 
  select(-text, code = coded) %>% 
  mutate(status_id = selected_ids[1:200])

handcode2 <- read_csv("Data/handcode_2plus.csv") %>% 
  select(-text, code = Sentiment) %>% 
  mutate(status_id = selected_ids[201:400])

coded <- bind_rows(handcode1,
                   handcode2,
                  read_csv("Data/handcode_3.csv") %>% select(-text),
                  read_csv("Data/handcode_4.csv") %>% select(-text), .id = "coder") 

coded %<>% select(status_id, code, coder)

# Save to an RDS file 
  
  saveRDS(coded, "Data/Combined_Handcoded_Tweets.RDS")

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

tweet_features %<>% left_join(coded,by="status_id")

test_set <- with_seed(seed=9983,tweet_features %>% sample_frac(.2))
write_rds(test_set,"Data/test_set.rds")
# Please reserve this for the "final" test after you have done your crossvalidation and etc using the training data!

train_set <- tweet_features %>% filter(not(status_id %in% test_set$status_id))
write_rds(train_set,"Data/train_set.rds")