library(tidyverse)
library(withr)

relevant_tweets <- read_rds("Data/tokenized_relevant_tweets_sentiment.rds")
# Do we want to do further cleaning of these tweets or stick with these?

# Split tweets for hand-coding --------------------------------------------
# Randomly select four sets of 200
totaltweets<-with_seed(seed=30820383,sample.int(dim(relevant_tweets)[1], size=800))
relevant_tweets %>% slice(totaltweets[1:200]) %>% select(status_id,text) %>% write_csv("Data/raw1.csv")
relevant_tweets %>% slice(totaltweets[201:400]) %>% select(status_id,text) %>% write_csv("Data/raw2.csv")
relevant_tweets %>% slice(totaltweets[401:600]) %>% select(status_id,text) %>% write_csv("Data/raw3.csv")
relevant_tweets %>% slice(totaltweets[601:800]) %>% select(status_id,text) %>% write_csv("Data/raw4.csv")


# Recombine coded tweets --------------------------------------------------
# Once the sets are coded, reimport them for merging (don't want to keep text from the CSV due to encoding issues)
coded1 <- read_csv("Data/raw1_coded.csv") %>% select(-text)
coded2 <- read_csv("Data/raw2_coded.csv") %>% select(-text)
coded3 <- read_csv("Data/raw3_coded.csv") %>% select(-text)
coded4 <- read_csv("Data/raw4_coded.csv") %>% select(-text)

relevant_tweets_coded <- relevant_tweets %>% 
  left_join(coded1,by="status_id") %>% 
  left_join(coded2,by="status_id") %>% 
  left_join(coded3,by="status_id") %>% 
  left_join(coded4,by="status_id")

# Hand-check the merge worked on a few rows -------------------------------



