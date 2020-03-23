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
coded <- bind_rows(read_csv("Data/raw1_handcoded.csv") %>% select(-text),
          read_csv("Data/raw2_handcoded.csv") %>% select(-text),
          read_csv("Data/raw3_handcoded.csv") %>% select(-text),
          read_csv("Data/raw4_handcoded.csv") %>% select(-text))

relevant_tweets_coded <- relevant_tweets %>% 
  right_join(coded,by="status_id")

# Hand check that the merge worked correctly and that codings seem to match up
# Change variable name if we change the variable name for the codes
with_seed(seed=30820383,relevant_tweets_coded %>% select(status_id,text,code) %>% sample_n(20))

write_rds(relevant_tweets_coded,path="Data/tokenized_relevant_tweets_handcoded.rds")



