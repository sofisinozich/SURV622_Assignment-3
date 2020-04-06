# Get metrics for our dictionary-based sentiment analysis

test_set <- read_rds("Data/test_set.rds") %>%
  # Change outcome variable to a factor
  mutate(code = factor(code, levels = c(1,2,3,4),
                       labels = c("Positive", "Neutral", "Negative", "Irrelevant")))

relevant_tweets <- read_rds("Data/tokenized_relevant_tweets_sentiment.rds")

outcomes <- test_set %>% 
  select(status_id,code) %>% 
  left_join(relevant_tweets %>% 
              select(status_id,SentimentGI) %>% 
              mutate(status_id = as.numeric(status_id))) %>% 
  mutate(SentimentGI_cat = case_when(SentimentGI < 0 ~ "Negative",
                                     SentimentGI == 0 ~ "Neutral",
                                     SentimentGI > 0 ~ "Positive")) %>% 
  mutate(correct = code == SentimentGI_cat)

outcomes2 <- outcomes %>% group_by(code,SentimentGI_cat,correct) %>% count

metrics <- tibble(accuracy = 0,
                  precision_pos = 0, precision_neu = 0, precision_neg = 0, precision_irr = 0,
                  recall_pos = 0, recall_neu = 0, recall_neg = 0, recall_irr = 0)

metrics$accuracy<-outcomes %>% group_by(correct) %>% count %>% select(n) %>% prop.table %>% extract(2,2)

metrics[1,2:4]<-
  outcomes2 %>% group_by(SentimentGI_cat) %>% mutate(percent = n/sum(n)) %>% filter(correct == TRUE) %>% select(percent) %>% t %>% extract(2,1:3) %>% as.numeric

metrics[1,6:8] <- 
  outcomes2 %>% group_by(code) %>% mutate(percent = n/sum(n)) %>% filter(SentimentGI_cat==code) %>% select(percent) %>% t %>% extract(2,1:3) %>% as.numeric()

