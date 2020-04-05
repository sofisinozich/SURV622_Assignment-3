library(magrittr)
library(tidyverse)
library(withr)

# Source shared_coded_tweets and handcoded_tweets from analyze_shared_coded_tweets.R.

#Write shared_coded_tweets to csv file, so I can manually assign "accurate" codes in Excel.
shared_coded_tweets2 <- shared_coded_tweets %>%
  mutate(status_id = paste0("ID_", status_id))

write.csv(shared_coded_tweets2, "shared_coded_tweets2.csv")

#Pull coded data back into R and remove the recodes so that all we're left with are the original
#coders' codes.
shared_coded_tweets2 <- read.csv("shared_coded_tweets2.csv", header = TRUE, sep = ",")

shared_coded_tweets3 <- shared_coded_tweets2 %>%
  mutate(status_id = str_remove(status_id, "ID_"),
         coder = as.character(coder))

shared_coded_tweets4 <- inner_join(x = shared_coded_tweets3,
                                   y = handcoded_tweets,
                                   by = c("status_id", "coder"))

#Accuracy of original codes
shared_accuracy_counts2 <- shared_coded_tweets4 %>%
  count(code.x, accurate)

shared_accuracy2 <- divide_by(shared_accuracy_counts2 %>% filter(accurate == code.x) %>% pull('n') %>% sum(),
                             shared_accuracy_counts2 %>% pull('n') %>% sum())
print(shared_accuracy2)

# Sensitivities of original code
shared_accuracy_counts2 %>%
  group_by(accurate) %>%
  mutate(sensitivity = n/sum(n)) %>%
  filter(accurate == code.x) %>%
  ungroup()

# Precisions of original code
shared_accuracy_counts2 %>%
  group_by(code.x) %>%
  mutate(precision = n/sum(n)) %>%
  filter(accurate == code.x) %>%
  ungroup()
