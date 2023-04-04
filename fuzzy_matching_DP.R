# ADD EXPLAINATION OF CODE HERE
# Fuzzy matching of Data Catalogue Datasets against NHS X and other lists

#### example with data catalogue ####

library(tidyverse)
library(dplyr)
library(fuzzyjoin)

#load dataset 1
df1 <- read_csv("C:/Users/DPhillips/Department of Health and Social Care/Evidence and Analysis - Analytical Leadership/Statistics and Data Science/Data/Data catalogue/DHSC Data Catalogue/Back-End/Comparison Excels/catalogue_input.csv") %>%
  select(dataset = "Data Asset Title", keyword = "R Code Keyword", keyword2 = "R Code Keyword 2")

df1 <- na.omit(df1)

#transform all into lower case
df1 <- df1 %>%
  mutate(dataset = tolower(dataset)) %>%
  mutate(keyword = tolower(keyword)) %>%
  mutate(keyword2 = tolower(keyword2))

#load dataset 2
df2 <- read_csv("C:/Users/DPhillips/Department of Health and Social Care/Evidence and Analysis - Analytical Leadership/Statistics and Data Science/Data/Data catalogue/DHSC Data Catalogue/Back-End/Comparison Excels/NHSE_Compare.csv") %>%
  select(dataset = "Data Asset Name")

df2 <- na.omit(df2)

#transform all into lower case
df2 <- df2 %>%
  mutate(dataset = tolower(dataset))

#join datasets calculating distance between dataset name diffrences
joined_data <-  df1 %>%
  stringdist_left_join(df2, ignore_case=T, method="jw", distance_col="dist")

#TESTING FOR N/A
TEST <- joined_data %>% mutate(logic_test=is.na(joined_data$dataset.x))
TEST %>% filter(logic_test==TRUE)

#calculate smallest distance between text strings
joined_data <- joined_data %>%
  group_by(dataset.y) %>%
  mutate(dist2=min(dist))

#flag to check if smallest distance also satisfies the keyword condition
joined_data <- joined_data %>%
  mutate(keyword_match =
           if_else(dist==dist2 & str_detect(dataset.y, keyword)==TRUE & str_detect(dataset.y, keyword2)==TRUE, 1,
                   if_else(dist==dist2 & (str_detect(dataset.y, keyword)==TRUE | str_detect(dataset.y, keyword2)==TRUE), 0.5, 0)))

#177 potential good matches
joined_data %>% filter(keyword_match==1)

#best match with fuzzy match method only
best_match_no_keyword <- joined_data %>%
  filter(dist==dist2)

#best match with fuzzy match method with extra condition to have the keyword within the title
joined_data_keyword_match <- joined_data %>%
  filter(keyword_match == 1 | keyword_match == 0.5) %>%
  group_by(dataset.x) %>%
  mutate(dist3=min(dist))

best_match_with_keyword <- joined_data_keyword_match %>%
  filter(dist==dist3)

#### matches of Data Catalogue on NHS X list ####
#safe matches
A_safe_matches <- best_match_with_keyword %>%
  filter(dist==dist3 & keyword_match ==1)

#potential matches
B_potential_matches <- best_match_with_keyword %>%
  filter(dist==dist3 & keyword_match ==0.5 & dist<=0.2)

#most likely not matches
C_unlikely_matches <- best_match_with_keyword %>%
  filter(dist==dist3 & keyword_match ==0.5 & dist>0.2)

#non-matches DHSC
joined_data_keyword_match_DHSC <- joined_data_keyword_match %>%
  select(dataset = dataset.x) %>%
  distinct(dataset) %>%
  mutate (matched_keyword = 1)

unmatched_DHSC <- left_join(df1, joined_data_keyword_match_DHSC)

D_not_matches_DHSC <- unmatched_DHSC %>% filter(is.na(matched_keyword)==TRUE)
D_does_matches_DHSC <- unmatched_DHSC %>% filter(is.na(matched_keyword)==FALSE)

#check
#check
nrow(A_safe_matches %>% distinct(dataset.x)) +
  nrow(B_potential_matches %>% distinct(dataset.x)) +
  nrow(C_unlikely_matches %>% distinct(dataset.x)) + 
  nrow(D_not_matches_DHSC %>% distinct(dataset))

D_not_matches_DHSC <-  D_not_matches_DHSC %>% select(dataset.x = dataset)
D_not_matches_DHSC %>% ungroup() %>% distinct(dataset.x)
all_DHSC <- rbind(A_safe_matches, B_potential_matches, C_unlikely_matches, D_not_matches_DHSC)
all_DHSC %>% ungroup() %>% distinct(dataset.x)

#have two duplicates in last check, get rid of 'COMMISSIONING DATA SETS (129,130), TESTING SITE PREDICTED CAPACITY (112,113)'

#non-matches NHS X
joined_data_keyword_match_foundry <- joined_data_keyword_match %>%
  ungroup() %>%
  select(dataset = dataset.y) %>%
  distinct(dataset) %>%
  mutate (matched_keyword = 1)

unmatched_foundry <- left_join(df2, joined_data_keyword_match_foundry)

E_not_matches_foundry <- unmatched_foundry %>% filter(is.na(matched_keyword)==TRUE)

#check
nrow(A_safe_matches %>% distinct(dataset.y)) +
  nrow(B_potential_matches %>% distinct(dataset.y)) +
  nrow(C_unlikely_matches %>% distinct(dataset.y)) +
  nrow(E_not_matches_foundry %>% distinct(dataset))

E_not_matches_foundry <-  E_not_matches_foundry %>% select(dataset.y = dataset)
E_not_matches_foundry %>% ungroup() %>% distinct(dataset.y)
all_foundry <- rbind(A_safe_matches, B_potential_matches, C_unlikely_matches, E_not_matches_foundry)
all_foundry %>% ungroup() %>% distinct(dataset.y)

#have duplicates

#remove unused
rm(joined_data, joined_data_keyword_match, joined_data_keyword_match_DHSC, joined_data_keyword_match_foundry,
   unmatched_DHSC, unmatched_foundry, best_match_no_keyword, best_match_with_keyword)

#duplicate
test <- joined_data %>% filter(dataset.x == 'diagnostic waiting list')

test2 <- joined_data %>% filter(dataset.y == 'diagnostics waiting times and activity data collection')

#weird dataset names - DHSC, deloitte - other 1 word or two word names

#number of audits in NHS X list
test <- E_not_matches_foundry %>%
  mutate(test1 = if_else(str_detect(dataset, "audit")==TRUE, 1, 0)) %>%
  mutate(test2 = if_else(str_detect(dataset, "covid-19")==TRUE, 1, 0)) %>%
  mutate(test3 = if_else(str_detect(dataset, "surveillance")==TRUE, 1, 0)) %>%
  mutate(test4 = if_else(str_detect(dataset, "screening")==TRUE, 1, 0)) %>%
  mutate(test5 = if_else(str_detect(dataset, "gpes")==TRUE, 1, 0))

test %>% filter(test1==1) %>% nrow()
test %>% filter(test2==1) %>% nrow()
test %>% filter(test3==1) %>% nrow()
test %>% filter(test4==1) %>% nrow()
test %>% filter(test5==1) %>% nrow()

# Check random not matches in DHSC list
random_number <- sort(sample.int(245,10))
random_not_matches_DHSC <- D_not_matches_DHSC[random_number,1]

#Write to Excel
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard-16384",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(A_safe_matches)

