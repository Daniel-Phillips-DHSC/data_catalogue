#Code to match onto existing data

library(tidyverse)
library(dplyr)
library(xlsx)

current_owners <- read_csv("C:/Users/DPhillips/Department of Health and Social Care/Evidence and Analysis - Analytical Leadership/Statistics and Data Science/Data/Data catalogue/DHSC Data Catalogue/Back-End/Matching New Owners on to PHE/UKHSA_full.csv")

new_owners <- read_csv("C:/Users/DPhillips/Department of Health and Social Care/Evidence and Analysis - Analytical Leadership/Statistics and Data Science/Data/Data catalogue/DHSC Data Catalogue/Back-End/Matching New Owners on to PHE/UKHSA_matched.csv")

#current_owners <- current_owners %>%
 #mutate(DataAssetName = tolower(DataAssetName))

owners <- left_join(current_owners, new_owners, by = "DataAssetName")

owners <- owners %>% filter(Flag == 1) 

#for (j in 1:(nrow(owners))) {
  
  # if flag is 1 then replace the word with the ngram 
  #if(is.na(owners$Flag[j])) {owners$Flag[j] = FALSE} else {if(owners$Flag[j] == 1) {
    
   #owners$Owner[j] <- owners$NewOwner[j]
 
  #}}
#}

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard-16384",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(owners)

write.xlsx(owners, file = "C:/Users/DPhillips/Department of Health and Social Care/Evidence and Analysis - Analytical Leadership/Statistics and Data Science/Data/Data catalogue/DHSC Data Catalogue/Back-End/Matching New Owners on to PHE/UKHSA.xlsx",
           sheetName = "UKHSA", append = FALSE)

write.csv(owners,"C:/Users/DPhillips/Department of Health and Social Care/Evidence and Analysis - Analytical Leadership/Statistics and Data Science/Data/Data catalogue/DHSC Data Catalogue/Back-End/Matching New Owners on to PHE/UKHSA.csv", row.names = FALSE)

