#Extract Foundry data dictionary: https://data.england.nhs.uk/covid-19/

#### load libraries ####
#libraries
library(tidyverse)
library(rvest)

#### webscrape NHS foundry data list ####
#specify webpage url
foundry_url <- "https://data.england.nhs.uk/covid-19/"

#read in text from url
foundry_read <- read_html(foundry_url)

#extract specific text from read text to get dataset name
dataset_name <- foundry_read %>%
  html_nodes("h3") %>%
  html_text()

#Splits string into title and owner
dataset_name <- as.data.frame(str_split_fixed(dataset_name,'\n',3)) %>% 
  select('title' = V2, 'owner'= V3)


#extract specific text from read text to get dataset source/ descr
description <- foundry_read %>%
  html_nodes("p") %>%
  html_text()



#removes blank rows
description <- description[!grepl("\n ",description)]

#removes first 3 intro lines at top of page
description <- description[4:62]
  
#remove unused data objects
rm(foundry_url, foundry_read)

#combined datasets
foundry_dictionary <- cbind(dataset_name, description)

#remove whitespace from text
foundry_dictionary$title <- trimws(foundry_dictionary$title)
foundry_dictionary$owner <- trimws(foundry_dictionary$owner)

#remove unused data
rm(dataset_name, description)

#write table into csv
write.csv(foundry_dictionary, paste("C:/Users/TDarwent/Department of Health and Social Care/Evidence and Analysis - DHSC Data Catalogue/Back-End/Dataset Excel Files/foundry_datasets.csv"),row.names=FALSE)





