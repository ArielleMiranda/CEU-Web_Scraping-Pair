########################################################################################################################

## Arielle Miranda & Veronika Palotai
## Coding 2: Web Scraping

## Get the list, open every university and get the basic info table from US News (https://www.usnews.com/best-colleges/rankings/national-universities)

# INSTRUCTIONS:
# Find a website, analyze the data source F12 network tab
# Find a json data source of the website, and make the POST or GET request
# Write a function that return with the processed data
# Save it, with saverds

########################################################################################################################

### LOAD LIBRARIES
library(rvest)
library(jsonlite)
library(dplyr)
library(tidyr)

t <- fromJSON('https://d1ep1269cs49qn.cloudfront.net/universities.json')
page_urls <- t$page_url 


### CREATE FUNCTION TO GET THE CONTENTS OF THE "TABLE" WITH 8 ROWS ON EACH PAGE
## some have rows <8 which makes making a wide-format table with pre-defined column names unconventional
## this code works for pages with missing information because we are building a long-table with key-value pairs

info_table <- function(i, url) {
  m <- read_html(url) %>%
    html_node(paste0(".section-uni-about__info:nth-child(",i,")")) %>% 
    html_text()%>% 
    trimws() %>%
    as.data.frame() 
  m <- separate(m, col=., into = c("key","value"), sep = ": " )
  return(m)
}

## CREATE A FUNCTION TO BIND THE DATA INTO A SINGLE TABLE WITH `key` & `value` PAIRS
info_table_list <- function(u) {
  rbindlist(lapply(1:8, info_table, url = u))
}

## CREATE A TABLE THAT CALLS THE FUNCTION `info_table_list` AND ADD COLUMN FOR NAME
## The result will be a tidy table - ID is `Name` and `key``
uni_info <- function(url) {
  Name <- 
    read_html(url) %>% 
    html_nodes('.current-item')%>%
    html_text()
  
  s <- info_table_list(url)
  return(cbind(Name, s))
}

## USE `rbindlist` AND `lapply` TO GET INFO ON ALL UNI, CONVERT LONG-FORMAT TO WIDE-FORMAT TABLE
c <- rbindlist(lapply(page_urls, uni_info))
Uni_ranking <- rbindlist(lapply(page_urls, uni_info)) %>%
  filter(!is.na(key)) %>% 
  unique() %>% 
  spread(key, value)


## SAVE DATA
saveRDS(Uni_ranking, "Uni_ranking.rds")
write.csv(Uni_ranking, "Uni_ranking.csv")
