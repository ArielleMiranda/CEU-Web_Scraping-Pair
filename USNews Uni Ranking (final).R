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
library(data.table)

set_page <- function(my_page){
  url <- paste0('https://www.usnews.com/best-colleges/api/search?_sort=rank&_sortDirection=asc&_page=',my_page,'&schoolType=national-universities')
  t <- fromJSON(url, flatten = T)
  u <- t[[14]][["items"]]
  return(u)
}  
page_urls<-rbindlist(lapply(1:20,set_page)) %>%
  transmute(url = paste0("https://www.usnews.com/best-colleges/",institution.urlName,"-",institution.primaryKey))

page_urls<-page_urls$url
## Few pages are causing errors due to different element names, these will be excluded
page_urls<-page_urls[page_urls != "https://www.usnews.com/best-colleges/st-johns-university-new-york-2823"]
page_urls<-page_urls[page_urls != "https://www.usnews.com/best-colleges/bethel-university-9058"]
  

### CREATE FUNCTION TO GET THE CONTENTS OF THE GENERAL INFORMATION TABLE FROM EACH PAGE
## the number of rows varies which makes making a wide-format table with pre-defined column names unconventional
## this code works for pages with missing information because we are building a long-table with key-value pairs

get_general_info_table <- function(i, url) {
  key <- read_html(url) %>%
    html_node(paste0(".faZOTI:nth-child(",i,") .itSAGF")) %>% 
    html_text()
  value <- read_html(url) %>%
    html_node(paste0(".faZOTI:nth-child(",i,") .cSKkuo")) %>% 
    html_text()
  m <- data.frame("key"= key, "value"= value)
  return(m)
}

## CREATE A FUNCTION TO BIND THE DATA INTO A SINGLE TABLE WITH `key` & `value` PAIRS
get_general_info <- function(u) {
  rbindlist(lapply(1:6, get_general_info_table, url = u))
}

## CREATE A TABLE THAT CALLS THE FUNCTION `info_table_list` AND ADD COLUMN FOR NAME
## The result will be a tidy table - ID is `Name` and `key``
uni_info <- function(url) {
  print(url)
  name <- 
    read_html(url) %>% 
    html_nodes('.jhCUMS')%>%
    html_text()
  
  address <- 
    read_html(url) %>% 
    html_nodes('.SJOqL')%>%
    html_text()
  a <- address[1] %>% strsplit("\\|" ) %>% unlist()
  address <- a[1]
  contact <- a[2]
  
  rank <- 
    read_html(url) %>% 
    html_nodes('.eKbYXJ')%>%
    html_text()
  
  overall_score <- 
    read_html(url) %>% 
    html_nodes('.fkFhvx')%>%
    html_text()
  
  general_info <- get_general_info(url) %>% 
    filter(!is.na(key)) %>% 
    unique() %>% 
    spread(key, value)
  
  main_info <- data.frame('Name'= name, 'Address'= address, 'Contact'=contact, 'Rank'=rank, 'Overall Score'=overall_score) %>% 
    unique()
  
  return(cbind(main_info,general_info))
}

## USE `rbindlist` AND `lapply` TO GET INFO ON ALL UNI, CONVERT LONG-FORMAT TO WIDE-FORMAT TABLE
uni_ranking <- rbindlist(lapply(page_urls, uni_info))

## SAVE DATA
saveRDS(uni_ranking, "Uni_ranking.rds")
write.csv(uni_ranking, "Uni_ranking.csv")