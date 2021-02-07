# In this script, we use API queries and web scraping to fetch data
library(httr) # for API queries
library(dplyr)
library(rvest) # for webscraping

# API query function
  
api <- function(base_url, endpoint, queries=list()) {
  # Prepare the URL
  url <- modify_url(base_url, path = endpoint)
  response <- GET(url, query=queries)
  
  # Track errors
  if (http_error(response)) {
    print(status_code(response))
    stop("Something went wrong.", call.=FALSE)
  }
  if (http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  # Extract content
  text<- content(response, as='text')
  
  # Convert content into dataframe
  df <- jsonlite::fromJSON(text)
  df[[2]]
  
  }

## Set parameters to get GDP by country in USD from World Bank API
base_url <- "http://api.worldbank.org"
endpoint <- "v2/country/all/indicator/NY.GDP.MKTP.CD" # Code for GDP (current$ indicator)
queries <- list(date=2019, format='json', page=1, per_page=500)

gdp <- api(base_url, endpoint, queries)

# Web scraping Carbon Brief Data

scraper <- function(url, selector, output="text", all_nodes=TRUE) {

    content <- read_html(url)
  
  if (all_nodes==TRUE) {
    extract <- content %>% html_nodes(selector)
  } else {
    extract <- content %>% html_node(selector) }
  
    if (output=="text") {
    result <- extract %>% html_text()
    } else if (output=="table") {
    result <- extract %>% html_table() 
    } else if (output=="attrs") {
      result <- extract %>% html_attrs()
    } else {
      result <- extract %>% html_attr(output)
    }
result
}

## set parameters
url <- "https://www.carbonbrief.org/coronavirus-tracking-how-the-worlds-green-recovery-plans-aim-to-cut-emissions"
url <- "https://carbonbrief.github.io/green-recovery-tracker/"

## Fetch table columns
selector <- "iframe  #greenRecoveryTracker"
green_recovery <- scraper(url, selector) 
green_recovery

## Must learn how to extract dynamic html 
