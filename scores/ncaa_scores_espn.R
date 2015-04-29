###########################
# File: ncaa_scores_espn.R
# Description: Scrape NCAA scores from ESPN.com
# Date: 4/28/2015
# Author: Jake Russ
# Notes:
#
# To do:
###########################

# Load packages
library("httr")
library("rvest")
library("stringr")
library("lubridate")
library("dplyr")
library("magrittr")

# Working directory
dir <- getwd()

# Set user agent
uagent <- "Mozilla/5.0"

# ESPN main site address
base_url <- "http://scores.espn.go.com"

# Create path to NCAA basketball scoreboard
scores_url <- paste0(base_url, "/ncb/scoreboard")

# Season start and end dates
startdate <- as.Date("2014-11-14")
enddate   <- as.Date("2015-03-15")

# Scoreboard parameters
conf_id <- 50 # 50 displays all conferences

# Create vector of dates for the season
season <- seq.Date(from = startdate, to = enddate, by = "day")

# Initialize an empty data frame to collect all of the game ids and 
# boxscore urls.
boxscores <- data_frame()

# Loop through the pages to get all of the press release link urls
for (s in 1:length(season)){
  
  datestring <- season[s] %>% str_replace_all(pattern = "-", replacement = "")
  
  # Use POST to make the request, return response object
  res <- scores_url %>%
    POST(user_agent(uagent), query = list(date = datestring, confId = conf_id))
  
  # Extract the html document
  doc <- html(res)
  
  # Handle pages with no games
  games <- doc %>% 
    html_nodes(xpath = "//div[@class = 'team visitor']/div/p/span") %>%
    html_text()
  
  if (length(games) != 0){
    
    # Get the away team names
    away_teams <- doc %>% 
      html_nodes(xpath = "//div[@class = 'team visitor']/div/p/span") %>%
      html_text() %>%
      str_trim() %>%
      data_frame(team = ., status = "Away") %>%
      filter(!(str_detect(team, pattern = "[0-9]")))
    
    # Get the home team names
    home_teams <- doc %>% 
      html_nodes(xpath = "//div[@class = 'team home']/div/p/span") %>%
      html_text() %>%
      str_trim() %>%
      data_frame(team = ., status = "Home") %>%
      filter(!(str_detect(team, pattern = "[0-9]")))
    
    # Collect ids and urls into temporary data frame.
    tmp <- bind_rows(away_teams, home_teams) %>%
      mutate(date = season[s])
    
    # Store the loop results
    boxscores <- bind_rows(boxscores, tmp)
    
  } # Else do nothing and keep going through the loop
  
  # Be polite to server; between url calls, have the scraper rest for 
  # a few seconds. Adds time to the loop but you don't want to overload 
  # the website's server.
  Sys.sleep(time = sample(2:4, 1))
  
}


# Store a copy of boxscores
boxscores %>%
  write.csv(paste0(dir, "/scores/ncaa_scores_espn.csv"), row.names = FALSE)

