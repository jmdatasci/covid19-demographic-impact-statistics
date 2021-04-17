###
# Function to get census data from Census CSV file
# TODO aswin: Add the source of the data here

# Load the tidyverse library
library(tidyverse)
library(lubridate)
library(ggplot2)

# Function that returns a data.frame that contains the census race data
get_nyt_data <- function(file_path, filter_start_date, filter_end_date, 
                         threshold=100) {
  
  # This is the census data 
  file_with_path <- paste0(file_path, '/covid-19-data/us-states.csv')
  
  # Read the dataframe skipping both the headers and convert it to code
  read_csv(file_with_path) -> nyt_df
  nyt_df$date <- as.Date(nyt_df$date , format = "%m/%d/%y")
  
  # Filter to keep dates below this value
  nyt_df %>% 
    filter(date >= as.Date(filter_start_date) & 
             date <= as.Date(filter_end_date) ) %>% 
    group_by(state) %>% filter(date == max(date)) %>% 
    select(c('state', 'cases')) -> max_date_df
  
  return(max_date_df)
}

# # # Get the NYT data
# nyt_df = get_nyt_data(paste0(getwd(), '/data/external/'),
#                       filter_start_date="2020-01-01",
#                       filter_end_date = "2020-12-14",
#                       threshold = 100)
