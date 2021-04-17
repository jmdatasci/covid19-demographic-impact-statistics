###
# Function to get census data from Census CSV file
# TODO aswin: Add the source of the data here

# Load the tidyverse library
library(tidyverse)
library(lubridate)

# Function that returns a data.frame that contains the census race data
get_mobility_data <- function(file_path, filter_start_date, filter_end_date) {
  
  # This is the census data 
  file_with_path <- paste0(file_path, 
                           'Region_Mobility_Report_CSVs/2020_US_Region_Mobility_Report.csv')
  # Read the dataframe skipping both the headers and convert it to code
  read_csv(file_with_path) -> df
  print(colnames(df))
  
  # This filters to keep only data that pertains to a state, all sub region
  # rows have been filtered out and the sub region column is eliminated
  df %>% filter(!is.na(iso_3166_2_code) ) -> df
  
  mobility_df <- data.frame(state = df$sub_region_1,  # Name of the state
                            date = df$date,  # Estimated total population of the state
                            retail_and_recreation_percent_change_from_baseline = df$retail_and_recreation_percent_change_from_baseline,
                            grocery_and_pharmacy_percent_change_from_baseline = df$grocery_and_pharmacy_percent_change_from_baseline,
                            parks_percent_change_from_baseline = df$parks_percent_change_from_baseline,
                            transit_stations_percent_change_from_baseline = df$transit_stations_percent_change_from_baseline,
                            workplaces_percent_change_from_baseline = df$workplaces_percent_change_from_baseline,
                            residential_percent_change_from_baseline = df$residential_percent_change_from_baseline)  
  

  # Convert the date column to date format
  mobility_df$date <- as.Date(mobility_df$date , format = "%m/%d/%y")
  
  # Filter to keep dates between these two values
  mobility_df %>% filter(date >= as.Date(filter_start_date) & date <= as.Date(filter_end_date) ) -> mobility_df
  
  #
  mobility_df %>% filter(is.na(retail_and_recreation_percent_change_from_baseline) | is.na(grocery_and_pharmacy_percent_change_from_baseline) | 
                           is.na(parks_percent_change_from_baseline) | 
                           is.na(transit_stations_percent_change_from_baseline) | is.na(workplaces_percent_change_from_baseline) |
                           is.na(residential_percent_change_from_baseline)) -> dirty_mob_df
 
  # Days with no data for transit visits
  dirty_mob_df %>% group_by(state) %>% 
    summarise(transit_missing = sum(is.na(transit_stations_percent_change_from_baseline))) -> transit_na
  
  # days with no data for park visits
  dirty_mob_df %>% group_by(state) %>% 
    summarise(parks_missing = sum(is.na(parks_percent_change_from_baseline))) -> parks_na 
  parks_na %>% left_join(transit_na, by='state') -> missing_df
  
  # Average residential mobility
  mobility_df %>% group_by(state) %>% 
    summarise(Recreation=mean(retail_and_recreation_percent_change_from_baseline, na.rm=TRUE), 
              Grocery=mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm=TRUE),
              Parks=mean(parks_percent_change_from_baseline, na.rm=TRUE),
              Transit=mean(transit_stations_percent_change_from_baseline, na.rm=TRUE),
              Workplace=mean(workplaces_percent_change_from_baseline, na.rm=TRUE),
              Residential=mean(residential_percent_change_from_baseline, na.rm=TRUE)
              ) %>% 
    select(c('Recreation', 'Grocery', 'Parks', 'Transit', 'Workplace', 
             'Residential', 'state')) -> sum_df
  
  sum_df %>% filter(is.na(Recreation) | is.na(Grocery) | is.na(Parks) | 
                      is.na(Transit) | is.na(Workplace) | is.na(Residential)) -> dirty_df
  
  return(list('mobility'=sum_df, 'missing'= missing_df))
}

# # 
# out = get_mobility_data(paste0(getwd(), '/data/external/'),
#                                  filter_start_date="2020-02-15",
#                                  filter_end_date = "2020-12-14")
