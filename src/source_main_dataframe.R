# Connects with each data source and gets the individual dataframes
# It then merges the individual datafrmes to get the main data frame
library(tidyverse)

source('src/data/source_census.R')
source('src/data/source_nyt.R')
source('src/data/source_mobility.R')
source('src/data/source_state_policy.R')

# Function that returns a data.frame that contains the census race data
get_all_df <- function() {
  file_path = paste0(getwd(), '/data/external/') 
  file_path_processed = paste0(getwd(), '/data/processed/')
  
  # Get the census data frame from the CSV
  census_df = get_census_data(file_path)
  
  # Get the NYT dataset as a data frame
  nyt_df = get_nyt_data(file_path,
                        filter_start_date = "2020-02-15",
                        filter_end_date="2020-12-14")
  
  # Get the mobility data frame
  out_list =  get_mobility_data(file_path,
                                filter_start_date="2020-02-15",
                                filter_end_date = "2020-12-14")
  mobility_df = out_list$mobility
  
  # Get the state policy data frame
  state_policy_df = get_covid_policy_data(file_path_processed)
  
  # The analysis will be restricted to the lower 48 states and District of Columbia(CONUS)
  census_df %>% filter(state != 'Puerto Rico' & state != 'Alaska'  & state != 'Hawaii') -> census_df
  
  #  Combine all data frames using state
  # It is a left join with census data frame as the left data frame and hecne only keeps the CONUS
  census_df %>% 
    left_join(nyt_df, by="state") %>% 
    left_join(mobility_df, by="state") %>%
    left_join(state_policy_df, by = "state") -> main_df
  
  # Add two columns that operationalize Cases.Per.100k and Percent.Person.Of.Color
  main_df %>% mutate(
    Cases.Per.100k = (cases/Total.Population) * 100000,
    Percent.Person.Of.Color = 100-Percent.White) %>% 
    select(-c('fips', 'state.land.sq.mile', 'Percent.White')) -> main_df
  
  all_df = list("mobility"=mobility_df, "mobility_missing"=out_list$missing, 
                "census"=census_df, "policy"=state_policy_df, 
                "covid"=nyt_df, "main"=main_df)
  return(all_df)
}
# main_list = get_all_df()