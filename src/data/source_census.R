###
# Function to get census data from Census CSV file
# TODO aswin: Add the source of the data here

# Load the tidyverse library
library(tidyverse)

# Function that returns a data.frame that contains the census race data
get_census_data <- function(file_path) {
  
  # This is the census data 
  file_with_path <- paste0(file_path, 
                           'ACSDP1Y2019.DP05_2021-03-26T085847/ACSDP1Y2019.DP05_data_with_overlays_2021-03-25T141408.csv')
  
  # First line has the codes. This will make data processing easier later
  read_csv(file_with_path, n_max = 0) %>% names() -> data_codes
  
  # Read the dataframe skipping both the headers and convert it to code
  read_csv(file_with_path, skip = 2, col_names=data_codes) -> df
  
  # Keep the columns of interest
  census_df <- data.frame(state = df$NAME,  # Name of the state
                          Total.Population = df$DP05_0001E,  # Estimated total population of the state
                          Percent.White = df$DP05_0037PE,  # Percent of white population in state
                          Percent.Hispanic.Or.Latino = df$DP05_0071PE)  # Percent of asian population in state
  
  return(census_df)
}

# Get the census data 
# census_data = get_census_data()

