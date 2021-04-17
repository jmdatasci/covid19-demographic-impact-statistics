# Load the tidyverse library
library(tidyverse)
library(lubridate)

# Function that returns a data.frame that contains the census race data
#, filter_start_date, filter_end_date
get_covid_policy_data <- function(file_path_cp) {
  # This is the State Policy Data processed data file
  # see scripts in <Folder>
  # raw data in <Folder>
  file_with_path <- paste0(file_path_cp, 'state_policy_attrs.csv')
  # Read the dataframe
  read_csv(file_with_path) -> cp_df
  cp_df<-data.frame(cp_df)
#  print(colnames(cp_df))
  cp_df <- cp_df %>%
    select(
      state,
      statecode,
      fips,
      days.stay.at.home,
      days.with.mask,
      days.restaurant.closed,
      days.bar.closed,
      days.gym.closed,
      days.nonesst.bus.closed,
      state.land.sq.mile,
      percent.poverty,
      religious.event.exempt
    ) %>% 
    rename(Days.Stay.At.Home=days.stay.at.home,
           Days.With.Mask=days.with.mask,
           Days.Restaurant.Closed=days.restaurant.closed,
           Days.Bar.Closed=days.bar.closed,
           Days.Gym.Closed=days.gym.closed,
           Days.Nonessential.Closed=days.nonesst.bus.closed,
           Religious.Event.Exempt=religious.event.exempt,
           Percent.Poverty=percent.poverty)  
  
  return(cp_df)
}
