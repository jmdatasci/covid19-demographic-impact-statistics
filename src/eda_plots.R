library("psych")
library("ggplot2")

get_covid_cases_histogram <- function(main_df, bins=30)
{
  covid_cases <- main_df %>%
    ggplot(aes(cases)) + geom_histogram(bins=bins) + 
    labs(title = "Distribution of COVID cases")
  
  covid_cases_100k <- main_df %>%
    ggplot(aes(Cases.Per.100k)) + geom_histogram(bins=bins) +
    labs (title = "Distribution of COVIDcases per \n 100K people") +
    theme(title = element_text(hjust = 0.5))
  
  
  return(list("cases" = covid_cases ,
              "cases_per_100k" = covid_cases_100k))
  
}

get_panels_plots <- function(main_df)
{
 main_df %>%
    select(Cases.Per.100k, Percent.Person.Of.Color, Percent.Hispanic.Or.Latino) %>%
    pairs.panels(method = "pearson",lm=FALSE, hist.col='magenta',
                 main="Scatterplot of Matrices \n Base Model - Response and Key Explanatory Variables",
                 line.main = 1.5) -> base_corr
  
 main_df %>%
    select(Cases.Per.100k, Days.Stay.At.Home, Days.Bar.Closed, Days.Restaurant.Closed, Days.With.Mask,Percent.Poverty) %>%
    pairs.panels(method = "pearson",lm=FALSE,hist.col="orange",
                 main="Scatterplot of Matrices \n State Policy and Socioeconomic Observations",
                 line.main = 1.5) -> state_corr
  
 main_df %>%
    select(Cases.Per.100k, Workplace, Residential, Parks, Transit, Recreation, Grocery) %>%
    pairs.panels(method = "pearson",lm=FALSE, hist.col='green', 
                 main="Scatterplot of Matrices \n Google Mobility Observations",
                 line.main = 1.5) ->  mobility_corr
  
  return(list("base" = base_corr ,
              "state" = state_corr,
              "mobility" = mobility_corr))
}
