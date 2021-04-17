

get_models <- function(main_df)
{
# Simplest of all models
model_1 = lm(Cases.Per.100k ~ 1 + Percent.Person.Of.Color + Percent.Hispanic.Or.Latino, data=main_df)

# Model 2: Parsimony with a good explanation
model_2 = lm(Cases.Per.100k ~ 1 + Percent.Person.Of.Color + Percent.Hispanic.Or.Latino + Workplace + sqrt(Days.Stay.At.Home), data=main_df) 

# Complex model with a lot of stuff going on
model_3 = lm(Cases.Per.100k ~ 1 + Percent.Person.Of.Color + Percent.Hispanic.Or.Latino + Workplace + sqrt(Days.Stay.At.Home) + sqrt(Days.With.Mask) +  I(Residential**2 + Grocery**2 + Recreation**2 + Parks**2 + Transit**2)  + I(sqrt(Days.Gym.Closed) + sqrt(Days.Bar.Closed) + sqrt(Days.Restaurant.Closed) + sqrt(Days.Nonessential.Closed)), data = main_df)

return(list("model_1"=model_1, "model_2"=model_2, "model_3"=model_3))
}

get_stargazer <- function(main_df){
  
  models = get_models(main_df)
  model_1 = models$model_1
  model_2 = models$model_2
  model_3 = models$model_3
  
  stargazer(title = "SARS Coronavirus-19 Cases per 100,000 Residents Covariate Regression Table using Classic Standard Errors", header=FALSE,
            model_1, model_2, model_3,report=('vcs*p'),
            column.labels = c("Base Model", "Efficient Model", "Best Fit Model"),
            dep.var.labels = "Confirmed Coronavirus-19 Cases per 100,000 Residents",
            covariate.labels = c("Intercept", "State - Percentage of Residents People of Color",
                                 "State - Percentage of Residents Hispanic or Latino",
                                 "Workplace Mobility Impact",
                                 "Square Root of Days State was Under Stay at Home Order",
                                 "Square Root of Days with Mask Mandate",
                                 "Sum of Squares, All Other Mobility Scores",
                                 "Sum of Square Roots, All Other Closures"),
             type="latex", single.row = FALSE,intercept.bottom = FALSE,digits=3,
            omit.stat="f", float.env = "sidewaystable") -> p
  return(p)
}
