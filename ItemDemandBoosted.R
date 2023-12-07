library(vroom)
library(timetk)
library(tidyverse)
library(patchwork)
library(tidymodels)
library(forecast)
library(modeltime)


###########
############

## Define the workflow
item_recipe <- recipe(sales~., data=trainCsv) %>%
  step_date(date, features=c("dow", "month","doy", "year")) %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome=vars(sales)) %>%
  step_rm(date, item, store) %>%
  step_normalize(all_numeric_predictors())

boosted_model <- boost_tree(tree_depth=2,
                            trees=1000,
                            learn_rate=0.01) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")

boost_wf <- workflow() %>%
  add_recipe(item_recipe) %>%
  add_model(boosted_model)


## Double Loop over all store-item combos
for(s in 1:n.stores){
  for(i in 1:n.items){
    
    ## Subset the data
    train <- trainCsv %>%
      filter(store==s, item==i)
    test <- testCsv %>%
      filter(store==s, item==i)
    
    ## Fit the data and forecast
    fitted_wf <- boost_wf %>%
      fit(data=train)
    preds <- predict(fitted_wf, new_data=test) %>%
      bind_cols(test) %>%
      rename(sales=.pred) %>%
      select(id, sales)
    
    ## Save the results
    if(s==1 && i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds,
                             preds)
    }
    
  }
}

write_csv(all_preds, "./submission.csv")














