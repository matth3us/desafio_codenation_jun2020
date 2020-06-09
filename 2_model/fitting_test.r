library(tidyverse)

train <- 
  read_csv("./0_raw/train.csv")

test <- 
  read_csv("./0_raw/test.csv")

features <- 
  readRDS('./2_model/final_features.rds')

fill_numeric_na <- function(x){
  
}

# new_train <- 
#   train[c(features$features, 'NU_NOTA_MT')] %>%
#   mutate_at(vars(c('NU_NOTA_CH', 'NU_NOTA_CN', 'NU_NOTA_LC', 'NU_NOTA_REDACAO', 'NU_NOTA_MT')), function(x){as.integer(ifelse(is.na(x), 0, x))}) %>%
#   mutate_at(vars(c('TP_PRESENCA_CH', 'TP_PRESENCA_CN', 'TP_PRESENCA_LC', 'TP_STATUS_REDACAO')), function(x){as.factor(ifelse(is.na(x), -1, x))})
# 
# new_test <- 
#   test[c(features$features, 'NU_INSCRICAO')] %>%
#   mutate_at(vars(c('NU_NOTA_CH', 'NU_NOTA_CN', 'NU_NOTA_LC', 'NU_NOTA_REDACAO')), 
#             function(x){as.integer(ifelse(is.na(x), 0, x))}) %>%
#   mutate_at(vars(c('TP_PRESENCA_CH', 'TP_PRESENCA_CN', 'TP_PRESENCA_LC', 'TP_STATUS_REDACAO')), function(x){as.factor(ifelse(is.na(x), -1, x))}) #%>%
#   #mutate(NU_NOTA_MT = NA)

new_train <- 
  train[c('NU_NOTA_CH', 'NU_NOTA_CN', 'NU_NOTA_LC', 'NU_NOTA_REDACAO', 'NU_NOTA_MT')] %>%
  mutate_at(vars(c('NU_NOTA_CH', 'NU_NOTA_CN', 'NU_NOTA_LC', 'NU_NOTA_REDACAO', 'NU_NOTA_MT')), 
                         function(x){as.integer(ifelse(is.na(x), -1, x))})

new_test <- 
  test[c('NU_NOTA_CH', 'NU_NOTA_CN', 'NU_NOTA_LC', 'NU_NOTA_REDACAO', 'NU_INSCRICAO')] %>%
  mutate_at(vars(c('NU_NOTA_CH', 'NU_NOTA_CN', 'NU_NOTA_LC', 'NU_NOTA_REDACAO')), 
            function(x){as.integer(ifelse(is.na(x), -1, x))})

recipe <- 
  recipe(NU_NOTA_MT ~ ., data = new_train) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  #remove_role(NU_INSCRICAO, old_role = "predictor") %>%
  #update_role(NU_INSCRICAO, new_role = "id variable") %>%
  prep(.)

train_baked <- 
  recipe %>%
  bake(new_data = new_train)

#summary(recipe)

modl_ml <-
  rand_forest(mode = "regression", trees = 100) %>%
  set_engine("ranger", importance = "impurity")

fit_ml <-
  modl_ml %>%
  fit(NU_NOTA_MT ~ ., data = train_baked)

test_baked <-
  recipe %>%
  bake(new_data = new_test) %>%
  bind_cols(., new_test %>% select(NU_INSCRICAO))
  
test_final <- test_baked

test_final$NU_NOTA_MT <- predict(fit_ml, new_test)$.pred
test_final <- test_final %>% select(NU_INSCRICAO, NU_NOTA_MT)
write_csv(test_final, './2_model/prediction_celso.csv')

