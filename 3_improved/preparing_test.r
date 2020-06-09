library(tidyverse)

train <- 
  read_csv("./0_raw/train.csv")

test <- 
  read_csv("./0_raw/test.csv")

features_final <- 
  readRDS('./2_model/final_features.rds')

new_train <- 
  train[c(features$features, 'NU_NOTA_MT', 'NU_INSCRICAO')] %>%
  mutate_at(vars(c('NU_NOTA_CH', 'NU_NOTA_CN', 'NU_NOTA_LC', 'NU_NOTA_REDACAO', 'NU_NOTA_MT')), fill_numeric_na) %>%
  mutate_at(vars(c('TP_PRESENCA_CH', 'TP_PRESENCA_CN', 'TP_PRESENCA_LC', 'TP_STATUS_REDACAO')), function(x){as.factor(ifelse(is.na(x), -1, x))})

new_test <- 
  test[c(features$features, 'NU_INSCRICAO')] %>%
  mutate_at(vars(c('NU_NOTA_CH', 'NU_NOTA_CN', 'NU_NOTA_LC', 'NU_NOTA_REDACAO')), fill_numeric_na) %>%
  mutate_at(vars(c('TP_PRESENCA_CH', 'TP_PRESENCA_CN', 'TP_PRESENCA_LC', 'TP_STATUS_REDACAO')), function(x){as.factor(ifelse(is.na(x), -1, x))}) %>%
  mutate(NU_NOTA_MT = NA)


fill_numeric_na <- function(x){
  as.integer(ifelse(is.na(x), 0, x))
  }

recipe <- 
  recipe(NU_NOTA_MT ~ ., data = new_train) %>%
  update_role(NU_INSCRICAO, new_role = "id variable") %>%
  remove_role(NU_INSCRICAO, old_role = "predictor") %>%
  step_normalize(all_numeric(), -all_outcomes(), -has_role('id variable')) %>%
  prep(.)

train_baked <- 
  recipe %>%
  bake(new_data = new_train)

modl_ml <-
  rand_forest(mode = "regression", trees = 100) %>%
  set_engine("ranger", importance = "impurity")

fit_ml <-
  modl_ml %>%
  fit(NU_NOTA_MT ~ ., data = train_baked) 

test_baked <-
  recipe %>%
  bake(new_data = new_test)
  
test_final <-
    new_test %>%
    select(NU_INSCRICAO, NU_NOTA_MT)
test_final$NU_NOTA_MT <- predict(fit_ml, new_test)$.pred
write_csv('./')

