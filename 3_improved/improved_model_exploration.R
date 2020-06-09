library(tidyverse)
library(tidymodels)
library(baguette)


############# PREPARAÇÃO DE DADOS
out_of_recipe <- function(dset){
  dset %>%
    mutate(TP_SEXO = as.numeric(as.factor(TP_SEXO))-1) %>%
    replace_na(list(
      NU_NOTA_LC = 0
      , NU_NOTA_MT = 0
      , NU_NOTA_REDACAO = 0
      , NU_NOTA_CN = 0
      , NU_NOTA_CH = 0
      , TP_STATUS_REDACAO = -1
      , TP_ANO_CONCLUIU = -1
    )) %>%
    mutate_at(vars(starts_with('Q0')), as.factor) %>%
    mutate_at(vars(c('TP_ST_CONCLUSAO','TP_COR_RACA','TP_ENSINO','TP_NACIONALIDADE','TP_PRESENCA_CH','TP_PRESENCA_CN','TP_PRESENCA_LC', 'TP_STATUS_REDACAO', 'TP_ANO_CONCLUIU', 'TP_SEXO')), as.factor) %>%
    select(-starts_with('IN_')) %>%
    select(-c(
      'CO_PROVA_CH'
      , 'CO_PROVA_CN'
      , 'CO_PROVA_LC'
      , 'CO_PROVA_MT'
      , 'TP_LINGUA'
      , 'TP_ESCOLA'
      , 'CO_UF_RESIDENCIA'
      , 'SG_UF_RESIDENCIA'
      , 'NU_NOTA_COMP1'
      , 'NU_NOTA_COMP2'
      , 'NU_NOTA_COMP3'
      , 'NU_NOTA_COMP4'
      , 'NU_NOTA_COMP5'
      , 'TP_ENSINO'
      , 'TP_DEPENDENCIA_ADM_ESC'
      , 'Q027'
      , 'Q047'
      , 'TP_NACIONALIDADE'
    ))
}

test <- 
  read_csv("./0_raw/test.csv") %>%
  out_of_recipe()

test_columns <- names(test)

train <- 
  read_csv("./0_raw/train.csv") %>%
  out_of_recipe() %>%
  select(c(test_columns, 'NU_NOTA_MT'))

recp_rf <- 
    recipe(NU_NOTA_MT ~ ., data = train) %>%
    update_role('NU_INSCRICAO', new_role = 'id') %>% #O QUE FAZER COM O NUMERO DE INSCRIÇÃO?
    step_dummy(one_of("TP_SEXO", "TP_COR_RACA", "TP_ST_CONCLUSAO","TP_PRESENCA_CN","TP_PRESENCA_CH","TP_PRESENCA_LC","TP_STATUS_REDACAO", 'TP_ANO_CONCLUIU')) %>%
    step_dummy(starts_with('Q0')) %>%
    step_zv(all_predictors()) %>%
    step_corr(all_numeric(), -all_outcomes(), threshold = 0.9, skip=T) %>%
    step_normalize(all_numeric(), -all_outcomes(), skip=T) %>%
    prep(data = train)

# Teste da receita
test_recipe <- bake(object=recp_rf, new_data=train)


############# ESCOLHA DOS MODELOS
set.seed(123)
cross_val <- vfold_cv(train, v = 4, repeats = 10)

ml_fun <- function(split, id, try, wflow) {
  
  analysis_baked <- 
    split %>% 
    analysis()
  
  assessment_baked <- 
    split %>% 
    assessment()
  
  analysis_fit <-
    wflow %>%
    fit(data = analysis_baked)
  
  prediction <- 
    analysis_fit %>%
    predict(assessment_baked)
  
  tibble(
    "id" = id,
    "truth" = assessment_baked$NU_NOTA_MT,
    "prediction" =
      analysis_fit %>%
      predict(new_data = assessment_baked) %>%
      unlist()
  )
  
}

#Random Forest
#https://www.rdocumentation.org/packages/parsnip/versions/0.0.0.9001/topics/rand_forest
modl_rf <-
  rand_forest(mode = "regression", trees = 100) %>%
  set_engine("ranger",importance = "impurity")

workflow_rf <- 
  workflow() %>%
  add_recipe(recp_rf) %>%
  add_model(modl_rf)

pred_rf <-
  map2_df(
    .x = cross_val$splits
    , .y = cross_val$id
    , ~ ml_fun(split = .x, id = .y, wflow = workflow_rf)
  )

result_rf <- pred_rf %>%
  group_split(id, keep=TRUE) %>%
  lapply(., function(df){metrics(df, truth, prediction) %>% mutate(id = df$id[1])}) %>%
  bind_rows(.) %>%
  select(-.estimator) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)

#Linear Regression


#1. Estratificar dados na regressão linear por TP_STATUS_REDACAO
# 1.1. https://www.tidymodels.org/learn/models/sub-sampling/
#2. Buscar parâmetros de LASSO para testar
#3. Testar Bagged MARS MOdel
#4. Testar 
# Bagged MARS Models
# https://baguette.tidymodels.org/reference/bag_mars.html

# PLS
# https://en.wikipedia.org/wiki/Partial_least_squares_regression
# https://plsmod.tidymodels.org/reference/pls.html


# XGBoost 



# Bagged MARS Models
# https://baguette.tidymodels.org/reference/bag_mars.html

# PLS
# https://en.wikipedia.org/wiki/Partial_least_squares_regression
# https://plsmod.tidymodels.org/reference/pls.html
  



# XGBoost 
# https://rdrr.io/cran/parsnip/man/xgb_train.html

modl_xgb <-
  boost_tree(mode = "regression", trees = 1000)

workflow_xgb <- 
  workflow() %>%
  add_recipe(recp_rf) %>%
  add_model(modl_xgb)

pred_xgb <-
  map2_df(
    .x = cross_val$splits
    , .y = cross_val$id
    , ~ ml_fun(split = .x, id = .y, wflow = workflow_xgb)
  )


### Feature Importance e Tunagem
# https://www.tidymodels.org/start/tuning/


# # Feature selection!!!
# complete_trained_model <-
#   rand_forest(
#     mode = "regression"
#     , trees = 100
#   ) %>%
#   set_engine("ranger",
#              importance = "impurity"
#   ) %>%
#   fit(NU_NOTA_MT ~ ., data = train)  
# 
# var_importance <- 
#   complete_trained_model$fit$variable.importance %>% as.data.frame(.) %>%
#   rename(importance = ".") %>%
#   rownames_to_column(., var = "features") %>%
#   arrange(desc(importance)) %>%
#   mutate(
#     perc = cumsum(importance/sum(importance))
#     , id = row_number()
#     , grp = 'g'
#     , delta_perc = perc-lag(perc)
#   ) %>%
#   replace_na(list(delta_perc = 0))