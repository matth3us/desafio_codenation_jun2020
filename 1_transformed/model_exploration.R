library(tidymodels)
library(tidyverse)

# Load Data
train <- readRDS('./1_transformed/train_transf.rds')

# Encontrar variáveis com muitos NAs
# find.nas <-
#     sapply(train, function(x) sum(is.na(x)))

train <- 
    train %>%
    #retirar, número muito grande de NAs para ser útil  
    select(-c(TP_ENSINO, TP_DEPENDENCIA_ADM_ESC, Q027)) %>%
    #prencher com 0
    mutate_at(vars(c('NU_NOTA_LC', 'NU_NOTA_MT', 'NU_NOTA_REDACAO', 'NU_NOTA_CN', 'NU_NOTA_CH')), function(x){as.integer(ifelse(is.na(x), 0, x))}) %>%
    mutate_at(vars(c('TP_STATUS_REDACAO', 'TP_ANO_CONCLUIU')), function(x){as.factor(ifelse(is.na(x), -1, x))}) %>%
    select(-starts_with('IN_')) %>%
    select(-c(Q047, TP_NACIONALIDADE))

# ggplot(data = train, aes(x = Q047)) + geom_histogram(stat='count', binwidth = 20)
# Remover:
  # TP_NACIONALIDADE
  # IN_BAIXA_VISAO
  # IN_SURDEZ
  # Todos os "IN_"
  # Q047
# Testar corr entre IN_TREINEIRO e NU_NOTA_MT

# Recipe for Datas  
recipe_rf <- function(dataset){
    recipe(NU_NOTA_MT ~ ., data = dataset) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric()) %>%
    step_dummy(one_of("TP_SEXO", "TP_COR_RACA", "TP_ST_CONCLUSAO","TP_PRESENCA_CN","TP_PRESENCA_CH","TP_PRESENCA_LC","TP_STATUS_REDACAO", 'TP_ANO_CONCLUIU')) %>%
    step_dummy(starts_with('Q0')) %>%
    #step_corr(all_predictors(), threshold = .9) %>%
    prep(.)
}


# min_train <- train %>%
#   select(c(NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_REDACAO, NU_NOTA_MT))

# Testes para garantir que recipe esteja do jeito correto
test_recipe <-
  train %>%
  recipe_rf() %>%
  bake(new_data = train)




#PROBLEMAS NO RANDOM FOREST: FEATURES EXAGERADAMENTE DESBALANCEADAS ESTÃO FODENDO A PREVISÃO
#TESTAR FAZER APENAS COM AS FEATURES DAS NOTAS DAS OUTRAS PROVAS E VER O QUE ACONTECE PRIMEIRO!

#Parâmetros de cross validation
set.seed(123)
cross_val_tbl <- vfold_cv(train, v = 4, repeats = 10)

#função de fit de Random Forest
#https://www.rdocumentation.org/packages/parsnip/versions/0.0.0.9001/topics/rand_forest
rf_fun <- function(split, id, try, tree) {
  analysis_set <- 
    split %>% 
    analysis()
  analysis_prepped <- 
    analysis_set %>% 
    recipe_rf()
  analysis_baked <- 
    analysis_prepped %>% 
    bake(new_data = analysis_set)  
  
  model_rf <-
    rand_forest(
      mode = "regression"
      , trees = tree
    ) %>%
    set_engine("ranger",
               importance = "impurity"
    ) %>%
    fit(NU_NOTA_MT ~ ., data = analysis_baked)  
  
  assessment_set <- 
    split %>% 
    assessment()
  assessment_prepped <- 
    assessment_set %>% 
    recipe_rf()
  assessment_baked <- 
    assessment_prepped %>% 
    bake(new_data = assessment_set)  
  
  tibble(
    "id" = id,
    "truth" = assessment_baked$NU_NOTA_MT,
    "prediction" = 
      model_rf %>%
      predict(new_data = assessment_baked) %>%
      unlist()
  )
  
}

#Predição de Random Forest
pred_rf <- 
    map2_df(
      .x = cross_val_tbl$splits,
      .y = cross_val_tbl$id,
      #   ~ rf_fun(split = .x, id = .y, try = 3, tree = 100)
      ~ rf_fun(split = .x, id = .y, tree = 100)
    )

# Mostrar métricas de predição (USAR RMSD, MAE E MAPE)
result <- pred_rf %>%
  group_split(id, keep=TRUE) %>%
  lapply(., function(df){metrics(df, truth, prediction) %>% mutate(id = df$id[1])}) %>%
  bind_rows(.) %>%
  select(-.estimator) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)

# Feature selection!!!
complete_trained_model <-
  rand_forest(
    mode = "regression"
    , trees = 100
  ) %>%
  set_engine("ranger",
             importance = "impurity"
  ) %>%
  fit(NU_NOTA_MT ~ ., data = train)  

var_importance <- 
    complete_trained_model$fit$variable.importance %>% as.data.frame(.) %>%
    rename(importance = ".") %>%
    rownames_to_column(., var = "features") %>%
    arrange(desc(importance)) %>%
    mutate(
        perc = cumsum(importance/sum(importance))
      , id = row_number()
      , grp = 'g'
      , delta_perc = perc-lag(perc)
      ) %>%
    replace_na(list(delta_perc = 0))

#importância acumulada das features
ggplot(data = var_importance, aes(x = id, y = perc, group = grp)) + geom_line() + geom_point() + scale_x_continuous(breaks = seq(0, 20, by = 1))
#importância incremental das features
ggplot(data = var_importance, aes(x = id, y = delta_perc, group = grp)) + geom_line() + geom_point() + scale_x_continuous(breaks = seq(0, 20, by = 1))

# Na ordem, a importância incremental das features basicamente se elimina depois da 8a feature. Ficar apenas com as 8 features mais importantes.


### Modelo Final


features <- 
    var_importance %>%
    filter(id < 9) %>%
    select(features)

final_train <- 
    train %>%
    select(features$features, NU_NOTA_MT)

final_trained_model <-
  rand_forest(
    mode = "regression"
    , trees = 100
  ) %>%
  set_engine("ranger",
             importance = "impurity"
  ) %>%
  fit(NU_NOTA_MT ~ ., data = final_train)  

saveRDS(final_trained_model, "./2_model/final_trained_model.rds")
saveRDS(features, "./2_model/final_features.rds")


test <- 
  read_csv("./0_raw/test.csv")
  
  
# Seria feito o treino e teste dos modelos abaixo, mas parece que não será necessário
# Bagged Decision Tree Models
# https://baguette.tidymodels.org/reference/bag_tree.html
# Bagged MARS Models
# https://baguette.tidymodels.org/reference/bag_mars.html
# PLS
# https://en.wikipedia.org/wiki/Partial_least_squares_regression
#https://plsmod.tidymodels.org/reference/pls.html
# XGBoost 
# https://rdrr.io/cran/parsnip/man/xgb_train.html
# Hyperparameter Three Best Models
# https://www.tidymodels.org/start/tuning/

