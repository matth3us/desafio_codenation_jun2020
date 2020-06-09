library(tidyverse)
test <- 
  read_csv("./0_raw/test.csv")

test_columns <- names(test)

train <- 
    read_csv("./0_raw/train.csv") %>%
    .[c(test_columns, 'NU_NOTA_MT')]

train_transformed <- 
    train %>% 
    select(-NU_INSCRICAO) %>% #hash de inscrição
    # retirar informações do tipo de prva
    select(-c(CO_PROVA_CH, CO_PROVA_CN, CO_PROVA_LC, CO_PROVA_MT)) %>%
    # retirar informação do idioma de lingua estrangeira escolhido
    select(-TP_LINGUA) %>%
    # manter informações mínimas da escola do participante (única informação geográfica será a UF da escola)
    select(-c(TP_ESCOLA)) %>%
    # retirar informações geográficas
    select(-c(CO_UF_RESIDENCIA, SG_UF_RESIDENCIA)) %>%
      # retirar informações extras de redação
    select(-c(NU_NOTA_COMP1, NU_NOTA_COMP2, NU_NOTA_COMP3, NU_NOTA_COMP4, NU_NOTA_COMP5))

# Conferir correlações de informações da redação
# red <- 
#     train %>%
#     select(id, TP_STATUS_REDACAO, NU_NOTA_REDACAO, NU_NOTA_COMP1, NU_NOTA_COMP2, NU_NOTA_COMP3, NU_NOTA_COMP4, NU_NOTA_COMP5) %>%
#     mutate_all(function(x){ifelse(is.na(x), -1, x)})
# 
# red.cor <- 
#     red %>%
#     select(-id) %>%
#     cor(.)
# library(corrplot)
# corrplot(red.cor)
# red.cor > 0.75
#Correlação alta entre informações de nota da redação, manter apenas a nota principal e a informação de presença

#Testar correlações entre variáveis numéricas e booleanas
# X_num <- 
#     X_train %>%
#     select(-id) %>%
#     mutate(TP_SEXO = as.numeric(as.factor(TP_SEXO))-1) %>%
#     select(-starts_with("Q0")) %>%
#     select(-c(TP_ST_CONCLUSAO,TP_COR_RACA,TP_ENSINO,TP_ESTADO_CIVIL,TP_NACIONALIDADE,TP_PRESENCA_CH,TP_PRESENCA_CN,TP_PRESENCA_LC,TP_PRESENCA_MT,TP_STATUS_REDACAO)) %>%
#     mutate_all(function(x){ifelse(is.na(x), -1, x)})

# X_num.cor <- 
#     cor(X_num) %>%
#     as.data.frame(.) %>%
#     mutate_all(function(x){ifelse(is.na(x), 0, x)}) %>%
#     mutate_all(function(x){x > 0.75}) %>%
#     mutate(sumVar = rowSums(.[1:63])) %>%
#     filter(sumVar > 1)
# 
# View(X_num.cor)
#Correlação alta entre dependência administrativa e localização da escola; pegar só a dependência administrativa

train_transformed <-
    train_transformed %>%
    mutate(TP_SEXO = as.numeric(as.factor(TP_SEXO))-1) %>%
    mutate_at(vars(starts_with('Q0')), as.factor) %>%
    mutate_at(c('TP_ST_CONCLUSAO','TP_COR_RACA','TP_ENSINO','TP_NACIONALIDADE','TP_PRESENCA_CH','TP_PRESENCA_CN','TP_PRESENCA_LC', 'TP_STATUS_REDACAO', 'TP_ANO_CONCLUIU', 'TP_SEXO'), as.factor)

recipe_rf <- function(dataset){
  recipe(NU_NOTA_MT ~ ., data = dataset) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric()) %>%
    step_dummy(one_of("TP_SEXO", "TP_COR_RACA", "TP_ST_CONCLUSAO","TP_PRESENCA_CN","TP_PRESENCA_CH","TP_PRESENCA_LC","TP_STATUS_REDACAO", 'TP_ANO_CONCLUIU')) %>%
    step_dummy(starts_with('Q0')) %>%
    #step_corr(all_predictors(), threshold = .9) %>%
    prep(.)
}

  
write_rds(train_transformed, "./1_transformed/train_transf.rds")
  

  







    
  
    