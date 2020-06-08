library(tidyverse)
train <- 
    read_csv("./0_raw/train.csv") %>%
    select(-X1) #id do csv

Y_train <- 
    train %>% 
    select(NU_NOTA_MT)

X_train <- 
    train %>% 
    select(-NU_NOTA_MT) %>% #variável alvo
    select(-NU_INSCRICAO) %>% #hash de inscrição
    select(-NU_ANO) %>% #ano da prova
    # retirar informações do local de prova
    select(-c(CO_MUNICIPIO_PROVA, NO_MUNICIPIO_PROVA, CO_UF_PROVA, SG_UF_PROVA)) %>%
    # retirar informações do tipo de prva
    select(-c(CO_PROVA_CH, CO_PROVA_CN, CO_PROVA_LC, CO_PROVA_MT)) %>%
    # retirar informação do idioma de lingua estrangeira escolhido
    select(-TP_LINGUA) %>%
    # retirar informações do vetor de resposta e do gabarito
    select(-c(TX_GABARITO_CH, TX_GABARITO_CN, TX_GABARITO_LC, TX_GABARITO_MT, TX_RESPOSTAS_CH, TX_RESPOSTAS_CN, TX_RESPOSTAS_LC, TX_RESPOSTAS_MT)) %>%
    # manter informações mínimas da escola do participante (única informação geográfica será a UF da escola)
    select(-c(CO_ESCOLA, CO_MUNICIPIO_ESC, NO_MUNICIPIO_ESC, CO_UF_ESC, SG_UF_ESC, TP_SIT_FUNC_ESC, TP_ESCOLA)) %>%
    # retirar informações geográficas
    select(-c(CO_MUNICIPIO_RESIDENCIA
              , NO_MUNICIPIO_RESIDENCIA
              , NO_MUNICIPIO_RESIDENCIA
              , CO_UF_RESIDENCIA
              , SG_UF_RESIDENCIA
              , NO_ENTIDADE_CERTIFICACAO
              , CO_UF_ENTIDADE_CERTIFICACAO
              , SG_UF_ENTIDADE_CERTIFICACAO
              , CO_MUNICIPIO_NASCIMENTO
              , NO_MUNICIPIO_NASCIMENTO
              , CO_UF_NASCIMENTO
              , SG_UF_NASCIMENTO)) %>%
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

X_train <-
    X_train %>%
    select(-TP_LOCALIZACAO_ESC) %>%
    mutate(TP_SEXO = as.numeric(as.factor(TP_SEXO))-1) %>%
    mutate_at(vars(starts_with('Q0')), as.factor) %>%
    mutate_at(c('TP_ST_CONCLUSAO','TP_COR_RACA','TP_ENSINO','TP_ESTADO_CIVIL','TP_NACIONALIDADE','IN_VISAO_MONOCULAR','TP_PRESENCA_CH','TP_PRESENCA_CN','TP_PRESENCA_LC', 'TP_PRESENCA_MT', 'TP_STATUS_REDACAO'), as.factor)
  
write_rds(Y_train, "./1_transformed/Y_train.rds")
write_rds(X_train, "./1_transformed/X_train.rds")
  

  







    
  
    