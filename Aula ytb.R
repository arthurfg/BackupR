

## Importando as Libs

library(survey)
library(basedosdados)
library(tidyverse)
library(openxlsx)


#### Importando e salvado  ####

## Billing id do projeto do google bigquery

set_billing_id("casebd") ### no caso, esse é o meu billing id. Caso queira baixar os dados no seu computador, posso mostrar como criar um projeto no google BiqQuery.

## Importa com as variáveis para o Bootstrap
#### Demora bastante, o arquivo fica bem mais pesado por conta das variáveis adicionais.

pnad <- bdplyr("br_ibge_pnadc.microdados")  %>%
  select(c(ano, trimestre, id_uf, id_upa, id_estrato, 
           id_domicilio, V1008, V1028, V4013, V2007, VD4009, V4019, VD4020, starts_with("V1028"))) %>%
  filter(ano == 2019)


## Exportando para .rds com bootstrap
basedosdados::bd_write_rds(.lazy_tbl = pnad,
                           "pnad_2012_2022-bs.rds")


## Importando o .rds com bootstrap
pnad_2019 <- readRDS(file = "pnad_2012_2022-bs.rds")




#### Dicionário das variáveis ####

### Imputando o dicionário das variáveis

pnad_2019 %>%
  mutate(V2007d = case_when(V2007 == 1 ~ "Homem",
                            V2007 == 2 ~ "Mulher",
                            TRUE ~ "NA"),
         VD4009d = case_when(VD4009 == 1 ~ "Empregado no setor privado com carteira de trabalho assinada",
                             VD4009 == 2 ~ "Empregado no setor privado sem carteira de trabalho assinada",
                             VD4009 == 3 ~ "Trabalhador doméstico com carteira de trabalho assinada",
                             VD4009 == 4 ~ "Trabalhador doméstico sem carteira de trabalho assinada",
                             VD4009 == 5 ~ "Empregado no setor público com carteira de trabalho assinada",
                             VD4009 == 6 ~ "Empregado no setor público sem carteira de trabalho assinada",
                             VD4009 == 7 ~ "Militar e servidor estatutário",
                             VD4009 == 8 ~ "Empregador",
                             VD4009 == 9 ~ "Conta-própria",
                             VD4009 == 10 ~ "Trabalhador familiar auxiliar",
                             TRUE ~ "NA"),
         V4019d = case_when(V4019 == 1 ~ "Sim",
                            V4019 == 2 ~ "Não",
                            TRUE ~ "NA")) -> pnad_2019
pnad_2019[pnad_2019 == "NA"] <- NA


##### Criando a tibble cada objeto svy.design, por ano e trimestre.
### Isso pode demorar.

df <- expand.grid(ano = 2019, trimestre = 1:4) %>%
  as_tibble() %>%
  mutate(pnad_design = map2(.x = ano, .y= trimestre, function(.x,.y){
    
    pnad_2019 %>% 
      filter(ano == .x, trimestre == .y)%>%
      select(starts_with("V1028") & !V1028) %>%
      as.matrix() -> bootweights.mat
    
    pnad_2019 %>%
      select(!(starts_with("V1028") & !V1028)) -> pnad
    
    pnadc_boot <- survey::svrepdesign(data = pnad_2019,
                                      type = "bootstrap",
                                      weights = ~V1028,
                                      repweights = bootweights.mat)
    
    return(pnadc_boot)

    
  }))

df %>%
  mutate(desemprego = map(.x = pnad_design, function(.x){
    total <- svytotal(x=~V4013>=10010 & V4013<=33002 ,
                      design = .x, na.rm = TRUE) 
    
    total <- as_tibble(total, rownames = "total") %>%
      mutate(trimestre =  max(.x$pnad_design[[i]]$variables$trimestre),
             ano = max(.x$pnad_design[[i]]$variables$ano))
    
    return(total)}),
         renda_media = map(.x = pnad_design, function(.x){
           total <- svytotal(x=~V4013>=10010 & V4013<=33002 ,
                             design = .x, na.rm = TRUE) 
           
           total <- as_tibble(total, rownames = "total") %>%
             mutate(trimestre =  max(.x$pnad_design[[i]]$variables$trimestre),
                    ano = max(.x$pnad_design[[i]]$variables$ano))
           
           return(total)}))



options(scipen = 999) # opdao que deixa de mostrar numeros no formato cientifico.

