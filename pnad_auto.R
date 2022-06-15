### Arthur de Figueiredo Gusmão

# instala os pacotes:
libs <- c("DBI", "RSQLite" , "fst" , "survey", "basedosdados", "openxlsx")
libs.novas <- libs[ !( libs %in% installed.packages()[ , "Package" ] ) ]
if( length( libs.novas ) ) install.packages( libs.novas )

# libs

library(DBI)
library(RSQLite)
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
           id_domicilio, V1008, V1028, V4013, V2007, VD4009, V4019, VD4020, starts_with("V1028"))) 


## Importa sem as variáveis de Bootstrap (fica bem mais leve, a estimativa é a mesma. O que muda é a estimação dos SE`s)
pnad <- bdplyr("br_ibge_pnadc.microdados")%>%
  select(c(ano, trimestre, id_uf, id_upa, id_estrato, 
           id_domicilio, V1008, V1028, V4013, V2007, VD4009, V4019, VD4020))


## Exportando para .rds sem bootstrap
basedosdados::bd_write_rds(.lazy_tbl = pnad,
                           "pnad_2012_2022-sbs.rds")


## Exportando para .rds com bootstrap
basedosdados::bd_write_rds(.lazy_tbl = pnad,
                           "pnad_2012_2022-bs.rds")
  

## Importando o .rds sem bootstrap
pnad_2019 <- readRDS(file = "pnad_2012_2022-sbs.rds")


## Importando o .rds com bootstrap
pnad_2019 <- readRDS(file = "pnad_2021-bs.rds")


#### Dicionário das variáveis ####

### Imputando o dicionário das variáveis
pnad_2019 %>%
  filter(ano > 2014) %>% 
  mutate(filtrar = as.numeric(paste(ano, trimestre, sep = ""))) %>% 
  filter(filtrar != 20151, filtrar != 20152, filtrar != 20153) -> pnad_2019

pnad_2019 <- pnad_2019 %>%
  filter(ano > 2019)

  
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

#### Criando o objeto principal ####
##### Função que cria o objeto de desenho amostral complexto.
### Ela ñ foi usada nesse trabalho, mas pode ser útil para os próximos.
my_func <- function(ano, trimestre) {
  pnad_2019  %>% 
    filter(ano == ano, trimestre == trimestre) -> pnad
  
  teste <- svydesign(
      ids = ~ id_upa + V1008 ,
      strata = ~ id_estrato ,
      weights = ~ V1028 ,
      data = pnad,
      nest = TRUE )
  
  return(teste)
  
  
}

##### Criando a tibble cada objeto svy.design, por ano e trimestre.
### Isso pode demorar.

a <- expand.grid(ano = 2015:2018, trimestre = 1:4) %>%
  as_tibble() %>%
  mutate(ano2 = case_when(ano == 2015 & trimestre == 1 ~ "NA",
                         ano == 2015 & trimestre == 2 ~ "NA",
                         ano == 2015 & trimestre == 3 ~ "NA",
                         TRUE ~ "2015"))%>%
  filter(ano2 != "NA") %>%
  arrange(ano) %>%
  select(ano, trimestre) %>%
  mutate(pnad_design = map2(.x = ano, .y= trimestre, function(.x,.y){

    pnad_2019 %>% 
      filter(ano == .x, trimestre == .y) -> pnad
    
    pnadc.design <-
      svydesign(
        ids = ~ id_upa + V1008 ,
        strata = ~ id_estrato ,
        weights = ~ V1028 ,
        data = pnad,
        nest = TRUE )
    return(pnadc.design)
  
    
  }))

a <- expand.grid(ano = 2020:2021, trimestre = 1:4) %>%
  as_tibble() %>%
  mutate(ano2 = case_when(ano == 2021 & trimestre == 4 ~ "NA",
                          TRUE ~ "2021"))%>%
  filter(ano2 != "NA") %>%
  arrange(ano) %>%
  select(ano, trimestre) %>%
  mutate(pnad_design = map2(.x = ano, .y= trimestre, function(.x,.y){
    
    pnad_2019 %>% 
      filter(ano == .x, trimestre == .y) -> pnad
    
    pnadc.design <-
      svydesign(
        ids = ~ id_upa + V1008 ,
        strata = ~ id_estrato ,
        weights = ~ V1028 ,
        data = pnad,
        nest = TRUE )
    return(pnadc.design)
    
    
  }))




options(scipen = 999) # opdao que deixa de mostrar numeros no formato cientifico.


######  Bootstrap (caso queira usar) #####

bootweights.col <- grep("V1028[0-9]{3}", colnames(pnad_2019), value = TRUE)
bootweights.mat <- pnad_2019[, bootweights.col]
bootweights.mat <- as.matrix(bootweights.mat)
pnad_2019[,bootweights.col] <- NULL


### Criando o objeto de desenho amostral complexo com bootstrap

pnadc_boot <- survey::svrepdesign(data = pnad_2019, type = "bootstrap", weights = ~V1028,
                                  repweights = bootweights.mat)



#### Neste ponto teremos o objeto necessário para efetuar todos os cálculos com o pacote survey. Estes serão feitos em um script
#### auxiliar, para melhor entendimento e limpeza do código.



