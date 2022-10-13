### Importing the Libs

library(tempdisagg)
library(readxl)
library(tidyverse)
library(lubridate)
library(xlsx)
library(zoo)


##### Importing


df <- read_excel("denton.xlsx", 
                         sheet = "Sheet1")


########### Agro ####

### Tidyng 

df %>%
    #filter(time > "1980-11-01") %>%
    select(agro_iae) %>%
    ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
    filter(time > "1981-02-01") %>%
    select(agro_ibge) %>%
    drop_na() %>%
    ts(start = c(1981, 1), frequency = 4) -> ibge_trimestral



#### Temp. Desagreggation (Denton)

## Série mensal que, somada, resulta no total da anual.

temp <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "sum", to =  "monthly",
           method = "denton-cholette",  criterion = "proportional")



## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)



denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))


df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(agro_ibge, .direction = "down") %>%
    select(agro_iae, agro_ibge, serie_ajustada) %>%
    rename(serie_ajustada_agro = serie_ajustada)-> agro


##### Extracao ######

### Tidyng 

df %>%
    #filter(time > "1980-11-01") %>%
    select(ext_iae) %>%
    ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
    filter(time > "1981-02-01") %>%
    select(ext_ibge) %>%
    drop_na() %>%
    ts(start = c(1981, 1), frequency = 4) -> ibge_trimestral



#### Temp. Desagreggation (Denton)


## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)


denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(ext_ibge, .direction = "down") %>%
    select(ext_iae, ext_ibge, serie_ajustada)%>%
    rename(serie_ajustada_extracao = serie_ajustada)  -> extracao



##### Transf ######

### Tidyng 

df %>%
    #filter(time > "1980-11-01") %>%
    select(transf_iae) %>%
    ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
    filter(time > "1981-02-01") %>%
    select(transf_ibge) %>%
    drop_na() %>%
    ts(start = c(1981, 1), frequency = 4) -> ibge_trimestral



#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)


denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(transf_ibge, .direction = "down") %>%
    select(transf_iae, transf_ibge, serie_ajustada)%>%
    rename(serie_ajustada_transformacao = serie_ajustada) -> transformacao


##### Elétrica ######

### Tidyng 

df %>%
    #filter(time > "1980-11-01") %>%
    select(eletr_iae) %>%
    ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
    filter(time > "1981-02-01") %>%
    select(eletr_ibge) %>%
    drop_na() %>%
    ts(start = c(1981, 1), frequency = 4) -> ibge_trimestral


#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)


denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(eletr_ibge, .direction = "down") %>%
    select(eletr_iae, eletr_ibge, serie_ajustada)%>%
    rename(serie_ajustada_eletrica = serie_ajustada) -> eletrica


######## Construção #########

### Tidyng 

df %>%
    #filter(time > "1980-11-01") %>%
    select(const_iae) %>%
    ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
    filter(time > "1981-02-01") %>%
    select(const_ibge) %>%
    drop_na() %>%
    ts(start = c(1981, 1), frequency = 4) -> ibge_trimestral




#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)



denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(const_ibge, .direction = "down") %>%
    select(const_iae, const_ibge, serie_ajustada)%>%
    rename(serie_ajustada_construcao = serie_ajustada) -> construcao



####### Comércio ########

### Tidyng 

df %>%
    #filter(time > "1980-11-01") %>%
    select(comer_iae) %>%
    ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
    filter(time > "1981-02-01") %>%
    select(comer_ibge) %>%
    drop_na() %>%
    ts(start = c(1981, 1), frequency = 4) -> ibge_trimestral


#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)


denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))



df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(comer_ibge, .direction = "down") %>%
    select(comer_iae, comer_ibge, serie_ajustada)%>%
    rename(serie_ajustada_comercio = serie_ajustada) -> comercio




####### Transporte ########



### Tidyng 

df %>%
    #filter(time > "1980-11-01") %>%
    select(transp_iae) %>%
    ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
    filter(time > "1981-02-01") %>%
    select(transp_ibge) %>%
    drop_na() %>%
    ts(start = c(1981, 1), frequency = 4) -> ibge_trimestral


#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)


denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))


df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(transp_ibge, .direction = "down") %>%
    select(transp_iae, transp_ibge, serie_ajustada)%>%
    rename(serie_ajustada_transporte = serie_ajustada) -> transporte




######### S Inform #########


### Tidyng 

df %>%
    #filter(time > "1980-11-01") %>%
    select(s_infor_iae) %>%
    ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
    filter(time > "1981-02-01") %>%
    select(s_infor_ibge) %>%
    drop_na() %>%
    ts(start = c(1981, 1), frequency = 4) -> ibge_trimestral


#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)


denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))


df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(s_infor_ibge, .direction = "down") %>%
    select(s_infor_iae, s_infor_ibge, serie_ajustada)%>%
    rename(serie_ajustada_s_infor = serie_ajustada) -> s_infor





##########  I financ ###########



### Tidyng 

df %>%
    #filter(time > "1980-11-01") %>%
    select(i_financ_iae) %>%
    ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
    filter(time > "1981-02-01") %>%
    select(i_financ_ibge) %>%
    drop_na() %>%
    ts(start = c(1981, 1), frequency = 4) -> ibge_trimestral



#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)


denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(i_financ_ibge, .direction = "down") %>%
    select(i_financ_iae, i_financ_ibge, serie_ajustada)%>%
    rename(serie_ajustada_i_financ = serie_ajustada) -> i_financ


######## S Imob #########

### Tidyng 

df %>%
    #filter(time > "1980-11-01") %>%
    select(s_imob_iae) %>%
    na_if(0) %>%
    drop_na() %>%
    ts(start = c(1995, 1), frequency = 12) -> iae_mensal


df %>%
    filter(time > "1981-02-01") %>%
    select(s_imob_ibge) %>%
    drop_na() %>%
    ts(start = c(1995, 1), frequency = 4) -> ibge_trimestral



#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)


denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(s_imob_ibge, .direction = "down") %>%
    select(s_imob_iae, s_imob_ibge, serie_ajustada)%>%
    rename(serie_ajustada_s_imob = serie_ajustada) -> s_imob



######### O Serv #######


### Tidyng 

df %>%
    #filter(time > "1980-11-01") %>%
    select(o_serv_iae) %>%
    ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
    filter(time > "1981-02-01") %>%
    select(o_serv_ibge) %>%
    drop_na() %>%
    ts(start = c(1981, 1), frequency = 4) -> ibge_trimestral




#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)


denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))


df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(o_serv_ibge, .direction = "down") %>%
    select(o_serv_iae, o_serv_ibge, serie_ajustada)%>%
    rename(serie_ajustada_o_serv = serie_ajustada) -> o_serv



######### APU ############

### Tidyng 

df %>%
    #filter(time > "1980-11-01") %>%
    select(apu_iae) %>%
    ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
    filter(time > "1981-02-01") %>%
    select(apu_ibge) %>%
    drop_na() %>%
    ts(start = c(1981, 1), frequency = 4) -> ibge_trimestral


#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)



denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(apu_ibge, .direction = "down") %>%
    select(apu_iae, apu_ibge, serie_ajustada)%>%
    rename(serie_ajustada_apu = serie_ajustada) -> apu




###### Impostos ########

### Tidyng 

df %>%
    #filter(time > "1980-11-01") %>%
    select(impostos_iae) %>%
    ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
    filter(time > "1981-02-01") %>%
    select(impostos_ibge) %>%
    drop_na() %>%
    ts(start = c(1995, 1), frequency = 4) -> ibge_trimestral


#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)


denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(impostos_ibge, .direction = "down") %>%
    select(impostos_iae, impostos_ibge, serie_ajustada)%>%
    rename(serie_ajustada_impostos = serie_ajustada) -> impostos




###### Consumo das Famílias  #####

### Tidyng 

df %>%
  filter(time < "1992-01-01") %>%
  select(consumo_familias_iae) %>%
  ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
  filter(time > "1980-02-01") %>%
  filter(time < "1992-02-01") %>%
  select(consumo_familias_ibge) %>%
  drop_na() %>%
  ts(start = c(1980), frequency = 1) -> ibge_trimestral

df %>%
  filter(time >= "1991-12-01") %>%
  select(consumo_familias_iae) %>%
  ts(start = c(1992, 1), frequency = 12) -> iae_mensal2


df %>%
  filter(time > "1992-02-01") %>%
  select(consumo_familias_ibge) %>%
  drop_na() %>%
  ts(start = c(1992, 1), frequency = 4) -> ibge_trimestral2




#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
            conversion = "mean", to =  "monthly",
            method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)



denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))


temp3 <- td(ibge_trimestral2 ~ 0 + iae_mensal2,
            conversion = "mean", to =  "monthly",
            method = "denton-cholette", criterion = "proportional")


denton_mean2 <- predict(temp3)



denton_mean2 <- data.frame(serie_ajustada = as.matrix(denton_mean2),
                          data = as.Date(as.yearmon(time(denton_mean2))))



df %>%
  mutate(data = as.Date(as.yearmon(time))) %>%
  filter(data < "1992-01-01" ) %>%
  left_join(denton_mean, by = "data") %>%
  fill(consumo_familias_ibge, .direction = "down") %>%
  select(consumo_familias_iae, consumo_familias_ibge, serie_ajustada) %>%
  rename(serie_ajustada_consumo_familias = serie_ajustada)-> consumo_familias


df %>%
  mutate(data = as.Date(as.yearmon(time))) %>%
  filter(data >= "1992-01-01" ) %>%
  left_join(denton_mean2, by = "data") %>%
  fill(consumo_familias_ibge, .direction = "down") %>%
  select(consumo_familias_iae, consumo_familias_ibge, serie_ajustada) %>%
  rename(serie_ajustada_consumo_familias = serie_ajustada)-> consumo_familias2

consumo_familias <- bind_rows(consumo_familias, consumo_familias2)

###### Consumo do Governo  #####

### Tidyng 

df %>%
  filter(time < "1992-01-01") %>%
  select(consumo_governo_iae) %>%
  ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
  filter(time > "1980-02-01") %>%
  filter(time < "1992-02-01") %>%
  select(consumo_governo_ibge) %>%
  drop_na() %>%
  ts(start = c(1980), frequency = 1) -> ibge_trimestral

df %>%
  filter(time >= "1991-12-01") %>%
  select(consumo_governo_iae) %>%
  ts(start = c(1992, 1), frequency = 12) -> iae_mensal2


df %>%
  filter(time > "1992-02-01") %>%
  select(consumo_governo_ibge) %>%
  drop_na() %>%
  ts(start = c(1992, 1), frequency = 4) -> ibge_trimestral2




#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
            conversion = "mean", to =  "monthly",
            method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)



denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))


temp3 <- td(ibge_trimestral2 ~ 0 + iae_mensal2,
            conversion = "mean", to =  "monthly",
            method = "denton-cholette", criterion = "proportional")


denton_mean2 <- predict(temp3)



denton_mean2 <- data.frame(serie_ajustada = as.matrix(denton_mean2),
                           data = as.Date(as.yearmon(time(denton_mean2))))



df %>%
  mutate(data = as.Date(as.yearmon(time))) %>%
  filter(data < "1992-01-01" ) %>%
  left_join(denton_mean, by = "data") %>%
  fill(consumo_governo_ibge, .direction = "down") %>%
  select(consumo_governo_iae, consumo_governo_ibge, serie_ajustada) %>%
  rename(serie_ajustada_consumo_governo = serie_ajustada)-> consumo_governo


df %>%
  mutate(data = as.Date(as.yearmon(time))) %>%
  filter(data >= "1992-01-01" ) %>%
  left_join(denton_mean2, by = "data") %>%
  fill(consumo_governo_ibge, .direction = "down") %>%
  select(consumo_governo_iae, consumo_governo_ibge, serie_ajustada) %>%
  rename(serie_ajustada_consumo_governo = serie_ajustada)-> consumo_governo2

consumo_governo <- bind_rows(consumo_governo, consumo_governo2)


###### FBCF  #####

### Tidyng 

df %>%
  filter(time < "1992-01-01") %>%
  select(fbcf_iae) %>%
  ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
  filter(time > "1980-02-01") %>%
  filter(time < "1992-02-01") %>%
  select(fbcf_ibge) %>%
  drop_na() %>%
  ts(start = c(1980), frequency = 1) -> ibge_trimestral

df %>%
  filter(time >= "1991-12-01") %>%
  select(fbcf_iae) %>%
  ts(start = c(1992, 1), frequency = 12) -> iae_mensal2


df %>%
  filter(time > "1992-02-01") %>%
  select(fbcf_ibge) %>%
  drop_na() %>%
  ts(start = c(1992, 1), frequency = 4) -> ibge_trimestral2




#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
            conversion = "mean", to =  "monthly",
            method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)



denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))


temp3 <- td(ibge_trimestral2 ~ 0 + iae_mensal2,
            conversion = "mean", to =  "monthly",
            method = "denton-cholette", criterion = "proportional")


denton_mean2 <- predict(temp3)



denton_mean2 <- data.frame(serie_ajustada = as.matrix(denton_mean2),
                           data = as.Date(as.yearmon(time(denton_mean2))))



df %>%
  mutate(data = as.Date(as.yearmon(time))) %>%
  filter(data < "1992-01-01" ) %>%
  left_join(denton_mean, by = "data") %>%
  fill(fbcf_ibge, .direction = "down") %>%
  select(fbcf_iae, fbcf_ibge, serie_ajustada) %>%
  rename(serie_ajustada_fbcf = serie_ajustada)-> fbcf


df %>%
  mutate(data = as.Date(as.yearmon(time))) %>%
  filter(data >= "1992-01-01" ) %>%
  left_join(denton_mean2, by = "data") %>%
  fill(fbcf_ibge, .direction = "down") %>%
  select(fbcf_iae, fbcf_ibge, serie_ajustada) %>%
  rename(serie_ajustada_fbcf = serie_ajustada)-> fbcf2

fbcf <- bind_rows(fbcf, fbcf2)



###### Exportação  #####

### Tidyng 

df %>%
  filter(time < "1992-01-01") %>%
  select(exportacao_iae) %>%
  ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
  filter(time > "1980-02-01") %>%
  filter(time < "1992-02-01") %>%
  select(exportacao_ibge) %>%
  drop_na() %>%
  ts(start = c(1980), frequency = 1) -> ibge_trimestral

df %>%
  filter(time >= "1991-12-01") %>%
  select(exportacao_iae) %>%
  ts(start = c(1992, 1), frequency = 12) -> iae_mensal2


df %>%
  filter(time > "1992-02-01") %>%
  select(exportacao_ibge) %>%
  drop_na() %>%
  ts(start = c(1992, 1), frequency = 4) -> ibge_trimestral2




#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
            conversion = "mean", to =  "monthly",
            method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)



denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))


temp3 <- td(ibge_trimestral2 ~ 0 + iae_mensal2,
            conversion = "mean", to =  "monthly",
            method = "denton-cholette", criterion = "proportional")


denton_mean2 <- predict(temp3)



denton_mean2 <- data.frame(serie_ajustada = as.matrix(denton_mean2),
                           data = as.Date(as.yearmon(time(denton_mean2))))



df %>%
  mutate(data = as.Date(as.yearmon(time))) %>%
  filter(data < "1992-01-01" ) %>%
  left_join(denton_mean, by = "data") %>%
  fill(exportacao_ibge, .direction = "down") %>%
  select(exportacao_iae, exportacao_ibge, serie_ajustada) %>%
  rename(serie_ajustada_exportacao = serie_ajustada) -> exportacao


df %>%
  mutate(data = as.Date(as.yearmon(time))) %>%
  filter(data >= "1992-01-01" ) %>%
  left_join(denton_mean2, by = "data") %>%
  fill(exportacao_ibge, .direction = "down") %>%
  select(exportacao_iae, exportacao_ibge, serie_ajustada) %>%
  rename(serie_ajustada_exportacao = serie_ajustada)-> exportacao2

exportacao <- bind_rows(exportacao, exportacao2)




###### Importação  #####

### Tidyng 

df %>%
  filter(time < "1992-01-01") %>%
  select(importacao_iae) %>%
  ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
  filter(time > "1980-02-01") %>%
  filter(time < "1992-02-01") %>%
  select(importacao_ibge) %>%
  drop_na() %>%
  ts(start = c(1980), frequency = 1) -> ibge_trimestral

df %>%
  filter(time >= "1991-12-01") %>%
  select(importacao_iae) %>%
  ts(start = c(1992, 1), frequency = 12) -> iae_mensal2


df %>%
  filter(time > "1992-02-01") %>%
  select(importacao_ibge) %>%
  drop_na() %>%
  ts(start = c(1992, 1), frequency = 4) -> ibge_trimestral2




#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
            conversion = "mean", to =  "monthly",
            method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)



denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))


temp3 <- td(ibge_trimestral2 ~ 0 + iae_mensal2,
            conversion = "mean", to =  "monthly",
            method = "denton-cholette", criterion = "proportional")


denton_mean2 <- predict(temp3)



denton_mean2 <- data.frame(serie_ajustada = as.matrix(denton_mean2),
                           data = as.Date(as.yearmon(time(denton_mean2))))



df %>%
  mutate(data = as.Date(as.yearmon(time))) %>%
  filter(data < "1992-01-01" ) %>%
  left_join(denton_mean, by = "data") %>%
  fill(importacao_ibge, .direction = "down") %>%
  select(importacao_iae, importacao_ibge, serie_ajustada) %>%
  rename(serie_ajustada_importacao = serie_ajustada) -> importacao


df %>%
  mutate(data = as.Date(as.yearmon(time))) %>%
  filter(data >= "1992-01-01" ) %>%
  left_join(denton_mean2, by = "data") %>%
  fill(importacao_ibge, .direction = "down") %>%
  select(importacao_iae, importacao_ibge, serie_ajustada) %>%
  rename(serie_ajustada_importacao = serie_ajustada)-> importacao2

importacao <- bind_rows(importacao, importacao2)





######### Bind ###########


exporta_denton <- function(caminho) {
  wb = createWorkbook()
  if (rlang::is_string(caminho)) {

    
    df_final <- bind_cols(agro, extracao, transformacao,
                          eletrica, construcao, comercio, transporte,
                          s_infor, i_financ, s_imob, o_serv, apu, impostos,
                          consumo_familias, consumo_governo, fbcf, exportacao, importacao)
    
    df_final %>%
      select(starts_with("serie_ajustada")) %>%
      names() -> nomes
    
    for (i in nomes) {
      sheet = createSheet(wb, i)
      
      df_final %>%
        select(starts_with(i)) %>% as_tibble() -> fast_obj
      
      addDataFrame(fast_obj, sheet=sheet, startColumn=1)
    }
    
    saveWorkbook(wb, paste(caminho, "xlsx", sep = "."))
    
  }
  else{
    rlang::abort(message = "O caminho precisa ser string :)")
  }
}

exporta_denton("Denton_Resultados_Final")

