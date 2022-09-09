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

df

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


plot(iae_mensal)
plot(ibge_trimestral)


#### Temp. Desagreggation (Denton)

## Série mensal que, somada, resulta no total da anual.

temp <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "sum", to =  "monthly",
           method = "denton-cholette",  criterion = "proportional")

plot(predict(temp))

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)

plot(iae_mensal)
lines(predict(temp2), col = "blue")

denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

glimpse(denton_mean)

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(agro_ibge, .direction = "down") %>%
    select(agro_iae, agro_ibge, serie_ajustada) -> agro


##### Extracao ######

df %>% glimpse()


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


plot(iae_mensal)
plot(ibge_trimestral)


#### Temp. Desagreggation (Denton)


## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)

plot(iae_mensal)
lines(predict(temp2), col = "blue")

denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

glimpse(denton_mean)

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(ext_ibge, .direction = "down") %>%
    select(ext_iae, ext_ibge, serie_ajustada) -> extracao

extracao %>% View()


##### Transf ######

df %>% glimpse()


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


plot(iae_mensal)
plot(ibge_trimestral)


#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)

plot(iae_mensal)
lines(predict(temp2), col = "blue")

denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

glimpse(denton_mean)

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(transf_ibge, .direction = "down") %>%
    select(transf_iae, transf_ibge, serie_ajustada) -> transformacao

transformacao %>% View()

##### Elétrica ######

df %>% glimpse()


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


plot(iae_mensal)
plot(ibge_trimestral)


#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)

plot(iae_mensal)
lines(predict(temp2), col = "blue")

denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

glimpse(denton_mean)

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(eletr_ibge, .direction = "down") %>%
    select(eletr_iae, eletr_ibge, serie_ajustada) -> eletrica

eletrica %>% View()

######## Construção #########

df %>% glimpse()


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


plot(iae_mensal)
plot(ibge_trimestral)


#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)

plot(iae_mensal)
lines(predict(temp2), col = "blue")

denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

glimpse(denton_mean)

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(const_ibge, .direction = "down") %>%
    select(const_iae, const_ibge, serie_ajustada) -> construcao

construcao %>% View()

####### Comércio ########



df %>% glimpse()


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


plot(iae_mensal)
plot(ibge_trimestral)


#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)

plot(iae_mensal)
lines(predict(temp2), col = "blue")

denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

glimpse(denton_mean)

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(comer_ibge, .direction = "down") %>%
    select(comer_iae, comer_ibge, serie_ajustada) -> comercio

comercio %>% View()



####### Transporte ########




df %>% glimpse()


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


plot(iae_mensal)
plot(ibge_trimestral)


#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)

plot(iae_mensal)
lines(predict(temp2), col = "blue")

denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

glimpse(denton_mean)

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(transp_ibge, .direction = "down") %>%
    select(transp_iae, transp_ibge, serie_ajustada) -> transporte

transporte %>% View()



######### S Inform #########




df %>% glimpse()


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


plot(iae_mensal)
plot(ibge_trimestral)


#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)

plot(iae_mensal)
lines(predict(temp2), col = "blue")

denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

glimpse(denton_mean)

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(s_infor_ibge, .direction = "down") %>%
    select(s_infor_iae, s_infor_ibge, serie_ajustada) -> s_infor

s_infor %>% View()




##########  I financ ###########

df %>% glimpse()


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


plot(iae_mensal)
plot(ibge_trimestral)


#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)

plot(iae_mensal)
lines(predict(temp2), col = "blue")

denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

glimpse(denton_mean)

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(i_financ_ibge, .direction = "down") %>%
    select(i_financ_iae, i_financ_ibge, serie_ajustada) -> i_financ

i_financ %>% View()


######## S Imob #########


df %>% glimpse()


### Tidyng 

df %>%
    #filter(time > "1980-11-01") %>%
    select(s_imob_iae) %>%
    ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
    filter(time > "1981-02-01") %>%
    select(s_imob_ibge) %>%
    drop_na() %>%
    ts(start = c(1981, 1), frequency = 4) -> ibge_trimestral


plot(iae_mensal)
plot(ibge_trimestral)


#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)

plot(iae_mensal)
lines(predict(temp2), col = "blue")

denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

glimpse(denton_mean)

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(s_imob_ibge, .direction = "down") %>%
    select(s_imob_iae, s_imob_ibge, serie_ajustada) -> s_imob

s_imob %>% View()


######### O Serv #######



df %>% glimpse()


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


plot(iae_mensal)
plot(ibge_trimestral)


#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)

plot(iae_mensal)
lines(predict(temp2), col = "blue")

denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

glimpse(denton_mean)

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(o_serv_ibge, .direction = "down") %>%
    select(o_serv_iae, o_serv_ibge, serie_ajustada) -> o_serv

o_serv %>% View()


######### APU ############



df %>% glimpse()


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


plot(iae_mensal)
plot(ibge_trimestral)


#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)

plot(iae_mensal)
lines(predict(temp2), col = "blue")

denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

glimpse(denton_mean)

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(apu_ibge, .direction = "down") %>%
    select(apu_iae, apu_ibge, serie_ajustada) -> apu

apu %>% View()



###### Impostos ########


df %>% glimpse()


### Tidyng 

df %>%
    #filter(time > "1980-11-01") %>%
    select(impostos_iae) %>%
    ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
    filter(time > "1981-02-01") %>%
    select(impostos_ibge) %>%
    drop_na() %>%
    ts(start = c(1981, 1), frequency = 4) -> ibge_trimestral


plot(iae_mensal)
plot(ibge_trimestral)


#### Temp. Desagreggation (Denton)

## Série mensal que, na média, resulta no total da anual.

temp2 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")


denton_mean <- predict(temp2)

plot(iae_mensal)
lines(predict(temp2), col = "blue")

denton_mean <- data.frame(serie_ajustada = as.matrix(denton_mean),
                          data = as.Date(as.yearmon(time(denton_mean))))

glimpse(denton_mean)

df %>%
    mutate(data = as.Date(as.yearmon(time))) %>%
    #filter(data > "1980-11-01" ) %>%
    left_join(denton_mean, by = "data") %>%
    fill(impostos_ibge, .direction = "down") %>%
    select(impostos_iae, impostos_ibge, serie_ajustada) -> impostos

impostos %>% View()



######### Bind ###########

df_final <- bind_cols(agro, extracao, transformacao, eletrica, construcao, comercio, transporte, s_infor, i_financ, s_imob, o_serv, apu, impostos)

df_final %>% glimpse()
xlsx::write.xlsx(x = df_final, file = "denton_iae.xlsx", sheetName = "Resultados")
