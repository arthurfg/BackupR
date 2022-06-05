## Arthur Gusmão
## 29/04/2022 -
### testse macbook

### Importing the Libs

library(tempdisagg)
library(readxl)
library(tidyverse)
library(lubridate)
library(xlsx)
library(zoo)


### Importing the data

ed_privada <- read_excel("ed_publica.xlsx", 
                         sheet = "Planilha2", col_types = c("date", 
                                                            "numeric"))
servicos <- read_excel("ed_publica.xlsx", 
                         sheet = "Planilha3", col_types = c("date", 
                                                            "numeric"))
ed_publica <- read_excel("ed_publica.xlsx", 
                         sheet = "Planilha4", col_types = c("numeric", 
                                                            "numeric"))
### Tidyng
# Criando os objetos tipo ts

ed_privada %>%
  select(-data) %>%
  ts(start = c(2000, 1), frequency = 12)-> ed_privada_ts

servicos %>%
  select(-data) %>%
  ts(start = c(2000, 1), frequency = 12)-> servicos_ts


ed_publica %>%
  select(-data) %>%
  ts(start = c(2000, 1), frequency = 1)-> ed_publica_ts

## Plot

plot(ed_privada_ts)
plot(servicos_ts)
plot(ed_publica_ts)

#### Temp. Desagreggation (Denton)

## Série mensal que, somada, resulta no total da anual.

temp <- td(ed_publica_ts ~ 0 + ed_privada_ts,
           conversion = "sum", to =  "monthly",
           method = "denton-cholette",  criterion = "proportional")

plot(predict(temp))

temp2 <- td(ed_publica_ts ~ 0 + servicos_ts,
            conversion = "sum", to =  "monthly",
            method = "denton-cholette",  criterion = "proportional")

plot(predict(temp2))

## Série mensal que, na média, resulta no total da anual.

temp3 <- td(ed_publica_ts ~ 0 + ed_privada_ts,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")

plot(predict(temp3))

temp4 <- td(ed_publica_ts ~ 0 + servicos_ts,
            conversion = "mean", to =  "monthly",
            method = "denton-cholette", criterion = "proportional")

plot(predict(temp4))


##### Exportando

denton_privada_sum <- predict(temp)
denton_servicos_sum <- predict(temp2)
denton_privada_mean <- predict(temp3)
denton_servicos_mean <- predict(temp4)

denton_privada_sum <- data.frame(serie_ajustada=as.matrix(denton_privada_sum), data=as.Date(as.yearmon(time(denton_privada_sum)))) ## data frame da série criada
denton_servicos_sum <- data.frame(serie_ajustada=as.matrix(denton_servicos_sum), data=as.Date(as.yearmon(time(denton_servicos_sum))))


ed_publica %>%
  mutate(data = as.Date(as.yearmon(data))) -> ed_publica


#### Tabelas comparando os resultados 

### Usando a educação privada como série indicadora


ed_privada %>%
  mutate(data = as.Date(as.yearmon(data))) %>%
  left_join(x = ., ed_publica, by = "data") %>%
  left_join(x = ., denton_privada_sum, by = "data") %>%
  fill(educacao_publica, .direction = "down") %>%
  group_by(as_factor(educacao_publica)) %>% 
  mutate(Soma = sum(serie_ajustada))%>%
  ungroup() %>%
  mutate(growth_rate_indicadora = (educacao_privada - lag(educacao_privada,1))/ lag(educacao_privada,1),
         growth_rate_serie_ajustada = (serie_ajustada - lag(serie_ajustada,1))/ lag(serie_ajustada,1)) %>%
  mutate(diferenca_growth = growth_rate_indicadora - growth_rate_serie_ajustada) %>%
  rename(tx_crescimento_indicadora = growth_rate_indicadora,
         tx_crescimento_ajustada = growth_rate_serie_ajustada,
         diferenca_tx_crescimento = diferenca_growth) -> denton_resultados_privada ## pipeline que cria o dataframe

### Usando a inflacao de serviços como série indicadora

servicos %>%
  mutate(data = as.Date(as.yearmon(data))) %>%
  left_join(x = ., ed_publica, by = "data") %>%
  left_join(x = ., denton_servicos_sum, by = "data") %>%
  fill(educacao_publica, .direction = "down") %>%
  group_by(as_factor(educacao_publica)) %>% 
  mutate(Soma = sum(serie_ajustada))%>%
  ungroup() %>%
  mutate(growth_rate_indicadora = (servicos_livres - lag(servicos_livres,1))/ lag(servicos_livres,1),
         growth_rate_serie_ajustada = (serie_ajustada - lag(serie_ajustada,1))/ lag(serie_ajustada,1)) %>%
  mutate(diferenca_growth = growth_rate_indicadora - growth_rate_serie_ajustada) %>%
  rename(tx_crescimento_indicadora = growth_rate_indicadora,
         tx_crescimento_ajustada = growth_rate_serie_ajustada,
         diferenca_tx_crescimento = diferenca_growth) -> denton_resultados_servicos


## Para xlsx
xlsx::write.xlsx(x = denton_resultados_privada, file = "denton_resultados_privada.xlsx", sheetName = "Resultados")
xlsx::write.xlsx(x = denton_resultados_servicos, file = "denton_resultados_servicos.xlsx", sheetName = "Resultados")

