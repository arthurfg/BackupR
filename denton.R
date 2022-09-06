### Importing the Libs

library(tempdisagg)
library(readxl)
library(tidyverse)
library(lubridate)
library(xlsx)
library(zoo)


#####


df <- read_excel("denton.xlsx", 
                         sheet = "Sheet1", col_types = c("date", 
                                                            "numeric", "numeric"))

df


### Tidyng 

df %>%
    select(-time, -ibge_trimestral) %>%
    ts(start = c(1980, 1), frequency = 12) -> iae_mensal


df %>%
    select(-time, -iae_mensal) %>%
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

temp3 <- td(ibge_trimestral ~ 0 + iae_mensal,
           conversion = "mean", to =  "monthly",
           method = "denton-cholette", criterion = "proportional")

plot(predict(temp3))

