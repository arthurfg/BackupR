install.packages("AER")
install.packages("stargazer")
install.packages("olsrr")
install.packages("plm")
library(olsrr)
library(AER)
library(tidyverse)
library(stargazer)
library(plm)
data("Fatalities")
glimpse(Fatalities)

# relação entre taxação sobre cerveja e acidentes de carro com vitimas fatais

# taxa de fatalidade

Fatalities1982 <- Fatalities %>%
  mutate(fatal_rate = fatal / pop*10000) %>%
  filter(year == "1982")
  
Fatalities1988 <- Fatalities %>%
  mutate(fatal_rate = fatal / pop*10000) %>%
  filter(year == "1988")

## regressão simples com os aos de 1982 e 1988 #### 

fatal1982 <- lm(fatal_rate ~ beertax,
                data = Fatalities1982)

fatal1988 <- lm(fatal_rate ~ beertax,
                data = Fatalities1988)


stargazer(fatal1982,fatal1988, type = "text", column.labels = c("1982","1988"))
ols_test_normality(fatal1988)



##  regressão com efeitos fixos ####
Fatalities <- Fatalities %>%
  mutate(fatal_rate = fatal / pop*10000)

fatal_fe <- plm(fatal_rate ~ beertax,
                data = Fatalities,
                index = c("state" , "year"),
                model = "within")

fatal_fe%>%
  stargazer(type = "text")

coeftest(fatal_fe, vcov. = vcovHC, type = "HC1")

?coeftest

# two way fixed effects

fatal_twfe <- plm(fatal_rate ~ beertax, 
                      data = Fatalities,
                      index = c("state", "year"), 
                      model = "within", 
                      effect = "twoways")

coeftest(fatal_twfe, vcov = vcovHC, type = "HC1")
