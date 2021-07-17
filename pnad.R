install.packages("PNADcIBGE")
library(PNADcIBGE)
library(survey)
library(tidyverse)
library(conve)
variaveis_selecionadas <- c("UF", "V2007", "V2009", "V2010", "V3007", "VD3001", "VD4001", "VD4002", "VD4020", "VD4035")
dadosPNADc <- get_pnadc(year = 2020, quarter = 2, vars = variaveis_selecionadas)

totalrenda <- svytotal(~VD4020, dadosPNADc, na.rm = T)
totalrenda

mediarenda <- svymean(~VD4020, dadosPNADc, na.rm = T)
mediarenda

txdesocup <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", dadosPNADc, na.rm = T)
txdesocup

dadosPNADc$one <-1

mediarendaM <- svymean(~VD4020, subset(dadosPNADc, UF == "Rio de Janeiro")  , na.rm = T)
mediarendaM

quantisrenda <- svyquantile(~VD4020, dadosPNADc, quantiles = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9 , 0.95, 0.99, 0.999), na.rm = T)
quantisrenda
dadosPNADc

variaveis_selecionadas <- c("UF", "V2007", "V2009", "V2010", "V3007", "VD3001", "VD4001", "VD4002", "VD4020", "VD4035")

sub <- subset(dadosPNADc, UF == "Rio de Janeiro")

quantisrenda <- svyquantile(~VD4020, sub, quantiles = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9 , 0.95, 0.99, 0.999), na.rm = T)
