# instala os pacotes:
libs <- c("DBI", "RSQLite" , "fst" , "survey", "basedosdados")
libs.novas <- libs[ !( libs %in% installed.packages()[ , "Package" ] ) ]
if( length( libs.novas ) ) install.packages( libs.novas )

# libs

library(DBI)
library(RSQLite)
library(survey)
library(basedosdados)
library(tidyverse)

### variáveis


vars <- c(ano, trimestre, id_uf, id_upa, id_estrato, 
          id_domicilio, V1008, V1028, V4013, V2007, VD4009, V4019, VD4020)


## Importando
set_billing_id("casebd")

pnad <- bdplyr("br_ibge_pnadc.microdados") %>%
  filter(ano == 2019, trimestre == 4) %>%
  select(c(ano, trimestre, id_uf, id_upa, id_estrato, 
           id_domicilio, V1008, V1028, V4013, V2007, VD4009, V4019, VD4020))

# to rds - with compression
basedosdados::bd_write_rds(.lazy_tbl = pnad,
                           "pnad_201904.rds")
  
df <- bd_collect(query)


pnad2019 <- readRDS(file = "pnad_201904.rds")

pnadc.design <-
  svydesign(
    ids = ~ id_upa + V1008 ,
    strata = ~ id_estrato ,
    weights = ~ V1028 ,
    data = pnad2019,
    nest = TRUE )

options(scipen = 999) # opdao que deixa de mostrar numeros no formato cientifico.

# total de pessoas na transformação

total_transf.19 <- svytotal(x=~V4013>=10010 & V4013<=33002 , design = pnadc.design, na.rm = TRUE)

a <- as.data.frame(total_transf.19)
# ver o total de pessoas para saber se estou pegando toda população

total.19 <- svytotal(x=~V2007, design = pnadc.design, na.rm = TRUE)

# ver o total de pessoas formais - empregado no setor privado com carteira assinada + empregador com cnpj

formais_transf.1 <- svytotal(x=~V4013>=10010 & V4013<=33002, design=subset(pnadc.design, VD4009 == 
                                                                             "Empregador" & V4019 == "Sim"), na.rm = TRUE) 

formais_transf.2 <- svytotal(x=~V4013>=10010 & V4013<=33002, design=subset(pnadc.design, VD4009 == 
                                                                             "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_transf <- formais_transf.1[[2]]+formais_transf.2[[2]]


# total de informais 

total_informais_scarteira_transf <- svytotal(x=~V4013>=10010 & V4013<=33002, design=subset(pnadc.design, 
                                                                                           VD4009 == "Empregado no setor privado sem carteira de trabalho assinada"),
                                             na.rm = TRUE)


informais_autonomos_transf.1 <- svytotal(x=~V4013>=10010 & V4013<=33002, design=subset(pnadc.design, 
                                                                                       VD4009 %in% c("Conta-própria", "Trabalhador familiar auxiliar")), na.rm = TRUE)


informais_autonomos_transf.2 <- svytotal(x=~V4013>=10010 & V4013<=33002, design=subset(pnadc.design, V4019 == 
                                                                                         "Não" & VD4009 == "Empregador"), na.rm = TRUE)


total_informais_autonomos <- informais_autonomos_transf.1[[2]] + informais_autonomos_transf.2[[2]]

