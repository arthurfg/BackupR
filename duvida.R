library(tidyverse)
library(survey)
library(srvyr)

### Importando e lendo os microdados

pof_morador <- ler_pof_geral("/Users/apple/Documents/pof_bd/dados_rds/MORADOR.txt")

pof_cv <- ler_pof_geral("/Users/apple/Documents/pof_bd/input/CONDICOES_VIDA.txt")

## Pequenas modificaçoes

pof_morador <- pof_morador %>% 
  mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
         NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
         COD_INFORMANTE = str_pad(COD_INFORMANTE, 2, "left", "0"),
         id_dom = str_c(COD_UPA, NUM_DOM),
         id_uc  = str_c(COD_UPA, NUM_DOM, NUM_UC),
         id_pes = str_c(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE),
         PESO_FINAL = as.numeric(PESO_FINAL),
         RENDA_TOTAL = as.numeric(RENDA_TOTAL),
         V0403 = as.numeric(V0403),
         cod_upa = as.numeric(COD_UPA),
         id = 1)

pof_cv <- pof_cv %>% 
  mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
         NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
         COD_INFORMANTE = str_pad(COD_INFORMANTE, 2, "left", "0"),
         id_dom = str_c(COD_UPA, NUM_DOM),
         id_uc  = str_c(COD_UPA, NUM_DOM, NUM_UC),
         id_pes = str_c(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE),
         PESO_FINAL = as.numeric(PESO_FINAL),
         RENDA_TOTAL = as.numeric(RENDA_TOTAL),
         id = 1) %>%
         mutate(fome = if_else(V6114 == 1 | V6120 == 1, 1, 0)) 

##

teste <- pof_cv %>%
    group_by(id_dom) %>%
    summarise(fome = max(fome))

teste <- pof_morador %>%
    left_join(teste, by = "id_dom")

options( survey.lonely.psu = "adjust" )

## objeto svydesign

pof.des.survobj <- svydesign( ids = ~cod_upa ,
                              strata = ~ESTRATO_POF , 
                              data = teste , 
                              weights = ~PESO_FINAL )

## total da população 

svytotal(~id, design = pof.des.survobj)

##       total      SE
## id 207103790 
## total deu certo para o BR

## testando para o seu outro artigo

# filtra domínio de interesse: pessoas indígenas em São Paulo
pof.des.survobj2 <- subset( pof.des.survobj , UF == 35 & V0405 == 5 )

# estima totais e variâncias
svytotal( ~id , pof.des.survobj2 )

##     total    SE
## id 148046 44622
## certinho

## agora para a fome
svyby(~id, ~fome, design = pof.des.survobj, svytotal)

##   fome       id       se
##0    0 49642144 862893.1
##1    1 13649504 410886.1

## 13649504/207103790 = 0.06590659 (diferente do resultado do artigo)