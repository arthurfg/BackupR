library(basedosdados)
library(tidyverse)

## Base de Turmas

set_billing_id("casebd")

query <- bdplyr("br_inep_censo_escolar.turma")%>%
  filter(ano == 2018, sigla_uf == "RJ")%>%
  select(id_municipio, ano, sigla_uf, rede, etapa_ensino, id_escola)

tumas_rj <- bd_collect(query)

## Agrupando as turmas por categoria de ensino

ed_infantil <- c(1:3,56)
fundamental <- c(4:21,41, 22:24)
medio <- 25:38
profissional <- c(39,40,64,68,67,73,74)
eja <- c(43:48,51,58,60:63,69:72)



turmas_rj <- tumas_rj %>%
  mutate(classes = case_when(etapa_ensino %in% ed_infantil ~ "Educação Infantil",
                             etapa_ensino %in% fundamental ~"Ensino Fundamental",
                             etapa_ensino %in% medio ~ "Ensino Médio",
                             etapa_ensino %in% profissional ~ "Ensino Profissional",
                             etapa_ensino %in% eja ~"Educação de Jovens e Adultos")) %>% distinct(id_escola) %>% nrow()
  drop_na(etapa_ensino)

turmas_rj %>% filter(classes == "Educação de Jovens e Adultos") %>% distinct(id_escola) %>%
  summarise(total = n())
  
## Base de Escolas

query2 <- bdplyr("br_inep_censo_escolar.escola")%>%
  filter(ano == 2018, sigla_uf == "RJ")%>%
  select(id_municipio, ano, sigla_uf, rede, id_escola, regular, comum_creche, comum_pre,
         comum_fund_ai, comum_fund_af, comum_medio_medio, comum_medio_integrado, comum_medio_normal, comum_prof,
         especial_exclusiva, especial_exclusiva_creche, especial_exclusiva_pre, especial_exclusiva_fund_ai,
         especial_exclusiva_fund_af, especial_exclusiva_medio_medio, especial_exclusiva_medio_integr, especial_exclusiva_medio_normal,
         especial_exclusiva_prof, especial_exclusiva_eja_fund, especial_exclusiva_eja_medio, eja, comum_eja_fund, comum_eja_medio,
         comum_eja_prof, fundamental_ciclos)

escolas_rj <- bd_collect(query2)

escolas_rj %>% distinct(id_escola)%>% nrow()

