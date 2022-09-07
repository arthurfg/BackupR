### Libs 

library(tidyverse)
library(basedosdados)
devtools::install_github("tidyverse/dbplyr")
devtools::install_version()


### Importing 

df <- read_csv("ieps_1.csv")
df2 <- read_csv("ieps_2.csv")


df <- df %>%
    left_join(df2, by = c("id_estado", "ano"))


### From BD

set_billing_id("casebd")

query <- "SELECT ano, populacao, id_municipio
FROM `basedosdados.br_ibge_populacao.municipio`
WHERE ano > 2009 AND ano < 2021"

query2 <- "SELECT ano, populacao, sigla_uf
FROM `basedosdados.br_ibge_populacao.uf`
WHERE ano > 2009 AND ano < 2021"

pop_mun <- read_sql(query)
pop_uf <- read_sql(query2)
## Join and tidying

pop_mun <- pop_mun %>%
    mutate(id_municipio = as.numeric(id_municipio),
           ano = as.numeric(ano))

pop_uf <- pop_uf %>%
    mutate(ano = as.numeric(ano))

df <- df %>%
    rename(id_municipio = id_munic_7,
           id_uf = id_estado,
           sigla_uf = estado_abrev) %>%
    left_join(pop_mun, by = c("id_municipio", "ano")) %>%
    left_join(pop_uf, by = c("sigla_uf", "ano")) %>%
    rename(populacao_mun = populacao.x,
           populacao_uf = populacao.y) %>%
    mutate(desp_tot_saude_mun = desp_tot_saude_pc_mun * populacao_mun,
           desp_recp_saude_mun = desp_recp_saude_pc_mun * populacao_mun,
           desp_tot_saude_mun_def = desp_tot_saude_pc_mun_def * populacao_mun,
           desp_recp_saude_mun_def = desp_recp_saude_pc_mun_def * populacao_mun,
           desp_tot_saude_uf = desp_tot_saude_pc_uf * populacao_uf,
           desp_recp_saude_uf = desp_recp_saude_pc_uf * populacao_uf,
           desp_tot_saude_uf_def = desp_tot_saude_pc_uf_def * populacao_uf,
           desp_recp_saude_uf_def = desp_recp_saude_pc_uf_def  * populacao_uf)


write.csv2(x = df, file = "ieps_gasto_saude.csv", sep = ";")
