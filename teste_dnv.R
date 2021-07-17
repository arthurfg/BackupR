library(tidyverse)
install.packages("tidyverse")
install.packages("basedosdados")
library("basedosdados")
set_billing_id("erudite-buckeye-273005")

query <- "SELECT * FROM `basedosdados.br_ibge_nomes_brasil.quantidade_municipio_nome_2010`"
df <- read_sql(query, page_size = 100000)
query2 <- "SELECT * FROM `basedosdados.br_bd_diretorios_brasil.municipio`"
municipio <- read_sql(query2, page_size = 1000)

br <- df %>% 
  group_by(nome) %>%
  summarise(total = sum(qtde_nascimentos_ate_2010))

sorted <- br %>%
  arrange(desc(total))

sorted["index"] <- 1:nrow(sorted)

achar_nomes <- function(name){
 indice <- sorted %>% 
    filter(nome == name) %>%
    pull(index)
 indice <- as.integer(indice)
 menor <- indice - 10
 maior <- indice + 10
 range_indice <- sorted %>%
   filter(index %in% menor:maior)
 print(range_indice)
  
}

a <- achar_nomes("Arthur")

indice + 10
df$id_municipio <- as.character(df$id_municipio)

merged <- inner_join(df, municipio, by = "id_municipio")

merged %>%
  filter(sigla_uf == "RJ") %>%
  select(id_municipio, sigla_uf, nome, qtde_nascimentos_ate_2010, municipio, regiao, uf) %>%
  filter(municipio == "Macaé") %>%
  summarise(total = sum(qtde_nascimentos_ate_2010),
            freq_relativa = qtde_nascimentos_ate_2010/total,
            nome = nome,
            per_10k = round(freq_relativa * 10000)) %>%
  arrange(desc(freq_relativa))


