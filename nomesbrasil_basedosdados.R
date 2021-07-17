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

query <- "SELECT * FROM `basedosdados.br_ibge_nomes_brasil.quantidade_municipio_nome_2010`"
df <- read_sql(query, page_size = 100000)
query2 <- "SELECT * FROM `basedosdados.br_bd_diretorios_brasil.municipio`"
municipio <- read_sql(query2, page_size = 1000)

merged <- inner_join(df, municipio, by = "id_municipio")

merged %>%
  filter(sigla_uf == "RJ") %>%
  select(id_municipio, sigla_uf, nome, qtde_nascimentos_ate_2010, municipio, regiao, uf) %>%
  filter(municipio == "Macaé") %>%
  summarise(total = sum(qtde_nascimentos_ate_2010),
            freq_relativa = qtde_nascimentos_ate_2010/total,
            nome = nome,
            per_10k = round(freq_relativa * 10000)) %>%
  arrange(freq_relativa) %>%
  mutate(nome=factor(nome, levels=nome))%>%
  top_n(70)%>%
  ggplot(aes(x= nome, y= per_10k))+
  geom_segment( aes(xend=nome, yend=0))+
  geom_point( size=2, color="orange")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()+
  labs(title = "Top 70 nomes mais usados em Macaé",
       subtitle = "",
       y = "",
       x = "",
       caption = "Fonte: Censo Demográfico 2010 \
       Elaboração: Arthur Gusmão") +
  theme_classic(base_size = 15,
                base_family = "Times")+
  theme(legend.position="top")+
  theme(plot.title = element_text(size=13, face="bold"),
        plot.subtitle = element_text(size = 13),
        legend.text = element_text(size = 8),
        plot.caption = element_text(size = 8, face = "bold"),
        axis.text.y = element_text(size=8),
        legend.title = element_blank(),
        axis.text.x=element_text(size=7, face = "bold"))

