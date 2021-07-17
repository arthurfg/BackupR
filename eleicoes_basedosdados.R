library(tidyverse)
install.packages("geobr")
library(geobr)
names(resultados)
resultados <- resultados %>%
  select(sigla_uf, id_municipio, id_municipio_tse, cargo, numero_candidato, nome_urna_candidato, sigla_partido, votos)

resultados$nome_urna_candidato <- tolower(resultados$nome_urna_candidato)
candidatos$nome_urna_candidato <- tolower(candidatos$nome_urna_candidato)


merged <- merge(candidatos, resultados, by = "nome_urna_candidato")

a <- merged %>%
  group_by(id_municipio,genero)%>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n))

merged %>%
  group_by(id_municipio,genero, raca)%>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n))

mun3 <- read_municipality(code_muni="RJ", year=2019)

mun4 <- read_municipality(code_muni="RJ", year=2019)

ggplot()+
  geom_sf(data = mun)+
  geom_sf(data = mun2, aes(fill= as.numeric(freq)))+
  theme_classic()+
  scale_fill_gradient(low="darkorange", high="red")

scale_fil

mun2 <- full_join(mun, a, by= c("code_muni" = "id_municipio"))
mun3 <- left_join(mun3, a, by= c("code_muni" = "id_municipio"))

mun2 <- mun2 %>%
  filter(genero == "feminino")

mun2$freq2 <- scales::percent(mun2$freq)

teste <- full_join(candidatos,resultados, by = c("numero_candidato" , "nome_urna_candidato"))
teste <- teste %>%
  drop_na(cargo)

a <- teste %>%
  group_by(id_municipio, genero)%>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n))

mun2 <- full_join(estate, a, by= c("abbrev_state" = "estado_abrev"))

estate <- read_state(code_state = "all", year = 2019)

mun2 %>%
  filter(genero== "feminino") %>%
  mutate(freq2 = as.character.numeric_version(freq2))%>%
  ggplot()+
  geom_sf(aes(fill= freq))+
  scale_fill_viridis_c(direction = -1, limits = c(0.1, 0.22)) + # Escala de cores
  labs(fill = "Porcentagem de \natendimento de \ncoleta de esgoto",
       title = "Porcentagem de atendimento de coleta de esgoto, por município",
       subtitle = "Dados da CETESB, para o ano de 2018.")+
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa 

b <- mun3 %>%
  filter(genero== "feminino") %>%
  mutate(freq2= freq*100)
  
hist(b$freq,breaks = 10)

tema_mapa <-
  theme_bw() + # Escolhe o tema. Eu gosto do theme_bw() por ser bem simples/limpo
  
  # Os códigos abaixo são referentes à estética do tema,
  # como o tamanho da fonte, direção do texto,
  # linhas ao fundo, etc.
  
  theme(
    axis.text.y = element_text(
      angle = 90,
      hjust = 0.5,
      size = 8
    ),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = rel(0.8)),
    axis.title.x = element_text(size = rel(0.8)),
    panel.grid.major = element_line(
      color = gray(0.9),
      linetype = "dashed",
      size = 0.1
    ),
    panel.background = element_rect(fill = "white") +
      annotation_scale(location = "br", width_hint = 0.30)
  )
library(ggspatial)
install.packages("ggspatial")

b %>% 
  ggplot()+
  geom_sf(data = mun3)+
    geom_sf(aes(fill= freq2))+
    scale_fill_viridis_c(direction = -1, limits = c(1, 35), option = "magma") + # Escala de cores
    labs(fill = "",
         title = "Porcentagem de vereadoras eleitas, no Estado do RJ",
         subtitle = "Dados do TSE, para as eleições de 2020.",
         caption = "Obs: Os municípios na cor cinza não elegeram nenhuma vereadora \nElaboração: Arthur Gusmão")+
    annotation_north_arrow(
      location = "br",
      which_north = "true",
      height = unit(1, "cm"),
      width = unit(1, "cm"),
      pad_x = unit(0.1, "in"),
      pad_y = unit(0.1, "in"),
      style = north_arrow_fancy_orienteering
    ) +
    ggspatial::annotation_scale() +
    tema_mapa 


a <- a %>%
  filter(genero =="feminino" & raca ==c("preta", "parda") ) 
