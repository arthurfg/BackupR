library(devtools)
library(sidrar)
library(tidyverse)
library(lubridate)
library(stringr)
library(stringi)
library(scales)

install_github("rpradosiqueira/sidrar")

###### importando os dados #####
tx_desocupacao_tri <- get_sidra(api = "/t/6468/n1/all/v/4099/p/all/d/v4099%201")
tx_desocupacao_mes <- get_sidra(api = "/t/6381/n1/all/v/4099/p/all/d/v4099%201")
po <- get_sidra(api = "/t/6320/n1/all/v/4090/p/all/c11913/96165")
tx_combinada <- get_sidra(api = "/t/6439/n1/all/v/4114/p/last%2075/d/v4114%201")
po_vinculo <- get_sidra(api = "/t/6320/n1/all/v/8435/p/last%2022/c11913/31722,31723,31727,96165,96171/d/v8435%201")



##### limpando as bases e fazendo os gráficos #####

tx_desocupacao_tri %>%
  mutate(date = yq(... =`Trimestre (Código)` ))%>%
  mutate(date1 = quarter(date, with_year = T))%>%
  select(date1,Valor, Trimestre, date)%>%
  ggplot(aes(x= date1, y = Valor))+
  geom_line(colour = "black")
  

tx_desocupacao_mes %>%
  mutate(date = ym(... =`Trimestre Móvel (Código)` ))%>%
  select(Valor, date)%>%
  ggplot(aes(x= date, y = Valor))+
  geom_line(colour = "black")+
  scale_x_date(breaks = date_breaks("3 months"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=90, hjust=1))

po %>%
  mutate(date = ym(... =`Trimestre Móvel (Código)` ))%>%
  select(Valor, date)%>%
  ggplot(aes(x= date, y = Valor))+
  geom_line(colour = "black", size = 0.8)+
  theme_classic()+
  scale_x_date(breaks = date_breaks("3 months"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
  

tx_combinada %>%
  mutate(date = ym(... =`Trimestre Móvel (Código)` ))%>%
  mutate(date1 = format(date,"%m" ))%>%
  mutate(ano = year(date))%>%
  select(Valor, date, ano, date1)%>%
  ggplot(mapping = aes(x= date1, y = Valor, group = as.factor(ano), colour = as.factor(ano)))+
  geom_line(size = 0.8)+
  geom_point()+
  theme_classic()+
  scale_color_brewer(palette="Dark2")

po_vinculo %>%
  mutate(date = ym(... =`Trimestre Móvel (Código)` ))%>%
  mutate(tipo = case_when(po_vinculo$`Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 31722 ~ "Privado com carteira",
                          po_vinculo$`Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 31723 ~ "Privado sem carteira",
                          po_vinculo$`Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 31727 ~ "Setor Público",
                          po_vinculo$`Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 96171 ~ "Conta própria",
                          TRUE ~ "Total"))%>%
  select(Valor, date, tipo)%>%
  ggplot(aes(x= date, y = Valor, group = as.factor(tipo), colour = as.factor(tipo)))+
  geom_line(size = 0.8, aes(linetype=as.factor(tipo)))+
  geom_point()+
  theme_classic()+
  scale_x_date(breaks = date_breaks("3 months"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  scale_color_brewer(palette="Dark2")



