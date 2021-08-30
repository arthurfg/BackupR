library(devtools)
library(sidrar)
library(tidyverse)
library(lubridate)
library(stringr)
library(stringi)
library(scales)
library(zoo)
library(kableExtra)
library(knitr)
library(ggalt)

install_github("rpradosiqueira/sidrar")
install.packages("kableExtra")
install.packages("xfun")
install.packages("ggalt")

devtools::install_github("haozhu233/kableExtra")
###### importando os dados #####

## pnad 
tx_desocupacao_tri <- get_sidra(api = "/t/6468/n1/all/v/4099/p/all/d/v4099%201")
tx_desocupacao_mes <- get_sidra(api = "/t/6381/n1/all/v/4099/p/all/d/v4099%201")
po <- get_sidra(api = "/t/6320/n1/all/v/4090/p/all/c11913/96165")
tx_combinada <- get_sidra(api = "/t/6439/n1/all/v/4114/p/last%2075/d/v4114%201")
po_vinculo <- get_sidra(api = "/t/6320/n1/all/v/8435/p/last%2022/c11913/31722,31723,31727,96165,96171/d/v8435%201")
desalento <- get_sidra(api = "/t/6803/n1/all/v/9839,9845/p/last%2019/d/v9839%201,v9845%201")
tx_desocupacao_idade <- get_sidra(api = "/t/4094/n1/all/v/4099/p/last%206/c58/allxt/d/v4099%201")
tx_desocupacao_idade_total <- get_sidra(api = "/t/4094/n1/all/v/4090/p/all/c58/allxt")
po_idade <- get_sidra(api = "/t/4095/n1/all/v/4090/p/all/c1568/11628,11630,11632,11779,120706")
po_setor <- get_sidra(api = "/t/5434/n1/all/v/4090/p/all/c693/allxt")

## caged



##### limpando as bases e fazendo os gráficos #####

tx_desocupacao_tri %>%
  mutate(date = yq(... =`Trimestre (Código)` ))%>%
  mutate(date1 = quarter(date, with_year = T))%>%
  select(date1,Valor, Trimestre, date)%>%
  ggplot(aes(x= date1, y = Valor))+
  geom_line(size = 1L, colour = "#cb181d")+
  labs(x = "", y = "", 
       title = "Taxa de desocupação", 
       subtitle = "(Em %)", 
       caption = "Fonte: PNAD Contínua/IBGE \ 
       Elaboração: Arthur Gusmão, Thiago Fortes e Leonardo Grandelle ") +
  theme_light()+ theme(plot.title = element_text(face = "bold", size = 15))+ 
  theme(plot.subtitle = element_text(size = 10))+
  scale_x_yearqtr(format = '%Y.q%q')
  

tx_desocupacao_mes %>%
  mutate(date = ym(... =`Trimestre Móvel (Código)` ))%>%
  select(Valor, date)%>%
  ggplot(aes(x= date, y = Valor))+
  geom_line(size = 1L, colour = "#cb181d")+
  scale_x_date(breaks = date_breaks("3 months"),
               labels = date_format("%b/%Y"))+
  labs(x = "", y = "", 
       title = "Taxa de desocupação", 
       subtitle = "(Em %)", 
       caption = "Fonte: PNAD Contínua/IBGE \ 
       Elaboração: Arthur Gusmão, Thiago Fortes e Leonardo Grandelle ") +
  theme_light()+ theme(plot.title = element_text(face = "bold", size = 15))+ 
  theme(plot.subtitle = element_text(size = 10))+
  theme(axis.text.x=element_text(angle=90, hjust=1))

po %>%
  mutate(date = ym(... =`Trimestre Móvel (Código)` ))%>%
  select(Valor, date)%>%
  ggplot(aes(x= date, y = Valor))+
  geom_line(size = 1L, colour = "#cb181d")+
  theme_classic()+
  scale_x_date(breaks = date_breaks("3 months"),
               labels = date_format("%b/%Y"))+
  labs(x = "", y = "", 
       title = "População ocupada", 
       subtitle = "(Em mil pessoas)", 
       caption = "Fonte: PNAD Contínua/IBGE \ 
       Elaboração: Arthur Gusmão, Thiago Fortes e Leonardo Grandelle ") +
  theme_light()+ theme(plot.title = element_text(face = "bold", size = 15))+ 
  theme(plot.subtitle = element_text(size = 10))+
  theme(axis.text.x=element_text(angle=90, hjust=1))
  

tx_combinada %>%
  mutate(date = ym(... =`Trimestre Móvel (Código)` ))%>%
  mutate(date1 = format(date,"%m" ))%>%
  mutate(ano = year(date),
         Valor = Valor/100)%>%
  select(Valor, date, ano, date1)%>%
  filter(date > "2017-01-01")%>%
  ggplot(mapping = aes(x= date1, y = Valor, group = as.factor(ano), colour = as.factor(ano)))+
  geom_line(size = 0.8)+
  labs(x = "", y = "", 
       title = "Taxa combinada de desocupação e subocupação", 
       subtitle = "(Em %)", 
       caption = "Fonte: PNAD Contínua/IBGE \ 
       Elaboração: Arthur Gusmão, Thiago Fortes e Leonardo Grandelle ",
       colour = "") +
  theme_light()+ theme(plot.title = element_text(face = "bold", size = 15))+ 
  theme(plot.subtitle = element_text(size = 10),
        legend.position = "bottom")+
  scale_y_continuous(labels = scales::percent)+
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
  geom_line(size = 0.8)+
  geom_point()+
  geom_hline(yintercept = 0,linetype="dashed", size=0.8)+
  scale_x_date(breaks = date_breaks("3 months"),
               labels = date_format("%b/%Y"))+
  labs(x = "", y = "", 
       title = "PO por vínculo empregatício", 
       subtitle = "(Taxa de variação interanual)", 
       caption = "Fonte: PNAD Contínua/IBGE \ 
       Elaboração: Arthur Gusmão, Thiago Fortes e Leonardo Grandelle ",
       colour = "") +
  theme_light()+ theme(plot.title = element_text(face = "bold", size = 15))+ 
  theme(plot.subtitle = element_text(size = 10),
        legend.position = "bottom")+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  scale_color_brewer(palette="Dark2")



desalento %>%
  mutate(date = ym(... =`Trimestre Móvel (Código)` ))%>%
  mutate(tipo = case_when(desalento$`Variável (Código)` == 9839 ~ "Percentual de pessoas desalentadas",
                          TRUE ~ "Variação em relação ao mesmo trimestre do ano anterior"))%>%
  select(Valor, date, tipo) %>%
  filter(tipo == "Percentual de pessoas desalentadas")%>%
  ggplot(aes(x= date, y = Valor))+
  geom_line(size = 1L, colour = "#cb181d")+
  scale_x_date(breaks = date_breaks("3 months"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(x = "", y = "", 
       title = "Percentual de pessoas desalentadas", 
       subtitle = "(Em %)", 
       caption = "Fonte: PNAD Contínua/IBGE \ 
       Elaboração: Arthur Gusmão, Thiago Fortes e Leonardo Grandelle ",
       colour = "") +
  theme_light()+ theme(plot.title = element_text(face = "bold", size = 15))
  theme(legend.position="none")
  
tx_desocupacao_idade %>%
  mutate(date = yq(... =`Trimestre (Código)` ))%>%
  mutate(date1 = quarter(date, with_year = T)) %>%
  mutate(grupo = as.factor(`Grupo de idade`))%>%
  select(Valor, date, date1, grupo)%>%
  ggplot(aes( x = date1, y = Valor, colour = grupo))+
  geom_line(size = 0.8)+
  scale_x_yearqtr(format = '%Y.q%q')

tx_desocupacao_idade_total %>%
  mutate(date = yq(... =`Trimestre (Código)` ))%>%
  mutate(date1 = quarter(date, with_year = T)) %>%
  mutate(grupo = as.factor(`Grupo de idade`))%>%
  select(Valor, date, date1, grupo)%>%
  group_by(grupo)%>%
  mutate(pct_change = (Valor - lag(Valor,4))/lag(Valor,4))%>%
  mutate(teste = lag(Valor, 4)) %>%
  filter(date1 > "2018-01-01") %>%
  ggplot(aes( x = date1, y = pct_change, colour = grupo))+
  geom_line(size = 0.8)+
  geom_hline(yintercept = 0,linetype="dashed", size=0.8)+
  labs(x = "", y = "", 
       title = "População ocupada - por faixa etária", 
       subtitle = "(Variação interanual, em %)", 
       caption = "Fonte: PNAD Contínua/IBGE \ 
       Elaboração: Arthur Gusmão, Thiago Fortes e Leonardo Grandelle ",
       colour = "") +
  theme_light()+ theme(plot.title = element_text(face = "bold", size = 15))+ 
  theme(plot.subtitle = element_text(size = 10),
        legend.position = "bottom")+
  scale_x_yearqtr(format = '%Y.q%q')+
  scale_y_continuous(labels = scales::percent)+
  scale_color_brewer(palette="Dark2")
  



po_idade %>%
  mutate(date = yq(... =`Trimestre (Código)` ))%>%
  mutate(date1 = quarter(date, with_year = T))%>%
  mutate(tipo = case_when(po_idade$`Nível de instrução (Código)` == 11628 ~ "Fundamental completo",
                   po_idade$`Nível de instrução (Código)` == 11630 ~ "Médio Completo",
                   po_idade$`Nível de instrução (Código)` == 11632 ~ "Superior Completo",
                   po_idade$`Nível de instrução (Código)` == 11779 ~ "Fundamental Incompleto",
                   TRUE ~ "Sem instrução e menos de 1 ano de estudo"))%>%
  mutate(tipo = as.factor(tipo))%>%
  select(date1,Valor, Trimestre, date, tipo) %>%
  group_by(tipo)%>%
  mutate(pct_change = (Valor - lag(Valor,4))/lag(Valor,4))%>%
  mutate(teste = lag(Valor, 4)) %>%
  filter(date1 > "2018-01-01") %>%
  ggplot(aes( x = date1, y = pct_change, colour = tipo))+
  geom_line(size = 0.8)+
  geom_hline(yintercept = 0,linetype="dashed", size=0.8)+
  labs(x = "", y = "", 
       title = "População ocupada - por grau de instrução", 
       subtitle = "(Variação interanual, em %)", 
       caption = "Fonte: PNAD Contínua/IBGE \ 
       Elaboração: Arthur Gusmão, Thiago Fortes e Leonardo Grandelle ",
       colour = "") +
  theme_light()+ theme(plot.title = element_text(face = "bold", size = 15))+ 
  theme(plot.subtitle = element_text(size = 10),
        legend.position = "bottom")+
  scale_x_yearqtr(format = '%Y.q%q')+
  scale_y_continuous(labels = scales::percent)+
  scale_color_brewer(palette="Dark2")

po_setor %>%
  mutate(date = yq(... =`Trimestre (Código)` ))%>%
  mutate(date1 = quarter(date, with_year = T))%>%
  mutate(tipo = as.factor(`Grupamento de atividades no trabalho principal - PNADC (Código)`))%>%
  group_by(tipo) %>%
  mutate(pct_change = (Valor - lag(Valor,4))/lag(Valor,4))%>%
  mutate(teste = lag(Valor, 4))%>%
  filter(date > "2019-01-01") %>%
  ungroup()%>%
  select(date1, `Grupamento de atividades no trabalho principal - PNADC`, pct_change)%>%
  mutate(pct_change = scales::percent(pct_change))%>%
  pivot_wider(names_from = date1,
              values_from = pct_change)%>%
  kbl()%>%
  kable_styling()


setor_caged %>%
  mutate(Setor = as.factor(Setor))%>%
  ggplot(aes(x = jun_20, xend = jun_21, y=Setor, group = Setor))+
  geom_dumbbell(colour = "#a3c4dc",
                colour_xend = "#0e668b",
                size = 4,
                dot_guide = T,
                dot_guide_size = 0.15,
                dot_guide_colour = "grey60")

setor_caged2 %>%
  mutate(`Variação Interanual`= scales::percent(`Variação Interanual`))%>%
  kbl() %>%
  kable_styling(font_size = 10)
