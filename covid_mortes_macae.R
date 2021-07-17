### Macaé Mortes 2020 vs 2017-2019
## Arthur de Figueiredo Gusmão
getwd()
library(tidyverse)
library(lubridate)
library(readxl)

macae$`Dia do óbito` <- paste(macae$`Dia do óbito`,"2020", sep = "/")
mortes_macae <- read_excel("Macae_mortes.xlsx")
macae$`Dia do óbito` <- str_replace_all(macae$`Dia do óbito`,"/","-")
macae$data <- as.Date(macae$`Dia do óbito`, format="%m-%d-%y")

mortes_macae <- Macae_mortes
mortes_macae$`2020` <- macae$`2020`

macae$dia <- day(macae$data)
macae$mês <- month(macae$data)
mortes_macae <- macae

macae <- mortes_macae %>%
  pivot_longer(cols = c("2017",'2018','2019','2020'),
               names_to= "Ano",
               values_to = "Óbitos",
               values_drop_na = TRUE)
macae$data <- paste(macae$Ano, macae$mês,macae$dia, sep = "-")
macae$data <- as_date(macae$data)
macae$Ano <- as_factor(macae$Ano)

macae$data <- str_replace(macae$data, "2017","2020")
macae$data <- str_replace(macae$data, "2018","2020")
macae$data <- str_replace(macae$data, "2019","2020")

macae$Ano[macae$Ano == "2017"] <- "2017-2019"
macae$Ano[macae$Ano == "2018"] <- "2017-2019"
macae$Ano[macae$Ano == "2019"] <- "2017-2019"

macae$Ano <- replace_na(macae$Ano, "2017")
macae$Ano <- as.character(macae$Ano)


plot_macae <- macae %>%
  group_by(data) %>%
  ggplot(aes(x = data, y = Óbitos)) +
  geom_line(aes(color = Ano), size= 1)+
  #facet_wrap( ~ Ano ) +
  labs(title = "Óbitos no Município de Macaé",
       subtitle = "Número de óbitos registrados por dia no ano de 2020 comparado com os anos anteriores",
       y = "Óbitos por residência",
       x = "Mês do ano",
       caption = "Fonte: Sistema de Informações sobre Mortalidade - SIM \
       Elaboração: Arthur Gusmão") +
  scale_color_manual(values=c('#999999','#264653'))+
  theme_minimal(base_size = 15)+
  theme(legend.position="top")+
  theme(plot.title = element_text(size=20, face="bold"),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10, face = "bold"))

plot_macae +scale_x_date(limits = c(min, max))+
  ylim(0, 11)
 

macae$mês <- as_factor(macae$mês)

plot3 <- macae %>%
  group_by(mês)%>%
  #filter(data<max)%>%
  ggplot(aes(x = mês, y = Óbitos)) +
  geom_boxplot(aes(fill=Ano))+
  labs(title = "Óbitos no Município de Macaé",
       subtitle = "Óbitos registrados por mês no ano de 2020 comparado com os anos anteriores",
       y = "Óbitos por residência",
       x = "Mês do ano",
       caption = "Fonte: Sistema de Informações sobre Mortalidade - SIM \
       Elaboração: Arthur Gusmão") +
  scale_fill_brewer(palette="Spectral")+
  scale_fill_manual(values=c('#2a9d8f','#e76f51'))+
  theme_minimal(base_size = 15)+
  theme(legend.position="top")+
  theme(plot.title = element_text(size=20, face="bold"),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10, face = "bold"))
plot3  
