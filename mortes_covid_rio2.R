rio <- read_excel("rio_mortes.xlsx")


rio$`Dia do óbito` <- paste(rio$`Dia do óbito`,"2020", sep = "/")
rio$`Dia do óbito` <- str_replace_all(rio$`Dia do óbito`,"/","-")
rio$data <- as.Date(rio$`Dia do óbito`, format="%m-%d-%y")
rio$dia <- day(rio$data)
rio$mês <- month(rio$data)


rio <- rio %>%
  pivot_longer(cols = c("2017",'2018','2019','2020'),
               names_to= "Ano",
               values_to = "Óbitos",
               values_drop_na = TRUE)
rio$data <- paste(rio$Ano, rio$mês,rio$dia, sep = "-")
rio$data <- as_date(rio$data)
rio$Ano <- as_factor(rio$Ano)

rio$data <- str_replace(rio$data, "2017","2020")
rio$data <- str_replace(rio$data, "2018","2020")
rio$data <- str_replace(rio$data, "2019","2020")

rio$Ano[rio$Ano == "2017"] <- "2017-2019"
rio$Ano[rio$Ano == "2018"] <- "2017-2019"
rio$Ano[rio$Ano == "2019"] <- "2017-2019"

rio$Ano <- replace_na(rio$Ano, "2017")
rio$Ano <- as.character(rio$Ano)
rio$mês <- as_factor(rio$mês)


plot4 <- rio %>%
  group_by(mês)%>%
  filter(Óbitos>111)%>%
  ggplot(aes(x = mês, y = Óbitos)) +
  geom_boxplot(aes(fill=Ano))+
  labs(title = "Óbitos no Município do Rio de Janeiro",
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
plot4

rio$data <- as_date(rio$data)

plot_rio <- rio %>%
  group_by(data) %>%
  filter(Óbitos>118)%>%
  ggplot(aes(x = data, y = Óbitos)) +
  geom_line(aes(color = Ano), size= 1)+
  #facet_wrap( ~ Ano ) +
  labs(title = "Óbitos no Município do Rio de Janeiro",
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

plot_rio +scale_x_date(limits = c(min,max
))+
  ylim(80,450)
plot_rio
