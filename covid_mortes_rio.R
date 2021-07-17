getwd()
library(viridis)
library(rlang)
library(tidyverse)
library(tm)
library(lubridate)
library(ggplot2)
mortes <- Mortes_rio %>%
  mutate(data = gsub("/","-",Mortes_rio$mes_ano))




mortes <- mortes %>%
  mutate(data = gsub("\\.","",Mortes_rio$mes_ano))

mortes$mes_ano <- NULL
mortes$data <- gsub("/","-",mortes$data)

mortes$data <- as_date(mortes$data)

write.csv(mortes, "mortes.csv")

mortes$ano <- as_factor(year(mortes$data))
mortes$mes <- month(mortes$data)

df %>%
  ggplot(aes(x=data, y=Óbitos, color= Ano))+
  geom_line()

mortes <- mortes[-69,]
mortes_dia$2017 <- gsub("-","", mortes_dia$2017)
mortes_dia$2018 <- gsub("-","", mortes_dia$2018)
mortes_dia$2019 <- gsub("-","", mortes_dia$2019)
mortes_dia$2020 <- gsub("-","", mortes_dia$2020)



df %>%
  na.omit() %>%
  ggplot(aes(x = data, y = Óbitos)) +
  geom_line(color = "darkorchid4") +
  facet_wrap( ~ Ano ) +
  labs(title = "Precipitation - Boulder, Colorado",
       subtitle = "Use facets to plot by a variable - year in this case",
       y = "Daily precipitation (inches)",
       x = "Date") + theme_bw(base_size = 15) 


mortes_dia$`Dia do óbito`<-NULL
mortes_dia$2020 <- as.character(mortes_dia$2020)
df <- mortes_dia %>%
  pivot_longer(cols = c("2017",'2018','2019','2020'),
               names_to= "Ano",
               values_to = "Óbitos",
               values_drop_na = TRUE)
df$data <- paste(df$Ano, df$mês,df$dia, sep = "-")
df$data <- as_date(df$data)
df$mês <- NULL
df$dia <- NULL
df$Ano <- as.factor(df$Ano)
df$Dia <- day(df$data)
df$Mes <- month(df$data)
df$data2 <- format(df$data, format="%m/%d")
df$data2 <- as_factor(df$data2)

min <- as.Date("2020-01-01")
max <- as.Date("2020-12-12")
p + scale_x_date(limits = c(min, max))

plot <- df2 %>%
  group_by(data) %>%
  #filter(data < "2020-08-31" | Ano_c == "2020")%>%
  ggplot(aes(x = data, y = Óbitos)) +
  geom_line(aes(color = Ano), size= 1)+
  #facet_wrap( ~ Ano ) +
  labs(title = "Óbitos no Estado do Rio de Janeiro",
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

plot1 <- plot +scale_x_date(limits = c(min, max))+
  ylim(320, 800)

a<- df[2:3]
ts_a <- ts(a, start = as.Date('2017-01-01'), end = as.Date('2020-08-31'), frequency = 12)  # Frequency is 12 so data is monthly

ggsave(plot = plot1,
       filename = "Estado.png",
       width = 1200, height = 611,
       dpi = 300,
       limitsize = F)

df$data <- str_replace(df$data, "2017","2020")
df$data <- str_replace(df$data, "2018","2020")
df$data <- str_replace(df$data, "2019","2020")


plot +scale_x_date(limits = c(min, max))+
  ylim(320, 800)

df2$Ano[df2$Ano == "2017"] <- "2017-2019"
df2$Ano[df2$Ano == "2018"] <- "2017-2019"
df2$Ano[df2$Ano == "2019"] <- "2017-2019"

df2<-df  

if (df2$Ano == "2020") {
  df2$Ano2 <- "2020"
} else {
  df2$Ano2 <- "2017-2019"
}
  
df2$Ano <- as.factor(df2$Ano)

df2$Mes <- as.factor(df2$Mes)

df2$Ano_c <- as.character(df2$Ano)


plot2 <- df2 %>%
  group_by(Mes)%>%
  ggplot(aes(x = Mes, y = Óbitos)) +
  geom_boxplot(aes(fill=Ano))+
  #facet_wrap(~Mes)
  labs(title = "Óbitos no Estado do Rio de Janeiro",
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
  
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  #geom_jitter(color="black", size=0.4, alpha=0.9)



variety=rep(LETTERS[1:7], each=40)
treatment=rep(c("high","low"),each=20)
note=seq(1:280)+sample(1:150, 280, replace=T)
data=data.frame(variety, treatment ,  note)