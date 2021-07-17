library(tidyverse)
library(lubridate)
library(scales)
# Install
install.packages("wesanderson")
# Load
library(wesanderson)

df<- df1
df <- Pasta1
df$ano2 <- paste(df$ano,"01","01", sep = "-")
df$ano2 <-as_date(df$ano2)
df$percent <- percent(df$porcentagem)
df$fact <- as.factor(df$fact)


## df e p&d
df2 <- receita_pd
df2$ano2 <- paste(df2$ano,"01","01", sep = "-")
df2$ano2 <-as_date(df2$ano2)
df2$percent <- percent(df2$`pd/receita`)
df2$fact <- as.factor(df2$fact)


df %>%
  ggplot(aes(x =fact, y = percent(porcentagem), fill=as.factor(tipo_ind))) +
  geom_bar(stat = "identity", position = "dodge",width = 0.7)+
  labs(title = "Porcentagem das empresas que investiram em P&D e receberam apoio do governo para o investimento",
       subtitle = "Indústria Farmoquímica, Farmacêutica e da Transformação (total)",
       y = "",
       x = "",
       caption = "Fonte: Pesquisa de Inovação Técnologica - PINTEC \
       Elaboração: Arthur Gusmão") +
  theme_minimal(base_size = 15)+
  theme(legend.position="top")+
  theme(plot.title = element_text(size=13, face="bold"),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 10, face = "bold"))
  
df %>%
  ggplot(aes(x =ano, y = porcentagem)) +
  geom_line(aes(color = tipo_ind))

df2$ano <- as.factor(df2$ano)
df2$ano <- factor(df2$ano,levels = c("2011","2014",'2017'))

df2 %>%
  arrange(ano2)%>%
  ggplot(aes(x =tipo2, y = percent, fill=fact)) +
  geom_col(position = "dodge", stat = "identity")

df2 %>%
  arrange(ano2)%>%
  ggplot(aes(x =tipo, y = percent, fill=fact)) +
  geom_col()
  geom_line()

df2$fact <- factor(df2$fact, levels = df2$fact[order(df2$ano2)])
df2$tipo2 <- factor(df2$tipo, levels = df2$tipo[order(df2$fact)])

df2$tipo <- as_factor(df2$tipo)


df2 <- 
  df2 %>% 
  mutate(Ano = factor(x = ano,
                      levels = c("2011", "2014","2017")))
df2 %>% 
  ggplot(aes( x = tipo, y = percent, fill= ano)) +
  geom_col(stat = "identity", position = "dodge") +
  theme(legend.position = "bottom")
#########

receita_pd2 <- receita_pd

receita_pd$tipo <- factor(receita_pd$tipo, levels = c("Indústria da transformação", "Indústria Farmacêutica e Farmoquímica", "Equipamentos médicos",
                                                      "Materiais médicos", "Produtos farmoquímicos",
                                                      "Produtos farmacêuticos"))

p<- receita_pd %>%
  ggplot(aes( x = tipo, y = `pd/receita`, fill= as.factor(fact)))+
  geom_col(stat="identity", position = "dodge", width = 0.7)+
  labs(title = "Gasto em P&D em relação à receita líquida de vendas",
       subtitle = "",
       y = "",
       x = "",
       caption = "Fonte: Pesquisa de Inovação - PINTEC \
       Elaboração: Arthur Gusmão") +
  theme_classic(base_size = 15,
                base_family = "Times")+
  theme(legend.position="top")+
  theme(plot.title = element_text(size=13, face="bold"),
        plot.subtitle = element_text(size = 13),
        legend.text = element_text(size = 8),
        plot.caption = element_text(size = 8, face = "bold"),
        axis.text.y = element_text(size=7),
        legend.title = element_blank(),
        axis.text.x=element_text(size=7, face = "bold"))+
  scale_y_continuous(labels = percent,
                     breaks = seq(from = 0.0, to = 0.2, by = 0.005))
p + scale_fill_manual(values=c("#56cfe1", "#5e60ce", "#7400b8"))

p5<- receita_pd %>%
  ggplot(aes( x = tipo, y = pd_in, fill= as.factor(fact)))+
  geom_col(stat="identity", position = "dodge", width = 0.7)+
  labs(title = "Gasto em P&D em relação ao gasto total com as atividades inovativas",
       subtitle = "",
       y = "",
       x = "",
       caption = "Fonte: Pesquisa de Inovação - PINTEC \
       Elaboração: Arthur Gusmão") +
  theme_classic(base_size = 15,
                base_family = "Times")+
  theme(legend.position="top")+
  theme(plot.title = element_text(size=13, face="bold"),
        plot.subtitle = element_text(size = 13),
        legend.text = element_text(size = 8),
        plot.caption = element_text(size = 8, face = "bold"),
        axis.text.y = element_text(size=7),
        legend.title = element_blank(),
        axis.text.x=element_text(size=7, face = "bold"))+
  scale_y_continuous(labels = percent,
                     breaks = seq(from = 0.0, to = 1, by = 0.1))
p5 + scale_fill_manual(values=c("#56cfe1", "#5e60ce", "#7400b8"))



df3 <- parceria

df3 <- df3 %>%
  pivot_longer(cols = `Incentivo Fiscal à P&D`:`Financiamento à compra de máquinas e equipamentos para inovação`,
               names_to = "tipo_apoio",
               values_to = "apoio")

df3 <- df3 %>%
  mutate(tipo_total = apoio/total_apoio)

df3$tipo <- factor(df3$tipo, levels = c("Indústria da transformação", "Indústria Farmacêutica e Farmoquímica", "Equipamentos médicos",
                                                      "Materiais médicos", "Produtos farmoquímicos",
                                                      "Produtos farmacêuticos"))


p2<- df3 %>%
  ggplot(aes( x = tipo, y = `apoio/inov`, fill= as.factor(fact)))+
  geom_col(stat="identity", position = "dodge", width = 0.7)+
  labs(title = "Empresas que receberam apoio do governo para suas atividades inovativas em relação ao total de empresas que implementaram inovações",
       subtitle = "",
       y = "",
       x = "",
       caption = "Fonte: Pesquisa de Inovação - PINTEC \
       Elaboração: Arthur Gusmão") +
  theme_classic(base_size = 15,
                base_family = "Times")+
  theme(legend.position="top")+
  theme(plot.title = element_text(size=13, face="bold"),
        plot.subtitle = element_text(size = 13),
        plot.caption = element_text(size = 8, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.y = element_text(size=7),
        axis.text.x=element_text(size=7, face = "bold"))+
  scale_y_continuous(labels = percent,
                     breaks = seq(from = 0.0, to = 1, by = 0.1))
p2 + scale_fill_manual(values=c("#56cfe1", "#5e60ce", "#7400b8"))


p3 <- df3 %>%
  ggplot(aes( x = tipo, y = tipo_total, fill= as.factor(fact)))+
  geom_col(stat="identity", position = "dodge", width = 0.7)+
  facet_wrap(~tipo_apoio)+
  labs(title = "Tipos de programa de apoio em relação ao total de empresas que receberam apoio do governo para suas atividades inovativas",
       subtitle = "",
       y = "",
       x = "",
       caption = "Fonte: Pesquisa de Inovação - PINTEC \
       Elaboração: Arthur Gusmão") +
  theme_classic(base_size = 15,
                base_family = "Times")+
  theme(legend.position="top")+
  theme(plot.title = element_text(size=13, face="bold"),
        plot.subtitle = element_text(size = 13),
        plot.caption = element_text(size = 8, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x=element_text(size=7, angle = 90, face = "bold"),
        axis.text.y = element_text(size=5.5))+
  scale_y_continuous(labels = percent,
                     breaks = seq(from = 0.0, to = 1, by = 0.1))

p3 + scale_fill_manual(values=c("#56cfe1", "#5e60ce", "#7400b8"))


#### tipo de financ

df4 <- financ1
df4$Total <- NULL

df4 <- df4 %>%
  pivot_longer(cols = `Financiamento Próprio`:Exterior,
               names_to = "tipo_financ",
               values_to = "percent")

df4$tipo <- factor(df4$tipo, levels = c("Indústria da transformação", "Indústria Farmacêutica e Farmoquímica", "Equipamentos médicos",
                                        "Materiais médicos", "Produtos farmoquímicos",
                                        "Produtos farmacêuticos"))


p4 <- df4 %>%
  ggplot(aes( x = as.factor(fact), y = percent, fill= as.factor(tipo_financ)))+
  geom_col(stat="identity",position = "dodge", width = 0.7)+
  facet_wrap(~tipo)+
  labs(title = "Fonte de financiamento das atividades internas de P&D",
       subtitle = "",
       y = "",
       x = "",
       caption = "Fonte: Pesquisa de Inovação - PINTEC \
       Elaboração: Arthur Gusmão") +
  theme_classic(base_size = 15,
                base_family = "Times")+
  theme(legend.position="top")+
  theme(plot.title = element_text(size=13, face="bold"),
        plot.subtitle = element_text(size = 13),
        plot.caption = element_text(size = 8, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x=element_text(size=7, face = "bold"),
        axis.text.y = element_text(size=7))+
  scale_y_continuous(breaks = seq(from = 0.0, to = 100, by = 10))

p4 + scale_fill_manual(values=c("#80ffdb","#56cfe1", "#5e60ce", "#7400b8"))

