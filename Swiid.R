library(tidyverse)

load("swiid9_0.rda")

p <- swiid_summary %>%
  filter(country == c("Brazil", "China", "India", "Argentina", "Ecuador"),
         year > 1960) %>%
  group_by(country)%>%
  ggplot(aes(x=year, y=gini_disp)) +
  geom_line(aes(color= country)) +
  scale_x_continuous(breaks=seq(1960, 2020, 5)) +
  scale_y_continuous(breaks = seq(0,100, 5))+
  geom_ribbon(aes(ymin = gini_disp-1.96*gini_disp_se,
                  ymax = gini_disp+1.96*gini_disp_se, fill= country), alpha = .25)+
  theme_bw() +
  labs(x = "Ano",
       y = "",
       title = "SWIID Índice de Gini, Renda per capita",
       caption = "Fonte: SWIID Versão 9.0 \
       Elaboração: Arthur Gusmão")


p + scale_fill_discrete(name= "País")

p + scale_fill_brewer(palette = "Set1")+ scale_color_brewer(palette = "Set1")

a <- swiid_summary %>%
  filter(country == c("United States", "Brazil"),
         year > 1995) %>%
  group_by(country)

a %>%
  #filter(country== "Brazil")%>%
  ggplot(aes(x=year, y=gini_disp)) +
  geom_line(aes(color= country)) +
  geom_ribbon(aes(ymin = gini_disp-1.96*gini_disp_se,
                  ymax = gini_disp+1.96*gini_disp_se,
                  linetype=NA), alpha = .25)

paises <- c("United States", "Brazil", 'China', "South Africa", "Egypt", "India")
