install.packages("repurrrsive")
library(repurrrsive)
library(tidyverse)
gap_small <- gap_split[1:10]

countries <- names(gap_small)


gap_small[[2]] %>%
  ggplot(aes(x= year, y = lifeExp)) +
  geom_line()+
  labs(title =  countries[[2]])

plots <- map2(.x = gap_small, .y = countries,
     ~ggplot(.x,aes(gdpPercap, lifeExp))+
       geom_point()+
       labs(title = .y))

plots
?map

regressions <- map(.x = gap_small,
                        .f = ~lm(lifeExp ~ gdpPercap,data = .x))

a <- regressions %>%
  map(~summary(.x))

ggplotRe
plot(lm_bel)

lm_bel <- lm(lifeExp ~ gdpPercap,data = gap_small[[10]])
summary(lm_bel)
pred <- tibble(pred = predict(lm_bel, gap_small[[10]] ), )


plots2 <- map2(.x = gap_small, .y = regressions,
              ~ggplot(.x, aes(gdpPercap, lifeExp))+
                geom_point()+
                geom_line(data = fortify(.y), aes(x = gdpPercap, y = .fitted))+
                labs(title = .x$country))


plots2
ggplot(gap_small[[10]], aes(gdpPercap, lifeExp))+
  geom_point() +
  # Add the line using the fortified fit data, plotting the x vs. the fitted values
  geom_line(data = fortify(lm_bel), aes(x = gdpPercap, y = .fitted))
