install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
install.packages("actuar")
install.packages("ggrepel")
library(IntroCompFinR)
library(ggrepel)
library(PerformanceAnalytics)
library(tidyverse)
library(purrr)
library(quantmod)
transporte <- "POMO.4.SA"
Financeiro <- c("BBDC4.SA", "ITUB4.SA")
Extração <- c("PETR4.SA", "VALE3.SA")
Siderúrgica <- c("CSNA3.SA", "USIM5.SA")

tickers2 <- c("POMO4.SA", "BBDC4.SA", 'ITUB4.SA', "EMBR3.SA", "AZUL4.SA", "PETR4.SA",
             "IRBR3.SA", "RSID3.SA", "TAEE3.SA", 
             "MEAL3.SA")

tickers <- c(Financeiro,
             Extração,
             Siderúrgica)

cotacao = getSymbols(tickers2,
                     src = "yahoo",
                     from= "2015-10-30",
                     to= "2020-10-30",
                     periodicity = 'daily',
                     auto.assign = T,
                     warnings = F)
cotacao <- cotacao %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  na.omit() %>%
  abs()

retornos <- Return.calculate(cotacao, method = "discrete")#
retornos[1,] = 0
benchmark = 0.000223205

media_varianca <- matrix(data=NA,
                         nrow = 2,
                         ncol = length(tickers2))
for(i in(1:length(tickers2))){
  media_varianca[1,i] = mean(retornos[,i])
  media_varianca[2,i] = sd(retornos[,i])
}
media_varianca <- data.frame(t(media_varianca))
media_varianca_df <- cbind(media_varianca, tickers2)

names(media_varianca_df) <- c("Retorno Médio", "Volatilidade", "Ticker")
## PRIMEIROS TICKERS
media_varianca_df <- media_varianca_df %>%
  mutate(Setor = case_when(Ticker %in% Financeiro ~"Financeiro",
                            Ticker %in% Extração ~ "Extração",
                            Ticker %in% Siderúrgica ~ "Siderúrgica"),
         Sharpe = (media_varianca_df$`Retorno Médio` - benchmark)/Volatilidade)


## CARTEIRA XUXU
media_varianca_df <- media_varianca_df %>%
  mutate(Sharpe = (media_varianca_df$`Retorno Médio` - benchmark)/Volatilidade)
  
media_varianca_df %>%
  ggplot(aes(x = `Retorno Médio`, y= Volatilidade, colour = Ticker, label= Sharpe )) +
  geom_point()
      

## Markowitz

n = 5000
x=10
acoes <- sample(tickers2,
                          size =  x,
                          replace = F)
print(ações_sorteadas)

dados_sorteados <- media_varianca_df %>%
  filter(Ticker %in% ações_sorteadas) %>%
  arrange(factor(Ticker, levels = ações_sorteadas))

dados_sorteados <- media_varianca_df %>%
  arrange(factor(Ticker, levels = tickers2))

retornos_sorteados <- retornos[,c("PETR4.SA", "VALE3.SA", "USIM5.SA")]
retornos_sorteados <- retornos


names(retornos) <- tickers2

retornos_sorteados <- xts::xts(retornos_sorteados, index(retornos))
Cov <- cov(retornos_sorteados)
corr <- cor(retornos_sorteados)

w <- matrix(data = actuar::rpareto(n*x, shape = 1, scale = 1),
            nrow = n,
            ncol = x,
            byrow = T)
for(i in c(1:n)){
  
  w[i,] = w[i,] / sum(w[i,])
}

hist(w[,1],breaks = 100)

vol_w <- rep(NA, n)

for(i in c(1:n)){
 w_momento = matrix(data = w[i,],
                    ncol = 1)
 vol_momento = sqrt(t(w_momento) %*% Cov %*% w_momento)
 
 vol_w[i] <- vol_momento 
 
}

ret_w <- rep(NA, n)
soma <- rep(NA,x)

for(i in c(1:n)){
  for(j in c(1:x)){
    soma[j] = w[i,j] * dados_sorteados$`Retorno Médio`[j]
    
  }
  ret_w[i] <- sum(soma)
}

a <- cbind(vol_w,ret_w)
a <- data.frame(a)
a$Sharpe <- (a$ret_w- benchmark) / vol_w

a %>%
  ggplot(aes(x = vol_w, y = ret_w, colour= Sharpe))+
  geom_point(size= 0.7)+
  geom_line(data = fronteira, 
             mapping = aes(x=Volatilidade, y =fronteira$`Retorno Médio`),
             colour = "green", size=1.2)+
  geom_point(data = carteira_otima, 
             mapping = aes(x=Volatilidade, y =carteira_otima$`Retorno Médio`),
             colour = "red", size= 3)+
  annotate(geom="text", x=0.022, y=carteira_otima$`Retorno Médio`, label="Carteira Ótima",
                 color="black", size= 3.5)+
  annotate(geom="text", x=0.0330, y=0.0012, label="Fronteira Eficiente",
           color="black", size= 4)+
  scale_y_continuous(labels= scales::percent_format(accuracy = 0.01))+
  scale_x_continuous(labels= scales::percent_format(accuracy = 0.01))+
  scale_color_gradient2(low = "red", high = "blue", mid = "forestgreen")+
  labs(title = "Otimização de carteira com o modelo de Markowitz, 5000 simulações, 10 Ações",
       y= "Retorno Médio",
       x= "Volatilidade")+
  theme_grey()

ótimo <- tangency.portfolio(er = dados_sorteados$`Retorno Médio`,
                            cov.mat = Cov,
                            risk.free = benchmark,
                            shorts = F)
ótimo
pesos <- ótimo[["weights"]]
portfolio <- cbind(pesos, tickers2)
ret_esp_otimo <- ótimo[[2]]
vol_esp_otima <- ótimo[[3]]

carteira_otima <- tibble(ret_esp_otimo, vol_esp_otima)
names(carteira_otima) <- c("Retorno Médio", "Volatilidade")

fronteira_eficiente <- efficient.frontier(dados_sorteados$`Retorno Médio`, Cov, nport = 5000, shorts = short_selling)
fronteira_eficiente
attributes(fronteira_eficiente)

eficiente_ret <- fronteira_eficiente[[2]]
eficiente_vol <- fronteira_eficiente[[3]]

fronteira <- tibble(eficiente_ret, eficiente_vol)
names(fronteira) <- c("Retorno Médio", "Volatilidade")

carteira_otima$tag <- "Carteira Ótima"

p + geom_text_repel(data= carteira_otima, aes(label = tag), color = 'black',
                     size = 3.5)


a %>%
  ggplot(aes(x = vol_w, y = ret_w, colour= Sharpe))+
  geom_point(size= 0.7)+
  scale_y_continuous(labels= scales::percent_format(accuracy = 0.01))+
  scale_x_continuous(labels= scales::percent_format(accuracy = 0.01))+
  scale_color_gradient2(low = "red", high = "blue", mid = "darkorange")+
  labs(title = "Otimização de carteira com o modelo de Markowitz, 5000 simulações, 10 Ações",
       y= "Retorno Médio",
       x= "Volatilidade")+
  theme_grey()


portfolio <- data.frame(portfolio)
portfolio$y <- "Carteira"
portfolio$pesos <- as.numeric(portfolio$pesos)
portfolio %>%
  ggplot(aes(x=y, y=pesos, fill=tickers2 ))+
  geom_col()+
  scale_y_continuous(labels= scales::percent_format(accuracy = 0.01))
