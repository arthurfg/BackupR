a <- c(1:20)
View(a)
a <- as.vector(a)

n1 <- 200 # número de observações
n2 <- 1000 # número de repetições
bh <- matrix(ncol = 3, nrow = n2) # armazenador das estimativas
for(k in 1:n2){
  x <- rnorm(n1,0,1) # gera a série
  x2 <- rnorm(n1,0,1)
  z1 <- rnorm(n1,0,1) # gera a série
  y <- 1 + 2*x + 3*x2 + z1 # gera a série
  #M01 <- lm(y~x) # estima a regressão
  bh[k, ] <- lm(y ~ x + x2)$coefficients # armazena os coeficientes
} # final do loop
par(mfrow = c(1, 3)) # divide o gráfico
hist(bh[,1]) # histograma das estimativas
hist(bh[,2]) # histograma das estimativas
hist(bh[,3]) # histograma das estimativas

mean(bh[,1])
mean(bh[,2])
mean(bh[,3])
