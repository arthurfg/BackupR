library(tidyverse)
if (!require("remotes")) {
  install.packages("remotes")
}
# on 64-bit Windows
remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")

library(tabulizer)
library(rJava)
install.packages("pdftools")
library(pdftools)

url <- "pdf_teste.pdf"

teste <- extract_tables(
  file   = url, 
  method = "decide", 
  output = "data.frame")

a <- pdf_text(url)
a[1]

cat(a[2])

install_github("expersso/pdftables")

library(pdftables)

a <- convert_pdf(url,"teste.csv",api_key = "mju1tlcc0vnu")
convert_pdf(url, output_file = "teste.xlsx", format = "xlsx-single", message = TRUE, api_key = "mju1tlcc0vnu")

library(readxl)
teste <- read_excel("teste.xlsx", col_names = FALSE)
View(teste)

a <- teste %>%
  select(2:7)

a <- a %>%
  rename_with(~ tolower(gsub("...", "coluna", .x, fixed = TRUE)))

lista <- list()
lista_limpa <- list()
#### isso aqui importa
patterns <- c("razão social|endereço:|r |cidade:|http|código|token|,|relatório| / |nome de contato:|sapucaia|telefone:|(21)")

vetor <- c("coluna2", "coluna3")

for (i in names(a)) {
  for (z in 1:6) {
    a %>%
      drop_na(i)%>%
      pull(i) -> lista[[i]]
    
    c <- as.data.frame(lista[[i]])%>%
      rename(coluna = colnames(.)) 
    
    c %>%
      mutate(x = tolower(coluna))%>% 
      select(x) %>%
      filter(!str_detect(x, patterns))%>% pull(x) -> lista_limpa[[i]]
    print(z)
  }
print(i)
}
a <- as.data.frame(lista[[1]]) %>% rename(coluna = colnames(.))

patterns <- c("razão social|endereço:|r |cidade:|http|código|token|,|relatório| / |nome de contato:|sapucaia")
b <- a %>%
  mutate(x = tolower(coluna)) %>%
  select(x) %>%
  filter(!str_detect(x, patterns)) %>%pull()

b
  
for (z in g) {
  print(z)
  
}
  
