library(tidyverse)
library(pdftools)
library(pdftables)
library(readxl)
library(janitor)


###### Abrindo todos os PDF's

dir <- "C://Arthur/BackupR/pdfs"

lista_pdfs <- list.files(dir,
                         pattern = "*.pdf", full.names = TRUE)

## renomeando

for(i in 1:length(lista_pdfs)){
  file.rename(lista_pdfs[i], paste0("arquivo", i, ".pdf"))
}


#### transformando todos em .xlsx ####

lista_pdfs <- list.files(dir,
                         pattern = "*.pdf", full.names = TRUE)

outputs <- map_chr(1:22, ~glue::glue("{.x}.xlsx"))

walk2(lista_pdfs, outputs, ~convert_pdf(.x, .y, 
                                        format = "xlsx-single",
                                        message = TRUE,
                                        api_key = "3qptp13nc0pb")) #obrigado purrr

### abrindo os xlsx's
df <- map(outputs,~read_excel(.x, col_names = FALSE))
patterns <- c("razão social|endereço:|cidade:|http|código|token|,|relatório| / |nome de contato:|sapucaia|telefone:|(21)|rio de janeiro|(ip: 200.0.202.72)|sao paulo|@|email|niterói|(11)|(17)|(22)|(24)|(19)|niteroi")

for (i in 1:22) {
  df[[i]] <- df[[i]]%>%
    select(2:ncol(df[[i]])) %>%
    rename_with(~ tolower(gsub("...", "coluna", .x, fixed = TRUE)))
    
}

##testando

a <- lapply(ops, function(df1) {df1 %>% filter(!is.na(coluna2))%>%
    select(coluna2)%>%
    mutate(coluna2 = tolower(coluna2)) %>%
    filter(!str_detect(coluna2, patterns))%>%
    pull()})


b <- lapply(ops, function(df1) {df1 %>% filter(!is.na(coluna3))%>%
    select(coluna3)%>%
    mutate(coluna3 = tolower(coluna3)) %>%
    filter(!str_detect(coluna3, patterns))%>%
    pull()})

base_final <- append(a %>% flatten_chr(), b %>% flatten_chr())
base_razao_social <- tibble(nome = base_final)

base_razao_social %>%
  distinct() %>%
  clean_names() %>%
  mutate(nome = abjutils::rm_accent(nome)) %>%
  xlsx::write.xlsx(file = "base_razao_social_v1.xlsx",
                   sheetName = "Tabela 1", append = FALSE)



### tentando pegar os e-mails

emails <- ops[[8]]
pattern_email <- c("Email:|@")

lapply(emails, function(df1) {df1 %>% filter()}) ## deixa pra depois

####teste####

url <- "pdf_teste.pdf"

convert_pdf("C://Arthur/BackupR/pdfs/arquivo1.pdf", output_file = "teste.xlsx", format = "xlsx-single", message = TRUE, api_key = "mju1tlcc0vnu")

teste <- read_excel("teste.xlsx", col_names = FALSE)

a <- teste %>%
  select(2:7)

a <- a %>%
  rename_with(~ tolower(gsub("...", "coluna", .x, fixed = TRUE)))

lista <- list()
lista_limpa <- list()
#### isso aqui importa
patterns <- c("razão social|endereço:|cidade:|http|código|token|,|relatório| / |nome de contato:|sapucaia|telefone:|(21)")

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
    
    ops[[i]]<- append(lista_limpa[["coluna2"]],lista_limpa[["coluna3"]])
    print(z)
  }
print(i)
}
ops <- as.data.frame(lista_limpa)
ops <- rbind(lista_limpa[[1]], lista_limpa[[2]])
ops<- list()

ops<- append(lista_limpa[["coluna2"]],lista_limpa[["coluna3"]])
