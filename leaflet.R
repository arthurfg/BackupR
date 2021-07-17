setwd("C:/Users/Arthur/Documents/Mapa")
install.packages("leaflet")
library(leaflet)
install.packages("rgdal")
library(rgdal)
library(dplyr)
install.packages("htmltools")
library(htmltools)
getwd()

m <- leaflet()
m 

m <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  setView(lng = -43.2097222222, lat = -22.9036111111, zoom = 10) %>%
  addPolygons(data=bairros,
              color = "#660000",
              weight = 1) %>%
  addCircleMarkers(lng = tiro_morte$longitude_ocorrencia,
                   lat = tiro_morte$latitude_ocorrencia,
                   color = "red",
                   weight = 1,
                   radius = 5,
                   group = "Com mortes",
                   label = lapply(tiro_morte$label, HTML)) %>%
  addCircleMarkers(lng = tiro_sem$longitude_ocorrencia,
                   lat = tiro_sem$latitude_ocorrencia,
                   color = "blue",
                   weight = 1,
                   radius = 5,
                   group = "Sem mortes",
                   label = lapply(tiro_sem$label, HTML)) %>%
  addLayersControl(overlayGroups = c("Com mortes", "Sem mortes"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(position = "topleft",
            pal = pal,
            values = com_sem$tipo
            )
  
m

?addLegend

bairros <- readOGR("Limite_Bairro.shp")
bairros2 <- readOGR("LM_REGIAO_SESEG.shp")


tiro <- read.csv("fogo_cruzado.csv", header = TRUE, sep = ";")
tiro_morte <- tiro %>%
  filter(qtd_morto_civil_ocorrencia != "0")

tiro_sem <- tiro %>%
  filter(qtd_morto_civil_ocorrencia == "0")

tiro_sem$label <- paste("<p>", tiro_sem$local_ocorrencia, "</p>",
                        "<p>", tiro_sem$data_ocorrencia, "</p>")

tiro_morte$label <- paste("<p>", tiro_morte$local_ocorrencia, "</p>",
                        "<p>", tiro_morte$data_ocorrencia, "</p>")

pal <- colorFactor(
  palette = c("red", "blue"),
  domain = levels(com_sem$tipo))



com_sem <- data.frame(tipo = c("Com Mortes", "Sem mortes"))
levels(com_sem$tipo)
