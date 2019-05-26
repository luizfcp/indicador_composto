library(rgdal);library(readxl);library(leaflet);library(tidyverse);library(htmltools)

setwd("C://Users//Lyncoln//Documents//Estatística Aplicada") 
BD = read_excel("baseEstAplicada.xls") #Base de dados com o indicador composto

#Pasta que contém os arquivos da malha
minas = readOGR(dsn="C://Users//Lyncoln//Documents//Estatística Aplicada//malha",
                use_iconv = TRUE,
                layer="31MUE250GC_SIR",
                encoding = "UTF-8")

BD2016 = BD %>% 
  filter(ano=="2016")
  
#Colocar o indicador dentro do data do shape

teste = minas@data

teste = teste %>% 
  mutate(CD_GEOCMU=as.character(CD_GEOCMU)) %>% 
  mutate(CD_GEOCMU=str_sub(CD_GEOCMU,1,-2)) %>% 
  inner_join(BD2016,by=c("CD_GEOCMU"="cod_ibge")) %>% 
  mutate(CD_GEOCMU = as.factor(CD_GEOCMU))


minas@data = teste


bin = c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
pal = colorBin(palette = "RdYlBu", domain =  minas@data$ISM, bins = bin)
labels = paste0("<p>","Municipio: ",minas@data$NM_MUNICIP,"</p>",
               "<p>","Indicador: ",round(minas@data$ISM,digits= 3),"</p>")

mapa = leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addPolygons(data = minas,
               weight = 1,
              smoothFactor = 0.5,
              color = "white",
              fillOpacity = 0.8,
              fillColor = pal(minas@data$ISM),
              highlightOptions = highlightOptions(color = "black", weight = 3,
                                                  bringToFront = FALSE),
              label = lapply(labels, HTML)
              ) %>% 
  addLegend(pal = pal,
            values = minas@data$ISM,
            opacity = 0.7,
            position = "bottomright",
            title = "Índice de Segurança Mineiro 2016")


mapa

