# library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)

# base <- read_csv2(paste0("../data/", list.files("../data/")[8]))
base <- read.csv2(paste0("../data/", list.files("../data/")[8]), encoding = "UTF-8")
base %>% View

base %>% 
  select(Cod.IBGE, Municipio, Natureza, Registros, Mes, Ano) %>% 
  as_tibble() %>% 
  nest(-Ano) %$% 
  data %>% 
  .[[1]] %>% 
  nest(-Municipio) %$% 
  data %>%  
  .[[1]] %>% 
  group_by(Natureza) %>% 
  summarise(sum = sum(Registros)) %>% 
  View

## Agregar todos os tipos de estupros em estupro e Homicidios em homicidios

## Bases anuais, onde as linhas sao municipios e as colunas os tipos de crimes

## Tentar correlacionar crime com outra coisa, tipo analfabetismo


