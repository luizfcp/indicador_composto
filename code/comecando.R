
library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)


base_alvos_de_roubo <- readRDS("../data/rds/Banco Alvos de Roubo 2019 - Por Municpio e Por RISP.rds")
base_crimes_violentos <- readRDS("../data/rds/Banco Crimes Violentos Armazm 2019 - Por municipio e Por RISP.rds")
base_outras_naturezas <- readRDS("../data/rds/Banco Outras Naturezas 2019 - Por Municpio e Por RISP.rds")
base_veiculos_roubados <- readRDS("../data/rds/Banco Veculos Roubados 2019 - Por Municpio e Por RISP.rds")
base_vitimas_homicidios <- readRDS("../data/rds/Banco Vtimas de Homicdio Consumado 2019 - Por Municpio e Por RISP.rds")


data <- 
  base_crimes_violentos %>% 
  select(Município, Natureza, Registros, Ano) %>% #`Cod. IBGE`, 
  nest(-Ano) %>% 
  mutate(
    data = map(
      data,
      ~ .x %>% 
        group_by(Município, Natureza) %>% 
        summarise(registro_anual_crimes_violentos = sum(Registros)) %>%
        spread(Natureza, registro_anual_crimes_violentos)
    )
  ) %>% 
  unnest() %>% 
  full_join(
    base_outras_naturezas %>% 
      select(Município, Natureza, Registros, Ano) %>% #`Cod. IBGE`, 
      nest(-Ano) %>% 
      mutate(
        data = map(
          data,
          ~ .x %>% 
            group_by(Município, Natureza) %>% 
            summarise(registro_anual_outras_naturezas = sum(Registros)) %>%
            spread(Natureza, registro_anual_outras_naturezas)
        )
      ) %>% 
      unnest()
  ) %>% 
  full_join(
    base_vitimas_homicidios %>% 
      select(Município, Natureza, Vítimas, Ano) %>% #`Cod. IBGE`, 
      nest(-Ano) %>% 
      mutate(
        data = map(
          data,
          ~ .x %>% 
            group_by(Município, Natureza) %>% 
            summarise(registro_anual_vitimas_homicidios = sum(Vítimas)) %>%
            spread(Natureza, registro_anual_vitimas_homicidios)
        )
      ) %>% 
      unnest()
  )

    
## a partir de 2015 apenas
base_alvos_de_roubo %>% 
  select(Município, Natureza, Registros, Ano) %>% #`Cod. IBGE`, 
  nest(-Ano) %>% 
  mutate(
    data = map(
      data,
      ~ .x %>% 
        group_by(Município, Natureza) %>% 
        summarise(registro_anual_alvos_de_roubo = sum(Registros)) %>%
        spread(Natureza, registro_anual_alvos_de_roubo)
    )
  ) %>% 
  unnest()
  
base_veiculos_roubados %>% 
  select(Município, Natureza, Registros, Ano) %>% #`Cod. IBGE`, 
  nest(-Ano) %>% 
  mutate(
    data = map(
      data,
      ~ .x %>% 
        group_by(Município, Natureza) %>% 
        summarise(registro_anual_veiculos_roubados = sum(Registros)) %>%
        spread(Natureza, registro_anual_veiculos_roubados)
    )
  ) %>% 
  unnest()


## Agregar todos os tipos de estupros em estupro e Homicidios em homicidios

## Bases anuais, onde as linhas sao municipios e as colunas os tipos de crimes

## Tentar correlacionar crime com outra coisa, tipo analfabetismo


