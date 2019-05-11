
# Pacotes -----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)


# Import ------------------------------------------------------------------

# base_alvos_de_roubo     <- readRDS("../data/rds/Banco Alvos de Roubo 2019 - Por Municpio e Por RISP.rds")
base_crimes_violentos   <- readRDS("../data/rds/Banco Crimes Violentos Armazm 2019 - Por municipio e Por RISP.rds")
base_outras_naturezas   <- readRDS("../data/rds/Banco Outras Naturezas 2019 - Por Municpio e Por RISP.rds")
# base_veiculos_roubados  <- readRDS("../data/rds/Banco Veculos Roubados 2019 - Por Municpio e Por RISP.rds")
base_vitimas_homicidios <- readRDS("../data/rds/Banco Vtimas de Homicdio Consumado 2019 - Por Municpio e Por RISP.rds")


# Manipulacao -------------------------------------------------------------

raw_data <- 
  # Crimes Violentos
  base_crimes_violentos %>% 
  unite(Município, c(`Cod. IBGE`, Município), sep = " ") %>% 
  select(Município, Natureza, Registros, Ano) %>% 
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
    # Outras Naturezas
    base_outras_naturezas %>% 
      unite(Município, c(`Cod. IBGE`, Município), sep = " ") %>% 
      select(Município, Natureza, Registros, Ano) %>%
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
    # Vitimas de Homicidios
    base_vitimas_homicidios %>% 
      unite(Município, c(`Cod. IBGE`, Município), sep = " ") %>% 
      select(Município, Natureza, Vítimas, Ano) %>% 
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
  ) %>% 
  filter(Ano!=2019)

data <- 
  raw_data %>% 
  mutate(
    # Agregando os tipos de Estupros (Consumado, Tentado, Vulneravel Consumado e Vulneravel Tentado)
    Estupro = `Estupro Consumado`+`Estupro Tentado`+`Estupro de Vulnerável Consumado`+`Estupro de Vulnerável Tentado`,
    
    # Agregando os tipos de Homicidios (Tentado e Consumado)
    Homicidios = `Homicídio Tentado`+`Homicídio Consumado (Registros)`
  ) %>% 
  select(Estupro, Homicidios, `Roubo Consumado`, `Furto Consumado`, `Extorsão Consumado`)
  



# ## a partir de 2015 apenas
# base_alvos_de_roubo %>% 
#   select(Município, Natureza, Registros, Ano) %>% #`Cod. IBGE`, 
#   nest(-Ano) %>% 
#   mutate(
#     data = map(
#       data,
#       ~ .x %>% 
#         group_by(Município, Natureza) %>% 
#         summarise(registro_anual_alvos_de_roubo = sum(Registros)) %>%
#         spread(Natureza, registro_anual_alvos_de_roubo)
#     )
#   ) %>% 
#   unnest()
#   
# base_veiculos_roubados %>% 
#   select(Município, Natureza, Registros, Ano) %>% #`Cod. IBGE`, 
#   nest(-Ano) %>% 
#   mutate(
#     data = map(
#       data,
#       ~ .x %>% 
#         group_by(Município, Natureza) %>% 
#         summarise(registro_anual_veiculos_roubados = sum(Registros)) %>%
#         spread(Natureza, registro_anual_veiculos_roubados)
#     )
#   ) %>% 
#   unnest()


## Agregar todos os tipos de estupros em estupro e Homicidios em homicidios

## Bases anuais, onde as linhas sao municipios e as colunas os tipos de crimes

## Tentar correlacionar crime com outra coisa, tipo analfabetismo

## incluir datasus internaÇao por agressao
## comparar com pib
## escolher 2 anos da base para analisas
## procurar por idh, desemprego se tem


