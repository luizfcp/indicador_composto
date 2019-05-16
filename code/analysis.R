
# Pacotes -----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(readxl)
library(janitor)
library(GGally)

options(scipen = 99999999)

# Import ------------------------------------------------------------------

base_crimes_violentos   <- readRDS("../data/rds/Banco Crimes Violentos Armazm 2019 - Por municipio e Por RISP.rds")     %>% clean_names()
base_outras_naturezas   <- readRDS("../data/rds/Banco Outras Naturezas 2019 - Por Municpio e Por RISP.rds")             %>% clean_names()
base_vitimas_homicidios <- readRDS("../data/rds/Banco Vtimas de Homicdio Consumado 2019 - Por Municpio e Por RISP.rds") %>% clean_names()

populacao <- read_excel("../data/2019-04-05 - Banco Outras Naturezas Armazm - Atualizado Maro 2019 - Por Municpio e Por RISP.xlsx",
                        sheet = 5)

# Wrangling ---------------------------------------------------------------

pop_mg_municipios <- 
  populacao %>%
  clean_names() %>%
  select(cod, municipio, x2013_8:x2016_11) %>% 
  filter(municipio!="(Tudo)") %>% 
  arrange(cod) %>% 
  select(-municipio) %>%
  `colnames<-`(c("cod_ibge", paste0("20", 13:16))) %>% 
  gather(ano, populacao, -cod_ibge) %>% 
  mutate(ano = as.numeric(ano))

raw_data <- 
  # Crimes Violentos
  base_crimes_violentos %>% 
  unite(municipio, c(cod_ibge, municipio), sep = "_") %>% 
  select(municipio, natureza, registros, ano) %>% 
  nest(-ano) %>% 
  mutate(
    data = map(
      data,
      ~ .x %>% 
        group_by(municipio, natureza) %>% 
        summarise(registro_anual_crimes_violentos = sum(registros)) %>%
        spread(natureza, registro_anual_crimes_violentos)
    )
  ) %>% 
  unnest() %>% 
  full_join(
    # Outras naturezas
    base_outras_naturezas %>% 
      unite(municipio, c(cod_ibge, municipio), sep = "_") %>% 
      select(municipio, natureza, registros, ano) %>%
      nest(-ano) %>% 
      mutate(
        data = map(
          data,
          ~ .x %>% 
            group_by(municipio, natureza) %>% 
            summarise(registro_anual_outras_naturezas = sum(registros)) %>%
            spread(natureza, registro_anual_outras_naturezas)
        )
      ) %>% 
      unnest()
  ) %>% 
  full_join(
    # Vitimas de Homicidios
    base_vitimas_homicidios %>% 
      unite(municipio, c(cod_ibge, municipio), sep = "_") %>% 
      select(municipio, natureza, vitimas, ano) %>% 
      nest(-ano) %>% 
      mutate(
        data = map(
          data,
          ~ .x %>% 
            group_by(municipio, natureza) %>% 
            summarise(registro_anual_vitimas_homicidios = sum(vitimas)) %>%
            spread(natureza, registro_anual_vitimas_homicidios)
        )
      ) %>% 
      unnest()
  ) %>% 
  clean_names() %>% 
  filter(ano %in% 2013:2016)


data <- 
  raw_data %>% 
  mutate(
    # Agregando os tipos de Homicidios (tentado, consumado e vitimas de homicidios consumado)
    homicidios = homicidio_tentado+homicidio_consumado_registros+vitima_de_homicidio_consumado,
    
    # Agregando os tipos de Extorsao (consumado e mediante sequestro consumado)
    extorsao = extorsao_consumado+extorsao_mediante_sequestro_consumado,
    
    # Agregando os tipos de Estupros (consumado, tentado, vulneravel consumado e vulneravel tentado)
    estupro = estupro_consumado+estupro_tentado+estupro_de_vulneravel_consumado+estupro_de_vulneravel_tentado,
    
    # Agregando roubo e furto
    assalto = roubo_consumado+furto_consumado
  ) %>% 
  select(ano, municipio, assalto, estupro, homicidios, extorsao, lesao_corporal_consumado) %>% 
  separate(municipio, c("cod_ibge", "municipio"), sep = "_")


# Proporcao dos dados (crime/populacao)
data_prop <- 
  full_join(data, pop_mg_municipios, by = c("ano", "cod_ibge")) %>% 
  nest(-ano) %>% 
  mutate(data_prop = map(data,
                         ~ .x %<>% mutate(
                           prop_assalto = .x$assalto/.x$populacao,
                           prop_estupro = .x$estupro/.x$populacao,
                           prop_homicidios = .x$homicidios/.x$populacao,
                           prop_extorsao = .x$extorsao/.x$populacao,
                           prop_lesao_corp = .x$lesao_corporal_consumado/.x$populacao
                         ))) %>% 
  select(ano, data_prop)


# Correlacao --------------------------------------------------------------

## Foi utilizado a correlacao de spearman, pois as variaveis sao assimetricas

data_prop %<>% 
  mutate(
    cor_graf = map2(
      data_prop, ano,
      ~ .x[, 9:13] %>% 
        `colnames<-`(c("assalto", "estupro", "homicídios", "extorsão", "lesão corporal")) %>% 
        ggpairs(lower = list(continuous = "smooth"), 
                upper = list(method = "spearman"),
                title = .y)
    )
  )



data_prop$cor_graf[[1]]


# library(corrplot)
#   data_prop$data_prop[[1]][, 10:15] %>% 
#   cor(method = "spearman") %>% 
#   corrplot(method="color")


# data %>% write.xlsx("base.xlsx")


## Tentar correlacionar crime com outra coisa, tipo analfabetismo

## incluir datasus internaÇao por agressao
## comparar com pib
## escolher 2 anos da base para analisas
## procurar por idh, desemprego se tem


