
# Pacotes -----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(readxl)
library(janitor)

options(scipen = 99999999)

# Import ------------------------------------------------------------------

base_crimes_violentos   <- readRDS("../data/rds/Banco Crimes Violentos Armazm 2019 - Por municipio e Por RISP.rds")     %>% clean_names()
base_outras_naturezas   <- readRDS("../data/rds/Banco Outras Naturezas 2019 - Por Municpio e Por RISP.rds")             %>% clean_names()
base_vitimas_homicidios <- readRDS("../data/rds/Banco Vtimas de Homicdio Consumado 2019 - Por Municpio e Por RISP.rds") %>% clean_names()
# base_alvos_de_roubo     <- readRDS("../data/rds/Banco Alvos de Roubo 2019 - Por Municpio e Por RISP.rds")
# base_veiculos_roubados  <- readRDS("../data/rds/Banco Veculos Roubados 2019 - Por Municpio e Por RISP.rds")

populacao <- read_excel("../data/2019-04-05 - Banco Outras Naturezas Armazm - Atualizado Maro 2019 - Por Municpio e Por RISP.xlsx",
                        sheet = 5)

# Manipulacao -------------------------------------------------------------

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
  filter(ano!=2019)

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
  select(ano, municipio, assalto, estupro, homicidios, extorsao, lesao_corporal_consumado, sequestro_e_carcere_privado_consumado) %>% 
  separate(municipio, c("cod_ibge", "municipio"), sep = "_")

# Proporcao dos dados (crime / população)
pop_mg_municipios <- 
  populacao %>%
  clean_names() %>%
  select(cod, municipio, x2012_7:x2018_13) %>% 
  filter(municipio!="(Tudo)") %>% 
  arrange(cod) %>% 
  select(-cod) %>% 
  `colnames<-`(c("municipio", paste0("20", 12:18))) %>% 
  gather(ano, populacao, -municipio) %>% 
  mutate(ano = as.numeric(ano))

### rever isso
data_prop <- 
  left_join(data, pop_mg_municipios, by = c("municipio", "ano")) %>% 
  nest(-ano) %>% 
  mutate(data_prop = map(data,
                         ~ .x %<>% mutate(
                           prop_assalto = .x$assalto/.x$populacao,
                           prop_estupro = .x$estupro/.x$populacao,
                           prop_homicidios = .x$homicidios/.x$populacao,
                           prop_extorsao = .x$extorsao/.x$populacao,
                           prop_lesao_corp = .x$lesao_corporal_consumado/.x$populacao,
                           prop_seq_carc_priv = .x$sequestro_e_carcere_privado_consumado/.x$populacao
                         ))) %>% 
  select(ano, data_prop) %>% 
  unnest()
  
  



a %>% 
  map(~ .x/pop_mg_municipios)


# Correlacao --------------------------------------------------------------

library(GGally)
ggpairs(a, lower = list(continuous = "smooth"))


a <- data %>% filter(ano==2012) %>% .[, 4:9]

a %>% 
  `colnames<-`(c("assalto", "estupro", "homicídios", "extorsão", "lesão corporal", "sequestro e cárcere")) %>% 
  cor %>% 
  corrplot(method="color")




# data %>% write.xlsx("base.xlsx")

# ## a partir de 2015 apenas
# base_alvos_de_roubo %>% 
#   select(municipio, natureza, registros, ano) %>% #cod_ibge, 
#   nest(-ano) %>% 
#   mutate(
#     data = map(
#       data,
#       ~ .x %>% 
#         group_by(municipio, natureza) %>% 
#         summarise(registro_anual_alvos_de_roubo = sum(registros)) %>%
#         spread(natureza, registro_anual_alvos_de_roubo)
#     )
#   ) %>% 
#   unnest()
#   
# base_veiculos_roubados %>% 
#   select(municipio, natureza, registros, ano) %>% #cod_ibge, 
#   nest(-ano) %>% 
#   mutate(
#     data = map(
#       data,
#       ~ .x %>% 
#         group_by(municipio, natureza) %>% 
#         summarise(registro_anual_veiculos_roubados = sum(registros)) %>%
#         spread(natureza, registro_anual_veiculos_roubados)
#     )
#   ) %>% 
#   unnest()





## Tentar correlacionar crime com outra coisa, tipo analfabetismo

## incluir datasus internaÇao por agressao
## comparar com pib
## escolher 2 anos da base para analisas
## procurar por idh, desemprego se tem


