
# Pacotes -----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(readxl)
library(janitor)
library(GGally)
library(stringr)
library(abjutils)

options(scipen = 99999999)

# Import ------------------------------------------------------------------

base_crimes_violentos   <- readRDS("../data/rds/Banco Crimes Violentos Armazm 2019 - Por municipio e Por RISP.rds")     %>% clean_names()
base_outras_naturezas   <- readRDS("../data/rds/Banco Outras Naturezas 2019 - Por Municpio e Por RISP.rds")             %>% clean_names()
base_vitimas_homicidios <- readRDS("../data/rds/Banco Vtimas de Homicdio Consumado 2019 - Por Municpio e Por RISP.rds") %>% clean_names()
base_internacoes        <- readRDS("../data/rds/internacoes.rds") %>% clean_names()
base_obitos             <- readRDS("../data/rds/obitos.rds") %>% clean_names()
base_pib_per_capita     <- readRDS("../data/rds/pib_per_capita.rds") %>% clean_names()
base_pib_per_corrente   <- readRDS("../data/rds/pib_per_corrente.rds") %>% clean_names()

populacao <- read_excel("../data/2019-04-05 - Banco Outras Naturezas Armazm - Atualizado Maro 2019 - Por Municpio e Por RISP.xlsx", sheet = 5)

area_mun <- read_excel("/cloud/project/data/area_mun_mg_2016.xls") %>% clean_names()

# Wrangling ---------------------------------------------------------------

# Base de dados populacao por municipio
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



area_mun %<>% 
  select(cd_gcmun, ar_mun_2016) %>% 
  mutate(cd_gcmun = str_sub(cd_gcmun, end = 6)) %>% 
  `colnames<-`(c("cod_ibge", "area_mun_2016"))

pop_mg_municipios %<>% 
  nest(-ano) %>% 
  mutate(pop_area_mun = map(
    data,
    ~ .x %>% full_join(area_mun)
  )) %>% 
  select(ano, pop_area_mun) %>% 
  unnest() %>% 
  mutate(dens_demografica = populacao/area_mun_2016)



# Base de dados primaria
bases_prim <- 
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
  separate(municipio, c("cod_ibge", "municipio"), sep = "_") %>% 
  filter(ano %in% 2013:2016)

# Base de dados secundaria
bases_sec <- 
  # Internacoes
  base_internacoes %>% 
  mutate(
    municipio = base_internacoes$municipio %>% 
      unlist() %>% 
      .[-(base_internacoes$municipio %>% unlist() %>% str_detect("^0${1}") %>% which)]
  ) %>% 
  filter(ano %in% 2013:2016) %>% 
  arrange(ano, municipio) %>% 
  mutate(municipio = bases_prim$cod_ibge) %>% 
  full_join(
    # Obitos
    base_obitos %>% 
      filter(ano %in% 2013:2016) %>% 
      mutate(municipio = bases_prim$cod_ibge)
  ) %>% 
  full_join(
    # PIB per capita
    base_pib_per_capita %>% 
      select(-municipio) %>%
      filter(ano %in% 2013:2016) %>% 
      mutate(codigo = codigo %>% str_sub(end = 6)) %>% 
      rename("municipio"="codigo")
  ) %>% 
  full_join(
    # PIB per corrente
    base_pib_per_corrente %>% 
      select(-municipio) %>%
      filter(ano %in% 2013:2016) %>% 
      mutate(codigo = codigo %>% str_sub(end = 6)) %>% 
      rename("municipio"="codigo")
  ) %>% 
  mutate(ano = as.numeric(ano)) %>% 
  `colnames<-`(c("cod_ibge", "ano", "internacoes", "obitos", "pib_per_capita", "pib_per_corrente"))


data <- 
  bases_prim %>% 
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
  select(ano, cod_ibge, municipio, assalto, estupro, homicidios, extorsao, lesao_corporal_consumado) %>% 
  full_join(bases_sec)


# Proporcao dos dados (crime/populacao)
data_prop <- 
  full_join(data, pop_mg_municipios) %>% 
  nest(-ano) %>% 
  mutate(data_prop = map(data,
                         ~ .x %>% mutate(
                           prop_assalto     = .x$assalto/.x$dens_demografica,
                           prop_estupro     = .x$estupro/.x$dens_demografica,
                           prop_homicidios  = .x$homicidios/.x$dens_demografica,
                           prop_extorsao    = .x$extorsao/.x$dens_demografica,
                           prop_lesao_corp  = .x$lesao_corporal_consumado/.x$dens_demografica,
                           prop_internacoes = .x$internacoes/.x$dens_demografica,
                           prop_obitos      = .x$obitos/.x$dens_demografica,
                           prop_pib_per_cap = .x$pib_per_capita/.x$dens_demografica,
                           prop_pib_per_cor = .x$pib_per_corrente/.x$dens_demografica
                           # prop_assalto     = .x$assalto/.x$populacao,
                           # prop_estupro     = .x$estupro/.x$populacao,
                           # prop_homicidios  = .x$homicidios/.x$populacao,
                           # prop_extorsao    = .x$extorsao/.x$populacao,
                           # prop_lesao_corp  = .x$lesao_corporal_consumado/.x$populacao,
                           # prop_internacoes = .x$internacoes/.x$populacao,
                           # prop_obitos      = .x$obitos/.x$populacao,
                           # prop_pib_per_cap = .x$pib_per_capita/.x$populacao,
                           # prop_pib_per_cor = .x$pib_per_corrente/.x$populacao
                         ))) %>% 
  select(ano, data_prop)


# Correlacao --------------------------------------------------------------

## Foi utilizado a correlacao de spearman, pois as variaveis sao assimetricas

data_prop %<>% 
  mutate(
    cor_graf = map2(
      data_prop, ano,
      ~ .x %>% 
        select(prop_assalto, prop_estupro, prop_homicidios, prop_extorsao, prop_lesao_corp,
               prop_internacoes, prop_obitos, prop_pib_per_cap, prop_pib_per_cor) %>% 
        `colnames<-`(c("Assalto", "Estupro", "Homicídios", "Extorsão", "Lesão Corporal",
                       "Internações", "Óbitos", "PIB per Capita", "PIB per Corrente")) %>% 
        ggpairs(lower = list(continuous = "smooth"), 
                upper = list(method = "spearman"),
                title = .y)
    )
  )

# Salvando
walk2(data_prop$cor_graf, data_prop$ano,
      ~ ggsave(
        paste0("../img/correlacao/", .y, ".png"),
        plot=.x, dpi="retina", width=16.00, height=8.09, scale=1
      )
)

#   -----------------------------------------------------------------------

padroniza <- function(x) {
  ( max(x)-x )/( max(x)-min(x) )
}

data_prop %$% data_prop %>% .[[1]] %$% prop_assalto %>% summary() 


base <- data_prop %$% data_prop %>% .[[1]] %>% 
  transmute(
    ind_assalto_padr = padroniza(prop_assalto),
    ind_estupro_padr = padroniza(prop_estupro),
    ind_homicidios_padr = padroniza(prop_homicidios),
    ind_extorsao_padr = padroniza(prop_extorsao),
    ind_lesao_corp_padr = padroniza(prop_lesao_corp),
    ind_internacoes_padr = padroniza(prop_internacoes),
    ind_obitos_padr = padroniza(prop_obitos),
    ind_pib_per_cap_padr = padroniza(prop_pib_per_cap),
    ind_pib_per_cor_padr = padroniza(prop_pib_per_cor)
  )
  
base %>% ggpairs(lower = list(continuous = "smooth"), upper = list(method = "spearman"))

base %>% 
  select(-ind_extorsao_padr, -ind_obitos_padr, -ind_pib_per_cap_padr) %>% 
  transmute(ind_composto = (ind_assalto_padr+ind_estupro_padr+ind_homicidios_padr+
                              ind_lesao_corp_padr+ind_internacoes_padr+(1-ind_pib_per_cor_padr))/6)
### O 0 é a pior situação de segurança, o 1 a melhor, após a padraonizao (fun padronzia)


#- Falta organizar e fazer para todos os anos e juntar a coluna cod dos municipios do ibge



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


