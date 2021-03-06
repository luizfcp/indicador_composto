
# Pacotes -----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(readxl)
library(janitor)
library(GGally)
library(stringr)
library(cowplot)
library(flextable)
library(pacotin)

options(scipen = 99999999)

# Import ------------------------------------------------------------------

base_crimes_violentos   <- readRDS("../data/rds/Banco Crimes Violentos Armazm 2019 - Por municipio e Por RISP.rds")     %>% clean_names()
base_outras_naturezas   <- readRDS("../data/rds/Banco Outras Naturezas 2019 - Por Municpio e Por RISP.rds")             %>% clean_names()
base_vitimas_homicidios <- readRDS("../data/rds/Banco Vtimas de Homicdio Consumado 2019 - Por Municpio e Por RISP.rds") %>% clean_names()
base_internacoes        <- readRDS("../data/rds/internacoes.rds")        %>% clean_names()
base_pib_preco_corrente <- readRDS("../data/rds/pib_preco_corrente.rds") %>% clean_names()

area_mun  <- read_excel("/cloud/project/data/area_mun_mg_2016.xls") %>% clean_names()
populacao <- read_excel("../data/2019-04-05 - Banco Outras Naturezas Armazm - Atualizado Maro 2019 - Por Municpio e Por RISP.xlsx", sheet = 5)

# Wrangling ---------------------------------------------------------------

# Area de cada municipio de MG (2016)
area_mun %<>% 
  select(cd_gcmun, ar_mun_2016) %>% 
  mutate(cd_gcmun = str_sub(cd_gcmun, end = 6)) %>% 
  `colnames<-`(c("cod_ibge", "area_mun_2016"))

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
  mutate(ano = as.numeric(ano)) %>% 
  nest(-ano) %>% 
  mutate(pop_area_mun = map(
    data,
    ~ .x %>% full_join(area_mun)
  )) %>% 
  select(ano, pop_area_mun) %>% 
  unnest() %>% 
  # Calculando a Densidade Demografica
  mutate(dens_demografica = populacao/area_mun_2016)

# Base de dados primaria - crimes violentos, outras naturezas e vitimas de homicidios
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

# Base de dados secundaria - internacoes e PIB preco corrente
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
    # PIB preco corrente
    base_pib_preco_corrente %>% 
      select(-municipio) %>%
      filter(ano %in% 2013:2016) %>% 
      mutate(codigo = codigo %>% str_sub(end = 6)) %>% 
      rename("municipio"="codigo")
  ) %>% 
  mutate(ano = as.numeric(ano)) %>% 
  `colnames<-`(c("cod_ibge", "ano", "internacoes", "pib_preco_corrente"))


data <- 
  bases_prim %>% 
  mutate(
    # Agregando os tipos de Homicidios (tentado, consumado e vitimas de homicidios consumado)
    homicidios = homicidio_tentado+homicidio_consumado_registros+vitima_de_homicidio_consumado,
    
    # Agregando os tipos de Estupros (consumado, tentado, vulneravel consumado e vulneravel tentado)
    estupro = estupro_consumado+estupro_tentado+estupro_de_vulneravel_consumado+estupro_de_vulneravel_tentado,
    
    # Agregando roubo e furto
    assalto = roubo_consumado+furto_consumado
  ) %>% 
  select(ano, cod_ibge, municipio, assalto, estupro, homicidios, lesao_corporal_consumado) %>% 
  full_join(bases_sec)


# Divisao dos indicadores pela densidade demografica
data %<>%  
  full_join(pop_mg_municipios) %>% 
  nest(-ano) %>% 
  mutate(razoes = map(data,
                      ~ .x %>% mutate(
                        ind_assalto       = .x$assalto/.x$dens_demografica,
                        ind_estupro       = .x$estupro/.x$dens_demografica,
                        ind_homicidios    = .x$homicidios/.x$dens_demografica,
                        ind_lesao_corp    = .x$lesao_corporal_consumado/.x$dens_demografica,
                        ind_internacoes   = .x$internacoes/.x$dens_demografica,
                        ind_pib_preco_cor = .x$pib_preco_corrente/.x$dens_demografica
                      ))) %>% 
  select(ano, razoes) 

# Construcao do Indice ----------------------------------------------------

# Funcao para padronizar os indicadores
padroniza <- function(x) ( max(x)-x )/( max(x)-min(x) )

indicadores <-
  data %>% 
  mutate(indicadores_padronizados = map(
    razoes,
    ~ .x %>% transmute(
      cod_ibge,
      ind_assalto_padr       = padroniza(ind_assalto),
      ind_estupro_padr       = padroniza(ind_estupro),
      ind_homicidios_padr    = padroniza(ind_homicidios),
      ind_lesao_corp_padr    = padroniza(ind_lesao_corp),
      ind_internacoes_padr   = padroniza(ind_internacoes),
      ind_pib_preco_cor_padr = padroniza(ind_pib_preco_cor)
    )
  )) %>% 
  select(ano, indicadores_padronizados)

## O 0 é a pior situação de segurança, o 1 a melhor, após a padronizacao (fun padroniza)
indice_seguranca_mineiro <- 
  indicadores %>% 
  mutate(ism = map(
    indicadores_padronizados,
    ~ .x %>% 
      transmute(
        cod_ibge,
        ISM = ( 
          ( ind_assalto_padr    + ind_estupro_padr     + ind_homicidios_padr        +
            ind_lesao_corp_padr + ind_internacoes_padr + (1-ind_pib_preco_cor_padr) )/6 
        )
      )
  )) %>% 
  select(ano, ism)

# Correlacao --------------------------------------------------------------

## Foi utilizado a correlacao de spearman, pois as variaveis sao assimetricas

indicadores %<>% 
  mutate(
    cor_graf = map2(
      indicadores_padronizados, ano,
      ~ .x %>% 
        select(-cod_ibge) %>% 
        `colnames<-`(c("Assalto", "Estupro", "Homicídios", "Lesão Corporal", "Internações", "PIB Preço Corrente")) %>% 
        ggpairs(upper = list(method = "spearman"), title = .y) + theme_linedraw()
    )
  )

# Salvando
walk2(indicadores$cor_graf, indicadores$ano,
      ~ ggsave(
        paste0("../img/correlacao/", .y, ".png"),
        plot=.x, dpi="retina", width=13.00, height=10.00, scale=1
      )
)

plot_grid(
  ggmatrix_gtable(indicadores$cor_graf[[1]]),
  ggmatrix_gtable(indicadores$cor_graf[[2]]),
  ggmatrix_gtable(indicadores$cor_graf[[3]]),
  ggmatrix_gtable(indicadores$cor_graf[[4]]),
  nrow = 2
) %>% 
  ggsave(
    "../img/correlacao/todos_os_anos.png",
    plot=., dpi="retina", width=20.00, height=17.00, scale=1)


# Tabela ------------------------------------------------------------------

data_tabela <- 
  indice_seguranca_mineiro %>%
  mutate(ism = map(ism,
                   ~ .x %>% 
                     mutate(condicao_indice = if_else(ISM<0.2, "MUITO RUIM",
                                                      if_else(ISM<0.4, "RUIM",
                                                              if_else(ISM<0.6, "REGULAR",
                                                                      if_else(ISM<0.8, "BOM", "MUITO BOM"))))) %>% 
                     inner_join(data$razoes[[1]] %>% select(cod_ibge, municipio)) %>% 
                     select(municipio, ISM, condicao_indice, -cod_ibge) %>% 
                     mutate(municipio = str_to_title(municipio),
                            condicao_indice = str_to_title(condicao_indice))),
         # 5 maiores negativos
         negativos = map2(ism, ano,
                         ~ .x %>% 
                           arrange(ISM) %>% 
                           head(5) %>% 
                           `colnames<-`(c("Município", "ISM", "Condição")) %>% 
                           flextable() %>% 
                           theme_zebra(odd_header = "#002b96", even_body = "white", odd_body = "#d6e1ff") %>% 
                           width(width = c(2.3, 1, 1)) %>% 
                           bold(part = "header") %>% 
                           color(part = "header", color = "white") %>%
                           align(part = "all", align = "center") %>% 
                           align(j = 1, align = "left") %>% 
                           fontsize(part = "all", size = 12) %>% 
                           add_footer(Condição = .y %>% as.integer()) %>% 
                           align(part = "footer", align = "right")),
         # 5 maiores positivos
         positivos = map2(ism, ano,
                         ~ .x %>% 
                           arrange(-ISM) %>% 
                           head(5) %>% 
                           `colnames<-`(c("Município", "ISM", "Condição")) %>% 
                           flextable() %>% 
                           theme_zebra(odd_header = "#002b96", even_body = "white", odd_body = "#d6e1ff") %>% 
                           width(width = c(2.3, 1, 1)) %>% 
                           bold(part = "header") %>% 
                           color(part = "header", color = "white") %>%
                           align(part = "all", align = "center") %>% 
                           align(j = 1, align = "left") %>% 
                           fontsize(part = "all", size = 12) %>% 
                           add_footer(Condição = .y %>% as.integer()) %>% 
                           align(part = "footer", align = "right"))
  )

# Salvando
{
  ## 5 maiores negativos
  walk2(data_tabela$ano, data_tabela$negativos,
        ~ insert_screenshot(
          .y, paste0("../img/tabela/", paste0(.x, "_neg"), ".png")
        )
  )
  ## 5 maiores positivos
  walk2(data_tabela$ano, data_tabela$positivos,
        ~ insert_screenshot(
          .y, paste0("../img/tabela/", paste0(.x, "_pos"), ".png")
        )
  )
}

#   -----------------------------------------------------------------------

# indice_seguranca_mineiro %>% unnest() %>% 
#   xlsx::write.xlsx("../ism.xls")
