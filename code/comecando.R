library(readxl)
library(dplyr)

base <- read_excel(paste0("data/", list.files("data/")[2]), sheet = 3)
base %>% View
