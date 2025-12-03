library(tidyverse)
library(purrr)
library(sf)
library(janitor)
library(survey)

source("./functions.R")

# 1. ENDES ----

## ANEMIA: Extraccion de datos ----

# ENDES 2016-2019
period<- 2017:2019

anemia_1<-
  map_df(
  .x = period,
  .f = ~ consulta_endes2(periodo = .x, base = "REC44", codigo_modulo ="74" ) %>% 
    mutate(
      year = .x
    )
) %>% 
  clean_names() %>% 
  select(year,caseid,hw53,hw57) %>% 
  mutate(
    hhid = str_extract(caseid, "\\d+\\s+\\d+$") %>%
      str_remove("\\s+\\d+$"),
    hb = hw53*0.1
  ) %>% 
  haven::zap_labels()

# ENDES 2020-2022
period<- c(2020:2022)

anemia_2<-
  map_df(
    .x = period,
    .f = ~ consulta_endes2(periodo = .x, base = "REC44", codigo_modulo ="1638" ) %>% 
      mutate(
        year = .x
      )
  ) %>% 
  clean_names() %>% 
  select(year,caseid,hw53,hw57) %>% 
  mutate(
    hhid = str_extract(caseid, "\\d+\\s+\\d+$") %>%
      str_remove("\\s+\\d+$"),
    hb = hw53*0.1
  ) %>% 
  haven::zap_labels()

# # ENDES 2023-2024

period<- c(2023:2024)

anemia_3<-
  map_df(
    .x = period,
    .f = ~ consulta_endes2(periodo = .x, 
                           base = paste0("REC44_", .x), 
                           codigo_modulo ="1638" ) %>% 
      mutate(
        year = .x
      )
  ) %>% 
  clean_names() %>% 
  select(year,caseid,hw53,hw57) %>% 
  mutate(
    hhid = str_extract(caseid, "\\d+\\s+\\d+$") %>%
      str_remove("\\s+\\d+$"),
    hb = hw53*0.1
  ) %>% 
  haven::zap_labels()


# Uniendo todo

df_anemia<-
  bind_rows(
  anemia_1,
  anemia_2,
  anemia_3
) %>% 
  select(year,hhid,caseid,hb,hw57) %>% 
  mutate(
    hhid = as.numeric(hhid),
    hw57 = as.factor(hw57)
  ) %>% 
  filter(
    !is.na(hw57)
  )



## ANEMIA: Creacion de objetos survey

key_endes <- read.csv("./data/key_endes_2016_2024.csv") %>% as_tibble()

options(survey.lonely.psu = "adjust") # solucion para cuando hay solo un elemento en una PSU
endes_anemia_svy<-
  
  df_anemia %>% 
  left_join(key_endes, by = c("year","hhid")) %>% 
  #group_by(caseid) %>%
  #mutate(row_num = row_number()) %>%
  #filter(row_num == 1 | row_num == 2) %>%   
  #slice_tail(n = 1) %>%                      
  group_by(year) %>% 
  nest() %>% 
  
  mutate(
    svy_data = map(.x = data, # objeto survey
                   .f = ~svydesign(ids = ~hv001, strata = ~hv022, 
                                   weights = ~hv005, data = .x, nest = T)),
    
    
    anemia_prop = map(svy_data, 
                      ~ svyby(~hw57, ~ubigeo, design = .x,
                                        FUN = svymean, keep.var = TRUE))
  )
  
 
# estimaciones directas de la categoria HW57 - prueba 
endes_anemia_svy$anemia_prop[[1]] %>% as_tibble()





