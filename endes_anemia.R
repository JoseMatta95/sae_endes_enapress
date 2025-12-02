library(tidyverse)
library(purrr)
library(sf)
library(janitor)

source("./functions.R")

# ENDES 2016-2019
period<- 2016:2019

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
    hhid = str_sub(caseid, -9,-4),
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
    hhid = str_sub(caseid, -9,-4),
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
    hhid = str_sub(caseid, -9,-4),
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
  select(year,caseid,hhid,hb,hw57)


