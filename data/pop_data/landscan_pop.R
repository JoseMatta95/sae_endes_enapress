library(tidyverse)
library(janitor)
library(rgee)
ee_Initialize(quiet = TRUE)
library(innovar)

distritos<- sf::st_read("./data/pop_data/districts/DISTRITOS.shp") %>% clean_names()

ee_districts <- pol_as_ee(distritos, id ="iddist", simplify = 1000)
pop_landscan_peru <- 
  ee$ImageCollection("projects/sat-io/open-datasets/ORNL/LANDSCAN_GLOBAL")$
  select('b1')$filterDate('2017-01-01','2023-12-31')


pop_landscan2_dist<-
  ee_extract(
    pop_landscan_peru,
    ee_districts,
    fun = ee$Reducer$sum(),
    scale = 1000
  ) %>% 
  as_tibble() %>% 
  mutate(
    across(.cols = -iddist, .f = ~round(.x,1))
  ) %>% 
  
  pivot_longer(cols = -iddist) %>% 
  mutate(
    year = str_extract(name, "\\d{4}"),
    pop_landscan = value
  ) %>% 
  select(year,iddist,pop_landscan)

write.csv(pop_landscan2_dist, "./data/pop_data/landscan_pop.csv", row.names = F)
