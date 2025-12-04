library(tidyverse)
library(purrr)
library(sf)
library(janitor)
library(survey)
library(INLA)
library(spdep)
library(yardstick)
library(gt)

source("./functions.R")

key_endes <- read.csv("./data/key_endes_2016_2024.csv") %>% as_tibble()

pop_landscan2_dist <- read.csv("./data/pop_data/landscan_pop.csv") %>% as_tibble()
distritos<- sf::st_read("./data/pop_data/districts/DISTRITOS.shp") %>% clean_names() %>% st_make_valid()

departamento <- sf::st_read("./data/pop_data/departamento_shapefile/SHPCensoDepartamento INEI 2007 geogpsperu SuyoPomalia.shp") %>% 
  clean_names() %>% 
  filter(id_0!=299) %>% 
  mutate(departamento = ifelse(name_1 == "Provincia Constitucional del Callao", 
                               "Lima", name_1)) %>%
  group_by(departamento) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

# 1. ENDES ----

## 1.1. ANEMIA: ----

### Extraccion de datos ----

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
    hw57 = as.factor(hw57),
    hw57_cat = ifelse(hw57 == 4,0,1)
  ) %>% 
  filter(
    !is.na(hw57)
  )



## ANEMIA: Creacion de objetos survey

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
                      ~ svyby(~hw57_cat, ~ubigeo, design = .x,
                                        FUN = svymean, keep.var = TRUE))
  )
  
 
### Estimaciones directas de la categoria HW57 - prueba ----
data_final <-
  
  pop_landscan2_dist %>% 
  rename(
    ubigeo = iddist
  ) %>% 
  mutate(
    ubigeo = as.character(ubigeo),
    ubigeo = as.character(str_pad(ubigeo, width = 6, pad = "0"))
  ) %>% 
  left_join(endes_anemia_svy %>% 
              select(year,anemia_prop) %>%
              ungroup() %>% 
              unnest(cols = c(anemia_prop)) %>% 
              filter(!year %in% c(2024)) %>% 
              mutate(
                #year = as.character(year),
                ubigeo = as.character(str_pad(ubigeo, width = 6, pad = "0"))
              )) %>% 
  
  
  mutate(
    count_anemia = round(hw57_cat * pop_landscan),
    dep = stringr::str_sub(ubigeo, 1, 2),
    dep = ifelse(dep == "07", "15", dep)
  ) %>% 
  group_by(ubigeo) %>% 
  mutate(
    id.sp_dist = cur_group_id(),
    id.sp_dist2 = cur_group_id()
  ) %>% 
  ungroup() %>% 
  group_by(dep) %>% 
  
  mutate(
    id.sp_dep = cur_group_id(),
    id.sp_dep2 = cur_group_id()
  ) %>% 
  ungroup() %>% 
  
  mutate(
    year_re = as.integer(as.factor(year))
  )
  


# 3. SAE INLA - m0

### V.A. espacial
#### distirto
peru.dist <- poly2nb(distritos)
w.peru_dist <- nb2mat(peru.dist, style = "W", zero.policy = TRUE)

### departamento
peru.dep <- poly2nb(departamento)
w.peru_dep <- nb2mat(peru.dep, style = "W")


### modelo 1

m1 <- inla(
  
  formula =  count_anemia ~ 1 + 
    
    f(id.sp_dist, model = "bym2", graph = w.peru_dist) +
    f(id.sp_dep, model = "bym2", graph = w.peru_dep) +
    f(year_re, model = "ar1"),
  
  data = data_final,
  family = "nbinomial",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE, config = TRUE)
)

### modelo 2
m2 <- inla(
  
  formula =  count_anemia ~ 1 + 
    
    f(id.sp_dist, model = "bym2", graph = w.peru_dist) +
    f(year_re, model = "ar1"),
  
  data = data_final,
  family = "nbinomial",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE, config = TRUE)
)

m3 <- inla(
  
  formula =  count_anemia ~ 1 + 
    
    f(id.sp_dist, model = "bym2", graph = w.peru_dist) +
    f(id.sp_dep, model = "bym2", graph = w.peru_dep) +
    f(year_re, model = "ar1") +
    f(id.sp_dist2, model = "bym2", graph = w.peru_dist, group = year_re,
      control.group = list(model = "ar1")),
  
  data = data_final,
  family = "nbinomial",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE, config = TRUE)
)

m4 <-
  inla(
    
    formula =  count_anemia ~ 1 + 
      
      f(id.sp_dist2, model = "bym2", graph = w.peru_dist, group = year_re,
        control.group = list(model = "ar1")) +
      f(id.sp_dep2, model = "bym2", graph = w.peru_dep, group = year_re,
        control.group = list(model = "ar1")),
    
    data = data_final,
    family = "nbinomial",
    offset = log(pop_landscan),  
    
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE, config = TRUE)
  )

m5 <-
  inla(
    
    formula =  count_anemia ~ 1 + 
      
      f(id.sp_dist, model = "bym2", graph = w.peru_dist) +
      f(id.sp_dep, model = "bym2", graph = w.peru_dep) +
      f(year_re, model = "ar1") +
      f(id.sp_dep2, model = "bym2", graph = w.peru_dep, group = year_re,
        control.group = list(model = "ar1")),
    
    data = data_final,
    family = "nbinomial",
    offset = log(pop_landscan),  
    
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE, config = TRUE)
  )


## evaluar 

df_eval <-
  data_final %>% 
  mutate(
    m1_pred = m1$summary.fitted.values$mean, # con distirto y departamento como v. aleatoria
    m2_pred = m2$summary.fitted.values$mean, # solo con distrito
    m3_pred = m3$summary.fitted.values$mean, # distr y dep + interaccion distrito -tiempo
    m4_pred = m4$summary.fitted.values$mean, # solo interacciones
    m5_pred = m4$summary.fitted.values$mean, # distr y dep + interaccion departamento - tiempo
  
    )


df_eval %>% 
  select(year,count_anemia,m1_pred,m2_pred,m3_pred,m4_pred,m5_pred) %>% 
  filter(!is.na(count_anemia)) %>%
  rename(directo = count_anemia) %>%
  pivot_longer(cols = starts_with("m"),
               names_to = "modelo",
               values_to = "predicho") %>%
  ggplot(aes(x = directo, y = predicho, color = modelo)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  facet_grid(year~modelo) +
  labs(
    x = "Estimación directa",
    y = "Predicción del modelo",
    title = "Comparación de estimaciones directas vs modelos SAE",
    subtitle = "Línea diagonal = ajuste perfecto"
  ) +
  theme_minimal()


data.frame(
  Modelo = c("m1","m2","m3","m4","m5"),
  DIC = c(m1$dic$dic, m2$dic$dic,m3$dic$dic,m4$dic$dic,m5$dic$dic),
  WAIC = c(m1$waic$waic,m2$waic$waic,m3$waic$waic,m4$waic$waic,m5$waic$waic)
)

## Performance metrics ----

fitted_vals  <-  
  df_eval %>% 
  select(year,ubigeo,m1_pred:m5_pred,count_anemia) %>% 
  pivot_longer(cols = c(m1_pred:m5_pred), values_to = "fit", names_to = "modelo") %>% 
  mutate(
    actual = count_anemia
  )

#### Metrics ----
perform.metrics <- yardstick::metric_set(mae,mase,smape,rmse)
perform.metrics.dist <- yardstick::metric_set(mae,smape,rmse)

##### total metrics----
tbl.yrd.full <-  fitted_vals %>% 
  group_by(modelo) %>%
  perform.metrics(truth = actual, estimate = fit)
##### metrics by districts----
tbl.yrd.dist_count <-  fitted_vals %>% 
  group_by(modelo,ubigeo) %>%
  perform.metrics.dist(truth = actual, estimate = fit)

tbl.yrd.full %>% 
  pivot_wider(id_cols = modelo,
              names_from = .metric,
              values_from = .estimate) %>%
  arrange(rmse) %>% 
  gt() %>%
  tab_header(title = md("in-sample accuracy metrics")) %>% 
  tab_style(
    style = list(
      #cell_fill(color = "#FFF3B0"),  # amarillo claro
      #cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = modelo %in% c("fit8_spat_2","fit10_spat_2","fit2_spat_2")
    )
  )
