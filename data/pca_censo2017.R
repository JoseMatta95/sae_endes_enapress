library(ENDES.PE)
library(tidyverse)
library(haven)
library(janitor)
library(data.table)
library(fastDummies)
library(factoextra)

## CENSO 2007

## CENSO 2017

## census data 2017 ----
censo_hogares <- read_sav("../Desktop/endes/CENSO 2017 - RAW/CPV2017_HOG.sav", 
                          col_select = c("ubigeo","area",'departamento','distrito',
                                         starts_with('c3_p1'),starts_with("c3_p2"),'c4_p1')) %>% 
  #filter(departamento == "LORETO") %>% 
  zap_labels() %>% 
  mutate(
    ubigeo = case_when(
      ubigeo %in% c("120606", "120604") ~ "120699",
      TRUE ~ ubigeo
    )
  )
  

censo_hogar_dist<-
  censo_hogares %>% 
  group_by(ubigeo) %>% 
  mutate(
    across(.cols = c3_p2_1:c3_p2_16, .f = ~ifelse(.x == 1,0,1)),
    ubigeo = as.character(ubigeo)
  ) %>% 
  summarise(
    across(.cols = c3_p1_1:c4_p1, .f = ~mean(.x,na.rm = T))
  )

#write.csv(censo_hogares, "./data processing/INLA_SAE/data/censo_2017/censo_hogar_2017.csv", row.names = F)

censo_vivienda <- read_sav("../Desktop/endes/CENSO 2017 - RAW/CPV2017_VIV.sav", 
                           col_select = c("ubigeo","area",'departamento','distrito',
                                          starts_with('c2_p'),'t_c4_p1')) %>% 
  select(-c2_p2,-c2_p7,-c2_p7a,-c2_p7b,-c2_p7c,-c2_p8,-c2_p9) %>% 
  #filter(departamento == "LORETO") %>% 
  zap_labels() %>% 
  mutate(
    ubigeo = case_when(
      ubigeo %in% c("120606", "120604") ~ "120699",
      TRUE ~ ubigeo
    )
  )

censo_vivienda_dist<-
  censo_vivienda %>% 
  mutate(
    c2_p11 = ifelse(c2_p11==1,0,1),
    area = ifelse(area==1,0,1)
  ) %>% 
  filter(!is.na(c2_p3)) %>% 
  dummy_cols(select_columns = c("c2_p1","c2_p3","c2_p4","c2_p5","c2_p6",
                                "c2_p10")) %>% 
  group_by(ubigeo) %>%
  
  summarise(
    across(.cols = c(c2_p1:c2_p10_8), .f = ~mean(.x, na.rm=T))
  ) %>% 
  select(-(c2_p1:c2_p10))

#write.csv(censo_vivienda, "./data processing/INLA_SAE/data/censo_2017/censo_vivienda_2017.csv", row.names = F)

censo_poblacion <- read_sav("../Desktop/endes/CENSO 2017 - RAW/CPV2017_POB.sav", 
                            col_select = c("ubigeo","area",'departamento','distrito',
                                           starts_with('c5_p8'),'c5_p11','c5_p12',
                                           'c5_p13_niv','c5_p15','c5_p16','c5_p23')) %>% 
  #filter(departamento == "LORETO") %>% 
  zap_labels()



library(data.table)

# Convertir a data.table (más eficiente)
setDT(censo_poblacion)

censo_poblacion[ubigeo %in% c("120606", "120604"), ubigeo := "120699"]
# Primero agregar, luego crear proporciones
censo_poblacion_dist <- censo_poblacion[
  , .(
    # Recodificar variables
    c5_p11_1 = mean(c5_p11 == 10, na.rm = TRUE),
    c5_p11_2 = mean(c5_p11 == 1, na.rm = TRUE),
    c5_p11_3 = mean(c5_p11 == 2, na.rm = TRUE),
    c5_p11_4 = mean(!c5_p11 %in% c(1, 2, 10), na.rm = TRUE),
    
    c5_p13_niv_1 = mean(c5_p13_niv == 2, na.rm = TRUE),
    c5_p13_niv_3 = mean(c5_p13_niv == 5, na.rm = TRUE),
    c5_p13_niv_6 = mean(c5_p13_niv > 5, na.rm = TRUE),
    
    c5_p12 = mean(ifelse(c5_p12 == 1, 0, 1), na.rm = TRUE),
    c5_p15 = mean(ifelse(c5_p15 == 1, 0, 1), na.rm = TRUE),
    c5_p16 = mean(ifelse(c5_p16 == 1, 0, 1), na.rm = TRUE),
    c5_p23 = mean(ifelse(c5_p23 == 1, 0, 1), na.rm = TRUE),
    
    # Añade el resto de variables que necesites
    c5_p8_1 = mean(c5_p8_1, na.rm = TRUE)
    # ... continúa con todas las que necesites
  ),
  by = ubigeo
][!is.na(ubigeo)]

# Convertir a tibble
censo_poblacion_dist <- as_tibble(censo_poblacion_dist)

#write.csv(censo_poblacion, "./data processing/INLA_SAE/data/censo_2017/censo_poblacion_2017.csv", row.names = F)


### final dataset CENSO-distrito 2017 ----

censo_final_loreto_2017<-
  censo_hogar_dist %>% 
  full_join(censo_vivienda_dist) %>% 
  full_join(censo_poblacion_dist) %>% 
  mutate(
    ubigeo = as.character(ubigeo)
  )
#write.csv(censo_final_loreto_2017, "./data/censo_2017/01.censo_final_loreto_2017.csv", row.names = F)


## PCA
# censo_final_loreto_2017 has 95 variables. we used PCA  to address the problem

censo_loreto_sinid<- 
  censo_final_loreto_2017 %>% 
  select(-ubigeo,-c3_p1_3,-c3_p1_6) # matrix sin id

#scaled_censo_loreto_sinid <- scale(censo_loreto_sinid) # escalar valores

pca <- prcomp(censo_loreto_sinid, center = TRUE, scale. = TRUE) 
summary(pca) # varianza explicada
plot(pca, type = "l", main = "Scree Plot")
pca1<-fviz_pca_var(pca, col.var = "contrib", repel = TRUE)

## devolviendo id y agregando outcome

censo_pca_25<-
  bind_cols(censo_final_loreto_2017$ubigeo,pca$x[,1:25]) %>% 
  clean_names() %>% 
  rename(ubigeo = x1) %>% 
  mutate(ubigeo = as.character(ubigeo))

write.csv(censo_pca_25, "./data/censo_2017_pca_25.csv", row.names = F)
