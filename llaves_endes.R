# LLAVE PARA ENDES 2016-2019

key_1<-
  map_df(.x = c(2016:2019),
         .f = ~consulta_endes2(periodo = .x,
                               codigo_modulo = 64,
                               base = 'RECH1',
                               guardar = F) %>% 
           left_join(consulta_endes2(periodo = .x,
                                     codigo_modulo = 65,
                                     base = 'RECH23', guardar = F) %>% 
                       mutate(
                         year = .x
                       ), by = c("HHID")) %>% 
           
           left_join(consulta_endes2(periodo = .x, 
                                     codigo_modulo = 64, 
                                     base = 'RECH0', guardar = F ), by = "HHID")%>% 
           select(year,everything())) %>%
  
  
  clean_names() %>% 
  select(hhid,
         shdistri,
         shprovin,
         year,
         hv001, #conglomerado
         hv002, #vivienda
         hv004, #unidad de muestreo
         hv023, #dominio - region
         ubigeo,
         hv005, # factor de ponderacion hogar
         hv022, #estrato
         hv025
  ) %>% 
  mutate(
    cod_dist = ifelse(shdistri<10,paste0("0",shdistri),shdistri),
    cod_prov = ifelse(shprovin<10,paste0("0",shprovin),shprovin),
    ubigeo = paste0(hv023,cod_prov,cod_dist)
  ) %>% 
  
  select(year,ubigeo,hhid,hv001,hv002,hv004,hv005,hv023,hv022,hv025)

# LLAVE PARA ENDES 2020-2022  
key_2<-
  map_df(.x = c(2020:2022),
         .f = ~consulta_endes2(periodo = .x,
                               codigo_modulo = 1629,
                               base = 'RECH1',
                               guardar = F)%>% 
           
           mutate(
             year = .x
           ) %>% 
           
           left_join(consulta_endes2(periodo = .x, 
                                     codigo_modulo = 1629, 
                                     base = 'RECH0', guardar = F ) %>% 
                       mutate(
                         year = .x
                       ),
                     by = c("HHID","year"))) %>% 
  
  haven::zap_labels() %>% 
  
  clean_names() %>% 
  select(
    year,
    hhid,
    hv001, #conglomerado
    hv002, #vivienda
    hv004, #unidad de muestreo
    hv023, #dominio - region
    ubigeo,
    hv005, # factor de ponderacion hogar
    hv022, #estrato
    hv025
  )

# LLAVE PARA ENDES 2023-2024
key_3<-
  map_df(.x = c(2023:2024),
         .f = ~consulta_endes2(periodo = .x,
                               codigo_modulo = 1629,
                               base = paste0("RECH1_", .x),
                               guardar = F)%>% 
           
           mutate(
             year = .x
           ) %>% 
           
           left_join(consulta_endes2(periodo = .x, 
                                     codigo_modulo = 1629, 
                                     base = paste0("RECH0_", .x), 
                                     guardar = F ) %>% 
                       mutate(
                         year = .x
                       ),
                     by = c("HHID","year"))) %>% 
  
  haven::zap_labels() %>% 
  
  clean_names() %>% 
  select(
    year,
    hhid,
    hv001, #conglomerado
    hv002, #vivienda
    hv004, #unidad de muestreo
    hv023, #dominio - region
    ubigeo,
    hv005, # factor de ponderacion hogar
    hv022, #estrato
    hv025
  )

# LLAVE FINAL

key_2016_2024<- 
  bind_rows(key_1,key_2,key_3)
           
  