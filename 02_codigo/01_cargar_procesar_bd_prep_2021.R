### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R") 

### Importar base de datos ----
bd_prep_2021 <- 
  read_csv("01_datos/20210607_2000_PREP_Diputaciones/Diputaciones_2021.csv", 
           skip = 5) %>% 
  clean_names()

### Excluir observaciones con valor "-" en clave_casilla ----

# En todas estas observaciones el valor en las columnas de las fuerzas políticas es "-"
bd_prep_2021 %>% 
  filter(clave_casilla == "-",
         clave_acta  == "-") %>% 
  select(pan:total_votos_asentado) %>% 
  print(n = Inf)

bd_prep_2021 <- 
  bd_prep_2021 %>% 
  filter(clave_casilla != "-",
         clave_acta  != "-")

# En este paso excluyo 3,225 observaciones y el tibble queda con 163,666 renglones

### Generar claves de estado, distrito, sección agregándoles 0s antes del número correspondiente, y generar claves de estado-distrito y estado-sección---- 
bd_prep_2021 <- 
  bd_prep_2021 %>% 
  filter(!is.na(clave_casilla)) %>% # Eliminar renglones sin datos
  mutate(cve_edo = str_pad(id_estado, 2, pad = "0"),
         cve_dtto = str_pad(id_distrito_federal, 2, pad = "0"),
         cve_seccion = str_pad(seccion, 4, pad = "0"),
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = ""),
         cve_edo_dtto_seccion = paste(cve_edo, cve_dtto, cve_seccion, sep = ""),
         cve_edo_seccion = paste(cve_edo, cve_seccion, sep = ""))


### Reemplazar cadenas de texto por NAs y el tipo de dato en columnas que contienen datos de boletas o votos ----
bd_prep_2021 <- 
  bd_prep_2021 %>% 
  mutate_at(
    # Seleccionar variables
    vars(total_boletas_sobrantes:total_votos_calculados),
    # Reemplazar cadenas de texto por NAs
    ~ ifelse(.x == "-", NA, 
             ifelse(.x == "Ilegible", NA, 
                    ifelse(.x == "Sin dato", NA, .x))) %>%
      # Cambiar tipo de dato a numeric
      as.numeric()
  ) 



### Cambiar tipo de dato a date time (dttm) de columnas que registran fecha y hora ----
bd_prep_2021 <- 
  bd_prep_2021 %>% 
  mutate(fecha_hora_acopio = ifelse(fecha_hora_acopio == "-", NA, fecha_hora_acopio),
         fecha_hora_captura = ifelse(fecha_hora_captura == "-", NA, fecha_hora_captura),
         fecha_hora_verificacion = ifelse(fecha_hora_verificacion == "-", NA, fecha_hora_verificacion),
         fecha_hora_acopio = dmy_hms(fecha_hora_acopio),
         fecha_hora_captura = dmy_hms(fecha_hora_captura),
         fecha_hora_verificacion = dmy_hms(fecha_hora_verificacion))


### Generar variable para registrar el número de actas que se tuvieron que procesar en las casillas de cada estado en 2021 ----

# Fuente: https://portal.ine.mx/voto-y-elecciones/calendario-electoral/
bd_prep_2021 <- 
  bd_prep_2021 %>% 
  mutate(num_act = case_when(estado == "AGUASCALIENTES" ~ 3, # DF, DL y M
                             estado == "BAJA CALIFORNIA" ~ 4, # DF, G, DL y M
                             estado == "BAJA CALIFORNIA SUR" ~ 4, # DF, G, DL y M
                             estado == "CAMPECHE" ~ 5, # DF, G, DL y M
                             estado == "CHIAPAS" ~ 3, # DF, DL y M
                             estado == "CHIHUAHUA" ~ 5, # DF, G, DL y M
                             estado == "CIUDAD DE MÉXICO" ~ 3, # DF, DL y M
                             estado == "COAHUILA" ~ 2, # DF y M
                             estado == "COLIMA" ~ 4, # DF, G, DL y M
                             estado == "DURANGO" ~ 2, # DF y DL
                             estado == "GUANAJUATO" ~ 3, # DF, DL y M
                             estado == "GUERRERO" ~ 4, # DF, G, DL y M
                             estado == "HIDALGO" ~ 3, # DF, DL y M
                             estado == "JALISCO" ~ 3, # DF, DL y M
                             estado == "MÉXICO" ~ 3, # DF, DL y M
                             estado == "MICHOACÁN" ~ 4, # DF, G, DL y M
                             estado == "MORELOS" ~ 3, # DF, DL y M
                             estado == "NAYARIT" ~ 5, # DF, G, DL y M
                             estado == "NUEVO LEÓN" ~ 4, # DF, G, DL y M
                             estado == "OAXACA" ~ 3, # DF, DL y M
                             estado == "PUEBLA" ~ 3, # DF, DL y M
                             estado == "QUERÉTARO" ~ 4, # DF, G, DL y M
                             estado == "QUINTANA ROO" ~ 2, # DF y M
                             estado == "SAN LUIS POTOSÍ" ~ 4, # DF, G, DL y M
                             estado == "SINALOA" ~ 4, # DF, G, DL y M
                             estado == "SONORA" ~ 4, # DF, G, DL y M
                             estado == "TABASCO" ~ 3, # DF, DL y M
                             estado == "TAMAULIPAS" ~ 3, # DF, DL y M
                             estado == "TLAXCALA" ~ 5, # DF, G, DL y M
                             estado == "VERACRUZ" ~ 3, # DF, DL y M
                             estado == "YUCATÁN" ~ 3, # DF, DL y M
                             estado == "ZACATECAS" ~ 4 # DF, G, DL y M
  ))


### Generar versión en altas y bajas de los nombres de los estados ----
bd_prep_2021 <- 
  bd_prep_2021 %>% 
  mutate(nom_edo = case_when(estado == "AGUASCALIENTES" ~ "Aguascalientes",
                             estado == "BAJA CALIFORNIA" ~ "Baja California",
                             estado == "BAJA CALIFORNIA SUR" ~ "Baja California Sur",
                             estado == "CAMPECHE" ~ "Campeche",
                             estado == "CHIAPAS" ~ "Chiapas",
                             estado == "CHIHUAHUA" ~ "Chihuahua",
                             estado == "CIUDAD DE MÉXICO" ~ "Ciudad de México",
                             estado == "COAHUILA" ~ "Coahuila",
                             estado == "COLIMA" ~ "Colima",
                             estado == "DURANGO" ~ "Durango",
                             estado == "GUANAJUATO" ~ "Guanajuato",
                             estado == "GUERRERO" ~ "Guerrero",
                             estado == "HIDALGO" ~ "Hidalgo",
                             estado == "JALISCO" ~ "Jalisco",
                             estado == "MÉXICO" ~ "Estado de México",
                             estado == "MICHOACÁN" ~ "Michoacán",
                             estado == "MORELOS" ~ "Morelos",
                             estado == "NAYARIT" ~ "Nayarit",
                             estado == "NUEVO LEÓN" ~ "Nuevo León",
                             estado == "OAXACA" ~ "Oaxaca",
                             estado == "PUEBLA" ~ "Puebla",
                             estado == "QUERÉTARO" ~ "Querétaro",
                             estado == "QUINTANA ROO" ~ "Quintana Roo",
                             estado == "SAN LUIS POTOSÍ" ~ "San Luis Potosí",
                             estado == "SINALOA" ~ "Sinaloa",
                             estado == "SONORA" ~ "Sonora",
                             estado == "TABASCO" ~ "Tabasco",
                             estado == "TAMAULIPAS" ~ "Tamaulipas",
                             estado == "TLAXCALA" ~ "Tlaxcala",
                             estado == "VERACRUZ" ~ "Veracruz",
                             estado == "YUCATÁN" ~ "Yucatán",
                             estado == "ZACATECAS" ~ "Zacatecas")) 

### Reordenar columnas ----
bd_prep_2021 <- 
  bd_prep_2021 %>% 
  select(clave_casilla:estado, nom_edo, id_distrito_federal:seccion, cve_edo, cve_dtto, cve_seccion, cve_edo_dtto, cve_edo_dtto_seccion, cve_edo_seccion, everything())

