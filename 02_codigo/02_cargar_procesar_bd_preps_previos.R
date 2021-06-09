### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R") 

### Cargar y procesar datos de los PREPs de 2006 a 2018 ----

# Fuentes:

# PREP 2006: https://portalanterior.ine.mx/documentos/proceso_2005-2006/prep2006/bd_prep2006/PREP2006-Diputados.zip

# PREP 2009: http://prep2009.ife.org.mx/PREP2009/20090706_2000-listaActas.tar.gz

# PREP 2012: http://prep2012.ife.org.mx/prep/20120702_2000-listaActas.tar.gz

# PREP 2015: http://prep2015.ine.mx/20150608_2010-listaActas.tar.gz

# PREP 2018: https://prep2018.ine.mx/#/descargaBase

bd_prep_2006 <- 
  read_delim("01_datos/PREP2006-Diputados/PREP2006-Diputados.txt", "|", escape_double = FALSE, trim_ws = TRUE, skip = 0, locale = locale(encoding = "latin1")) %>% 
  clean_names() %>% 
  filter(!is.na(nom_estado))

bd_prep_2009 <- 
  read_delim("01_datos/20090706_2000-listaActas/diputados.txt", "|", escape_double = FALSE, trim_ws = TRUE, skip = 5, locale = locale(encoding = "latin1")) %>% 
  clean_names()

bd_prep_2012 <- 
  read_delim("01_datos/20120702_2000-listaActas/diputados.txt", "|", escape_double = FALSE, trim_ws = TRUE, skip = 5, locale = locale(encoding = "latin1"))  %>% 
  clean_names()

problems(bd_prep_2012) %>% print(n = Inf)

bd_prep_2015 <- 
  read_delim("01_datos/20150608_2010-listaActas/diputados.csv", "|", escape_double = FALSE, trim_ws = TRUE, skip = 6, locale = locale(encoding = "latin1")) %>% 
  clean_names()

problems(bd_prep_2015) %>% print(n = 1000)

bd_prep_2018 <- 
  read_delim("01_datos/20180702_2100_PREP_diputaciones/diputaciones.csv", delim = "|", escape_double = FALSE, trim_ws = TRUE, skip = 6, locale = locale(encoding = "latin1"))  %>% 
  clean_names() %>%
  # Excluir renglones con valor "-" en clave_casilla y/o "null" en distrito_federal 
  filter(clave_casilla != "-",
         distrito_federal != "null")  


## Echar ojito a datos ----
bd_prep_2006 %>% glimpse()
bd_prep_2009 %>% glimpse()
bd_prep_2012 %>% glimpse()
bd_prep_2015 %>% glimpse()
bd_prep_2018 %>% glimpse()

## Transformar columnas de tiempo ----

# PREP 2006
bd_prep_2006 <- 
  bd_prep_2006 %>% 
  mutate(hora_captura = str_replace_all(hora_captura_cedat, "/", "-"), 
         hora_captura = str_replace_all(hora_captura, "-06", "-2006"),
         hora_captura = str_replace_all(hora_captura, ",000000", ""),
         hora_captura = dmy_hms(hora_captura),
         hora_recepcion = str_replace_all(hora_recepcion_cedat, "/", "-"), 
         hora_recepcion = str_replace_all(hora_recepcion, "-06", "-2006"),
         hora_recepcion = str_replace_all(hora_recepcion, ",000000", ""),
         hora_recepcion = dmy_hms(hora_recepcion)) %>%
  separate(hora_registro, into = c("hora_registro", "basura"), sep = ",") %>% 
  select(-basura) %>% 
  mutate(hora_registro = str_replace_all(hora_registro, "/", "-"), 
         hora_registro = str_replace_all(hora_registro, "-06", "-2006"),
         hora_registro = dmy_hms(hora_registro)) 

bd_prep_2006 %>% glimpse()

# PREP 2009
bd_prep_2009 <- 
  bd_prep_2009 %>% 
  mutate(hora_acopio = ifelse(hora_acopio == "null", NA, hora_acopio), # Reemplazar valores "null" por NA
         hora_captura = ifelse(hora_captura == "null", NA, hora_captura), # Reemplazar valores "null" por NA
         hora_acopio = dmy_hms(hora_acopio), # Transformar tipo de dato a ymd_hms
         hora_captura = dmy_hms(hora_captura)) # Transformar tipo de dato a ymd_hms

bd_prep_2009 %>% glimpse()


# PREP 2018
bd_prep_2018 <- 
  bd_prep_2018 %>% 
  mutate(fecha_hora_acopio = ymd_hms(fecha_hora_acopio), # Transformar tipo de dato a ymd_hms
         fecha_hora_captura = ymd_hms(fecha_hora_captura)) # Transformar tipo de dato a ymd_hms

bd_prep_2018 %>% glimpse()


## Generar variable para registrar el número de actas que se tuvieron que procesar en las casillas de cada estado en 2015 ----

# Fuente: https://portal.ine.mx/voto-y-elecciones/calendario-electoral/

# 2015
bd_prep_2015 <- 
  bd_prep_2015 %>% 
  mutate(num_act = case_when(estado == "BAJA CALIFORNIA SUR" ~ 4,
                             estado == "CAMPECHE" ~ 4,
                             estado == "COLIMA" ~ 4,
                             estado == "DISTRITO FEDERAL" ~ 3,
                             estado == "GUANAJUATO" ~ 3,
                             estado == "GUERRERO" ~ 4,
                             estado == "JALISCO" ~ 3,
                             estado == "MÉXICO" ~ 3,
                             estado == "MICHOACÁN" ~ 4,
                             estado == "MORELOS" ~ 3,
                             estado == "NUEVO LEÓN" ~ 4,
                             estado == "QUERÉTARO" ~ 4,
                             estado == "SAN LUIS POTOSÍ" ~ 4,
                             estado == "SONORA" ~ 4,
                             estado == "TABASCO" ~ 3,
                             estado == "YUCATÁN" ~ 3),
         num_act = ifelse(is.na(num_act), 1, num_act))


# 2018
bd_prep_2018 <- 
  bd_prep_2018 %>% 
  mutate(num_act = case_when(estado == "AGUASCALIENTES" ~ 4,
                             estado == "BAJA CALIFORNIA" ~ 3,
                             estado == "BAJA CALIFORNIA SUR" ~ 5,
                             estado == "CAMPECHE" ~ 5,
                             estado == "CHIAPAS" ~ 6,
                             estado == "CHIHUAHUA" ~ 5,
                             estado == "CIUDAD DE MÉXICO" ~ 6,
                             estado == "COAHUILA" ~ 4,
                             estado == "COLIMA" ~ 5,
                             estado == "DURANGO" ~ 4,
                             estado == "GUANAJUATO" ~ 6,
                             estado == "GUERRERO" ~ 5,
                             estado == "HIDALGO" ~ 4,
                             estado == "JALISCO" ~ 6,
                             estado == "MÉXICO" ~ 5,
                             estado == "MICHOACÁN" ~ 5,
                             estado == "MORELOS" ~ 6,
                             estado == "NAYARIT" ~ 3,
                             estado == "NUEVO LEÓN" ~ 5,
                             estado == "OAXACA" ~ 5,
                             estado == "PUEBLA" ~ 6,
                             estado == "QUERÉTARO" ~ 5,
                             estado == "QUINTANA ROO" ~ 4,
                             estado == "SAN LUIS POTOSÍ" ~ 5,
                             estado == "SINALOA" ~ 5,
                             estado == "SONORA" ~ 5,
                             estado == "TABASCO" ~ 6,
                             estado == "TAMAULIPAS" ~ 4,
                             estado == "TLAXCALA" ~ 4,
                             estado == "VERACRUZ" ~ 5,
                             estado == "YUCATÁN" ~ 6,
                             estado == "ZACATECAS" ~ 5))