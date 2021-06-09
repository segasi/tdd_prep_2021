### Cargar y procesar base de datos----
source("02_codigo/01_cargar_procesar_bd_prep_2021.R") 

### Definir paletas de colores para gráficas -----
paleta_2 <- c("#e56b6f", "#355070")
paleta_4 <- c("#e56b6f", "#b56576", "#6d597a", "#355070") 

### Análisis de la evolución del ACOPIO de actas PREP 2021 ----

## Eliminar observaciones con valores en fecha_hora_acopio i) menores a las 17:00 hrs. del 6 de junio; ii) mayores a las 20:00 hrs. del 7 de junio; o, iii) faltantes ----

# Contar núm. de obs. que cumplen con uno de estos criterios
bd_prep_2021 %>%
  summarise(fuera_rango_antes = sum(fecha_hora_acopio < as_datetime("2021-06-06 17:00:00"), na.rm = T),
            fuera_rango_despues = sum(fecha_hora_acopio > as_datetime("2021-06-07 20:00:00"), na.rm = T),
            valor_faltante = sum(is.na(fecha_hora_acopio)))

# En total excluiré 4,719 renglones, de los cuales 89 tienen fecha-hora de acopio previa a las 17:00 hrs. del 6 de junio, una tienen fecha-hora de acopio posterior a las 20:00 hrs. del 7 de junio y 4,629 tienen valores faltantes.

bd_prep_2021_aco <- 
  bd_prep_2021 %>%
  filter(!is.na(fecha_hora_acopio),
         fecha_hora_acopio >= as_datetime("2021-06-06 17:00:00"),
         fecha_hora_acopio <= as_datetime("2021-06-07 20:00:00"))

# Después de este paso el tibble tiene 158,947 renglones

# Generar texto que incluiré en la nota al pie de todas las gráficas en las que analice la evolución del acopio

## Generar textos para notas al pie ----
nota_acopio <- "\nElaborada por @segasi / Fuente: INE, PREP 2021\nNota: La gráfica excluye 90 actas cuya fecha de acopio es previa a las 17:00 hrs. del 6 de junio o posterior a las 20:00 hrs. del 7 de junio. Excluye también 4,629 actas que\nno fueron acopiadas."

nota_acopio_digi <- "\nElaborada por @segasi / Fuente: INE, PREP 2021\nNota: La gráfica excluye 90 actas cuya fecha de acopio es previa a las 17:00 hrs. del 6 de junio o posterior a las 20:00 hrs. del 7 de junio. Excluye también 4,629 actas que\nno fueron acopiadas y 96 que fueron digitalizadas mediante urna electrónica."

## Evolución del número y número acumulado de actas acopiadas por minuto ----
bd_prep_2021_aco %>%
  count(fecha_hora_acopio, 
        name = "Número") %>% 
  mutate(`Número acumulado` = cumsum(Número)) %>% 
  pivot_longer(-fecha_hora_acopio) %>% 
  ggplot(aes(x = fecha_hora_acopio, 
             y = value, 
             color = name)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 hours", 
                   date_labels = "%R",
                   expand = c(0, 0),
                   limits = c(as_datetime("2021-06-06 16:40:00"), 
                              as_datetime("2021-06-07 20:20:00"))) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = paleta_2,
                     guide = guide_legend(title.position = "top",
                                          keyheight = 0.4)) +
  facet_wrap(~ name, scales = "free_y") +
  labs(title = "PREP 2021 | Evolución del número y número acumulado de actas acopiadas por minuto",
       x = "\nFecha y hora de acopio",
       y = NULL,
       caption = nota_acopio) +
  tema +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5,
                                   size = 14),
        strip.text = element_text(size = 20),
        panel.border = element_rect(color = "grey80", 
                                    size = 0.5, 
                                    fill = "transparent")) +
  ggsave("03_graficas/evolucion/acopio/evolucion_numero_y_numero_acumulado_actas_acopiadas_por_minuto.png", width = 16, height = 9, dpi = 200)
  


## Evolución del número y número acumulado de actas acopiadas por minuto y medio de digitalización ----
bd_prep_2021_aco %>%
  filter(digitalizacion != "Urna Electrónica") %>%
  group_by(digitalizacion, fecha_hora_acopio) %>% 
  summarise(Número = n()) %>% 
  ungroup() %>%
  # Generar y juntar observaciones con valores de fecha_hora_acopio faltantes para "Móvil" hasta el último minuto que se acopiaron actas vía escáner
  bind_rows(tibble(digitalizacion = rep("Móvil", 495),
                   # Fechas-horas 
                   fecha_hora_acopio = seq(from = as_datetime("2021-06-07 11:30:00"), to = as_datetime("2021-06-07 19:44:00"), by = "1 min"),
                   Número = 0)) %>% 
  arrange(digitalizacion, fecha_hora_acopio) %>% 
  group_by(digitalizacion) %>% 
  mutate(`Número acumulado` = cumsum(Número)) %>% 
  ungroup() %>% 
  mutate(digitalizacion = ifelse(digitalizacion == "Móvil", "PREP casilla", digitalizacion),
         digitalizacion = fct_relevel(digitalizacion, "PREP casilla", "Escáner")) %>% 
  ggplot(aes(x = fecha_hora_acopio, 
             y = `Número acumulado`, 
             color = digitalizacion)) +
  geom_line(size = 1.5) + 
  scale_x_datetime(date_breaks = "1 hours", 
                   date_labels = "%R",
                   expand = c(0, 0),
                   limits = c(as_datetime("2021-06-06 16:40:00"), 
                              as_datetime("2021-06-07 20:20:00"))) +
  scale_y_continuous(labels = comma, 
                     breaks = seq(0, 90000, 10000)) +
  scale_color_manual(values = paleta_2,
                     guide = guide_legend(title.position = "top",
                                          keyheight = 0.4)) +
  labs(title = "PREP 2021 | Evolución del número y número acumulado de actas acopiadas por minuto y\nmedio de digitalización",
       x = "\nFecha y hora de acopio",
       y = NULL,
       color = "Medio de digitalización",
       caption = nota_acopio_digi) +
  tema +
  theme(legend.position = c(0.82, 0.2),
        legend.direction = "horizontal",
        legend.title = element_text(size = 18, hjust = 0),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggsave("03_graficas/evolucion/acopio/evolucion_numero_acumulado_actas_acopiadas_dado_medio_digitalizacion.png", width = 16, height = 9, dpi = 200)


## Evolución del porcentaje acumulado de actas acopiadas por minuto, dado el número de elecciones que debían ser computadas en la casilla ----
bd_prep_2021_aco %>% 
  arrange(num_act, fecha_hora_acopio) %>% 
  group_by(num_act) %>% 
  mutate(id = 1,
         acumuladas = cumsum(id), 
         acumuladas_por = ((acumuladas/max(acumuladas)))) %>% 
  ungroup() %>% 
  select(num_act, fecha_hora_acopio, id, acumuladas, acumuladas_por) %>% 
  ggplot(aes(x = fecha_hora_acopio, 
             y = acumuladas_por,
             color = as.factor(num_act))) +
  geom_line(size = 1.5) +
  scale_x_datetime(date_breaks = "1 hours", 
                   date_labels = "%R",
                   expand = c(0, 0),
                   limits = c(as_datetime("2021-06-06 16:40:00"), 
                              as_datetime("2021-06-07 20:20:00"))) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = paleta_4,
                     guide = guide_legend(title.position = "top",
                                          keyheight = 0.4)) +
  labs(title = "PREP 2021 | Evolución del porcentaje acumulado de actas acopiadas por minuto, dado el\nnúmero de elecciones que debían ser computadas en la casilla",
       x = "\nFecha y hora de acopio",
       y = NULL,
       color = "\nNúmero de elecciones\ncomputadas en casilla",
       caption = nota_acopio) +
  tema +
  theme(legend.position = c(0.87, 0.2),
        legend.direction = "horizontal",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggsave("03_graficas/evolucion/acopio/evolucion_porcentaje_acumulado_actas_acopiadas_dado_numero_de_elecciones_en_casilla.png", width = 16, height = 9, dpi = 200)


## Evolución del porcentaje acumulado de actas de casillas urbanas y y no urbanas acopiadas por minuto ----
bd_prep_2021_aco %>% 
  arrange(ubicacion_casilla, fecha_hora_acopio) %>%
  group_by(ubicacion_casilla) %>% 
  mutate(id = 1,
         acumuladas = cumsum(id), 
         acumuladas_por = ((acumuladas/max(acumuladas)))) %>% 
  ungroup() %>% 
  ggplot(aes(x = fecha_hora_acopio, 
             y = acumuladas_por,
             color = ubicacion_casilla)) +
  geom_line(size = 1.5) +
  scale_x_datetime(date_breaks = "1 hours", 
                   date_labels = "%R",
                   expand = c(0, 0),
                   limits = c(as_datetime("2021-06-06 16:40:00"), 
                              as_datetime("2021-06-07 20:20:00"))) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = paleta_2,
                     guide = guide_legend(title.position = "top",
                                          keyheight = 0.4)) +
  labs(title = "PREP 2021 | Evolución del porcentaje acumulado de actas de casillas urbanas y y no urbanas\nacopiadas por minuto",
       x = "\nFecha y hora de acopio",
       y = NULL,
       color = "\nTipo de casilla",
       caption = nota_acopio) +
  tema +
  theme(legend.position = c(0.87, 0.2),
        legend.direction = "horizontal",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggsave("03_graficas/evolucion/acopio/evolucion_porcentaje_acumulado_actas_acopiadas_dada_ubicacion_casilla.png", width = 16.5, height = 9, dpi = 200)

## Evolución del porcentaje acumulado de actas de casillas urbanas y no urbanas acopiadas por minuto, dado el medio de digitalización del acta ----
foo <- 
  bd_prep_2021_aco %>%
  filter(digitalizacion != "Urna Electrónica") %>% 
  arrange(digitalizacion, ubicacion_casilla, fecha_hora_acopio) %>%
  mutate(id = 1) %>%  
  select(digitalizacion, ubicacion_casilla, fecha_hora_acopio, id) %>% 
# Generar y juntar observaciones con valores de fecha_hora_acopio faltantes para "Móvil" hasta el último minuto que se acopiaron actas vía escáner
bind_rows(tibble(digitalizacion = c(rep("Móvil", 781),
                                    rep("Móvil", 512)),
                 ubicacion_casilla = c(rep("Urbana", 781),
                                       rep("No Urbana", 512)),
                 # Fechas-horas 
                 fecha_hora_acopio = c(seq(from = as_datetime("2021-06-07 06:44:00"), to = as_datetime("2021-06-07 19:44:00"), by = "1 min"),
                                       seq(from = as_datetime("2021-06-07 11:13:00"), to = as_datetime("2021-06-07 19:44:00"), by = "1 min")),
                 id = 0)) %>% 
  group_by(digitalizacion, ubicacion_casilla) %>% 
  mutate(acumuladas = cumsum(id),
         acumuladas_por = ((acumuladas/max(acumuladas)))) %>% 
  ungroup() %>%
  mutate(ubicacion_casilla = ifelse(ubicacion_casilla == "Urbana", "Casillas urbanas", "Casillas no urbanas"),
         ubicacion_casilla = fct_relevel(ubicacion_casilla, "Casillas urbanas", "Casillas no urbanas"),
         digitalizacion = ifelse(digitalizacion == "Móvil", "PREP casilla", digitalizacion),
         digitalizacion = fct_relevel(digitalizacion, "PREP casilla", "Escáner")) 


foo %>% 
  ggplot(aes(x = fecha_hora_acopio, 
             y = acumuladas_por, 
             color = as.factor(digitalizacion))) +
  geom_line(size = 1.5) +
  facet_wrap(~ ubicacion_casilla) +
  scale_x_datetime(date_breaks = "1 hours", 
                   date_labels = "%R",
                   expand = c(0, 0),
                   limits = c(as_datetime("2021-06-06 16:40:00"), 
                              as_datetime("2021-06-07 20:20:00"))) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = paleta_2,
                     guide = guide_legend(title.position = "top",
                                          keyheight = 0.4)) +
  labs(title = "PREP 2021 | Evolución del porcentaje acumulado de actas de casillas urbanas y no urbanas\nacopiadas por minuto, dado el medio de digitalización",
       x = "\nFecha y hora de acopio",
       y = NULL,
       color = "\nMedio de digitalización",
       caption = nota_acopio_digi) +
  tema +
  theme(legend.position = c(0.87, 0.2),
        legend.direction = "horizontal",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5,
                                   size = 14),
        strip.text = element_text(size = 20),
        panel.border = element_rect(color = "grey80", 
                                    size = 0.5, 
                                    fill = "transparent")) +
  ggsave("03_graficas/evolucion/acopio/evolucion_porcentaje_acumulado_actas_acopiadas_dado_numero_de_elecciones_en_casilla_y_medio_digitalizacion.png", width = 16.5, height = 9, dpi = 200)


## Evolución del número acumulado de actas de casillas urbanas y no urbanas acopiadas por minuto, dado el medio de digitalización del acta ----
foo %>% 
  ggplot(aes(x = fecha_hora_acopio, 
             y = acumuladas, 
             color = as.factor(digitalizacion))) +
  geom_line(size = 1.5) +
  facet_wrap(~ ubicacion_casilla) +
  scale_x_datetime(date_breaks = "1 hours", 
                   date_labels = "%R",
                   expand = c(0, 0),
                   limits = c(as_datetime("2021-06-06 16:40:00"), 
                              as_datetime("2021-06-07 20:20:00"))) +
  scale_y_continuous(labels = comma,
                     breaks = seq(0, 60000, 10000)) +
  scale_color_manual(values = paleta_2,
                     guide = guide_legend(title.position = "top",
                                          keyheight = 0.4)) +
  labs(title = "PREP 2021 | Evolución del número acumulado de actas de casillas urbanas y no urbanas\nacopiadas por minuto, dado el medio de digitalización",
       x = "\nFecha y hora de acopio",
       y = NULL,
       color = "\nMedio de digitalización",
       caption = nota_acopio_digi) +
  tema +
  theme(legend.position = c(0.87, 0.2),
        legend.direction = "horizontal",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5,
                                   size = 14),
        strip.text = element_text(size = 20),
        panel.border = element_rect(color = "grey80", 
                                    size = 0.5, 
                                    fill = "transparent")) +
  ggsave("03_graficas/evolucion/acopio/evolucion_numero_acumulado_actas_acopiadas_dado_numero_de_elecciones_en_casilla_y_medio_digitalizacion.png", width = 16.5, height = 9, dpi = 200)


## Evolución del porcentaje acumulado de actas de casillas urbanas y no urbanas acopiadas por minuto, dado el número de elecciones que debían ser computadas en la casilla ----
bd_prep_2021_aco %>%
  arrange(num_act, ubicacion_casilla, fecha_hora_acopio) %>%
  group_by(num_act, ubicacion_casilla) %>% 
  mutate(id = 1,
         acumuladas = cumsum(id), 
         acumuladas_por = ((acumuladas/max(acumuladas)))) %>% 
  ungroup() %>% 
  select(num_act, ubicacion_casilla, fecha_hora_acopio, id, acumuladas, acumuladas_por) %>% 
  mutate(ubicacion_casilla = ifelse(ubicacion_casilla == "Urbana", "Casillas urbanas", "Casillas no urbanas"),
         ubicacion_casilla = fct_relevel(ubicacion_casilla, "Casillas urbanas", "Casillas no urbanas")) %>% 
  ggplot(aes(x = fecha_hora_acopio, 
             y = acumuladas_por, 
             color = as.factor(num_act))) +
  geom_line(size = 1.5) +
  facet_wrap(~ ubicacion_casilla) +
  scale_x_datetime(date_breaks = "1 hours", 
                   date_labels = "%R",
                   expand = c(0, 0),
                   limits = c(as_datetime("2021-06-06 16:40:00"), 
                              as_datetime("2021-06-07 20:20:00"))) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = paleta_4,
                     guide = guide_legend(title.position = "top",
                                          keyheight = 0.4)) +
  labs(title = "PREP 2021 | Evolución del porcentaje acumulado de actas de casillas urbanas y no urbanas\nacopiadas por minuto, dado el número de elecciones que debían ser computadas en la casilla",
       x = "\nFecha y hora de acopio",
       y = NULL,
       color = "\nNúmero de elecciones\ncomputadas en casilla",
       caption = nota_acopio) +
  tema +
  theme(legend.position = c(0.87, 0.2),
        legend.direction = "horizontal",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5,
                                   size = 14),
        strip.text = element_text(size = 20),
        panel.border = element_rect(color = "grey80", 
                                    size = 0.5, 
                                    fill = "transparent")) +
  ggsave("03_graficas/evolucion/acopio/evolucion_porcentaje_acumulado_actas_acopiadas_dado_numero_de_elecciones_en_casilla_y_ubicacion_casilla.png", width = 16.5, height = 9, dpi = 200)







## Evolución del porcentaje acumulado de actas de casillas acopiadas por minuto,dado el método de digitalizaión y el número de elecciones computadas en la casilla ----
faa <- 
  bd_prep_2021_aco %>%
  filter(digitalizacion != "Urna Electrónica") %>%
  mutate(id = 1) %>% 
  select(num_act, digitalizacion, fecha_hora_acopio, id) %>% 
  # Generar y juntar observaciones con valores de fecha_hora_acopio faltantes para "Móvil" hasta el último minuto que se acopiaron actas vía escáner
  bind_rows(tibble(digitalizacion = c(rep("Móvil", 978),
                                      rep("Móvil", 512),
                                      rep("Móvil", 621),
                                      rep("Móvil", 1157)),
                   num_act = c(rep(2, 978),
                               rep(3, 512),
                               rep(4, 621),
                               rep(5, 1157)),
                   # Fechas-horas 
                   fecha_hora_acopio = c(seq(from = as_datetime("2021-06-07 03:27:00"), to = as_datetime("2021-06-07 19:44:00"), by = "1 min"),
                                         seq(from = as_datetime("2021-06-07 11:13:00"), to = as_datetime("2021-06-07 19:44:00"), by = "1 min"),
                                         seq(from = as_datetime("2021-06-07 09:24:00"), to = as_datetime("2021-06-07 19:44:00"), by = "1 min"),
                                         seq(from = as_datetime("2021-06-07 00:27:20"), to = as_datetime("2021-06-07 19:44:00"), by = "1 min")),
                   id = 0)) %>% 
  arrange(num_act, digitalizacion, fecha_hora_acopio) %>%
  group_by(num_act, digitalizacion) %>% 
  mutate(acumuladas = cumsum(id), 
         acumuladas_por = ((acumuladas/max(acumuladas)))) %>% 
  ungroup() 


faa %>% 
  mutate(digitalizacion = ifelse(digitalizacion == "Móvil", "PREP casilla", digitalizacion),
         digitalizacion = fct_relevel(digitalizacion, "PREP casilla", "Escáner")) %>% 
  
  ggplot(aes(x = fecha_hora_acopio, 
             y = acumuladas_por, 
             color = as.factor(num_act))) +
  geom_line(size = 1.5) +
  facet_wrap(~ digitalizacion) +
  scale_x_datetime(date_breaks = "1 hours", 
                   date_labels = "%R",
                   expand = c(0, 0),
                   limits = c(as_datetime("2021-06-06 16:40:00"), 
                              as_datetime("2021-06-07 20:20:00"))) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = paleta_4,
                     guide = guide_legend(title.position = "top",
                                          keyheight = 0.4)) +
  labs(title = "PREP 2021 | Evolución del porcentaje acumulado de actas de casillas acopiadas por minuto,\ndado el método de digitalizaión y el número de elecciones computadas en la casilla",
       x = "\nFecha y hora de acopio",
       y = NULL,
       color = "\nNúmero de elecciones\ncomputadas en casilla",
       caption = nota_acopio) +
  tema +
  theme(plot.title = element_text(size = 28),
        legend.position = c(0.87, 0.2),
        legend.direction = "horizontal",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5,
                                   size = 14),
        strip.text = element_text(size = 20),
        panel.border = element_rect(color = "grey80", 
                                    size = 0.5, 
                                    fill = "transparent")) +
  ggsave("03_graficas/evolucion/acopio/evolucion_porcentaje_acumulado_actas_acopiadas_dado_numero_de_elecciones_en_casilla_y_digitalizacion.png", width = 16.5, height = 9, dpi = 200)

# Identificar a qué hora se acopió el 75% de las actas digitalizadas con PREP casilla, por número de elecciones por estado

faa %>% 
  filter(digitalizacion == "Móvil",
         acumuladas_por > 0.7498 & acumuladas_por < 0.7502) %>% 
  group_by(num_act) %>% 
  filter(acumuladas_por == max(acumuladas_por))

### Análisis de la evolución de la CAPTURA de actas PREP 2021 ----

## Eliminar observaciones con valores faltantes en fecha_hora_captura ----

# Contar núm. de obs. que cumplen con uno de estos criterios
bd_prep_2021 %>%
  summarise(valor_faltante = sum(is.na(fecha_hora_captura)))

# En total excluiré 4,629 rengloes; todos tienen valores faltantes.

bd_prep_2021_cap <- 
  bd_prep_2021 %>%
  filter(!is.na(fecha_hora_captura))

# Después de este paso el tibble tiene 159,037 renglones

# Generar texto que incluiré en la nota al pie de todas las gráficas en las que analice la evolución de la captura

## Generar textos para notas al pie ----
nota_cap <- "\nElaborada por @segasi / Fuente: INE, PREP 2021\nNota: La gráfica excluye 4,629 actas que no fueron capturadas."

nota_cap_digi <- "\nElaborada por @segasi / Fuente: INE, PREP 2021\nNota:La gráfica excluye 4,629 actas que no fueron capturadas y 96 que fueron digitalizadas mediante urna electrónica."

## Evolución del número y número acumulado de actas capturadas por minuto ----
bd_prep_2021_cap %>%
  count(fecha_hora_captura, 
        name = "Número") %>% 
  mutate(`Número acumulado` = cumsum(Número)) %>% 
  pivot_longer(-fecha_hora_captura) %>% 
  ggplot(aes(x = fecha_hora_captura, 
             y = value, 
             color = name)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 hours", 
                   date_labels = "%R",
                   expand = c(0, 0),
                   limits = c(as_datetime("2021-06-06 16:40:00"), 
                              as_datetime("2021-06-07 20:20:00"))) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = paleta_2,
                     guide = guide_legend(title.position = "top",
                                          keyheight = 0.4)) +
  facet_wrap(~ name, scales = "free_y") +
  labs(title = "PREP 2021 | Evolución del número y número acumulado de actas capturadas por minuto",
       x = "\nFecha y hora de captura",
       y = NULL,
       caption = nota_cap) +
  tema +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5,
                                   size = 14),
        strip.text = element_text(size = 20),
        panel.border = element_rect(color = "grey80", 
                                    size = 0.5, 
                                    fill = "transparent")) +
  ggsave("03_graficas/evolucion/captura/evolucion_numero_y_numero_acumulado_actas_capturadas_por_minuto.png", width = 16, height = 9, dpi = 200)



## Evolución del número y número acumulado de actas capturadas por minuto y medio de digitalización ----
bd_prep_2021_cap %>%
  filter(digitalizacion != "Urna Electrónica") %>%
  group_by(digitalizacion, fecha_hora_captura) %>% 
  summarise(Número = n()) %>% 
  ungroup() %>%  
  # Generar y juntar observaciones con valores de fecha_hora_captura faltantes para "Móvil" hasta el último minuto que se capturaron actas vía escáner
  bind_rows(tibble(digitalizacion = rep("Móvil", 495),
                   # Fechas-horas 
                   fecha_hora_captura = seq(from = as_datetime("2021-06-07 11:30:00"), to = as_datetime("2021-06-07 19:44:00"), by = "1 min"),
                   Número = 0)) %>% 
  arrange(digitalizacion, fecha_hora_captura) %>% 
  group_by(digitalizacion) %>% 
  mutate(`Número acumulado` = cumsum(Número)) %>% 
  ungroup() %>% 
  mutate(digitalizacion = ifelse(digitalizacion == "Móvil", "PREP casilla", digitalizacion),
         digitalizacion = fct_relevel(digitalizacion, "PREP casilla", "Escáner")) %>% 
  ggplot(aes(x = fecha_hora_captura, 
             y = `Número acumulado`, 
             color = digitalizacion)) +
  geom_line(size = 1.5) + 
  scale_x_datetime(date_breaks = "1 hours", 
                   date_labels = "%R",
                   expand = c(0, 0),
                   limits = c(as_datetime("2021-06-06 16:40:00"), 
                              as_datetime("2021-06-07 20:20:00"))) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = paleta_2,
                     guide = guide_legend(title.position = "top",
                                          keyheight = 0.4)) +
  labs(title = "PREP 2021 | Evolución del número acumulado de actas capturadas por minuto y medio\nde digitalización",
       x = "\nFecha y hora de captura",
       y = NULL,
       color = "Medio de digitalización",
       caption = nota_cap_digi) +
  tema +
  theme(legend.position = c(0.82, 0.2),
        legend.direction = "horizontal",
        legend.title = element_text(size = 18, hjust = 0),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggsave("03_graficas/evolucion/captura/evolucion_numero_acumulado_actas_capturadas_dado_medio_digitalizacion.png", width = 16, height = 9, dpi = 200)


## Evolución del porcentaje acumulado de actas capturadas por minuto, dado el número de elecciones que debían ser computadas en la casilla ----
bd_prep_2021_cap %>% 
  arrange(num_act, fecha_hora_captura) %>% 
  group_by(num_act) %>% 
  mutate(id = 1,
         acumuladas = cumsum(id), 
         acumuladas_por = ((acumuladas/max(acumuladas)))) %>% 
  ungroup() %>% 
  select(num_act, fecha_hora_captura, id, acumuladas, acumuladas_por) %>% 
  ggplot(aes(x = fecha_hora_captura, 
             y = acumuladas_por,
             color = as.factor(num_act))) +
  geom_line(size = 1.5) +
  scale_x_datetime(date_breaks = "1 hours", 
                   date_labels = "%R",
                   expand = c(0, 0),
                   limits = c(as_datetime("2021-06-06 16:40:00"), 
                              as_datetime("2021-06-07 20:20:00"))) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = paleta_4,
                     guide = guide_legend(title.position = "top",
                                          keyheight = 0.4)) +
  labs(title = "PREP 2021 | Evolución del porcentaje acumulado de actas capturadas por minuto, dado el\nnúmero de elecciones que debían ser computadas en la casilla",
       x = "\nFecha y hora de captura",
       y = NULL,
       color = "\nNúmero de elecciones\ncomputadas en casilla",
       caption = nota_cap) +
  tema +
  theme(legend.position = c(0.87, 0.2),
        legend.direction = "horizontal",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggsave("03_graficas/evolucion/captura/evolucion_porcentaje_acumulado_actas_capturadas_dado_numero_de_elecciones_en_casilla.png", width = 16, height = 9, dpi = 200)


## Evolución del porcentaje acumulado de actas de casillas urbanas y y no urbanas capturadas por minuto ----
bd_prep_2021_cap %>% 
  arrange(ubicacion_casilla, fecha_hora_captura) %>%
  group_by(ubicacion_casilla) %>% 
  mutate(id = 1,
         acumuladas = cumsum(id), 
         acumuladas_por = ((acumuladas/max(acumuladas)))) %>% 
  ungroup() %>% 
  ggplot(aes(x = fecha_hora_captura, 
             y = acumuladas_por,
             color = ubicacion_casilla)) +
  geom_line(size = 1.5) +
  scale_x_datetime(date_breaks = "1 hours", 
                   date_labels = "%R",
                   expand = c(0, 0),
                   limits = c(as_datetime("2021-06-06 16:40:00"), 
                              as_datetime("2021-06-07 20:20:00"))) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = paleta_2,
                     guide = guide_legend(title.position = "top",
                                          keyheight = 0.4)) +
  labs(title = "PREP 2021 | Evolución del porcentaje acumulado de actas de casillas urbanas y y no urbanas\ncapturadas por minuto",
       x = "\nFecha y hora de captura",
       y = NULL,
       color = "\nTipo de casilla",
       caption = nota_cap) +
  tema +
  theme(legend.position = c(0.87, 0.2),
        legend.direction = "horizontal",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggsave("03_graficas/evolucion/captura/evolucion_porcentaje_acumulado_actas_capturadas_dada_ubicacion_casilla.png", width = 16.5, height = 9, dpi = 200)

## Evolución del porcentaje acumulado de actas de casillas urbanas y no urbanas capturadas por minuto, dado el medio de digitalización del acta ----
foo <- 
  bd_prep_2021_cap %>%
  filter(digitalizacion != "Urna Electrónica") %>% 
  arrange(digitalizacion, ubicacion_casilla, fecha_hora_captura) %>%
  mutate(id = 1) %>%  
  select(digitalizacion, ubicacion_casilla, fecha_hora_captura, id) %>% 
  # Generar y juntar observaciones con valores de fecha_hora_captura faltantes para "Móvil" hasta el último minuto que se capturaron actas vía escáner
  bind_rows(tibble(digitalizacion = c(rep("Móvil", 781),
                                      rep("Móvil", 512)),
                   ubicacion_casilla = c(rep("Urbana", 781),
                                         rep("No Urbana", 512)),
                   # Fechas-horas 
                   fecha_hora_captura = c(seq(from = as_datetime("2021-06-07 06:44:00"), to = as_datetime("2021-06-07 19:44:00"), by = "1 min"),
                                          seq(from = as_datetime("2021-06-07 11:13:00"), to = as_datetime("2021-06-07 19:44:00"), by = "1 min")),
                   id = 0)) %>% 
  group_by(digitalizacion, ubicacion_casilla) %>% 
  mutate(acumuladas = cumsum(id),
         acumuladas_por = ((acumuladas/max(acumuladas)))) %>% 
  ungroup() %>%
  mutate(ubicacion_casilla = fct_relevel(ubicacion_casilla, "Urbana", "No Urbana"),
         digitalizacion = ifelse(digitalizacion == "Móvil", "PREP casilla", digitalizacion),
         digitalizacion = fct_relevel(digitalizacion, "PREP casilla", "Escáner")) 


foo %>% 
  ggplot(aes(x = fecha_hora_captura, 
             y = acumuladas_por, 
             color = as.factor(digitalizacion))) +
  geom_line(size = 1.5) +
  facet_wrap(~ ubicacion_casilla) +
  scale_x_datetime(date_breaks = "1 hours", 
                   date_labels = "%R",
                   expand = c(0, 0),
                   limits = c(as_datetime("2021-06-06 16:40:00"), 
                              as_datetime("2021-06-07 20:20:00"))) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = paleta_2,
                     guide = guide_legend(title.position = "top",
                                          keyheight = 0.4)) +
  labs(title = "PREP 2021 | Evolución del porcentaje acumulado de actas de casillas urbanas y no urbanas\ncapturadas por minuto, dado el medio de digitalización",
       x = "\nFecha y hora de captura",
       y = NULL,
       color = "\nMedio de digitalización",
       caption = nota_cap_digi) +
  tema +
  theme(legend.position = c(0.87, 0.2),
        legend.direction = "horizontal",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5,
                                   size = 14),
        strip.text = element_text(size = 20),
        panel.border = element_rect(color = "grey80", 
                                    size = 0.5, 
                                    fill = "transparent")) +
  ggsave("03_graficas/evolucion/captura/evolucion_porcentaje_acumulado_actas_capturadas_dado_numero_de_elecciones_en_casilla_y_medio_digitalizacion.png", width = 16.5, height = 9, dpi = 200)


## Evolución del número acumulado de actas de casillas urbanas y no urbanas capturadas por minuto, dado el medio de digitalización del acta ----
foo %>% 
  ggplot(aes(x = fecha_hora_captura, 
             y = acumuladas, 
             color = as.factor(digitalizacion))) +
  geom_line(size = 1.5) +
  facet_wrap(~ ubicacion_casilla) +
  scale_x_datetime(date_breaks = "1 hours", 
                   date_labels = "%R",
                   expand = c(0, 0),
                   limits = c(as_datetime("2021-06-06 16:40:00"), 
                              as_datetime("2021-06-07 20:20:00"))) +
  scale_y_continuous(labels = comma,
                     breaks = seq(0, 60000, 10000)) +
  scale_color_manual(values = paleta_2,
                     guide = guide_legend(title.position = "top",
                                          keyheight = 0.4)) +
  labs(title = "PREP 2021 | Evolución del número acumulado de actas de casillas urbanas y no urbanas\ncapturadas por minuto, dado el medio de digitalización",
       x = "\nFecha y hora de captura",
       y = NULL,
       color = "\nMedio de digitalización",
       caption = nota_cap_digi) +
  tema +
  theme(legend.position = c(0.87, 0.2),
        legend.direction = "horizontal",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5,
                                   size = 14),
        strip.text = element_text(size = 20),
        panel.border = element_rect(color = "grey80", 
                                    size = 0.5, 
                                    fill = "transparent")) +
  ggsave("03_graficas/evolucion/captura/evolucion_numero_acumulado_actas_capturadas_dado_numero_de_elecciones_en_casilla_y_medio_digitalizacion.png", width = 16.5, height = 9, dpi = 200)


## Evolución del porcentaje acumulado de actas de casillas urbanas y no urbanas capturadas por minuto, dado el número de elecciones que debían ser computadas en la casilla ----
bd_prep_2021_cap %>%
  arrange(num_act, ubicacion_casilla, fecha_hora_captura) %>%
  group_by(num_act, ubicacion_casilla) %>% 
  mutate(id = 1,
         acumuladas = cumsum(id), 
         acumuladas_por = ((acumuladas/max(acumuladas)))) %>% 
  ungroup() %>% 
  select(num_act, ubicacion_casilla, fecha_hora_captura, id, acumuladas, acumuladas_por) %>% 
  mutate(ubicacion_casilla = fct_relevel(ubicacion_casilla, "Urbana", "No Urbana")) %>% 
  ggplot(aes(x = fecha_hora_captura, 
             y = acumuladas_por, 
             color = as.factor(num_act))) +
  geom_line(size = 1.5) +
  facet_wrap(~ ubicacion_casilla) +
  scale_x_datetime(date_breaks = "1 hours", 
                   date_labels = "%R",
                   expand = c(0, 0),
                   limits = c(as_datetime("2021-06-06 16:40:00"), 
                              as_datetime("2021-06-07 20:20:00"))) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = paleta_4,
                     guide = guide_legend(title.position = "top",
                                          keyheight = 0.4)) +
  labs(title = "PREP 2021 | Evolución del porcentaje acumulado de actas de casillas urbanas y no urbanas\ncapturadas por minuto, dado el número de elecciones que debían ser computadas en la casilla",
       x = "\nFecha y hora de captura",
       y = NULL,
       color = "\nNúmero de elecciones\ncomputadas en casilla",
       caption = nota_cap) +
  tema +
  theme(legend.position = c(0.87, 0.2),
        legend.direction = "horizontal",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5,
                                   size = 14),
        strip.text = element_text(size = 20),
        panel.border = element_rect(color = "grey80", 
                                    size = 0.5, 
                                    fill = "transparent")) +
  ggsave("03_graficas/evolucion/captura/evolucion_porcentaje_acumulado_actas_capturadas_dado_numero_de_elecciones_en_casilla_y_ubicacion_casilla.png", width = 16.5, height = 9, dpi = 200)