### Cargar y procesar bases de datos de PREPs ----
source("02_codigo/01_cargar_procesar_bd_prep_2021.R") 
source("02_codigo/02_cargar_procesar_bd_preps_previos.R") 


### Preparar datos para poder comparar la evolución del número y % acumulado de actas capturadas y contabilizadas en los PREPs de las elecciones de diputaciones federales de 2006 a 2021 ----

# Calcular diferencia en días de las jornadas electorales de 2006, 2009, 2012, 2015, 2018 y la de 2021
ajuste_2006 <- ymd("2021-06-06") - ymd("2006-07-02") # 5453 días
ajuste_2009 <- ymd("2021-06-06") - ymd("2009-07-05") # 4354 días
ajuste_2012 <- ymd("2021-06-06") - ymd("2012-07-01") # 3262 días
ajuste_2015 <- ymd("2021-06-06") - ymd("2015-06-07") # 2191 días
ajuste_2018 <- ymd("2021-06-06") - ymd("2018-07-01") # 1071 días

# Generar tibbles con fechas-horas comunes 
bd_prep_2006_union <- bd_prep_2006 %>% select(hora_captura) %>% mutate(año = 2006) %>% arrange(hora_captura) %>% mutate(hora_captura_comun = hora_captura + days(ajuste_2006))

bd_prep_2009_union <- bd_prep_2009 %>% select(hora_captura) %>% mutate(año = 2009) %>% arrange(hora_captura) %>% mutate(hora_captura_comun = hora_captura + days(ajuste_2009))

bd_prep_2012_union <- bd_prep_2012 %>% select(hora_captura) %>% mutate(año = 2012) %>% arrange(hora_captura) %>% mutate(hora_captura_comun = hora_captura + days(ajuste_2012))

bd_prep_2015_union <- bd_prep_2015 %>% select(hora_captura) %>% mutate(año = 2015) %>% arrange(hora_captura) %>% mutate(hora_captura_comun = hora_captura + days(ajuste_2015))

bd_prep_2018_union <- bd_prep_2018 %>% select(hora_captura = fecha_hora_captura) %>% mutate(año = 2018) %>% arrange(hora_captura) %>% mutate(hora_captura_comun = hora_captura + days(ajuste_2018)) 

bd_prep_2021_union <- bd_prep_2021 %>% select(hora_captura = fecha_hora_captura) %>% mutate(año = 2021) %>% arrange(hora_captura) %>% mutate(hora_captura_comun = hora_captura)

# Unir tibbles
bd_prep_todos <- 
  bind_rows(bd_prep_2006_union, bd_prep_2009_union, 
            bd_prep_2012_union, bd_prep_2015_union, 
            bd_prep_2018_union, bd_prep_2021_union)

### Calcular número y % acumulado de actas capturadas y contabilizadas en los PREPs ----
datos <- 
  bd_prep_todos %>% 
  arrange(año, hora_captura) %>% 
  group_by(año) %>% 
  mutate(id = 1,
         acumuladas = cumsum(id), 
         acumuladas_por = acumuladas/max(acumuladas)) %>% 
  ungroup() 

### Gráfica del % acumulado de actas capturadas y contabilizadas en los PREPs de 2006 a 2021 ----
datos %>%     
  ggplot(aes(x = hora_captura_comun, 
             y = acumuladas_por, 
             group = año, 
             color = factor(año))) +
  annotate(geom = "rect",  xmin = as_datetime("2021-06-06 17:00:00"), xmax = as_datetime("2021-06-06 23:59:59"), ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.5) +
  geom_line(size = 1.5) +
  annotate(geom = "text", x = as_datetime("2021-06-06 20:30:00"), y = 0.95, label = "Día de la\nelección", fontface = "bold", color = "white", hjust = 0.5, size = 7, family = "Trebuchet MS Bold") +
  annotate(geom = "text", x = as_datetime("2021-06-07 02:00:00"), y = 0.95, label = "Día posterior\na la elección", fontface = "bold", color = "grey50", hjust = 0.5, size = 7, family = "Trebuchet MS Bold") +
  annotate(geom = "text", x = as_datetime("2021-06-07 07:00:00"), y = 0.92, label = "2006", fontface = "bold", color = "grey60", hjust = 0.5, size = 6, family = "Trebuchet MS Bold") +
  annotate(geom = "text", x = as_datetime("2021-06-06 23:00:00"), y = 0.65, label = "2009", fontface = "bold", color = "white", hjust = 0.5, size = 6, family = "Trebuchet MS Bold") +
  annotate(geom = "text", x = as_datetime("2021-06-07 20:35:00"), y = 0.97, label = "2012", fontface = "bold", color = "grey60", hjust = 0.5, size = 6, family = "Trebuchet MS Bold") +
  annotate(geom = "text", x = as_datetime("2021-06-07 20:35:00"), y = 0.91, label = "2015", fontface = "bold", color = "grey60", hjust = 0.5, size = 6, family = "Trebuchet MS Bold") +
  annotate(geom = "text", x = as_datetime("2021-06-07 20:35:00"), y = 0.75, label = "2018", fontface = "bold", color = "grey60", hjust = 0.5, size = 6, family = "Trebuchet MS Bold") +
  annotate(geom = "text", x = as_datetime("2021-06-07 04:30:00"), y = 0.8, label = "2021", fontface = "bold", color = "#f07167", hjust = 0.5, size = 8, family = "Trebuchet MS Bold") +
  scale_color_manual(values = c("grey10", "grey30", "grey50", "grey70", "grey90", "#f07167"),
    guide = guide_legend(title.position = "top", keyheight = 0.4, default.unit = "inch")) +
  scale_x_datetime(breaks = date_breaks("1 hour"), 
                   labels = date_format("%H:%M"),
                   limits = c(as_datetime("2021-06-06 16:40:00"), 
                              as_datetime("2021-06-07 21:20:00")),
                   expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), 
                     breaks = seq(0, 1, 0.1), 
                     labels = percent_format(accuracy = 1)) +
  labs(title = "PREP 2021 | Porcentaje acumulado de actas capturadas y contabilizadas en los PREPs\nde la elección de diputaciones federales de 2006 a 2021",
       x = "\nHora de captura",
       y = NULL,
       caption = "Elaborado por @segasi / Fuente: INE, Corte: 20:00 p.m. del 7 de junio de 2021.",
       color = "Año") + 
  tema +
  theme(plot.title = element_text(face = "bold"), 
        legend.position = "none", #c(.85, .2),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.direction = "horizontal",
        legend.title.align = 0,
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggsave("03_graficas/comparacion/XX_comparacion_evolucion_porcentaje_acumulado_actas_capturadas_prep_dip_fed_2021_vs_previos.png", width = 16, height = 10, dpi = 200)
