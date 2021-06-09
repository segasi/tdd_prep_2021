### Paquetes ----
library(pacman)
p_load(av, cowplot, DBI, flexdashboard, gganimate, GGally, ggforce, ggmap, ggrepel, gifski, glue, gpclib, ggtext, gt, Hmisc, janitor, lubridate, odbc, openxlsx, RColorBrewer, rcartocolor, readxl, RPostgreSQL, scales, sf, shadowtext, tidyverse, treemapify, viridis, wesanderson, zoo)

### Setup general ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)
theme_set(theme_gray())

### Definir tema de gráficas ----
tema <-  
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = "white"), 
        text = element_text(family = "Roboto", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,20,0), family = "Roboto Black", color = "grey25"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family = "Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.caption.position = "plot",
        panel.grid = element_line(linetype = 3, color = "grey90"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family = "Roboto Black"),
        legend.text = element_text(size = 14, family = "Didact Gothic Regular"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family = "Didact Gothic Regular"),
        axis.text = element_text(size = 16, face = "bold", family = "Didact Gothic Regular"),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(color = "white", size = 14))

