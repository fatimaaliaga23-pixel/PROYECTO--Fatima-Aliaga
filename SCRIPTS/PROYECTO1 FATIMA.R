## ============================================================
## Proyecto 1: Analisis exploratorio del Tipo de cambio interbancario en el año 2020
## ESTUDIANTE : FATIMA ALIAGA CARBAJAL
## Dataset: Series económicas mensuales del Perú
##   - Tipo de cambio interbancario promedio (S/ por US$)
##   - Índice de Precios al Consumidor (IPC, Dic.2009=100)
##   - Tasa de Referencia de la Política Monetaria (%)
## Fuente: Banco Central de Reserva del Perú (BCRP) 
## ============================================================

## ------------------------------------------------------------
## Librerías
## ------------------------------------------------------------
paquetes <- c("tidyverse", "janitor", "skimr", "scales", "patchwork", "lubridate")
instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar) > 0) install.packages(instalar)

library(tidyverse)
library(janitor)
library(skimr)
library(scales)
library(patchwork)
library(lubridate)

## ------------------------------------------------------------
## Contexto del conjunto de datos
## ------------------------------------------------------------
# Institución: Banco Central de Reserva del Perú (BCRP), a través de su
#   API pública de series estadísticas (BCRPData).
# Objetivo/temática: monitorear la evolución mensual de tres indicadores
#   macroeconómicos clave de la economía peruana entre 2015 y 2025:
#   el tipo de cambio, el nivel de precios (inflación) y la tasa de
#   interés de referencia que fija el BCRP como instrumento de política
#   monetaria.
# Variables principales que se analizarán:
#   - fecha             (periodo mensual)
#   - tipo_cambio       (S/ por US$, interbancario promedio - PN01207PM)
#   - ipc               (Índice de Precios al Consumidor, Dic.2009=100 - PN01270PM)
#   - tasa_referencia   (Tasa de referencia de política monetaria, % - PD04722MM)
#   - inflacion_mensual (variable creada: variación % mensual del IPC)

## ------------------------------------------------------------
## 2. Importación de datos
## ------------------------------------------------------------

library(readxl)

datos <- read_xlsx("C:/Users/FATIMA/OneDrive/Documentos/CURSOS/OFIMATICA/datosbcrp.xlsx")




## ------------------------------------------------------------
## 3. Limpieza y preparación
## ------------------------------------------------------------

# 3.1 Estandarizar nombres de columnas
datos <- datos %>%
  clean_names()

names(datos)  # confirme los nombres reales (p.ej. fecha, pn01207pm, pn01270pm, pd04722mm)

# 3.2 Renombrar variables clave a nombres claros de trabajo.
#     AJUSTE el lado derecho según lo que arrojó names(datos).
datos <- datos %>%
  rename(
    fecha = fecha,
    tc = tcip,
    ipc = ipc,
    tr = trpm)

head(datos$fecha, 12)

library(dplyr)
library(stringr)
library(lubridate)

datos <- datos %>%
  mutate(
    mes_abrev = str_sub(fecha, 1, 3),
    anio = paste0("20", str_sub(fecha, 4, 5)),
    mes_num = meses_es[mes_abrev],
    fecha = ymd(paste(anio, mes_num, "01", sep = "-")),
    tc = as.numeric(tc),
    ipc = as.numeric(ipc),
    tr = as.numeric(tr)
  ) %>%
  select(-mes_abrev, -anio, -mes_num) %>%
  arrange(fecha)

head(datos$fecha, 12)

# 3.4 Crear nueva variable: inflación mensual (variación % del IPC)
datos <- datos %>%
  mutate(inflacion_mensual = (ipc / lag(ipc) - 1) * 100)

# 3.5 Filtrar observaciones incompletas
datos <- datos %>%
  filter(!is.na(tc), !is.na(ipc), !is.na(tr))

# 3.6 Variable de agrupación: año
datos <- datos %>%
  mutate(anio = year(fecha))

## ------------------------------------------------------------
## 4. Estadísticas descriptivas
## ------------------------------------------------------------

# Resumen general
skim(datos %>% select(tc, ipc, tr, inflacion_mensual))

# Promedios anuales
datos %>%
  group_by(anio) %>%
  summarise(
    tc_prom     = mean(tr, na.rm = TRUE),
    inflacion_prom       = mean(inflacion_mensual, na.rm = TRUE),
    tr_prom = mean(tr, na.rm = TRUE)
  ) %>%
  print(n = Inf)

# Correlación entre variables numéricas
datos %>%
  select(tc, ipc, tr, inflacion_mensual) %>%
  cor(use = "complete.obs") %>%
  round(2)

## ------------------------------------------------------------
## 5. Visualización de datos (ggplot2)
## ------------------------------------------------------------

# Gráfico 1: Evolución del tipo de cambio en el tiempo
g1 <- datos %>%
  ggplot(aes(x = fecha, y = tc)) +
  geom_line(color = "#2c3e50", linewidth = 0.8) +
  labs(
    title    = "Evolución del tipo de cambio interbancario (S/ por US$)",
    subtitle = "Perú, 2020",
    x        = "Fecha",
    y        = "Tipo de cambio (S/ por US$)",
    caption  = "Fuente: BCRP - BCRPData (serie PN01207PM)"
  ) +
  theme_minimal(base_size = 12)

g1

# Gráfico 2: 
datos_indexado <- datos %>%
  arrange(fecha) %>%
  mutate(
    tc_idx = tc / first(tc) * 100,
    ipc_idx = ipc / first(ipc) * 100,
    tr_idx = tr / first(tr) * 100
  ) %>%
  select(fecha, tc_idx, ipc_idx, tr_idx) %>%
  pivot_longer(-fecha, names_to = "variable", values_to = "indice") %>%
  mutate(variable = recode(variable,
                           tc_idx  = "Tipo de cambio",
                           ipc_idx = "IPC",
                           tr_idx  = "Tasa de referencia"
  ))

g2 <- datos_indexado %>%
  ggplot(aes(x = fecha, y = indice, color = variable)) +
  geom_line(linewidth = 0.8) +
  labs(
    title    = "Trayectoria comparada de los indicadores macroeconómicos",
    subtitle = paste0("Índice base 100 = ", fecha_min),
    x        = "Fecha",
    y        = "Índice (base 100)",
    color    = "Indicador",
    caption  = "Fuente: BCRP - BCRPData. Elaboración propia."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

g2


# Gráfico 3: Relación entre tasa de referencia e inflación mensual
g3 <- datos %>%
  filter(!is.na(inflacion_mensual)) %>%
  ggplot(aes(x = tr, y = inflacion_mensual)) +
  geom_point(alpha = 0.6, color = "#c0392b") +
  geom_smooth(method = "lm", se = TRUE, color = "#2c3e50") +
  labs(
    title    = "Relación entre la tasa de referencia y la inflación mensual",
    subtitle = "Perú, 2020",
    x        =  "Tasa de referencia de política monetaria (%)",
    y        = "Inflación mensual (variación % del IPC)",
    caption  = "Fuente: BCRP - BCRPData (series PD04722MM y PN01270PM)"
  ) +
  theme_minimal(base_size = 12)

g3

# Gráfico 4
g4 <- datos %>%
  filter(!is.na(inflacion_mensual)) %>%
  ggplot(aes(x = inflacion_mensual)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "#2980b9", color = "white", alpha = 0.8) +
  geom_density(color = "#c0392b", linewidth = 0.9) +
  geom_vline(xintercept = infl_prom_gen, linetype = "dashed", color = "#2c3e50") +
  labs(
    title    = "Distribución de la inflación mensual",
    subtitle = paste0("Perú, ", fecha_min, " - ", fecha_max, " (línea punteada = media)"),
    x        = "Inflación mensual (%)",
    y        = "Densidad",
    caption  = "Fuente: BCRP - BCRPData (serie PN01270PM). Elaboración propia."
  ) +
  theme_minimal(base_size = 12)

g4


# Grafico 5
g5 <- datos %>%
  ggplot(aes(x = factor(anio), y = tc)) +
  geom_boxplot(fill = "#7fb3d5", color = "#2c3e50", alpha = 0.8) +
  labs(
    title    = "Dispersión del tipo de cambio por año",
    subtitle = "Perú - Diagrama de cajas",
    x        = "Año",
    y        = "Tipo de cambio (S/ por US$)",
    caption  = "Fuente: BCRP - BCRPData (serie PN01207PM). Elaboración propia."
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g5
# Guardar gráficos individuales
if (!dir.exists("figures")) dir.create("figures")
ggsave("figures/01_tc_tiempo.png",       g1, width = 8, height = 5, dpi = 300)
ggsave("figures/02_tr_inflacion.png", g2, width = 8, height = 5, dpi = 300)

# Collage con los gráficos del EDA

library(patchwork)

# Crear el collage
collage <- (g1 | g3) / (g4 | g5) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Análisis Exploratorio de Datos (EDA)",
    subtitle = paste0("Indicadores Macroeconómicos del Perú (BCRP, ", fecha_min, " - ", fecha_max, ")"),
    caption = "Fuente: Elaboración propia con datos del BCRP",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 13, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 1)
    )
  ) &
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    plot.margin = margin(8, 8, 8, 8)
  )

collage
