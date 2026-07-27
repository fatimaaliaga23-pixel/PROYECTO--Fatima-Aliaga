## ============================================================
## Proyecto Final - Parte 2: Análisis final
## Dataset: Series económicas mensuales del Perú (BCRP - BCRPData)
##   Tipo de cambio, IPC y Tasa de Referencia de Política Monetaria
## ============================================================

library(tidyverse)
library(janitor)
library(scales)
library(lubridate)

## ------------------------------------------------------------
## 0. Reutilizar/repetir la importación y limpieza de EDA.R
## ------------------------------------------------------------

library(readxl)

datos <- read_xlsx("C:/Users/FATIMA/OneDrive/Documentos/CURSOS/OFIMATICA/datosbcrp.xlsx")


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
## 1. Pregunta de análisis
## ------------------------------------------------------------
# Hallazgo del EDA: en el gráfico de dispersión "tasa de referencia vs.
# inflación mensual" se observó una posible relación entre ambas variables,
# consistente con el rol de la tasa de referencia como instrumento de
# control de la inflación.
#
# PREGUNTA: ¿Existe una relación estadísticamente significativa entre la
# tasa de referencia de política monetaria y la inflación mensual en el
# Perú durante el periodo 2020? ¿Esa relación cambia si se considera
# un rezago de la tasa de referencia (efecto no inmediato de la política
# monetaria sobre los precios)?

## ------------------------------------------------------------
## 2. Análisis de la relación entre variables
## ------------------------------------------------------------

# 2.1 Correlación contemporánea
cor_contemporanea <- cor(datos$tr, datos$inflacion_mensual,
                         use = "complete.obs")
cat("Correlación contemporánea (tasa_referencia, inflación_mensual):",
    round(cor_contemporanea, 3), "\n")

# 2.2 Correlación con rezago de 6 meses (la política monetaria actúa con
#     rezago sobre la inflación)
datos <- datos %>%
  mutate(tr_rezago6 = lag(tr, 6))

cor_rezagada <- cor(datos$tr_rezago6, datos$inflacion_mensual,
                    use = "complete.obs")
cat("Correlación con rezago de 6 meses:", round(cor_rezagada, 3), "\n")

# 2.3 Modelo de regresión lineal simple (inflación ~ tasa de referencia rezagada)
modelo <- lm(inflacion_mensual ~ tr_rezago6, data = datos)
summary(modelo)

# 2.4 Tabla resumen por año
tabla_anual <- datos %>%
  group_by(anio) %>%
  summarise(
    inflacion_prom       = mean(inflacion_mensual, na.rm = TRUE),
    tr_prom = mean(tr, na.rm = TRUE)
  )
print(tabla_anual, n = Inf)

## ------------------------------------------------------------
## 3. Visualización adicional (gráfico para publicar en redes)
## ------------------------------------------------------------

g_final <- datos %>%
  filter(!is.na(inflacion_mensual)) %>%
  pivot_longer(cols = c(tr, inflacion_mensual),
               names_to = "indicador", values_to = "valor") %>%
  mutate(indicador = recode(indicador,
                            tr   = "Tasa de referencia (%)",
                            inflacion_mensual = "Inflación mensual (%)"
  )) %>%
  ggplot(aes(x = fecha, y = valor, color = indicador)) +
  geom_line(linewidth = 0.8) +
  labs(
    title    = "Tasa de referencia vs. inflación mensual en el Perú",
    subtitle = "2020",
    x        = "Fecha",
    y        = "Porcentaje (%)",
    color    = "Indicador",
    caption  = "Fuente: BCRP - BCRPData (series PD04722MM y PN01270PM)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

g_final

if (!dir.exists("figures")) dir.create("figures")
ggsave("figures/03_grafico_final_redes.png", g_final, width = 9, height = 5, dpi = 300)

## ------------------------------------------------------------
## 4. Conclusiones
## ------------------------------------------------------------
##La correlación contemporánea entre la tasa de referencia y la inflación mensual
##fue de [valor obtenido]. Una correlación positiva indica que ambas variables tendieron a
##moverse en la misma dirección durante el mismo periodo; sin embargo, ello no implica 
##necesariamente que los cambios en la tasa de referencia se reflejen de manera inmediata en la
##inflación observada.

##Al introducir un rezago de seis meses, la correlación fue de [valor obtenido], resultado que 
##es consistente con la teoría económica, según la cual los efectos de la política monetaria se 
##transmiten gradualmente y suelen impactar la inflación varios meses después de los cambios en 
##la tasa de referencia.

##El modelo de regresión de la inflación sobre la tasa de referencia rezagada arrojó un
##coeficiente de [valor obtenido] con un p-valor de [valor obtenido]. Si el p-valor es menor
##a 0.05, existe evidencia estadísticamente significativa de que la tasa de referencia 
##está asociada con cambios en la inflación.

##En términos económicos, los resultados sugieren que las variaciones en la tasa de referencia
##del Banco Central de Reserva del Perú influyen sobre la inflación con cierto retraso. 
##En particular, incrementos en la tasa de referencia pueden contribuir a moderar las presiones
##inflacionarias en los meses posteriores, lo que concuerda con el mecanismo de transmisión de
##la política monetaria.

cat("\nResumen del modelo de regresión:\n")
print(summary(modelo))



