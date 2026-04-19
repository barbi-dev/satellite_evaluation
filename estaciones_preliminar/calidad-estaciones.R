# =========================================================
# SCRIPT 2: DIAGNÓSTICO DE CALIDAD Y COBERTURA
# Base: temperatura_media_diaria_lista.csv

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(purrr)

# =========================================================
# 2) PARÁMETROS
# =========================================================
archivo <- "tmp_media_diaria_lista.csv"

fecha_inicio <- as.Date("2005-01-01")
fecha_fin    <- as.Date("2025-12-31")

# Umbral sugerido de cobertura anual para considerar un año como aceptable
umbral_cobertura_anual <- 80

# Umbral sugerido de cobertura mensual
umbral_cobertura_mensual <- 80

# Número mínimo de días consecutivos faltantes para reportar un hueco largo
umbral_hueco_largo <- 7

# =========================================================
# 3) LEER BASE FINAL
# =========================================================
df <- read_csv(
  archivo,
  show_col_types = FALSE
)

# Si el CSV trae una primera columna índice tipo X1, ...1, etc., la eliminamos
if (names(df)[1] %in% c("X1", "...1", "X", "ï..1")) {
  df <- df %>% select(-1)
}

# Verificación básica
print(names(df))
str(df)

# =========================================================
# 4) ASEGURAR TIPOS Y FILTRAR PERIODO DE ESTUDIO
# =========================================================
df <- df %>%
  mutate(
    fecha = as.Date(fecha),
    estacion = as.character(estacion),
    temp_media_diaria = as.numeric(temp_media_diaria),
    n_horas_validas = as.integer(n_horas_validas)
  ) %>%
  filter(fecha >= fecha_inicio, fecha <= fecha_fin)

cat("\nRango temporal efectivo en la base filtrada:\n")
print(range(df$fecha, na.rm = TRUE))

cat("\nEstaciones incluidas:\n")
print(sort(unique(df$estacion)))

# =========================================================
# 5) CREAR CALENDARIO COMPLETO POR ESTACIÓN
# =========================================================
# Esto es clave: necesitamos saber no solo los datos presentes,
# sino también los días ausentes en el periodo 2005-2025.

calendario <- expand_grid(
  estacion = sort(unique(df$estacion)),
  fecha = seq.Date(fecha_inicio, fecha_fin, by = "day")
)

df_full <- calendario %>%
  left_join(df, by = c("estacion", "fecha")) %>%
  mutate(
    anio = year(fecha),
    mes = month(fecha),
    nombre_mes = month(fecha, label = TRUE, abbr = FALSE),
    dato_disponible = !is.na(temp_media_diaria)
  )

# =========================================================
# 6) COBERTURA GLOBAL POR ESTACIÓN
# =========================================================
resumen_global <- df_full %>%
  group_by(estacion) %>%
  summarise(
    dias_esperados = n(),
    dias_validos = sum(dato_disponible),
    dias_faltantes = sum(!dato_disponible),
    cobertura_pct = 100 * dias_validos / dias_esperados,
    fecha_min_obs = min(fecha[dato_disponible], na.rm = TRUE),
    fecha_max_obs = max(fecha[dato_disponible], na.rm = TRUE),
    media_temp = mean(temp_media_diaria, na.rm = TRUE),
    sd_temp = sd(temp_media_diaria, na.rm = TRUE),
    min_temp = min(temp_media_diaria, na.rm = TRUE),
    p25_temp = quantile(temp_media_diaria, 0.25, na.rm = TRUE),
    mediana_temp = median(temp_media_diaria, na.rm = TRUE),
    p75_temp = quantile(temp_media_diaria, 0.75, na.rm = TRUE),
    max_temp = max(temp_media_diaria, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(cobertura_pct))

cat("\nResumen global por estación:\n")
print(resumen_global)

# =========================================================
# 7) COBERTURA ANUAL POR ESTACIÓN
# =========================================================
cobertura_anual <- df_full %>%
  group_by(estacion, anio) %>%
  summarise(
    dias_esperados = n(),
    dias_validos = sum(dato_disponible),
    dias_faltantes = sum(!dato_disponible),
    cobertura_pct = 100 * dias_validos / dias_esperados,
    media_temp = mean(temp_media_diaria, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(estacion, anio)

cat("\nCobertura anual:\n")
print(head(cobertura_anual, 20))

# Anos problematicos por debajo del umbral
anios_problematicos <- cobertura_anual %>%
  filter(cobertura_pct < umbral_cobertura_anual) %>%
  arrange(estacion, anio)

cat("\nAños con cobertura anual menor a ", umbral_cobertura_anual, "%:\n", sep = "")
print(anios_problematicos)

# =========================================================
# 8) COBERTURA MENSUAL POR ESTACIÓN
# =========================================================
cobertura_mensual <- df_full %>%
  group_by(estacion, anio, mes, nombre_mes) %>%
  summarise(
    dias_esperados = n(),
    dias_validos = sum(dato_disponible),
    dias_faltantes = sum(!dato_disponible),
    cobertura_pct = 100 * dias_validos / dias_esperados,
    media_temp = mean(temp_media_diaria, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(estacion, anio, mes)

cat("\nCobertura mensual:\n")
print(head(cobertura_mensual, 20))

meses_problematicos <- cobertura_mensual %>%
  filter(cobertura_pct < umbral_cobertura_mensual) %>%
  arrange(estacion, anio, mes)

cat("\nMeses con cobertura mensual menor a ", umbral_cobertura_mensual, "%:\n", sep = "")
print(head(meses_problematicos, 50))

# =========================================================
# 9) DETECCIÓN DE HUECOS LARGOS
# =========================================================
# Identificamos secuencias consecutivas de días faltantes por estación

huecos_largos <- df_full %>%
  arrange(estacion, fecha) %>%
  group_by(estacion) %>%
  mutate(
    faltante = !dato_disponible,
    cambio_grupo = faltante != lag(faltante, default = first(faltante)),
    grupo = cumsum(cambio_grupo)
  ) %>%
  group_by(estacion, grupo, faltante) %>%
  summarise(
    fecha_inicio = min(fecha),
    fecha_fin = max(fecha),
    longitud_dias = n(),
    .groups = "drop"
  ) %>%
  filter(faltante == TRUE, longitud_dias >= umbral_hueco_largo) %>%
  arrange(estacion, desc(longitud_dias))

cat("\nHuecos largos (>= ", umbral_hueco_largo, " días):\n", sep = "")
print(huecos_largos)

# =========================================================
# 10) ESTADÍSTICA DESCRIPTIVA ANUAL POR ESTACIÓN
# =========================================================
estadistica_anual <- df_full %>%
  filter(dato_disponible) %>%
  group_by(estacion, anio) %>%
  summarise(
    n = n(),
    media = mean(temp_media_diaria, na.rm = TRUE),
    sd = sd(temp_media_diaria, na.rm = TRUE),
    minimo = min(temp_media_diaria, na.rm = TRUE),
    q1 = quantile(temp_media_diaria, 0.25, na.rm = TRUE),
    mediana = median(temp_media_diaria, na.rm = TRUE),
    q3 = quantile(temp_media_diaria, 0.75, na.rm = TRUE),
    maximo = max(temp_media_diaria, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(estacion, anio)

cat("\nEstadística anual por estación:\n")
print(head(estadistica_anual, 20))

# =========================================================
# 11) BASE FINAL DIARIA COMPLETA PARA ANÁLISIS
# =========================================================
# Esta base conserva el calendario completo del periodo.
# Es muy útil para cruzar con satélite y para estudiar faltantes.

write_csv(df_full, "base_diaria_completa_2005_2025.csv")

# Esta base conserva solo los datos observados disponibles
df_observada <- df_full %>%
  filter(dato_disponible)

write_csv(df_observada, "base_diaria_buena_2005_2025.csv")

# =========================================================
# 12) EXPORTAR TABLAS
# =========================================================
write_csv(resumen_global, "resumen_global_estaciones_2005_2025.csv")
write_csv(cobertura_anual, "cobertura_anual_estaciones_2005_2025.csv")
write_csv(cobertura_mensual, "cobertura_mensual_estaciones_2005_2025.csv")
write_csv(anios_problematicos, "anios_problematicos_cobertura.csv")
write_csv(meses_problematicos, "meses_problematicos_cobertura.csv")
write_csv(huecos_largos, "huecos_largos_estaciones.csv")
write_csv(estadistica_anual, "estadistica_anual_estaciones.csv")

# =========================================================
# 13) GRÁFICOS
# =========================================================

# -------------------------
# 13.1 Serie temporal diaria
# -------------------------
g_serie <- ggplot(df_observada, aes(x = fecha, y = temp_media_diaria)) +
  geom_line(linewidth = 0.2) +
  facet_wrap(~ estacion, scales = "free_y", ncol = 2) +
  labs(
    title = "Serie temporal diaria de temperatura media",
    subtitle = "Periodo 2005-2025",
    x = "Fecha",
    y = "Temperatura media diaria (°C)"
  ) +
  theme_minimal()

ggsave("grafico_serie_temporal_diaria.png", g_serie, width = 12, height = 10, dpi = 300)

# -------------------------
# 13.2 Cobertura anual
# -------------------------
g_cobertura_anual <- ggplot(cobertura_anual, aes(x = anio, y = cobertura_pct, color = estacion)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = umbral_cobertura_anual, linetype = "dashed") +
  labs(
    title = "Cobertura anual por estación",
    x = "Año",
    y = "Cobertura (%)",
    color = "Estación"
  ) +
  theme_minimal()

ggsave("grafico_cobertura_anual.png", g_cobertura_anual, width = 12, height = 6, dpi = 300)

# -------------------------
# 13.3 Heatmap de cobertura mensual
# -------------------------
g_heatmap <- ggplot(cobertura_mensual, aes(x = mes, y = anio, fill = cobertura_pct)) +
  geom_tile(color = "white") +
  facet_wrap(~ estacion, ncol = 2) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title = "Cobertura mensual por estación",
    x = "Mes",
    y = "Año",
    fill = "Cobertura (%)"
  ) +
  theme_minimal()

ggsave("grafico_heatmap_cobertura_mensual.png", g_heatmap, width = 12, height = 10, dpi = 300)

# -------------------------
# 13.4 Boxplot por estación
# -------------------------
g_box <- ggplot(df_observada, aes(x = estacion, y = temp_media_diaria)) +
  geom_boxplot() +
  labs(
    title = "Distribución de temperatura media diaria por estación",
    x = "Estación",
    y = "Temperatura media diaria (°C)"
  ) +
  theme_minimal()

ggsave("grafico_boxplot_estaciones.png", g_box, width = 10, height = 6, dpi = 300)

# -------------------------
# 13.5 Cobertura global por estación
# -------------------------
g_global <- ggplot(resumen_global, aes(x = reorder(estacion, cobertura_pct), y = cobertura_pct)) +
  geom_col() +
  geom_hline(yintercept = 100, linetype = "solid") +
  labs(
    title = "Cobertura global por estación (2005-2025)",
    x = "Estación",
    y = "Cobertura (%)"
  ) +
  coord_flip() +
  theme_minimal()

ggsave("grafico_cobertura_global_estaciones.png", g_global, width = 8, height = 5, dpi = 300)

cat("\nScript completado. Archivos exportados correctamente.\n")
