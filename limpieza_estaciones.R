# =========================================================
# 1) LIBRERÍAS
# =========================================================
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# =========================================================
# 2) PARÁMETROS EDITABLES
# =========================================================

# Ruta del archivo
archivo <- "tmp_remmaq.xlsx"

# Umbral máximo permitido de faltantes por estación (%)
umbral_faltantes <- 20

# Nombre de la columna de fecha/hora en el archivo
# Ajusta este nombre si en tu archivo aparece distinto
col_fecha <- "fecha_hora"

# Diferencia horaria UTC -> Ecuador continental
offset_horas <- -5

# =========================================================
# 3) LECTURA DEL ARCHIVO
# =========================================================
# Nota:
# - Si tu archivo usa coma decimal, luego adaptamos.
# - Si tiene una primera columna índice, la eliminamos.

df_raw <- read_excel(
  archivo,
  na = c("", "NA", "NaN", "null", "NULL", " ")
)

# Ver estructura inicial
glimpse(df_raw)

# =========================================================
# 5) CONVERSIÓN DE FECHA/HORA
# =========================================================
# Ejemplo esperado en tu archivo:
# "2022-11-20 06:00:00 UTC"

df_raw <- df_raw %>%
  mutate(
    !!col_fecha := ymd_hms(.data[[col_fecha]], tz = "UTC")
  )

# Verificar si hubo problemas
if (any(is.na(df_raw[[col_fecha]]))) {
  warning("Hay fechas que no pudieron convertirse correctamente.")
}

# =========================================================
# 6) LIMPIEZA DE VALORES EN COLUMNAS DE ESTACIONES
# =========================================================
# todas las columnas de estaciones a numéricas.

columnas_estaciones <- setdiff(names(df_raw), col_fecha)

df_clean <- df_raw %>%
  mutate(
    across(
      all_of(columnas_estaciones),
      ~ str_replace_all(as.character(.x), ",", ".")
    )
  ) %>%
  mutate(
    across(
      all_of(columnas_estaciones),
      ~ as.numeric(.x)
    )
  )

# =========================================================
# 7) DETECCIÓN DE NA Y RESUMEN DE FALTANTES POR ESTACIÓN
# =========================================================

resumen_faltantes <- tibble(
  estacion = columnas_estaciones,
  n_total = sapply(df_clean[columnas_estaciones], length),
  n_na = sapply(df_clean[columnas_estaciones], function(x) sum(is.na(x))),
  pct_na = sapply(df_clean[columnas_estaciones], function(x) mean(is.na(x)) * 100)
) %>%
  arrange(desc(pct_na))

print(resumen_faltantes)

# =========================================================
# 8) FILTRADO AUTOMÁTICO DE ESTACIONES
# =========================================================

estaciones_validas <- resumen_faltantes %>%
  filter(pct_na <= umbral_faltantes) %>%
  pull(estacion)

estaciones_excluidas <- resumen_faltantes %>%
  filter(pct_na > umbral_faltantes) %>%
  pull(estacion)

cat("\nEstaciones válidas:\n")
print(estaciones_validas)

cat("\nEstaciones excluidas por superar el umbral de", umbral_faltantes, "%:\n")
print(estaciones_excluidas)

# Base conservando solo fecha + estaciones válidas
df_filtrado <- df_clean %>%
  select(all_of(c(col_fecha, estaciones_validas)))

# =========================================================
# 9) PASAR A FORMATO LARGO
# =========================================================

df_long <- df_filtrado %>%
  pivot_longer(
    cols = -all_of(col_fecha),
    names_to = "estacion",
    values_to = "temperatura"
  )

# =========================================================
# 10) AJUSTE DE ZONA HORARIA
# =========================================================
# movemos la hora para que el "día" quede correctamente asignado.

df_long <- df_long %>%
  mutate(
    fecha_utc = .data[[col_fecha]],
    fecha_local = fecha_utc + hours(offset_horas),
    fecha = as_date(fecha_local)
  ) %>%
  select(estacion, fecha_utc, fecha_local, fecha, temperatura)

# =========================================================
# 11) DATASET HORARIO LISTO PARA ANÁLISIS
# =========================================================
# Aquí aún no eliminamos todas las filas globalmente:
# solo conservamos observaciones no faltantes por estación.

df_horario_listo <- df_long %>%
  filter(!is.na(temperatura)) %>%
  arrange(estacion, fecha_local)

# Vista rápida
print(head(df_horario_listo, 10))

# =========================================================
# 12) AGREGACIÓN A TEMPERATURA MEDIA DIARIA
# =========================================================
# Para cada estación y día calculamos la media diaria.
# También guardamos cuántos registros horarios válidos aportaron a esa media.

df_diario <- df_horario_listo %>%
  group_by(estacion, fecha) %>%
  summarise(
    temp_media_diaria = mean(temperatura, na.rm = TRUE),
    n_horas_validas = sum(!is.na(temperatura)),
    .groups = "drop"
  ) %>%
  arrange(estacion, fecha)

print(head(df_diario, 10))

# =========================================================
# 13) CONTROL DE COBERTURA DIARIA
# =========================================================
# Puedes exigir una cobertura mínima de horas por día.
# Por ejemplo: aceptar solo días con al menos 18 horas válidas.

min_horas_por_dia <- 18

df_diario_final <- df_diario %>%
  filter(n_horas_validas >= min_horas_por_dia)

print(head(df_diario_final, 10))

# =========================================================
# 14) RESUMEN FINAL POR ESTACIÓN
# =========================================================

resumen_final <- df_diario_final %>%
  group_by(estacion) %>%
  summarise(
    n_dias_validos = n(),
    fecha_min = min(fecha),
    fecha_max = max(fecha),
    .groups = "drop"
  ) %>%
  arrange(desc(n_dias_validos))

print(resumen_final)

# =========================================================
# 15) EXPORTAR RESULTADOS
# =========================================================
library(readr)

write_csv(resumen_faltantes, "resumen_faltantes_estaciones.csv")
write_csv(df_horario_listo, "tmp_horaria_limpia.csv")
write.csv(df_diario_final, "tmp_media_diaria_lista.csv")
write_csv(resumen_final, "resumen_final_estaciones_validas.csv")