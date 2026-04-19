# =========================================================
# 02_read_observed.R
# Lectura y limpieza de datos observados
# =========================================================

source("scripts/01_setup.R")

obs_aux <- read_csv(path_obs, show_col_types = FALSE)

# Eliminar posible columna índice
if (names(obs)[1] %in% c("X1", "...1", "X", "ï..1")) {
  obs <- obs %>% select(-1)
}

obs <- obs %>%
  mutate(
    fecha = as.Date(fecha),
    estacion = as.character(estacion),
    temp_media_diaria = as.numeric(temp_media_diaria),
    n_horas_validas = as.integer(n_horas_validas)
  ) %>%
  filter(
    fecha >= fecha_inicio,
    fecha <= fecha_fin,
    estacion %in% estaciones$estacion
  ) %>%
  rename(obs_temp = temp_media_diaria)

print(obs)
write_csv(obs, file.path(path_tables, "observado_limpio_2006_2024.csv"))
