# =========================================================
# 03_read_era5.R
# Lectura y limpieza de ERA5
# =========================================================

source("scripts/01_setup.R")

# ---------------------------------------------------------
# 1. Leer archivos ERA5
# ---------------------------------------------------------
era5_025 <- read_csv(path_era5_025, show_col_types = FALSE)
era5_000 <- read_csv(path_era5_000, show_col_types = FALSE)

# ---------------------------------------------------------
# 2. Función de limpieza
# ---------------------------------------------------------
clean_era5 <- function(df, grid_name) {
  df %>%
    mutate(
      valid_time = ymd_hms(valid_time, tz = "UTC"),
      datetime_local = valid_time - hours(5),
      fecha = as.Date(datetime_local),
      era5_temp = t2m - 273.15
    ) %>%
    group_by(fecha, latitude, longitude) %>%
    summarise(
      era5_temp = mean(era5_temp, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(era5_file = grid_name) %>%
    filter(fecha >= fecha_inicio, fecha <= fecha_fin)
}

era5_025_clean <- clean_era5(era5_025, "ERA5_785_025")
era5_000_clean <- clean_era5(era5_000, "ERA5_785_000")

# ---------------------------------------------------------
# 3. Unir las dos series ERA5
# ---------------------------------------------------------
era5_all <- bind_rows(era5_025_clean, era5_000_clean)

# ---------------------------------------------------------
# 4. Asignar cada serie ERA5 a sus estaciones
# ---------------------------------------------------------
era5_by_station <- estaciones %>%
  select(estacion, era5_file) %>%
  left_join(era5_all, by = "era5_file", relationship = "many-to-many") %>%
  select(estacion, fecha, era5_temp, latitude, longitude, era5_file) %>%
  arrange(estacion, fecha)


head(era5_by_station)
write_csv(era5_by_station, file.path(path_tables, "era5_diario_por_estacion.csv"))
