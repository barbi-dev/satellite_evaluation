# =========================================================
# 05_matching.R
# Matching observado - ERA5 - MERRA2
# =========================================================

source("scripts/01_setup.R")

obs <- read_csv(file.path(path_tables, "observado_limpio_2006_2024.csv"), show_col_types = FALSE)
era5 <- read_csv(file.path(path_tables, "era5_diario_por_estacion.csv"), show_col_types = FALSE)
merra2 <- read_csv(file.path(path_tables, "merra2_diario_por_estacion.csv"), show_col_types = FALSE)

obs <- obs %>% mutate(fecha = as.Date(fecha))
era5 <- era5 %>% mutate(fecha = as.Date(fecha))
merra2 <- merra2 %>% mutate(fecha = as.Date(fecha))

# ---------------------------------------------------------
# 1. Base combinada
# ---------------------------------------------------------
base_match <- obs %>%
  select(estacion, fecha, obs_temp, n_horas_validas) %>%
  left_join(
    era5 %>% select(estacion, fecha, era5_temp),
    by = c("estacion", "fecha")
  ) %>%
  left_join(
    merra2 %>% select(estacion, fecha, merra2_temp),
    by = c("estacion", "fecha")
  ) %>%
  arrange(estacion, fecha)

# ---------------------------------------------------------
# 2. Subbases coincidentes
# ---------------------------------------------------------
base_era5 <- base_match %>%
  filter(!is.na(obs_temp), !is.na(era5_temp))

base_merra2 <- base_match %>%
  filter(!is.na(obs_temp), !is.na(merra2_temp))

base_full <- base_match %>%
  filter(!is.na(obs_temp), !is.na(era5_temp), !is.na(merra2_temp))


write_csv(base_match, file.path(path_tables, "base_match_completa.csv"))
write_csv(base_era5, file.path(path_tables, "base_match_obs_era5.csv"))
write_csv(base_merra2, file.path(path_tables, "base_match_obs_merra2.csv"))
write_csv(base_full, file.path(path_tables, "base_match_obs_era5_merra2.csv"))
