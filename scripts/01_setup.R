# =========================================================
# 01_setup.R
# Configuración general del proyecto
# =========================================================

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(ggplot2)

# ---------------------------------------------------------
# 1. Rutas de archivos
# ---------------------------------------------------------
path_obs <- "data/observed/tmp_media_diaria_lista.csv"
path_era5_025 <- "data/era5/ERA5_785_025.csv"   # lon -78.50, lat -0.25
path_era5_000 <- "data/era5/ERA5_785_000.csv"   # lon -78.50, lat  0.00
path_merra2 <- "data/merra2/"

path_tables <- "output/tables/"
path_figures <- "output/figures/"

# ---------------------------------------------------------
# 2. Periodo de estudio
# ---------------------------------------------------------
fecha_inicio <- as.Date("2006-01-01")
fecha_fin    <- as.Date("2024-09-30")

# ---------------------------------------------------------
# 3. Tabla maestra de estaciones
# ---------------------------------------------------------
estaciones <- tibble(
  estacion = c("Belisario", "Cotocollao", "Tumbaco", "Carapungo", "LosChillos"),
  lat = c(-0.1847, -0.1077, -0.2149, -0.0954, -0.2970),
  lon = c(-78.4959, -78.4972, -78.4032, -78.4498, -78.4552),
  altitud_msnm = c(2843, 2752, 2336, 2644, 2463),
  era5_lat = c(-0.25,  0.00, -0.25,  0.00, -0.25),
  era5_lon = c(-78.50, -78.50, -78.50, -78.50, -78.50),
  era5_file = c("ERA5_785_025", "ERA5_785_000", "ERA5_785_025", "ERA5_785_000", "ERA5_785_025"),
  merra2_file = c(
    "MERRA2_Belisario_2006_2024.csv",
    "MERRA2_Cotocollao_2006_2024.csv",
    "MERRA2_Tumbaco_2006_2024.csv",
    "MERRA2_Carapungo_2006_2024.csv",
    "MERRA2_Chillos_2006_2024.csv"
  )
)

print(estaciones)

write_csv(estaciones, file.path(path_tables, "tabla_estaciones.csv"))
