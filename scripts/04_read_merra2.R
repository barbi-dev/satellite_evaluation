# =========================================================
# 04_read_merra2.R
# Lectura y limpieza de MERRA-2 (Giovanni)
# =========================================================

source("scripts/01_setup.R")

# ---------------------------------------------------------
# 1. Función para leer un CSV de Giovanni
# ---------------------------------------------------------
read_merra_giovanni <- function(file_path, station_name) {
  
  lines <- readLines(file_path, warn = FALSE)
  
  # Buscar la línea donde empieza la tabla real
  # Giovanni suele incluir una fila tipo:
  # "time,mean"
  # o alguna variante con time y el valor
  start_idx <- which(str_detect(lines, regex("^time|^Time", ignore_case = TRUE)))
  
  if (length(start_idx) == 0) {
    # Intento alternativo: buscar una línea que contenga una fecha ISO
    start_idx <- which(str_detect(lines, "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z"))
    if (length(start_idx) == 0) {
      stop(paste("No se encontró el inicio de la serie temporal en:", file_path))
    }
    # Si empieza directamente con datos, construimos cabecera artificial
    data_lines <- lines[start_idx:length(lines)]
    tmp_file <- tempfile(fileext = ".csv")
    writeLines(c("time,merra_raw", data_lines), tmp_file)
    df <- read_csv(tmp_file, show_col_types = FALSE)
  } else {
    tmp_file <- tempfile(fileext = ".csv")
    writeLines(lines[start_idx:length(lines)], tmp_file)
    df <- read_csv(tmp_file, show_col_types = FALSE)
  }
  
  # Normalizar nombres de columnas
  names(df) <- names(df) %>%
    str_trim() %>%
    str_replace_all("\\s+", "_")
  
  # Identificar columna temporal
  time_col <- names(df)[str_detect(names(df), regex("time|date", ignore_case = TRUE))][1]
  
  # Identificar columna de valor
  value_candidates <- setdiff(names(df), time_col)
  value_col <- value_candidates[1]
  
  df %>%
    rename(
      time_raw = all_of(time_col),
      merra_raw = all_of(value_col)
    ) %>%
    mutate(
      time_raw = str_trim(as.character(time_raw)),
      merra_raw = as.numeric(merra_raw),
      datetime_utc = ymd_hms(time_raw, tz = "UTC"),
      datetime_local = datetime_utc - hours(5),
      fecha = as.Date(datetime_local),
      merra2_temp = merra_raw - 273.15,
      estacion = station_name
    ) %>%
    filter(!is.na(datetime_utc), !is.na(merra2_temp)) %>%
    group_by(estacion, fecha) %>%
    summarise(
      merra2_temp = mean(merra2_temp, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(fecha >= fecha_inicio, fecha <= fecha_fin) %>%
    arrange(estacion, fecha)
}

# ---------------------------------------------------------
# 2. Leer todos los archivos MERRA-2
# ---------------------------------------------------------
merra2_list <- map2(
  .x = file.path(path_merra2, estaciones$merra2_file),
  .y = estaciones$estacion,
  .f = read_merra_giovanni
)

merra2_all <- bind_rows(merra2_list)

print(merra2_all)

write_csv(merra2_all, file.path(path_tables, "merra2_diario_por_estacion.csv"))
