# =========================================================
# 06_metrics_plots.R
# Métricas y gráficos
# =========================================================

source("scripts/01_setup.R")

base_era5 <- read_csv(file.path(path_tables, "base_match_obs_era5.csv"), show_col_types = FALSE) %>%
  mutate(fecha = as.Date(fecha))

base_merra2 <- read_csv(file.path(path_tables, "base_match_obs_merra2.csv"), show_col_types = FALSE) %>%
  mutate(fecha = as.Date(fecha))

base_full <- read_csv(file.path(path_tables, "base_match_obs_era5_merra2.csv"), show_col_types = FALSE) %>%
  mutate(fecha = as.Date(fecha))

# ---------------------------------------------------------
# 1. Función de métricas
# ---------------------------------------------------------
calc_metrics <- function(df, obs_col, sat_col) {
  df %>%
    summarise(
      n = n(),
      RMSE = sqrt(mean((.data[[sat_col]] - .data[[obs_col]])^2, na.rm = TRUE)),
      MAE = mean(abs(.data[[sat_col]] - .data[[obs_col]]), na.rm = TRUE),
      Bias = mean(.data[[sat_col]] - .data[[obs_col]], na.rm = TRUE),
      Cor_Pearson = cor(.data[[obs_col]], .data[[sat_col]], use = "complete.obs", method = "pearson"),
      Cor_Spearman = cor(.data[[obs_col]], .data[[sat_col]], use = "complete.obs", method = "spearman")
    )
}

# ---------------------------------------------------------
# 2. Métricas por estación
# ---------------------------------------------------------
metrics_era5 <- base_era5 %>%
  group_by(estacion) %>%
  group_modify(~ calc_metrics(.x, "obs_temp", "era5_temp")) %>%
  ungroup() %>%
  mutate(producto = "ERA5")

metrics_merra2 <- base_merra2 %>%
  group_by(estacion) %>%
  group_modify(~ calc_metrics(.x, "obs_temp", "merra2_temp")) %>%
  ungroup() %>%
  mutate(producto = "MERRA2")

metrics_all <- bind_rows(metrics_era5, metrics_merra2)


write_csv(metrics_era5, file.path(path_tables, "metricas_era5_por_estacion.csv"))
write_csv(metrics_merra2, file.path(path_tables, "metricas_merra2_por_estacion.csv"))
write_csv(metrics_all, file.path(path_tables, "metricas_todos_productos.csv"))

# ---------------------------------------------------------
# 3. Métricas globales
# ---------------------------------------------------------
metrics_global_era5 <- calc_metrics(base_era5, "obs_temp", "era5_temp") %>%
  mutate(producto = "ERA5")

metrics_global_merra2 <- calc_metrics(base_merra2, "obs_temp", "merra2_temp") %>%
  mutate(producto = "MERRA2")

metrics_global <- bind_rows(metrics_global_era5, metrics_global_merra2)

write_csv(metrics_global, file.path(path_tables, "metricas_globales.csv"))

# ---------------------------------------------------------
# 4. Gráfico de series temporales
# ---------------------------------------------------------
g_series <- base_full %>%
  pivot_longer(
    cols = c(obs_temp, era5_temp, merra2_temp),
    names_to = "fuente",
    values_to = "temperatura"
  ) %>%
  mutate(
    fuente = factor(
      fuente,
      levels = c("obs_temp", "era5_temp", "merra2_temp"),
      labels = c("Observado", "ERA5", "MERRA-2")
    )
  ) %>%
  ggplot(aes(x = fecha, y = temperatura, color = fuente, linetype = fuente)) +
  geom_line(linewidth = 0.35, alpha = 0.9) +
  facet_wrap(~ estacion, scales = "free_y", ncol = 2) +
  scale_color_manual(
    values = c(
      "Observado" = "black",
      "ERA5" = "#1f78b4",
      "MERRA-2" = "#e31a1c"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Observado" = "solid",
      "ERA5" = "dashed",
      "MERRA-2" = "dotdash"
    )
  ) +
  labs(
    title = "Comparación de temperatura media diaria",
    subtitle = "Observado vs ERA5 vs MERRA-2",
    x = "Fecha",
    y = "Temperatura (°C)",
    color = "Fuente",
    linetype = "Fuente"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.2),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.2)
  )

ggsave(file.path(path_figures, "series_temporales_comparacion.png"),
       g_series, width = 12, height = 10, dpi = 300)

g_series_monthly <- base_full %>%
  mutate(
    mes = floor_date(fecha, unit = "month")
  ) %>%
  group_by(estacion, mes) %>%
  summarise(
    obs_temp = mean(obs_temp, na.rm = TRUE),
    era5_temp = mean(era5_temp, na.rm = TRUE),
    merra2_temp = mean(merra2_temp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(obs_temp, era5_temp, merra2_temp),
    names_to = "fuente",
    values_to = "temperatura"
  ) %>%
  mutate(
    fuente = factor(
      fuente,
      levels = c("obs_temp", "era5_temp", "merra2_temp"),
      labels = c("Observado", "ERA5", "MERRA-2")
    )
  ) %>%
  ggplot(aes(x = mes, y = temperatura, color = fuente)) +
  geom_line(linewidth = 0.6, alpha = 0.95) +
  facet_wrap(~ estacion, scales = "free_y", ncol = 2) +
  scale_color_manual(
    values = c(
      "Observado" = "black",
      "ERA5" = "#1f78b4",
      "MERRA-2" = "#e31a1c"
    )
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(
    title = "Comparación mensual de temperatura media",
    subtitle = "Promedios mensuales observados, ERA5 y MERRA-2",
    x = "Fecha",
    y = "Temperatura media mensual (°C)",
    color = "Fuente"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.25),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.25)
  )

ggsave(
  file.path(path_figures, "series_temporales_mensuales_comparacion.png"),
  g_series_monthly,  width = 12,  height = 10,  dpi = 300
)

# ---------------------------------------------------------
# 5. Scatter plot ERA5
# ---------------------------------------------------------

stats_era5_plot <- base_era5 %>%
  group_by(estacion) %>%
  summarise(
    n = n(),
    r = cor(obs_temp, era5_temp, use = "complete.obs", method = "pearson"),
    RMSE = sqrt(mean((era5_temp - obs_temp)^2, na.rm = TRUE)),
    Bias = mean(era5_temp - obs_temp, na.rm = TRUE),
    x_pos = quantile(obs_temp, 0.05, na.rm = TRUE),
    y_pos = quantile(era5_temp, 0.95, na.rm = TRUE),
    etiqueta = paste0(
      "n = ", n,
      "\nr = ", round(r, 2),
      "\nRMSE = ", round(RMSE, 2),
      "\nBias = ", round(Bias, 2)
    ),
    .groups = "drop"
  )

g_scatter_era5_mejorado <- ggplot(base_era5, aes(x = obs_temp, y = era5_temp)) +
  geom_point(
    alpha = 0.18,
    size = 0.5,
    color = "#00aaff"
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.55,
    color = "#d62728"
  ) +
  geom_text(
    data = stats_era5_plot,
    aes(x = x_pos, y = y_pos, label = etiqueta),
    inherit.aes = FALSE,
    hjust = 0.75,
    vjust = 0.5,
    size = 3.2,
    lineheight = 1.0,
    family = ""
  ) +
  facet_wrap(~ estacion, scales = "fixed", ncol = 2) +
  coord_equal() +
  labs(
    title = "Observed vs ERA5 daily mean temperature",
    subtitle = "Dashed red line indicates 1:1 agreement",
    x = "Observed (°C)",
    y = "ERA5 (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 11),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.25),
    legend.position = "none",
    plot.margin = margin(10, 12, 10, 10)
  )

ggsave(
  file.path(path_figures, "scatter_obs_vs_era5_mejorado.png"),
  g_scatter_era5_mejorado,  width = 10.5,  height = 8.5,  dpi = 300)

# ---------------------------------------------------------
# 6. Scatter plot MERRA-2
# ---------------------------------------------------------
library(dplyr)
library(ggplot2)

stats_merra2_plot <- base_merra2 %>%
  group_by(estacion) %>%
  summarise(
    n = n(),
    r = cor(obs_temp, merra2_temp, use = "complete.obs", method = "pearson"),
    RMSE = sqrt(mean((merra2_temp - obs_temp)^2, na.rm = TRUE)),
    Bias = mean(merra2_temp - obs_temp, na.rm = TRUE),
    x_pos = quantile(obs_temp, 0.05, na.rm = TRUE),
    y_pos = quantile(merra2_temp, 0.95, na.rm = TRUE),
    etiqueta = paste0(
      "n = ", n,
      "\nr = ", round(r, 2),
      "\nRMSE = ", round(RMSE, 2),
      "\nBias = ", round(Bias, 2)
    ),
    .groups = "drop"
  )

g_scatter_merra2_mejorado <- ggplot(base_merra2, aes(x = obs_temp, y = merra2_temp)) +
  geom_point(
    alpha = 0.18,
    size = 0.5,
    color = "#00aaff"
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.55,
    color = "#d62728"
  ) +
  geom_text(
    data = stats_merra2_plot,
    aes(x = x_pos, y = y_pos, label = etiqueta),
    inherit.aes = FALSE,
    hjust = 0.5,
    vjust = 0.7,
    size = 3.2,
    lineheight = 1.0,
    family = ""
  ) +
  facet_wrap(~ estacion, scales = "fixed", ncol = 2) +
  coord_equal() +
  labs(
    title = "Observed vs MERRA-2 daily mean temperature",
    subtitle = "Dashed red line indicates 1:1 agreement",
    x = "Observed (°C)",
    y = "MERRA-2 (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 11),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.25),
    legend.position = "none",
    plot.margin = margin(10, 12, 10, 10)
  )

ggsave(
  file.path(path_figures, "scatter_obs_vs_merra2_mejorado.png"),
  g_scatter_merra2_mejorado,
  width = 10.5,
  height = 8.5,
  dpi = 300
)
##### graficar con mismos limites ####
valores_comunes <- c(
  base_era5$obs_temp,
  base_era5$era5_temp,
  base_merra2$obs_temp,
  base_merra2$merra2_temp)

rango_global <- range(valores_comunes, na.rm = TRUE)

margen <- 0.04 * diff(rango_global)

lims_comunes <- c(
  rango_global[1] - margen,
  rango_global[2] + margen
)
stats_era5_plot <- base_era5 %>%
  group_by(estacion) %>%
  summarise(
    n = n(),
    r = cor(obs_temp, era5_temp, use = "complete.obs", method = "pearson"),
    RMSE = sqrt(mean((era5_temp - obs_temp)^2, na.rm = TRUE)),
    Bias = mean(era5_temp - obs_temp, na.rm = TRUE),
    x_pos = lims_comunes[1] + 0.06 * diff(lims_comunes),
    y_pos = lims_comunes[2] - 0.06 * diff(lims_comunes),
    etiqueta = paste0(
      "n = ", n,
      "\nr = ", round(r, 2),
      "\nRMSE = ", round(RMSE, 2),
      "\nBias = ", round(Bias, 2)
    ),
    .groups = "drop"
  )
#ERA5
g_scatter_era5_paper <- ggplot(base_era5, aes(x = obs_temp, y = era5_temp)) +
  geom_point(
    alpha = 0.18,
    size = 0.5,
    color = "#00aaff"
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.55,
    color = "#d62728"
  ) +
  geom_text(
    data = stats_era5_plot,
    aes(x = x_pos, y = y_pos, label = etiqueta),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = 0,
    size = 3.2,
    lineheight = 1.0
  ) +
  facet_wrap(~ estacion, scales = "fixed", ncol = 2) +
  coord_equal(xlim = lims_comunes, ylim = lims_comunes, expand = FALSE) +
  labs(
    title = "Observed vs ERA5 daily mean temperature",
    subtitle = "Common axis limits used for direct comparison with MERRA-2",
    x = "Observed (°C)",
    y = "ERA5 (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 11),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.25),
    legend.position = "none",
    plot.margin = margin(10, 12, 10, 10)
  )
#MERRA2
stats_merra2_plot <- base_merra2 %>%
  group_by(estacion) %>%
  summarise(
    n = n(),
    r = cor(obs_temp, merra2_temp, use = "complete.obs", method = "pearson"),
    RMSE = sqrt(mean((merra2_temp - obs_temp)^2, na.rm = TRUE)),
    Bias = mean(merra2_temp - obs_temp, na.rm = TRUE),
    x_pos = lims_comunes[1] + 0.06 * diff(lims_comunes),
    y_pos = lims_comunes[2] - 0.06 * diff(lims_comunes),
    etiqueta = paste0(
      "n = ", n,
      "\nr = ", round(r, 2),
      "\nRMSE = ", round(RMSE, 2),
      "\nBias = ", round(Bias, 2)
    ),
    .groups = "drop"
  )

g_scatter_merra2_paper <- ggplot(base_merra2, aes(x = obs_temp, y = merra2_temp)) +
  geom_point(
    alpha = 0.18,
    size = 0.5,
    color = "#00aaff"
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.55,
    color = "#d62728"
  ) +
  geom_text(
    data = stats_merra2_plot,
    aes(x = x_pos, y = y_pos, label = etiqueta),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    size = 3.2,
    lineheight = 1.0
  ) +
  facet_wrap(~ estacion, scales = "fixed", ncol = 2) +
  coord_equal(xlim = lims_comunes, ylim = lims_comunes, expand = FALSE) +
  labs(
    title = "Observed vs MERRA-2 daily mean temperature",
    subtitle = "Common axis limits used for direct comparison with ERA5",
    x = "Observed (°C)",
    y = "MERRA-2 (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 11),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.25),
    legend.position = "none",
    plot.margin = margin(10, 12, 10, 10)
  )
ggsave(
  file.path(path_figures, "scatter_obs_vs_era5_paper.png"),
  g_scatter_era5_paper,
  width = 10.5,
  height = 8.5,
  dpi = 300
)

ggsave(
  file.path(path_figures, "scatter_obs_vs_merra2_paper.png"),
  g_scatter_merra2_paper,
  width = 10.5,
  height = 8.5,
  dpi = 300
)

# ---------------------------------------------------------
# 7. Boxplot de errores
# ---------------------------------------------------------
errores <- base_full %>%
  mutate(
    error_era5   = era5_temp - obs_temp,
    error_merra2 = merra2_temp - obs_temp
  ) %>%
  select(estacion, fecha,obs_temp, error_era5, error_merra2) %>%
  pivot_longer(
    cols = c(error_era5, error_merra2),
    names_to = "producto",
    values_to = "error"
  ) %>%
  mutate(
    producto = recode(
      producto,
      "error_era5"   = "ERA5",
      "error_merra2" = "MERRA-2"
    ),
    producto = factor(producto, levels = c("ERA5", "MERRA-2"))
  )

g_box_mejorado <- ggplot(errores, aes(x = producto, y = error, fill = producto)) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.5,
    color = "red3"
  ) +
  geom_boxplot(
    width = 0.58,
    outlier.shape = NA,
    alpha = 0.75,
    linewidth = 0.45,
    color = "grey20"
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.10,
    size = 0.6,
    color = "black"
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 2.2,
    color = "black"
  ) +
  facet_wrap(~ estacion, ncol = 2) +
  scale_fill_manual(
    values = c("ERA5" = "#4C78A8", "MERRA-2" = "#F58518")
  ) +
  labs(
    title = "Distribution of temperature errors by product and station",
    subtitle = "Boxes show median and IQR; black diamond indicates mean; dashed red line indicates zero error",
    x = "Product",
    y = expression("Error (product - observed, " * degree * "C)")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10.5),
    strip.text = element_text(face = "bold", size = 11),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    legend.position = "none",
    plot.margin = margin(10, 12, 10, 10)
  )

ggsave(
  file.path(path_figures, "boxplot_errores_era5_merra2.png"),
  g_box_mejorado,
  width = 10.5,
  height = 8.5,
  dpi = 300
)

#### ERRORES ####
errores_ts <- errores %>%
  group_by(fecha, producto) %>%
  summarise(error_mean = mean(error, na.rm = TRUE), .groups = "drop")

g_ts <- ggplot(errores_ts, aes(x = fecha, y = error_mean, color = producto)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ producto, ncol = 1) +
  labs(
    title = "Temporal evolution of mean error",
    x = "Date",
    y = "Mean error (°C)"
  ) +
  theme_minimal()

ggsave(
  file.path(path_figures, "serie_temporal_errores.png"),
  g_ts,  width = 10.5,  height = 8.5,  dpi = 300)

g_density <- ggplot(errores, aes(x = error, fill = producto)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ estacion, ncol = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Error distribution density",
    x = "Error (°C)",
    y = "Density"
  ) +
  theme_minimal()
ggsave(
  file.path(path_figures, "densidad_errores.png"),
  g_density,  width = 10.5,  height = 8.5,  dpi = 300)


g_error_vs_temp <- ggplot(errores, aes(x = obs_temp, y = error, color = producto)) +
  geom_point(alpha = 0.15, size = 0.5) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ estacion, ncol = 2) +
  labs(
    title = "Error dependence on observed temperature",
    x = "Observed temperature (°C)",
    y = "Error (°C)"
  ) +
  theme_minimal()
ggsave(
  file.path(path_figures, "error_vs_temperatura.png"),
  g_error_vs_temp,  width = 10.5,  height = 8.5,  dpi = 300)



# metricas
metrics <- errores %>%
  group_by(estacion, producto) %>%
  summarise(
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    Bias = mean(error, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(RMSE, Bias), names_to = "metric", values_to = "value")

ggplot(metrics, aes(x = producto, y = estacion, fill = value)) +
  geom_tile() +
  facet_wrap(~ metric) +
  theme_minimal()
