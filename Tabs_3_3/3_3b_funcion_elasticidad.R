###############################
##### Libraries ###############
###############################

pacman::p_load(
  readr, lubridate, dplyr, ggplot2, zoo, readxl, glue,
  tidyverse, gridExtra, corrplot, plotly, arrow
)

options(scipen = 999)
options(encoding = "UTF-8")

# ============================================================
# Cargar datos
# ============================================================
data <- readRDS("base_elasticidad_3_3.rds") %>% ungroup()



# ============================================================
# Preparación de datos mensual por producto y año
# ============================================================
preparar_df_producto_anual <- function(data, producto_sel, anio_sel) {
  
  meses_es <- c(
    "Enero","Febrero","Marzo","Abril","Mayo","Junio",
    "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"
  )
  
  df_pre <- data %>%
    mutate(
      mes_y_ano = as.Date(mes_y_ano),
      mes_num   = month(mes_y_ano)
    ) %>%
    filter(
      producto == producto_sel,
      year(mes_y_ano) == anio_sel
    )
  
  if (nrow(df_pre) == 0) return(NULL)
  
  df_base <- tibble(
    mes_num = 1:12,
    Mes     = meses_es
  )
  
  df_final <- df_pre %>%
    group_by(mes_num) %>%
    summarise(
      precio_prom = mean(precio_prom, na.rm = TRUE) / 1000,  # miles
      ton         = sum(suma_kg, na.rm = TRUE) / 1000,
      elasticidad = mean(elasticidad, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    right_join(df_base, by = "mes_num") %>%
    mutate(
      Mes  = factor(Mes, levels = meses_es),
      x_id = mes_num
    ) %>%
    arrange(x_id)
  
  df_final
}


# ============================================================
# 1. Preparación de datos (FUNCIÓN ÚNICA)
# ============================================================
grafico_producto_anual_plano <- function(df_final) {
  
  req(df_final)
  
  # ============================================================
  # Escalamiento SOLO para cantidad (segundo eje)
  # ============================================================
  p_min <- min(df_final$precio_prom, na.rm = TRUE)
  p_max <- max(df_final$precio_prom, na.rm = TRUE)
  
  t_min <- min(df_final$ton, na.rm = TRUE)
  t_max <- max(df_final$ton, na.rm = TRUE)
  
  if (!is.finite(p_min)) { p_min <- 0; p_max <- 1 }
  if (!is.finite(t_min) || t_min == t_max) { t_min <- 0; t_max <- 1 }
  
  df_plot <- df_final %>%
    mutate(
      ton_scaled = p_min + (ton - t_min) * (p_max - p_min) / (t_max - t_min)
    )
  
  ggplot(df_plot, aes(x = x_id)) +
    
    # ============================================================
  # Línea de referencia elasticidad = 0
  # ============================================================
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    linewidth = 0.6,
    color = "gray40",
    alpha = 0.7
  ) +
    
    # ------------------------------------------------------------
  # Precio
  # ------------------------------------------------------------
  geom_line(
    aes(y = precio_prom, color = "Precio promedio"),
    linewidth = 1,
    linetype = "dashed",
    na.rm = TRUE
  ) +
    geom_point(
      aes(y = precio_prom, color = "Precio promedio"),
      size = 2,
      na.rm = TRUE
    ) +
    
    # ------------------------------------------------------------
  # Elasticidad (escala real, alrededor de 0)
  # ------------------------------------------------------------
  geom_line(
    aes(y = elasticidad, color = "Elasticidad"),
    linewidth = 1.2,
    na.rm = TRUE
  ) +
    geom_point(
      aes(y = elasticidad, color = "Elasticidad"),
      size = 2,
      na.rm = TRUE
    ) +
    
    # ------------------------------------------------------------
  # Cantidad (reescalada para eje derecho)
  # ------------------------------------------------------------
  geom_line(
    aes(y = ton_scaled, color = "Cantidad (t)"),
    linewidth = 1,
    linetype = "dotted",
    na.rm = TRUE
  ) +
    geom_point(
      aes(y = ton_scaled, color = "Cantidad (t)"),
      size = 2,
      na.rm = TRUE
    ) +
    
    # ============================================================
  # EJE X – igual al dinámico
  # ============================================================
  scale_x_continuous(
    name   = "Mes",
    breaks = 1:12,
    labels = levels(df_final$Mes),
    limits = c(1, 12),
    expand = c(0, 0)
  ) +
    
    # ============================================================
  # EJE Y – precio + elasticidad / cantidades
  # ============================================================
  scale_y_continuous(
    name = "Precio (miles) / Elasticidad",
    labels = function(x)
      gsub("\\.", ",", formatC(x, format = "f", digits = 2)),
    sec.axis = sec_axis(
      trans = ~ t_min + (. - p_min) * (t_max - t_min) / (p_max - p_min),
      name  = "Toneladas",
      labels = function(x)
        gsub("\\.", ",", formatC(x, format = "f", digits = 2))
    )
  ) +
    
    # ============================================================
  # LEYENDA LIMPIA
  # ============================================================
  scale_color_manual(
    name = NULL,
    values = c(
      "Precio promedio" = "#494634",
      "Elasticidad"     = "#DBC21F",
      "Cantidad (t)"    = "#6D673E"
    )
  ) +
    
    theme(
      axis.text.x = element_text(
        angle = 90,      # 👈 vertical
        vjust = 0.5,
        hjust = 1
      ),
      legend.position = "bottom"
    )
}



grafico_producto_anual_plotly <- function(df_final) {
   ggplotly(grafico_producto_anual_plano(df_final)) 
}

