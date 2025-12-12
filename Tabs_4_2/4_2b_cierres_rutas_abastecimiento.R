# Proyecto FAO - Procesamiento datos SIPSA
################################################################################-
# Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
# Optimización por ChatGPT - 2025
################################################################################-

pacman::p_load(
  readr, readxl, dplyr, tidyr, janitor, lubridate,
  stringr, geosphere, arrow, osrm, sf, ggplot2,
  jsonlite, purrr, leaflet, scales
)

options(scipen = 999)

# -----------------------------------------------------------
# Formato porcentaje español (coma decimal, sin ceros inútiles)
# -----------------------------------------------------------
formato_pct_es <- function(x, digits = 2) {
  x <- x * 100
  if (abs(x - round(x)) < 1e-8) {
    paste0(formatC(round(x), format = "f", digits = 0), "%")
  } else {
    paste0(
      formatC(x, format = "f", digits = digits, decimal.mark = ","),
      "%"
    )
  }
}

################################################################################-
# 1. CARGA DE BASES
################################################################################-

data_merged <- readRDS("data_sin_rutas.rds")
rutas       <- readRDS("rutas_abas.rds")

data_cierres_final <- left_join(
  data_merged,
  rutas,
  by = c("codigo_mpio_origen")
)

remove(rutas, data_merged)

################################################################################-
# 2. CÁLCULOS DE IMPORTANCIA
################################################################################-

data_cierres_final <- data_cierres_final %>%
  group_by(anio, mes, producto) %>%
  mutate(total_kilogramos_año_mes_producto = sum(suma_kg, na.rm = TRUE)) %>%
  ungroup()

data_cierres_final <- data_cierres_final %>%
  group_by(anio, mes, producto, codigo_mpio_origen) %>%
  mutate(total_kilogramos_año_mes_producto_mpio = sum(suma_kg, na.rm = TRUE)) %>%
  ungroup()

data_cierres_final <- data_cierres_final %>%
  mutate(importancia_ruta =
           total_kilogramos_año_mes_producto_mpio /
           total_kilogramos_año_mes_producto)

################################################################################-
# 3. CÁLCULO DE ÁNGULO Y REGIONES
################################################################################-

data_cierres_final <- data_cierres_final %>%
  mutate(
    bearing = bearing(
      cbind(LONGITUD_BOG, LATITUD_BOG),
      cbind(LONGITUD, LATITUD)
    ),
    bearing = ifelse(bearing < 0, bearing + 360, bearing),
    region_geo = case_when(
      bearing >= 337.5 | bearing < 22.5   ~ "Norte",
      bearing >= 22.5  & bearing < 67.5   ~ "Nororiente",
      bearing >= 67.5  & bearing < 112.5  ~ "Oriente",
      bearing >= 112.5 & bearing < 157.5  ~ "Suroriente",
      bearing >= 157.5 & bearing < 202.5  ~ "Sur",
      bearing >= 202.5 & bearing < 247.5  ~ "Suroccidente",
      bearing >= 247.5 & bearing < 292.5  ~ "Occidente",
      bearing >= 292.5 & bearing < 337.5  ~ "Noroccidente",
      TRUE ~ NA_character_
    )
  )

################################################################################-
# 4. PROPORCIÓN POR REGIÓN
################################################################################-

data_cierres_final <- data_cierres_final %>%
  group_by(producto, region_geo) %>%
  mutate(total_region_producto = sum(importancia_ruta, na.rm = TRUE)) %>%
  group_by(producto) %>%
  mutate(
    total_producto = sum(importancia_ruta, na.rm = TRUE),
    prop_region_producto = total_region_producto / total_producto
  ) %>%
  ungroup()

#################################################################################
# 5. *** OPTIMIZACIÓN — QUEDARSE SOLO CON VARIABLES ÚTILES ***
#################################################################################

data_cierres_final <- data_cierres_final %>%
  select(
    # filtros
    anio, mes, producto,
    
    # identificación
    codigo_mpio_destino, codigo_mpio_origen,
    mpio_destino, mpio_origen, depto_origen,
    
    # volumen
    suma_kg,
    
    # coordenadas y rutas
    LATITUD, LONGITUD, LATITUD_BOG, LONGITUD_BOG,
    routes_coords_str,
    
    # variables calculadas
    total_kilogramos_año_mes_producto,
    total_kilogramos_año_mes_producto_mpio,
    importancia_ruta,
    bearing,
    region_geo,
    prop_region_producto
  )

################################################################################-
# 6. FUNCIÓN DE MAPA
################################################################################-

graficar_rutas_color_importancia <- function(df, Año = NULL, Mes = NULL, Producto = NULL) {
  
  if (!is.null(Año)) df <- df %>% filter(anio == Año)
  if (!is.null(Mes)) df <- df %>% filter(mes == Mes)
  if (!is.null(Producto)) df <- df %>% filter(producto == Producto)
  
  df <- df %>% distinct(codigo_mpio_destino, codigo_mpio_origen, .keep_all = TRUE)
  if (nrow(df) == 0) return(NULL)
  
  df <- df %>% mutate(grosor = scales::rescale(as.numeric(prop_region_producto), to = c(1, 10)))
  
  df <- df %>% mutate(
    color_region = case_when(
      region_geo == "Noroccidente"  ~ "#e31a1c",
      region_geo == "Nororiente"    ~ "#ff7f00",
      region_geo == "Norte"         ~ "#6a3d9a",
      region_geo == "Oriente"       ~ "#1f78b4",
      region_geo == "Suroriente"    ~ "#D4AF37",
      region_geo == "Sur"           ~ "#b15928",
      region_geo == "Suroccidente"  ~ "#a6cee3",
      region_geo == "Occidente"     ~ "#33a02c",
      TRUE                          ~ "#999999"
    )
  )
  
  df <- df %>% mutate(across(everything(), as.character))
  
  rutas_list <- purrr::map(df$routes_coords_str, function(coords_str) {
    if (is.na(coords_str) || coords_str == "") return(NULL)
    coords <- stringr::str_split(coords_str, ";")[[1]]
    mat <- do.call(rbind, stringr::str_split(coords, ","))
    suppressWarnings(matrix(as.numeric(mat), ncol = 2, byrow = FALSE))
  })
  
  map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>% addTiles()
  
  valid_idx <- which(!purrr::map_lgl(rutas_list, is.null))
  for (i in valid_idx) {
    coords <- rutas_list[[i]]
    
    lbl_text <- paste0(
      "Origen: ", df$mpio_origen[i], " (", df$depto_origen[i], "). ",
      "Región: ", df$region_geo[i], ". ",
      "Proporción regional del producto: ",
      formato_pct_es(as.numeric(df$prop_region_producto[i]), digits = 2)
    )
    
    map <- map %>% addPolylines(
      lng = coords[, 1],
      lat = coords[, 2],
      color = df$color_region[i],
      weight = as.numeric(df$grosor[i]),
      opacity = 0.85,
      label = lbl_text,
      labelOptions = labelOptions(noHide = FALSE, direction = "top")
    )
  }
  
  map <- map %>% addLegend(
    position = "bottomright",
    colors = unique(df$color_region),
    labels = unique(df$region_geo),
    title = "Regiones geográficas",
    opacity = 0.9
  )
  
  return(map)
}

################################################################################-
# 7. FUNCIÓN DE EXPORTACIÓN A PNG
################################################################################-

grafico_plano <- function(mapa_leaflet) {
  
  if (is.null(mapa_leaflet)) return(NULL)
  
  tmp_dir  <- tempdir()
  tmp_html <- file.path(tmp_dir, "mapa_tmp.html")
  tmp_png  <- file.path(tmp_dir, "mapa_tmp.png")
  
  ok1 <- tryCatch({
    htmlwidgets::saveWidget(mapa_leaflet, tmp_html, selfcontained = TRUE)
    TRUE
  }, error = function(e) FALSE)
  
  if (!ok1) return(NULL)
  
  ok2 <- tryCatch({
    webshot2::webshot(
      url    = tmp_html,
      file   = tmp_png,
      vwidth = 1600,
      vheight = 900,
      zoom   = 1,
      delay  = 1
    )
    TRUE
  }, error = function(e) FALSE)
  
  if (!ok2) return(NULL)
  
  return(tmp_png)
}

################################################################################-
# FIN ARCHIVO
################################################################################-
