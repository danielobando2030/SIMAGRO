################################################################################-
# Proyecto FAO
# Procesamiento datos SIPSA (OPTIMIZADO EN MEMORIA)
################################################################################-
# Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
# Fecha de creación: 24/02/2024
# Última modificación: 06/11/2025
################################################################################-

rm(list = ls())

# Paquetes ----------------------------------------------------------------------
pacman::p_load(
  readr, readxl, dplyr, tidyr, janitor, lubridate, stringr,
  arrow, purrr, leaflet, scales, htmltools, maps
)

options(scipen = 999)

################################################################################-
# Cargar datos
################################################################################-

data_merged <- readRDS("data_sin_rutas.rds")
rutas       <- readRDS("rutas_abas.rds")

# Unir rutas
data_merged <- left_join(
  data_merged,
  rutas,
  by = "codigo_mpio_origen"
)

rm(rutas); gc()

################################################################################-
# Mantener SOLO lo necesario para el cálculo
################################################################################-

data_merged <- data_merged %>% 
  select(
    anio,
    mes,
    producto,
    codigo_mpio_destino,
    codigo_mpio_origen,
    mpio_origen,
    depto_origen,
    routes_coords_str,
    suma_kg
  )

################################################################################-
# Cálculo de importancia relativa
################################################################################-

data_merged <- data_merged %>%
  group_by(anio, mes, producto) %>%
  mutate(total_kilogramos_año_mes_producto = sum(suma_kg, na.rm = TRUE)) %>%
  ungroup()

data_merged <- data_merged %>%
  group_by(anio, mes, producto, codigo_mpio_origen) %>%
  mutate(total_kilogramos_año_mes_producto_mpio = sum(suma_kg, na.rm = TRUE)) %>%
  ungroup()

data_merged <- data_merged %>%
  mutate(
    importancia_ruta =
      total_kilogramos_año_mes_producto_mpio /
      total_kilogramos_año_mes_producto
  )

################################################################################-
# 🔥 LIMPIEZA FINAL: borrar lo que ya no se usa
################################################################################-

data_merged <- data_merged %>%
  select(
    anio,
    mes,
    producto,
    codigo_mpio_destino,
    codigo_mpio_origen,
    mpio_origen,
    depto_origen,
    routes_coords_str,
    importancia_ruta
  )

gc()

################################################################################-
# FUNCIÓN OPTIMIZADA: graficar_rutas (SIN CAMBIOS FUNCIONALES)
################################################################################-

graficar_rutas <- function(df, Año = NULL, Mes = NULL, Producto = NULL) {
  
  # ------------------------------------------------------------
  # Formato porcentaje LATAM
  # ------------------------------------------------------------
  fmt_pct <- function(x, dec = 2) {
    paste0(
      format(round(x * 100, dec),
             big.mark = ".", decimal.mark = ",", nsmall = dec),
      "%"
    )
  }
  
  # ------------------------------------------------------------
  # Filtros
  # ------------------------------------------------------------
  df <- df %>%
    { if (!is.null(Año)) filter(., anio == Año) else . } %>%
    { if (!is.null(Mes)) filter(., mes == Mes) else . } %>%
    { if (!is.null(Producto)) filter(., producto == Producto) else . } %>%
    distinct(codigo_mpio_destino, codigo_mpio_origen, .keep_all = TRUE)
  
  if (nrow(df) == 0) {
    return(
      leaflet() %>%
        addTiles() %>%
        addControl("No hay datos disponibles", position = "topright")
    )
  }
  
  # ------------------------------------------------------------
  # Paleta
  # ------------------------------------------------------------
  pal <- colorNumeric(
    palette = c("#332728", "#4F3032", "#743639", "#983136", "#BC222A"),
    domain = df$importancia_ruta
  )
  
  # ------------------------------------------------------------
  # Formateo
  # ------------------------------------------------------------
  df <- df %>%
    mutate(
      grosor = scales::rescale(importancia_ruta, to = c(1, 6)),
      color_hex = pal(importancia_ruta),
      mpio_origen_fmt  = str_to_title(str_to_lower(mpio_origen)),
      depto_origen_fmt = str_to_title(str_to_lower(depto_origen)),
      importancia_txt  = fmt_pct(importancia_ruta),
      routes_coords_str = as.character(routes_coords_str)
    ) %>%
    filter(!is.na(routes_coords_str), routes_coords_str != "")
  
  # ------------------------------------------------------------
  # Parseo rutas
  # ------------------------------------------------------------
  df$ruta_coords <- purrr::map(df$routes_coords_str, function(x) {
    coords <- str_split(x, ";", simplify = TRUE)
    mat <- str_split(coords, ",", simplify = TRUE)
    suppressWarnings({
      m <- matrix(as.numeric(mat), ncol = 2)
      if (nrow(m) < 2 || anyNA(m)) NULL else m
    })
  })
  
  df <- df %>% filter(!purrr::map_lgl(ruta_coords, is.null))
  
  if (nrow(df) == 0) {
    return(
      leaflet() %>%
        addTiles() %>%
        addControl("No hay rutas válidas", position = "topright")
    )
  }
  
  # ------------------------------------------------------------
  # Mapa
  # ------------------------------------------------------------
  titulo <- paste(
    c(
      if (!is.null(Producto)) paste0("Producto: ", Producto),
      if (!is.null(Año)) paste0("Año: ", Año),
      if (!is.null(Mes)) paste0("Mes: ", Mes)
    ),
    collapse = " | "
  )
  
  map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    addTiles() %>%
    addControl(titulo, position = "topright")
  
  purrr::walk(seq_len(nrow(df)), function(i) {
    coords <- df$ruta_coords[[i]]
    map <<- map %>%
      addPolylines(
        lng = coords[, 1],
        lat = coords[, 2],
        color = df$color_hex[i],
        weight = df$grosor[i],
        opacity = 0.8,
        label = HTML(
          paste0(
            "<b>Origen:</b> ", df$mpio_origen_fmt[i],
            " (", df$depto_origen_fmt[i], ")<br>",
            "<b>Importancia:</b> ", df$importancia_txt[i]
          )
        )
      )
  })
  
  map %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = df$importancia_ruta,
      title = "Importancia de la ruta (%)",
      labFormat = function(type, cuts, p) {
        paste0(
          format(round(cuts * 100, 2),
                 big.mark = ".", decimal.mark = ",", nsmall = 2),
          "%"
        )
      }
    )
}

leaflet_a_png <- function(leaflet_map,
                          file,
                          vwidth  = 1600,
                          vheight = 1000,
                          delay   = 0.3) {

  tmp_html <- tempfile(fileext = ".html")

  htmlwidgets::saveWidget(
    leaflet_map,
    tmp_html,
    selfcontained = TRUE
  )

  webshot2::webshot(
    url     = tmp_html,
    file    = file,
    vwidth  = vwidth,
    vheight = vheight,
    delay   = delay
  )

  invisible(file)
}

####

library(sf)

colombia_map <- map_data("world", region = "Colombia")

graficar_rutas_estatico_ggplot <- function(df,
                                           Año = NULL,
                                           Mes = NULL,
                                           Producto = NULL) {
  
  # ------------------------------------------------------------
  # 1. Filtros
  # ------------------------------------------------------------
  df <- df %>%
    { if (!is.null(Año)) filter(., anio == Año) else . } %>%
    { if (!is.null(Mes)) filter(., mes == Mes) else . } %>%
    { if (!is.null(Producto)) filter(., producto == Producto) else . } %>%
    distinct(codigo_mpio_destino, codigo_mpio_origen, .keep_all = TRUE) %>%
    filter(!is.na(routes_coords_str), routes_coords_str != "")
  
  if (nrow(df) == 0) return(NULL)
  
  # ------------------------------------------------------------
  # 2. Expandir rutas
  # ------------------------------------------------------------
  rutas_df <- df %>%
    mutate(id_ruta = row_number()) %>%
    select(id_ruta, importancia_ruta, routes_coords_str) %>%
    mutate(routes_coords_str = strsplit(routes_coords_str, ";")) %>%
    unnest(routes_coords_str) %>%
    separate(routes_coords_str,
             into = c("lon", "lat"),
             sep = ",",
             convert = TRUE) %>%
    filter(!is.na(lon), !is.na(lat))
  
  if (nrow(rutas_df) == 0) return(NULL)
  
  rutas_df <- rutas_df %>%
    mutate(grosor = scales::rescale(importancia_ruta, to = c(0.4, 1.6)))
  
  # ------------------------------------------------------------
  # 3. Título
  # ------------------------------------------------------------
  titulo <- paste(
    c(
      paste0("Producto: ", Producto),
      paste0("Año: ", Año),
      paste0("Mes: ", Mes)
    ),
    collapse = " | "
  )
  
  # ------------------------------------------------------------
  # 4. MAPA ESTÁTICO FINAL (SIN SF)
  # ------------------------------------------------------------
  ggplot() +
    
    ## 🌍 BASE GEOGRÁFICA COLOMBIA
    geom_polygon(
      data = colombia_map,
      aes(x = long, y = lat, group = group),
      fill = "gray95",
      color = "gray70",
      linewidth = 0.3
    ) +
    
    ## 🚚 RUTAS
    geom_path(
      data = rutas_df,
      aes(x = lon, y = lat,
          group = id_ruta,
          color = importancia_ruta,
          size  = grosor),
      alpha = 0.85,
      lineend = "round"
    ) +
    
    scale_color_gradientn(
      colours = c("#332728", "#4F3032", "#743639", "#983136", "#BC222A"),
      labels  = scales::percent_format(
        accuracy = 0.1,
        decimal.mark = ","
      ),
      name = "Importancia de la ruta (%)"
    ) +
    
    scale_size_identity() +
    coord_equal() +
    
    labs(title = titulo) +
    
    theme_void() +
    theme(
      plot.title = element_text(
        size = 12,
        face = "bold",
        hjust = 0.5
      ),
      legend.title = element_text(size = 9),
      legend.text  = element_text(size = 8),
      legend.position = "right"
    )
}


