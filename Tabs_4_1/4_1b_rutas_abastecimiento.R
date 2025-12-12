################################################################################-
# Proyecto FAO
# Procesamiento datos SIPSA
################################################################################-
# Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
# Fecha de creación: 24/02/2024
# Fecha de última modificación: 06/11/2025
################################################################################-

rm(list = ls())

# Paquetes ----------------------------------------------------------------------
pacman::p_load(
  readr, readxl, dplyr, tidyr, janitor, lubridate, stringr, geosphere,
  arrow, osrm, sf, ggplot2, jsonlite, purrr, leaflet, scales, htmltools,
  callr, mapview
)

options(scipen = 999)

################################################################################-
# Cargar datos
################################################################################-

data_merged <- readRDS("data_sin_rutas.rds")
rutas       <- readRDS("rutas_abas.rds")

# Unir rutas
data_merged <- left_join(data_merged, rutas, by = c("codigo_mpio_origen"))

# ============================================================
# ELIMINAR VARIABLES QUE EL DASHBOARD NO NECESITA
# ============================================================
data_merged <- data_merged %>% 
  select(
    # FILTROS
    anio, mes, producto,
    
    # MAPA
    codigo_mpio_destino,
    codigo_mpio_origen,
    mpio_origen,
    depto_origen,
    routes_coords_str,
    
    # CÁLCULOS
    suma_kg
  )

remove(rutas)

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
  mutate(importancia_ruta =
           total_kilogramos_año_mes_producto_mpio /
           total_kilogramos_año_mes_producto)

################################################################################-
# Función principal: graficar_rutas
################################################################################-

graficar_rutas <- function(df, Año = NULL, Mes = NULL, Producto = NULL) {
  
  # ------------------------------------------------------------------
  # Filtros
  # ------------------------------------------------------------------
  if (!is.null(Año)) df <- df %>% filter(anio == Año)
  if (!is.null(Mes)) df <- df %>% filter(mes == Mes)
  if (!is.null(Producto)) df <- df %>% filter(producto == Producto)
  
  # Evitar duplicados
  df <- df %>% distinct(codigo_mpio_destino, codigo_mpio_origen, .keep_all = TRUE)
  
  # Si no hay datos
  if (nrow(df) == 0) {
    return(
      leaflet() %>% 
        addTiles() %>% 
        addControl("No hay datos disponibles", position = "topright")
    )
  }
  
  # ------------------------------------------------------------------
  # Paleta
  # ------------------------------------------------------------------
  pal <- colorNumeric(
    palette = c("#332728", "#4F3032", "#743639", "#983136", "#BC222A"),
    domain = df$importancia_ruta
  )
  
  # ------------------------------------------------------------------
  # Formateo variables
  # ------------------------------------------------------------------
  df <- df %>%
    mutate(
      grosor = scales::rescale(importancia_ruta, to = c(1, 6)),
      color_hex = pal(importancia_ruta),
      mpio_origen_fmt = str_to_title(str_to_lower(as.character(mpio_origen))),
      depto_origen_fmt = str_to_title(str_to_lower(as.character(depto_origen))),
      
      # ---- FORMATO ESPAÑOL ----
      importancia_txt = paste0(
        formatC(importancia_ruta * 100,
                format = "f",
                digits = 2,
                decimal.mark = ","),
        "%"
      )
    )
  
  # Filtrar rutas válidas
  df <- df %>% filter(!is.na(routes_coords_str), routes_coords_str != "")
  
  if (nrow(df) == 0) {
    return(
      leaflet() %>% 
        addTiles() %>% 
        addControl("No hay rutas disponibles", position = "topright")
    )
  }
  
  # ------------------------------------------------------------------
  # Parsear rutas
  # ------------------------------------------------------------------
  rutas_list <- purrr::map(df$routes_coords_str, function(coords_str) {
    coords <- str_split(as.character(coords_str), ";")[[1]]
    mat <- do.call(rbind, str_split(coords, ","))
    suppressWarnings(matrix(as.numeric(mat), ncol = 2, byrow = FALSE))
  })
  
  valid_idx <- which(!purrr::map_lgl(rutas_list, ~ is.null(.x) || any(is.na(.x))))
  
  # ------------------------------------------------------------------
  # Mapa base
  # ------------------------------------------------------------------
  titulo <- paste0(
    ifelse(!is.null(Producto), paste0("Producto: ", Producto, " | "), ""),
    ifelse(!is.null(Año), paste0("Año: ", Año, " | "), ""),
    ifelse(!is.null(Mes), paste0("Mes: ", Mes), "")
  )
  
  map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    addTiles() %>%
    addControl(titulo, position = "topright")
  
  # ------------------------------------------------------------------
  # Agregar rutas
  # ------------------------------------------------------------------
  for (i in valid_idx) {
    coords <- rutas_list[[i]]
    if (is.null(coords) || nrow(coords) < 2) next
    
    map <- map %>%
      addPolylines(
        lng = coords[, 1],
        lat = coords[, 2],
        color = df$color_hex[i],
        weight = df$grosor[i],
        opacity = 0.8,
        label = htmltools::HTML(
          paste0(
            "<b>Origen:</b> ", df$mpio_origen_fmt[i],
            " (", df$depto_origen_fmt[i], ")<br>",
            "<b>Importancia:</b> ", df$importancia_txt[i]
          )
        )
      )
  }
  
  # ------------------------------------------------------------------
  # Leyenda (formato español)
  # ------------------------------------------------------------------
  map <- map %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = df$importancia_ruta,
      title = "Importancia de la ruta (%)",
      labFormat = function(type, cuts, p) {
        paste0(
          formatC(cuts * 100,
                  format = "f",
                  digits = 2,
                  decimal.mark = ","),
          "%"
        )
      },
      opacity = 0.9
    )
  
  return(map)
}

################################################################################-
# Ejemplo de uso
################################################################################-
# mapa <- graficar_rutas(
#   data_merged,
#   Año = "2024",
#   Mes = "11",
#   Producto = "Maracuyá"
# )
# mapa
