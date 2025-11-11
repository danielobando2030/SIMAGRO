# Proyecto FAO
# Procesamiento datos SIPSA
################################################################################-
# Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
# Fecha de creación: 24/02/2024
# Fecha de última modificación: 06/11/2025
################################################################################-
# Limpiar el entorno de trabajo
################################################################################-
rm(list = ls())

# Paquetes ----------------------------------------------------------------------
pacman::p_load(
  readr, readxl, dplyr, tidyr, janitor, lubridate, stringr, geosphere,
  arrow, osrm, sf, ggplot2, jsonlite, purrr, leaflet, scales, htmltools,
  callr
)

options(scipen = 999)

# Directorio -------------------------------------------------------------------

################################################################################-
# Cargar datos y preparar base
################################################################################-

# Bases de datos ---------------------------------------------------------------
data_merged <- readRDS("data_sin_rutas.rds")
rutas       <- readRDS("rutas_abas.rds")

# Unir rutas a base principal
data_merged <- left_join(data_merged, rutas, by = c("codigo_mpio_origen"))

# Liberar memoria
remove(rutas)

# Calcular importancia relativa a carretera ------------------------------------
data_merged <- data_merged %>%
  group_by(anio, mes, producto) %>%
  mutate(total_kilogramos_año_mes_producto = sum(suma_kg, na.rm = TRUE)) %>%
  ungroup()

data_merged <- data_merged %>%
  group_by(anio, mes, producto, codigo_mpio_origen) %>%
  mutate(total_kilogramos_año_mes_producto_mpio = sum(suma_kg, na.rm = TRUE)) %>%
  ungroup()

data_merged <- data_merged %>%
  mutate(importancia_ruta = total_kilogramos_año_mes_producto_mpio /
           total_kilogramos_año_mes_producto)

################################################################################-
# Función principal: graficar_rutas
################################################################################-

graficar_rutas <- function(df, Año = NULL, Mes = NULL, Producto = NULL) {
  
  # Filtro condicional ---------------------------------------------------------
  if (!is.null(Año)) df <- df %>% filter(anio == Año)
  if (!is.null(Mes)) df <- df %>% filter(mes == Mes)
  if (!is.null(Producto)) df <- df %>% filter(producto == Producto)
  
  # Evitar duplicados ----------------------------------------------------------
  df <- df %>% distinct(codigo_mpio_destino, codigo_mpio_origen, .keep_all = TRUE)
  if (nrow(df) == 0) {
    message("No hay datos disponibles para los filtros aplicados.")
    return(NULL)
  }
  
  # Escalado y color -----------------------------------------------------------
  pal <- colorNumeric(
    palette = c("#E0BBE4", "#9D4EDD", "#3C096C"),
    domain = df$importancia_ruta
  )
  
  df <- df %>%
    mutate(
      grosor = scales::rescale(importancia_ruta, to = c(1, 6)),
      color_hex = pal(importancia_ruta),
      mpio_origen_fmt = str_to_title(str_to_lower(as.character(mpio_origen))),
      depto_origen_fmt = str_to_title(str_to_lower(as.character(depto_origen))),
      importancia_txt = sprintf("%.2f%%", importancia_ruta * 100)
    )
  
  # Verificar columnas de coordenadas -----------------------------------------
  df <- df %>% filter(!is.na(routes_coords_str), routes_coords_str != "")
  
  # Convertir rutas a lista de coordenadas ------------------------------------
  rutas_list <- purrr::map(df$routes_coords_str, function(coords_str) {
    coords_str <- as.character(coords_str)
    if (is.na(coords_str) || coords_str == "") return(NULL)
    coords <- str_split(coords_str, ";")[[1]]
    mat <- do.call(rbind, str_split(coords, ","))
    suppressWarnings(matrix(as.numeric(mat), ncol = 2, byrow = FALSE))
  })
  
  # Crear mapa base ------------------------------------------------------------
  titulo <- paste0(
    ifelse(!is.null(Producto) && Producto != "", paste0("Producto: ", Producto, " | "), ""),
    ifelse(!is.null(Año) && Año != "", paste0("Año: ", Año, " | "), ""),
    ifelse(!is.null(Mes) && Mes != "", paste0("Mes: ", Mes), "")
  )
  
  titulo_html <- HTML(paste0("<strong>", titulo, "</strong>"))
  
  map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    addTiles() %>%
    addControl(as.character(titulo_html), position = "topright")
  
  # Filtrar rutas válidas ------------------------------------------------------
  valid_idx <- which(!purrr::map_lgl(rutas_list, is.null))
  
  # Añadir líneas --------------------------------------------------------------
  for (i in valid_idx) {
    coords <- rutas_list[[i]]
    if (nrow(coords) < 2 || any(is.na(coords))) next
    
    label_txt <- paste0(
      "Origen: ", df$mpio_origen_fmt[i], " (", df$depto_origen_fmt[i], ")<br>",
      "Importancia de la ruta: ", df$importancia_txt[i]
    )
    
    map <- map %>%
      addPolylines(
        lng = coords[, 1],
        lat = coords[, 2],
        color = df$color_hex[i],
        weight = df$grosor[i],
        opacity = 0.8,
        label = htmltools::HTML(label_txt),
        labelOptions = labelOptions(noHide = FALSE, direction = "top")
      )
  }
  
  # Leyenda --------------------------------------------------------------------
  map %>%
    addLegend(
      "bottomright",
      pal = pal,
      values = df$importancia_ruta,
      title = "Importancia de la ruta (%)",
      labFormat = labelFormat(suffix = "%", transform = function(x) x * 100),
      opacity = 0.9
    )
}

################################################################################-
# Ejemplo de uso directo (fuera de Shiny)
################################################################################-
mapa <- graficar_rutas(data_merged, Año = "2024", Mes = "11", Producto = "Maracuyá")
mapa
