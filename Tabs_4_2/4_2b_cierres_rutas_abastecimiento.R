#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 24/02/2024
#Fecha de ultima modificacion: 24/02/2024
################################################################################-
# Limpiar el entorno de trabajo
# Paquetes 
################################################################################-
pacman::p_load(readr,readxl, dplyr, tidyr, janitor, lubridate, stringr, geosphere,
               arrow, osrm, sf, ggplot2, jsonlite, purrr, leaflet, scales, stringr)
options(scipen = 999)


################################################################################-

## Cargamos la base de datos de origen destino 

## Importancia relativa a carretera

data_merged <- readRDS("data_sin_rutas.rds")
rutas       <- readRDS("rutas_abas.rds")

data_cierres_final <- left_join(data_merged, rutas, by = c("codigo_mpio_origen"))

remove(rutas, data_merged)

## Importancia relativa a carretera

data_cierres_final <- data_cierres_final %>%
                      group_by(anio, mes, producto) %>%
                      mutate(total_kilogramos_año_mes_producto = sum(suma_kg, na.rm = TRUE)) %>%
                      ungroup()


data_cierres_final <- data_cierres_final %>%
                      group_by(anio, mes, producto, codigo_mpio_origen) %>%
                      mutate(total_kilogramos_año_mes_producto_mpio = sum(suma_kg, na.rm = TRUE)) %>%
                      ungroup()


data_cierres_final$importancia_ruta <- data_cierres_final$total_kilogramos_año_mes_producto_mpio/data_cierres_final$total_kilogramos_año_mes_producto


data_cierres_final <- data_cierres_final %>%
  mutate(
    # Calculamos el ángulo (bearing) desde Bogotá hasta el municipio de origen
    bearing = bearing(
      cbind(LONGITUD_BOG, LATITUD_BOG),
      cbind(LONGITUD, LATITUD)
    ),
    
    # Ajustamos los ángulos negativos (para que todos estén entre 0 y 360 grados)
    bearing = ifelse(bearing < 0, bearing + 360, bearing),
    
    # Clasificamos según el ángulo en 8 regiones
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


data_cierres_final <- data_cierres_final %>%
  group_by(producto, region_geo) %>%
  mutate(
    total_region_producto = sum(importancia_ruta, na.rm = TRUE)  # total por producto y región
  ) %>%
  group_by(producto) %>%
  mutate(
    total_producto = sum(importancia_ruta, na.rm = TRUE),        # total global del producto
    prop_region_producto = total_region_producto / total_producto  # proporción regional
  ) %>%
  ungroup()


#######

graficar_rutas_color_importancia <- function(df, Año = NULL, Mes = NULL, Producto = NULL) {
  
  # 1. Filtrar
  if (!is.null(Año)) df <- df %>% filter(anio == Año)
  if (!is.null(Mes)) df <- df %>% filter(mes == Mes)
  if (!is.null(Producto)) df <- df %>% filter(producto == Producto)
  
  # 2. Evitar duplicados
  df <- df %>% distinct(codigo_mpio_destino, codigo_mpio_origen, .keep_all = TRUE)
  if (nrow(df) == 0) return(NULL)
  
  # 3. Grosor
  df <- df %>% mutate(grosor = scales::rescale(as.numeric(prop_region_producto), to = c(1, 10)))
  
  # 4. Colores
  df <- df %>% mutate(
    color_region = case_when(
      region_geo == "Noroccidente"  ~ "#e31a1c",
      region_geo == "Nororiente"    ~ "#ff7f00",
      region_geo == "Norte"         ~ "#6a3d9a",
      region_geo == "Oriente"       ~ "#1f78b4",
      region_geo == "Suroriente"    ~ "#b2df8a",
      region_geo == "Sur"           ~ "#b15928",
      region_geo == "Suroccidente"  ~ "#a6cee3",
      region_geo == "Occidente"     ~ "#33a02c",
      TRUE                          ~ "#999999"
    )
  )
  
  # 5. Asegurar texto
  df <- df %>% mutate(across(everything(), as.character))
  
  # 6. Parsear coordenadas
  rutas_list <- purrr::map(df$routes_coords_str, function(coords_str) {
    if (is.na(coords_str) || coords_str == "") return(NULL)
    coords <- stringr::str_split(coords_str, ";")[[1]]
    mat <- do.call(rbind, stringr::str_split(coords, ","))
    suppressWarnings(matrix(as.numeric(mat), ncol = 2, byrow = FALSE))
  })
  
  # *** 7. ELIMINADO: no crear ningún título ***
  
  # 8. Mapa SIN título ni objetos HTML que generen espacio
  map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    addTiles()
  
  # 9. Añadir líneas
  valid_idx <- which(!purrr::map_lgl(rutas_list, is.null))
  for (i in valid_idx) {
    coords <- rutas_list[[i]]
    lbl_text <- sprintf(
      "Origen: %s (%s). Región: %s. Proporción regional del producto: %.2f%%",
      df$mpio_origen[i],
      df$depto_origen[i],
      df$region_geo[i],
      as.numeric(df$prop_region_producto[i]) * 100
    )
    
    map <- map %>%
      addPolylines(
        lng = coords[, 1],
        lat = coords[, 2],
        color = df$color_region[i],
        weight = as.numeric(df$grosor[i]),
        opacity = 0.85,
        label = lbl_text,
        labelOptions = labelOptions(noHide = FALSE, direction = "top")
      )
  }
  
  # 10. Leyenda
  map <- map %>%
    addLegend(
      position = "bottomright",
      colors = unique(df$color_region),
      labels = unique(df$region_geo),
      title = "Regiones geográficas",
      opacity = 0.9
    )
  
  return(map)
}




# ---- Ejemplo de uso ----
#mapa <- graficar_rutas_color_importancia(
#  data_cierres_final,
#  Año = 2018,
#  Mes = "11",
#  Producto = "Maracuyá"
#)
#mapa


