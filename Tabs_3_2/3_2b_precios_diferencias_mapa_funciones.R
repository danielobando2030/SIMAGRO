###############################
##### Libraries
###############################

pacman::p_load(sf, dplyr, leaflet, janitor, htmltools, lubridate, ggplot2, shiny, stringr)
options(scipen = 999)
options(encoding = "UTF-8")

###############################
##### Load shapefile
###############################


# --- Base principal ---
data <- readRDS("base_precios_vs_bogota_3_2.rds") %>%
  select(producto, year, mes, mes_y_ano, cod_depto, departamento, comparacion_mensual_producto) %>%
  arrange(producto, cod_depto, mes_y_ano) %>%
  mutate(cod_depto = as.numeric(cod_depto))

# --- Panel Bogotá para referencia ---
productos <- c(
  "Aguacate", "Ahuyama", "Arracacha", "Arveja Verde En Vaina", "Banano",
  "Cebolla Cabezona Blanca", "Cebolla Junca", "Chócolo Mazorca", "Coco", "Fríjol verde",
  "Granadilla", "Guayaba", "Habichuela", "Lechuga Batavia", "Limón Común",
  "Limón Tahití", "Lulo", "Mandarina", "Mango Tommy", "Manzana royal gala",
  "Maracuyá", "Mora De Castilla", "Naranja", "Papa criolla", "Papa negra",
  "Papaya Maradol", "Pepino Cohombro", "Pimentón", "Piña", "Plátano Guineo",
  "Plátano Hartón Verde", "Remolacha", "Tomate", "Tomate De Árbol", "Yuca", "Zanahoria"
)

mes_y_ano <- seq(ymd("2012-06-01"), ymd("2025-09-01"), by = "month")
meses_str <- format(mes_y_ano, "%Y-%m")

panel <- expand.grid(producto = productos, mes_y_ano = meses_str) %>%
  arrange(producto, mes_y_ano) %>%
  mutate(
    departamento = "Bogotá, D.C.",
    cod_depto = 11,
    comparacion_mensual_producto = 0,
    year = as.integer(substr(mes_y_ano, 1, 4)),
    mes  = as.integer(substr(mes_y_ano, 6, 7))
  ) %>%
  select(producto, year, mes, mes_y_ano, cod_depto, departamento, comparacion_mensual_producto)

data <- bind_rows(data, panel)
rm(panel)

# --- Shapefile ---
shapefile <- readRDS("shapefile_departamentos_simple.rds")
shapefile$cod_depto <- as.numeric(shapefile$cod_depto)

if (sf::st_crs(shapefile)$epsg != 4326) {
  shapefile <- sf::st_transform(shapefile, crs = 4326)
}

###############################
##### Datos globales
###############################

app_dir <- getwd()
data_global <- data
shapefile_global <- shapefile

###############################
##### Función principal
###############################

mapa_dif <- function(Anio = NULL, Mes = NULL, Producto = NULL) {
  df <- data_global
  shapefile <- shapefile_global
  
  # --- Filtros ---
  if (!is.null(Anio)) df <- df %>% filter(year == Anio)
  if (!is.null(Mes)) df <- df %>% filter(mes == Mes)
  if (!is.null(Producto)) df <- df %>% filter(producto == Producto)
  
  # --- Variables relevantes ---
  df <- df %>%
    rename(comp = comparacion_mensual_producto) %>%
    select(cod_depto, departamento, comp) %>%
    distinct()
  
  if (nrow(df) == 0) {
    validate("No hay datos disponibles")
    return(NULL)
  }
  
  # --- Unión con shapefile ---
  mapa <- shapefile %>% left_join(df, by = "cod_depto")
  
  # --- Crear nombre del departamento unificado y formateado ---
  mapa$departamento_nombre <- dplyr::coalesce(
    mapa$departamento,
    mapa$departamento.x,
    mapa$departamento.y,
    mapa$DPTO_CNMBR,
    mapa$NOMBRE_DPT,
    mapa$NOM_DPTO,
    as.character(mapa$cod_depto)
  )
  
  # Formatear nombres: primera letra mayúscula, resto minúscula
  mapa$departamento_nombre <- str_to_title(str_to_lower(mapa$departamento_nombre))
  
  # --- Tooltip (para clic) ---
  mapa$tooltip_text <- ifelse(
    is.na(mapa$comp),
    paste0("<strong>", mapa$departamento_nombre, "</strong><br>Sin datos disponibles"),
    paste0("<strong>", mapa$departamento_nombre, "</strong><br>Diferencia de precio: $", round(mapa$comp))
  )
  
  # --- Escala de color púrpura (segura) ---
  if (all(is.na(mapa$comp))) {
    max_abs <- 1
  } else {
    max_abs <- max(abs(na.omit(mapa$comp)), na.rm = TRUE)
    if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1
  }
  
  my_palette <- colorNumeric(
    palette = colorRampPalette(c("#3F007D", "#8C6BB1", "#CBC9E2", "#F2F0F7"))(100),
    domain = c(-max_abs, max_abs),
    na.color = "#D9D9D9"
  )
  
  # --- Mapa interactivo ---
  p <- leaflet(mapa) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~my_palette(comp),
      fillOpacity = 0.8,
      color = "#D5D5D5",
      weight = 1,
      popup = ~tooltip_text,
      label = ~htmlEscape(departamento_nombre),  # Hover label
      labelOptions = labelOptions(
        style = list("font-weight" = "bold", "font-size" = "12px"),
        textsize = "15px",
        direction = "auto",
        sticky = TRUE
      ),
      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
    ) %>%
    addLegend(
      pal = my_palette,
      values = ~comp,
      opacity = 0.7,
      title = "Diferencia del precio"
    )
  
  # --- Resumen ---
  precio_max <- round(max(df$comp, na.rm = TRUE))
  precio_min <- round(min(df$comp, na.rm = TRUE))
  ciudad_max <- df$departamento[which.max(df$comp)]
  ciudad_min <- df$departamento[which.min(df$comp)]
  
  return(list(
    grafico = p,
    datos = df,
    precio_max = precio_max,
    precio_min = precio_min,
    ciudad_max = ciudad_max,
    ciudad_min = ciudad_min
  ))
}

###############################
##### Test
###############################

mapa_dif(Anio = 2014, Mes = 1, Producto = "Aguacate")
