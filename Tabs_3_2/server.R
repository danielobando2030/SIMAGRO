################################################################################-
# Proyecto FAO - VP - 2025
# Procesamiento datos SIPSA - Comparación con Bogotá
################################################################################-
# Autores: Luis Miguel García, Laura Quintero, Juliana Lalinde
# Última modificación: 07/11/2025
################################################################################-

library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(sf)
library(stringr)
library(glue)
library(webshot2)
library(htmlwidgets)
library(rmarkdown)

# Cargar función y datos
source("3_2b_precios_diferencias_mapa_funciones.R")

################################################################################-
# SERVER
################################################################################-
server <- function(input, output, session) {
  
  # --- 1. Render inicial vacío ---
  output$grafico <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -74.1, lat = 4.6, zoom = 6)
  })
  
  # --- 2. Reactive principal ---
  datos_reactivos <- reactive({
    req(input$anio, input$mes, input$producto)
    res <- tryCatch({
      mapa_dif(
        Anio = if (input$anio == "todo") NULL else input$anio,
        Mes = if (input$mes == "todo") NULL else input$mes,
        Producto = if (input$producto == "todo") NULL else input$producto
      )
    }, error = function(e) NULL)
    if (is.null(res) || is.null(res$datos) || nrow(res$datos) == 0) return(NULL)
    res
  })
  
  # --- 3. Actualizar mapa ---
  observe({
    res <- datos_reactivos()
    req(res)
    
    shp <- shapefile_global %>%
      left_join(res$datos, by = "cod_depto")
    
    shp$nombre_final <- coalesce(shp$departamento.y, shp$departamento.x, shp$departamento, as.character(shp$cod_depto))
    shp$nombre_final <- str_to_title(str_to_lower(shp$nombre_final))
    
    pal <- colorNumeric(
      palette = colorRampPalette(c("#3F007D", "#8C6BB1", "#CBC9E2", "#F2F0F7"))(100),
      domain = shp$comp,
      na.color = "#D9D9D9"
    )
    
    shp$tooltip_text <- ifelse(
      is.na(shp$comp),
      paste0("<strong>", shp$nombre_final, "</strong><br>Sin datos disponibles"),
      paste0("<strong>", shp$nombre_final, "</strong><br>Diferencia de precio: $", round(shp$comp))
    )
    
    leafletProxy("grafico", data = shp) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~ifelse(is.na(comp), "#D9D9D9", pal(comp)),
        fillOpacity = 0.8,
        color = "#D5D5D5",
        weight = 1,
        popup = ~tooltip_text,
        label = ~htmlEscape(nombre_final),
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      ) %>%
      clearControls() %>%
      addLegend(
        pal = pal,
        values = ~shp$comp,
        opacity = 0.7,
        title = "Diferencia del precio"
      )
  })
  
  # --- 4. Mensajes dinámicos ---
  observe({
    res <- datos_reactivos()
    req(res)
    output$mensaje1 <- renderText({
      glue("El lugar más costoso para comprar {if (input$producto != 'todo') input$producto else 'alimentos'} fue {res$ciudad_max}, con una diferencia de ${format(res$precio_max, big.mark='.', decimal.mark=',')} respecto a Bogotá.")
    })
    output$mensaje2 <- renderText({
      glue("{res$ciudad_min} ofreció el precio más bajo {if (input$producto != 'todo') paste0('para ', input$producto) else 'para alimentos'}, con una diferencia de ${format(res$precio_min, big.mark='.', decimal.mark=',')} menos que Bogotá.")
    })
  })
  
  # --- 5. Descarga de datos ---
  output$descargarDatos <- downloadHandler(
    filename = function() glue("datos_{Sys.Date()}.csv"),
    content = function(file) {
      res <- datos_reactivos()
      write_csv(res$datos, file)
    }
  )
  
  # --- 6. Descargar mapa como imagen ---
  output$descargar <- downloadHandler(
    filename = function() glue("mapa_{Sys.Date()}.png"),
    content = function(file) {
      res <- datos_reactivos()
      req(res)
      tempFile <- tempfile(fileext = ".html")
      saveWidget(res$grafico, tempFile, selfcontained = TRUE)
      webshot2::webshot(tempFile, file = file, delay = 2, vwidth = 1600, vheight = 1000)
    }
  )
  
  # --- 7. Generar informe institucional FAO ---
  output$descargarInforme <- downloadHandler(
    filename = function() glue("informe_precios_vs_bogota_{Sys.Date()}.pdf"),
    content = function(file) {
      res <- datos_reactivos()
      req(res)
      
      # Guardar el mapa actual como imagen temporal
      temp_html <- tempfile(fileext = ".html")
      temp_png  <- tempfile(fileext = ".png")
      saveWidget(res$grafico, temp_html, selfcontained = TRUE)
      webshot2::webshot(temp_html, file = temp_png, delay = 2, vwidth = 1600, vheight = 1000)
      
      # Crear entorno limpio
      e <- new.env(parent = globalenv())
      
      # Renderizar el PDF estableciendo el directorio raíz del knit
      rmarkdown::render(
        input = file.path(getwd(), "informe.Rmd"),
        output_file = normalizePath(file),
        params = list(
          producto    = input$producto,
          anio        = input$anio,
          mes         = input$mes,
          mensaje1    = glue("El lugar más costoso para comprar {if (input$producto != 'todo') input$producto else 'alimentos'} fue {res$ciudad_max}, con una diferencia de ${format(res$precio_max, big.mark='.', decimal.mark=',')} respecto a Bogotá."),
          mensaje2    = glue("{res$ciudad_min} ofreció el precio más bajo {if (input$producto != 'todo') paste0('para ', input$producto) else 'para alimentos'}, con una diferencia de ${format(res$precio_min, big.mark='.', decimal.mark=',')} menos que Bogotá."),
          mapa_png    = temp_png,
          tabla_datos = res$datos
        ),
        envir = e,
        knit_root_dir = getwd(),         # 👈 fuerza a buscar logos y fuentes aquí
        encoding = "UTF-8",
        output_format = "pdf_document"   # 👈 fuerza a usar PDF (no HTML fallback)
      )
    },
    contentType = "application/pdf"
  )
  
  # --- 8. Botón restablecer ---
  observeEvent(input$reset, {
    updateSelectInput(session, "anio", selected = 2024)
    updateSelectInput(session, "mes", selected = 1)
    updateSelectInput(session, "producto", selected = "Aguacate")
  })
}
