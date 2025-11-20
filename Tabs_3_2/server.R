################################################################################-
# Proyecto FAO - VP - 2025
# Procesamiento datos SIPSA - Comparación con Bogotá
################################################################################-
# Autores: Luis Miguel García, Laura Quintero, Juliana Lalinde
# Última modificación: 15/11/2025
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
  
  # --- 1. Mapa inicial vacío ---
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
        Anio     = if (input$anio == "todo") NULL else input$anio,
        Mes      = if (input$mes  == "todo") NULL else input$mes,
        Producto = if (input$producto == "todo") NULL else input$producto
      )
    }, error = function(e) {
      message("Error en mapa_dif(): ", e$message)
      NULL
    })
    
    if (is.null(res) || is.null(res$datos) || nrow(res$datos) == 0) return(NULL)
    res
  })
  
  # --- 3. Actualizar mapa ---
  observe({
    res <- datos_reactivos()
    req(res)
    
    shp <- shapefile_global %>%
      left_join(res$datos, by = "cod_depto")
    
    shp$nombre_final <- coalesce(
      shp$departamento.y,
      shp$departamento.x,
      shp$departamento,
      as.character(shp$cod_depto)
    )
    shp$nombre_final <- str_to_title(str_to_lower(shp$nombre_final))
    
    pal <- colorNumeric(
      palette = colorRampPalette(c("#DBC21F", "#B6A534", "#6D673E", "#494634"))(100),
      domain  = shp$comp,
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
        fillColor = ~ ifelse(is.na(comp), "#D9D9D9", pal(comp)),
        fillOpacity = 0.8,
        color = "#D5D5D5",
        weight = 1,
        popup = ~tooltip_text,
        label = ~htmlEscape(nombre_final),
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      ) %>%
      clearControls() %>%
      addLegend(
        pal     = pal,
        values  = ~shp$comp,
        opacity = 0.7,
        title   = "Diferencia del precio"
      )
  })
  
  # --- 4. Mensajes dinámicos ---
  observe({
    res <- datos_reactivos()
    req(res)
    
    output$mensaje1 <- renderText({
      glue(
        "El lugar más costoso para comprar {if (input$producto != 'todo') input$producto else 'alimentos'} ",
        "fue {res$ciudad_max}, con una diferencia de ${format(res$precio_max, big.mark='.', decimal.mark=',')} respecto a Bogotá."
      )
    })
    
    output$mensaje2 <- renderText({
      glue(
        "{res$ciudad_min} ofreció el precio más bajo ",
        "{if (input$producto != 'todo') paste0('para ', input$producto) else 'para alimentos'}, ",
        "con una diferencia de ${format(res$precio_min, big.mark='.', decimal.mark=',')} menos que Bogotá."
      )
    })
  })
  
  # --- 5. Descarga de datos ---
  output$descargarDatos <- downloadHandler(
    filename = function() glue("datos_{Sys.Date()}.csv"),
    content  = function(file) {
      res <- datos_reactivos()
      req(res)
      write_csv(res$datos, file)
    }
  )
  
  # --- 6. Descargar imagen ---
  output$descargar <- downloadHandler(
    filename = function() glue("mapa_{Sys.Date()}.png"),
    content  = function(file) {
      res <- datos_reactivos()
      req(res)
      temphtml <- tempfile(fileext = ".html")
      saveWidget(res$grafico, temphtml, selfcontained = TRUE)
      webshot2::webshot(temphtml, file, delay = 2, vwidth = 1600, vheight = 1000)
    }
  )
  
  # --- 7. Generar informe institucional PDF (xelatex) ---
  output$descargarInforme <- downloadHandler(
    filename = function() glue("informe_precios_vs_bogota_{Sys.Date()}.pdf"),
    content  = function(file) {
      
      res <- datos_reactivos()
      req(res)
      
      mensaje1_texto <- glue(
        "El lugar más costoso para comprar {if (input$producto != 'todo') input$producto else 'alimentos'} ",
        "fue {res$ciudad_max}, con una diferencia de ${format(res$precio_max, big.mark='.', decimal.mark=',')} respecto a Bogotá."
      )
      mensaje2_texto <- glue(
        "{res$ciudad_min} ofreció el precio más bajo ",
        "{if (input$producto != 'todo') paste0('para ', input$producto) else 'para alimentos'}, ",
        "con una diferencia de ${format(res$precio_min, big.mark='.', decimal.mark=',')} menos que Bogotá."
      )
      
      # Convertir Leaflet a PNG
      temphtml <- tempfile(fileext = ".html")
      temppng  <- tempfile(fileext = ".png")
      saveWidget(res$grafico, temphtml, selfcontained = TRUE)
      webshot2::webshot(temphtml, temppng, delay = 2, vwidth = 1600, vheight = 1000)
      
      # PDF temporal
      tmp_pdf <- tempfile(fileext = ".pdf")
      
      # Render PDF (siempre xelatex)
      e <- new.env(parent = globalenv())
      rmarkdown::render(
        input = file.path(getwd(), "informe.Rmd"),
        output_file = tmp_pdf,
        params = list(
          producto    = input$producto,
          anio        = if (identical(input$anio, "todo")) NA else input$anio,
          mes         = if (identical(input$mes,  "todo")) NA else input$mes,
          mensaje1    = mensaje1_texto,
          mensaje2    = mensaje2_texto,
          mapa_png    = temppng,
          tabla_datos = res$datos,
          logo_sup    = file.path(getwd(), "www", "logo_3.png"),
          logo_inf    = file.path(getwd(), "www", "logo_2.png")
        ),
        envir = e,
        knit_root_dir = getwd(),
        output_format = rmarkdown::pdf_document(latex_engine = "xelatex")
      )
      
      # Copiar PDF final
      file.copy(tmp_pdf, file, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )
  
  # --- 8. Reset ---
  observeEvent(input$reset, {
    updateSelectInput(session, "anio",  selected = 2024)
    updateSelectInput(session, "mes",   selected = 1)
    updateSelectInput(session, "producto", selected = "Aguacate")
  })
}
