################################################################################-
# Proyecto FAO - VP - 2025
# SERVER - Comparativo de Precios (Producto y Año)
################################################################################-
# Autores: Luis Miguel García, Laura Quintero, Juliana Lalinde
# Última modificación: 2025/11/xx
################################################################################-

rm(list = ls())

library(shiny)
library(dplyr)
library(plotly)
library(glue)
library(htmlwidgets)
library(webshot2)
library(rmarkdown)

options(scipen = 999)
options(encoding = "UTF-8")

# ============================================================
# Cargar funciones (SOLO DEFINICIONES)
# ============================================================
source("3_4b_precios_diferencias_municipios_funciones.R", encoding = "UTF-8")

################################################################################-
# SERVER
################################################################################-

server <- function(input, output, session) {
  
  ###############################################################################
  # Reactivo principal
  ###############################################################################
  resultado <- reactive({
    req(input$anio, input$producto)
    
    res <- diferencias_precios_interactivo(
      opcion1 = 0,                         # por producto
      opcion2 = as.numeric(input$anio),    # año
      opcion4 = input$producto             # producto
    )
    
    if (is.null(res$datos) || nrow(res$datos) == 0) {
      return(NULL)
    }
    
    res
  })
  
  ###############################################################################
  # Gráfico interactivo (PLOTLY)
  ###############################################################################
  output$grafico <- renderPlotly({
    res <- resultado()
    
    if (is.null(res)) {
      return(
        plot_ly() %>%
          layout(
            annotations = list(
              text = "No hay datos disponibles para la selección.",
              x = 0.5, y = 0.5,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 18, color = "#DBC21F")
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }
    
    res$grafico
  })
  
  ###############################################################################
  # Subtítulo
  ###############################################################################
  output$subtitulo <- renderText({
    res <- resultado()
    
    if (is.null(res)) {
      return("Visualizando diferencias promedio de precios por producto y año.")
    }
    
    glue(
      "Producto: {input$producto} | Año: {input$anio}. ",
      "El precio más bajo se observó en {res$ciudad_min} (",
      res$precio_min,
      " pesos menos que Bogotá) y el más alto en {res$ciudad_max} (",
      res$precio_max,
      " pesos más que Bogotá)."
    )
  })
  
  ###############################################################################
  # Mensaje lateral
  ###############################################################################
  output$mensaje1 <- renderText({
    res <- resultado()
    
    if (is.null(res)) {
      return("No hay información disponible.")
    }
    
    avg_sd <- mean(res$datos$dev, na.rm = TRUE)
    
    glue(
      "La desviación estándar promedio fue de ",
      formatC(avg_sd, format = "f", digits = 0, big.mark = ".", decimal.mark = ","),
      ", indicando el grado de variabilidad de precios entre ciudades."
    )
  })
  
  ###############################################################################
  # Descargar datos CSV
  ###############################################################################
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste0("datos_comparativo_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <- resultado()
      req(res)
      write.csv(res$datos, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  ###############################################################################
  # Descargar gráfica PNG (INTERACTIVA)
  ###############################################################################
  output$descargar <- downloadHandler(
    filename = function() {
      paste0("grafico_comparativo_", Sys.Date(), ".png")
    },
    content = function(file) {
      res <- resultado()
      req(res)
      
      tmp_html <- tempfile(fileext = ".html")
      tmp_png  <- tempfile(fileext = ".png")
      
      htmlwidgets::saveWidget(
        widget = plotly::as_widget(res$grafico),
        file   = tmp_html,
        selfcontained = TRUE
      )
      
      webshot2::webshot(
        tmp_html,
        file   = tmp_png,
        vwidth = 2800,
        vheight = 1800,
        zoom = 1.5,
        delay = 1
      )
      
      file.copy(tmp_png, file, overwrite = TRUE)
    }
  )
  
  # ============================================================
  # Texto para informe PDF
  # ============================================================
  
  resumen_reactivo <- reactive({
    res <- resultado()
    if (is.null(res)) return("")
    
    glue(
      "Producto: {input$producto} | Año: {input$anio}. ",
      "El precio más bajo se observó en {res$ciudad_min} (",
      res$precio_min,
      " pesos menos que Bogotá) y el más alto en {res$ciudad_max} (",
      res$precio_max,
      " pesos más que Bogotá)."
    )
  })
  
  mensaje1_reactivo <- reactive({
    res <- resultado()
    if (is.null(res)) return("")
    
    avg_sd <- mean(res$datos$dev, na.rm = TRUE)
    
    glue(
      "La desviación estándar promedio fue de ",
      formatC(avg_sd, format = "f", digits = 0,
              big.mark = ".", decimal.mark = ","),
      ", indicando el grado de variabilidad de precios entre ciudades."
    )
  })
  
  ###############################################################################
  # Generar informe PDF FAO (GRÁFICO ESTÁTICO)
  ###############################################################################
  output$report <- downloadHandler(
    filename = function() {
      paste0("informe_comparativo_", input$producto, "_", input$anio, ".pdf")
    },
    content = function(file) {
      res <- resultado()
      req(res)
      
      # --- Gráfico estático ggplot ---
      grafico_plano <- diferencias_precios_estatico(res$datos)
      
      temp_pdf <- tempfile(fileext = ".pdf")
      
      rmarkdown::render(
        input         = file.path(getwd(), "informe.Rmd"),
        output_file   = temp_pdf,
        output_format = "pdf_document",
        params = list(
          producto = input$producto,
          anio     = input$anio,
          datos    = res$datos,
          plot     = grafico_plano,
          resumen  = resumen_reactivo(),
          mensaje1 = mensaje1_reactivo()
        ),
        envir    = new.env(parent = globalenv()),
        encoding = "UTF-8"
      )
      
      file.copy(temp_pdf, file, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )
  ###############################################################################
  # Botón Reset
  ###############################################################################
  observeEvent(input$reset, {
    updateSelectInput(session, "anio", selected = "2024")
    updateSelectInput(session, "producto", selected = "Aguacate")
  })
  
}
