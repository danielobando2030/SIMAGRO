################################################################################-
# Proyecto FAO - VP - 2025
# Comparativo de Precios (Producto y Año)
# Versión institucional con estructura de botones FAO
################################################################################-
# Autores: Luis Miguel García, Laura Quintero, Juliana Lalinde
# Última modificación: 08/11/2025
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

# Función principal
source("3_4b_precios_diferencias_municipios_funciones.R")

################################################################################-
# SERVER
################################################################################-

server <- function(input, output, session) {
  
  # --- FAO configuración LaTeX para entorno Shiny ---
  if (requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex()) {
    tex_path <- tinytex::tinytex_root()
    Sys.setenv(PATH = paste0(tex_path, "/bin/win32;", Sys.getenv("PATH")))
    message("✅ TinyTeX detectado en: ", tex_path)
    message("✅ xelatex disponible en: ", Sys.which("xelatex"))
  }
  
  ###############################################################################
  # Reactivo principal
  ###############################################################################
  resultado <- reactive({
    req(input$anio, input$producto)
    
    res <- diferencias_precios_interactivo(
      opcion1 = 0,
      opcion2 = as.numeric(input$anio),
      opcion4 = input$producto
    )
    
    if (is.null(res$datos) || nrow(res$datos) == 0) {
      return(NULL)
    }
    res
  })
  
  ###############################################################################
  # Gráfico principal
  ###############################################################################
  output$grafico <- renderPlotly({
    res <- resultado()
    
    # --- Si no hay datos o res no trae gráfico válido ---
    if (is.null(res) || is.null(res$grafico)) {
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
    
    # --- Si sí hay gráfico ---
    res$grafico
  })
  
  ###############################################################################
  # Subtítulo y mensaje lateral
  ###############################################################################
  values <- reactiveValues(subtitulo = NULL, mensaje1 = NULL)
  
  output$subtitulo <- renderText({
    res <- resultado()
    if (is.null(res)) {
      values$subtitulo <- "Visualizando diferencias promedio de precios por producto y año seleccionados."
    } else {
      values$subtitulo <- glue(
        "Producto: {input$producto} | Año: {input$anio}. ",
        "El precio más bajo se observó en {res$ciudad_min} (",
        formatC(res$precio_min, big.mark = ",", digits = 0, format = "f"),
        " pesos menos que Bogotá), y el más alto en {res$ciudad_max} (",
        formatC(res$precio_max, big.mark = ",", digits = 0, format = "f"),
        " pesos más que Bogotá)."
      )
    }
    values$subtitulo
  })
  
  output$mensaje1 <- renderText({
    res <- resultado()
    if (is.null(res)) {
      values$mensaje1 <- "No hay información disponible."
    } else {
      avg_sd <- round(mean(res$datos$dev, na.rm = TRUE), 0)
      values$mensaje1 <- glue(
        "La desviación estándar promedio fue de {format(avg_sd, big.mark = ',', decimal.mark='.')}, ",
        "indicando el grado de variabilidad de precios entre ciudades."
      )
    }
    values$mensaje1
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
  # Descargar gráfica PNG
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
        file = tmp_html,
        selfcontained = TRUE
      )
      
      webshot2::webshot(
        tmp_html,
        file   = tmp_png,
        vwidth = 2800,      # mucho más grande
        vheight = 1800,     # alta resolución
        zoom = 1.5,         # más zoom interno de webshot
        delay  = 1
      )
      
      file.copy(tmp_png, file, overwrite = TRUE)
    }
  )
  
  ###############################################################################
  # Generar informe PDF institucional FAO
  ###############################################################################
  output$report <- downloadHandler(
    filename = function() {
      paste0("informe_comparativo_", input$producto, "_", input$anio, ".pdf")
    },
    content = function(file) {
      res <- resultado()
      req(res)
      
      # ---- Convertir el plotly a PNG para el informe ----
      tmp_html <- tempfile(fileext = ".html")
      tmp_png  <- tempfile(fileext = ".png")
      
      htmlwidgets::saveWidget(
        widget = plotly::as_widget(res$grafico),
        file = tmp_html,
        selfcontained = TRUE
      )
      
      webshot2::webshot(
        tmp_html,
        file   = tmp_png,
        vwidth = 1600,
        vheight = 900,
        delay  = 1
      )
      
      temp_pdf <- tempfile(fileext = ".pdf")
      
      rmarkdown::render(
        input         = file.path(getwd(), "informe.Rmd"),
        output_file   = temp_pdf,
        output_format = "pdf_document",
        params = list(
          producto    = input$producto,
          anio        = input$anio,
          datos       = res$datos,
          grafico_png = tmp_png,
          resumen     = values$subtitulo,
          mensaje1    = values$mensaje1    # <--- AÑADIR ESTO
        ),
        envir         = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        encoding      = "UTF-8"
      )
      
      file.copy(temp_pdf, file, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )
  
  ###############################################################################
  # Botón Reset
  ###############################################################################
  observeEvent(input$reset, {
    updateSelectInput(session, "anio", selected = "2014")
    updateSelectInput(session, "producto", selected = "Aguacate")
  })
}
