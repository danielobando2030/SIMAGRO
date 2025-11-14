################################################################################-
# Proyecto FAO - VP - 2025
# Elasticidad de precios y abastecimiento de alimentos
################################################################################-
# Autores: Luis Miguel García, Laura Quintero, Juliana Lalinde
# Última modificación: 07/11/2025
################################################################################-

# Paquetes
pacman::p_load(
  readr, lubridate, dplyr, ggplot2, zoo, readxl, glue,
  tidyverse, gridExtra, corrplot, plotly, arrow, shiny,
  webshot2, htmlwidgets, rmarkdown, shinyscreenshot, kableExtra
)

options(scipen = 999)
options(encoding = "UTF-8")

################################################################################-
# Cargar función y base de datos
################################################################################-

#source("3_3b_funcion_elasticidad.R")  # Contiene: data y grafico_producto_anual()

################################################################################-
# SERVER
################################################################################-

server <- function(input, output, session) {
  
  # --- Reactivo principal ---
  resultado <- reactive({
    req(input$producto, input$anio)
    
    if (input$anio == "todos") {
      df <- data %>% filter(producto == input$producto)
      anios_df <- sort(unique(year(df$mes_y_ano)))
      if (length(anios_df) == 0) return(NULL)
      fig <- grafico_producto_anual(data, input$producto, anios_df)
    } else {
      anio_sel <- as.numeric(input$anio)
      df <- data %>% filter(producto == input$producto, year(mes_y_ano) == anio_sel)
      fig <- grafico_producto_anual(data, input$producto, anio_sel)
    }
    
    if (is.null(df) || nrow(df) == 0 || is.null(fig)) return(NULL)
    
    list(
      grafico = fig,
      datos   = df
    )
  })
  
  # --- Render del gráfico principal ---
  output$grafico <- plotly::renderPlotly({
    res <- resultado()
    validate(need(!is.null(res), "No hay datos disponibles para el producto y año seleccionados."))
    res$grafico
  })
  
  # --- Subtítulo y mensaje principal ---
  values <- reactiveValues(subtitulo = NULL, mensaje1 = NULL)
  
  output$subtitulo <- renderText({
    res <- resultado()
    if (is.null(res) || nrow(res$datos) == 0) {
      values$subtitulo <- "No hay datos disponibles."
    } else {
      mes_max <- res$datos$mes_y_ano[which.max(res$datos$precio_prom)]
      precio_max <- round(max(res$datos$precio_prom, na.rm = TRUE), 0)
      mes_txt <- format(mes_max, "%B")
      values$subtitulo <- glue(
        "En {mes_txt} se observó el precio promedio más alto (${format(precio_max, big.mark='.', decimal.mark=',')})."
      )
    }
    values$subtitulo
  })
  
  output$mensaje1 <- renderText({
    res <- resultado()
    if (is.null(res) || nrow(res$datos) == 0) {
      values$mensaje1 <- "No hay información disponible."
    } else {
      elast_media <- mean(res$datos$elasticidad, na.rm = TRUE)
      values$mensaje1 <- glue(
        "La elasticidad promedio del producto fue de {sprintf('%.2f', elast_media)}, ",
        "indicando el grado de sensibilidad del precio ante variaciones en la cantidad ofertada."
      )
    }
    values$mensaje1
  })
  
  # --- Botón reset ---
  observeEvent(input$reset, {
    updateSelectInput(session, "producto", selected = "Aguacate")
    updateSelectInput(session, "anio", selected = "todos")
  })
  
  # --- Descargar datos CSV ---
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste0("datos_elasticidad_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <- resultado()
      req(res)
      readr::write_csv(res$datos, file)
    }
  )
  
  # --- Descargar gráfica PNG ---
  output$descargar <- downloadHandler(
    filename = function() {
      paste0("grafico_elasticidad_", Sys.Date(), ".png")
    },
    content = function(file) {
      res <- resultado()
      req(res)
      
      temp_html <- tempfile(fileext = ".html")
      temp_png  <- tempfile(fileext = ".png")
      
      htmlwidgets::saveWidget(
        widget = plotly::as_widget(res$grafico),
        file = temp_html,
        selfcontained = TRUE
      )
      
      webshot2::webshot(
        temp_html,
        file   = temp_png,
        vwidth = 1600,
        vheight = 900,
        delay  = 1
      )
      
      file.copy(temp_png, file, overwrite = TRUE)
    }
  )
  
  # --- Generar informe PDF institucional FAO ---
  output$report <- downloadHandler(
    filename = function() {
      paste0("informe_elasticidad_", input$producto, "_", input$anio, ".pdf")
    },
    content = function(file) {
      res <- resultado()
      req(res)
      
      # Archivo temporal PDF
      temp_pdf <- tempfile(fileext = ".pdf")
      
      rmarkdown::render(
        input         = file.path(getwd(), "informe.Rmd"),
        output_file   = temp_pdf,
        output_format = "pdf_document",      # ✅ Fuerza PDF
        params = list(
          datos     = res$datos,
          plot      = res$grafico,
          producto  = input$producto,
          anio      = input$anio,
          subtitulo = values$subtitulo,
          mensaje1  = values$mensaje1
        ),
        envir         = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        encoding      = "UTF-8"
      )
      
      file.copy(temp_pdf, file, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )
}
