################################################################################-
# Proyecto FAO - VP - 2025
# SERVER - Elasticidad, precios y abastecimiento de alimentos
################################################################################-
# Autores: Luis Miguel García, Laura Quintero, Juliana Lalinde
# Última modificación: 08/11/2025
################################################################################-

rm(list = ls())

# Paquetes
pacman::p_load(
  readr, lubridate, dplyr, ggplot2, zoo, readxl, glue,
  tidyverse, gridExtra, corrplot, plotly, arrow, shiny,
  htmlwidgets, rmarkdown, shinyscreenshot, kableExtra
)

options(scipen = 999)
options(encoding = "UTF-8")

################################################################################-
# Cargar función y base de datos (OBLIGATORIO)
################################################################################-

# AÑADE ESTO AQUÍ (Entorno Global del Server)
meses_es <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
              "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

source("3_3b_funcion_elasticidad.R", encoding = "UTF-8")  
# Este archivo DEBE declarar: data y grafico_producto_anual()

################################################################################-
# SERVER
################################################################################-

server <- function(input, output, session) {
  
  ###############################################################################
  # REACTIVO PRINCIPAL
  ###############################################################################
  resultado <- reactive({
    req(input$producto, input$anio)
    
    if (input$anio == "todos") {
      
      df <- data %>%
        mutate(mes_y_ano = as.Date(mes_y_ano)) %>%
        filter(producto == input$producto)
      
      if (nrow(df) == 0) return(NULL)
      
      anio_sel <- max(year(df$mes_y_ano), na.rm = TRUE)
      
    } else {
      
      anio_sel <- as.numeric(input$anio)
      
      df <- data %>%
        mutate(mes_y_ano = as.Date(mes_y_ano)) %>%
        filter(
          producto == input$producto,
          year(mes_y_ano) == anio_sel
        )
      
      if (nrow(df) == 0) return(NULL)
    }
    
    df_final <- preparar_df_producto_anual(
      data,
      input$producto,
      anio_sel
    )
    
    if (is.null(df_final)) return(NULL)
    
    list(
      df_final = df_final,
      datos    = df,
      anio     = anio_sel
    )
  })
  
  ###############################################################################
  # MOSTRAR GRÁFICO PLOTLY
  ###############################################################################
  output$grafico <- renderPlotly({
    res <- resultado()
    req(res)
    
    grafico_producto_anual_plotly(res$df_final)
  })
  ###############################################################################
  # SUBTÍTULO
  ###############################################################################
  output$subtitulo <- renderText({
    res <- resultado()
    if (is.null(res)) return("No hay datos disponibles.")
    
    precios <- res$datos$precio_prom
    if (all(is.na(precios))) return("No hay precios disponibles para generar el subtítulo.")
    
    max_idx <- which.max(precios)
    if (is.na(max_idx) || length(max_idx) == 0) {
      return("No fue posible identificar el mes de mayor precio.")
    }
    
    mes_max <- res$datos$mes_y_ano[max_idx]
    mes_txt <- format(mes_max, "%B")
    precio_max <- round(precios[max_idx], 0)
    
    glue("En {mes_txt} se observó el precio promedio más alto (${format(precio_max, big.mark='.', decimal.mark=',')}).")
  })
  
  ###############################################################################
  # DESCARGAR GRÁFICA PNG
  ###############################################################################
  output$descargar <- downloadHandler(
    filename = function() {
      paste("IND2_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      # Forzar la ejecución de la función reactiva
  
      
      res <- resultado()
      req(res)
      
      # Usa ggsave para guardar el gráfico
      ggplot2::ggsave(filename = file, plot = grafico_producto_anual_plano(res$df_final), width = 13, height = 7, dpi = 200)
    }
  )
  
  ###############################################################################
  # MENSAJE 1 (para UI)
  ###############################################################################
  output$mensaje1 <- renderText({
    res <- resultado()
    if (is.null(res)) return("No hay información disponible.")
    
    elast <- res$datos$elasticidad
    if (all(is.na(elast))) return("No existen datos de elasticidad para esta selección.")
    
    elast_media <- mean(elast, na.rm = TRUE)
    
    # === FORMATO LATAM (coma decimal) ===
    elast_media_txt <- gsub("\\.", ",", sprintf('%.2f', elast_media))
    
    glue(
      "La elasticidad promedio del producto fue de {elast_media_txt}, ",
      "indicando el grado de sensibilidad del precio ante variaciones en la oferta."
    )
  })
  
  ###############################################################################
  # MENSAJE 1 (versión para el INFORME PDF)
  ###############################################################################
  mensaje1_reactivo <- reactive({
    res <- resultado()
    req(res)
    
    elast <- res$datos$elasticidad
    if (all(is.na(elast))) return("No existen datos de elasticidad para esta selección.")
    
    elast_media <- mean(elast, na.rm = TRUE)
    
    # === FORMATO LATAM ===
    elast_media_txt <- gsub("\\.", ",", sprintf('%.2f', elast_media))
    
    glue(
      "La elasticidad promedio del producto fue de {elast_media_txt}, ",
      "indicando el grado de sensibilidad del precio ante variaciones en la oferta."
    )
  })
  
  ###############################################################################
  # SUBTÍTULO (versión para el INFORME PDF)
  ###############################################################################
  subtitulo_reactivo <- reactive({
    res <- resultado()
    req(res)
    
    precios <- res$datos$precio_prom
    if (all(is.na(precios))) {
      return("No hay precios disponibles para generar el subtítulo.")
    }
    
    max_idx <- which.max(precios)
    if (is.na(max_idx)) return("No fue posible identificar el mes de mayor precio.")
    
    mes_max <- res$datos$mes_y_ano[max_idx]
    mes_txt <- format(mes_max, "%B")
    precio_max <- round(precios[max_idx], 0)
    
    glue("En {mes_txt} se observó el precio promedio más alto (${format(precio_max, big.mark='.', decimal.mark=',')}).")
  })
  
  ###############################################################################
  # BOTÓN RESET
  ###############################################################################
  observeEvent(input$reset, {
    updateSelectInput(session, "producto", selected = "Aguacate")
    updateSelectInput(session, "anio", selected = "2024")
  })
  
  ###############################################################################
  # DESCARGAR DATOS CSV
  ###############################################################################
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
  
  ###############################################################################
  # GENERAR INFORME PDF INSTITUCIONAL (GRÁFICO PLANO)
  ###############################################################################
  output$report <- downloadHandler(
    filename = function() {
      paste0("informe_elasticidad_", input$producto, "_", input$anio, ".pdf")
    },
    content = function(file) {
      res <- resultado()
      req(res)
      
      # --------------------------------------------------
      # 1. Crear GRÁFICO PLANO (ggplot)
      # --------------------------------------------------
      df_final <- preparar_df_producto_anual(
        data,
        input$producto,
        res$anio
      )
      print(df_final)
      
      grafico_plano <- grafico_producto_anual_plano(df_final)
      
      # --------------------------------------------------
      # 2. Renderizar informe PDF (SIN webshot, SIN PNG)
      # --------------------------------------------------
      temp_pdf <- tempfile(fileext = ".pdf")
      
      rmarkdown::render(
        input = file.path(getwd(), "informe.Rmd"),
        output_file = temp_pdf,
        output_format = rmarkdown::pdf_document(latex_engine = "xelatex"),
        encoding = "UTF-8",
        params = list(
          datos     = res$datos,
          plot      = grafico_plano,   # 👈 ggplot directo
          producto  = input$producto,
          anio      = input$anio,
          mensaje1  = mensaje1_reactivo()
        ),
        envir = new.env(parent = globalenv())
      )
      
      file.copy(temp_pdf, file, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )
  
}
