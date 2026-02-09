################################################################################
# Proyecto FAO - VP - 2025
# SERVER - Bandas de precios normalizados con informe FAO (plot estático)
################################################################################

library(shiny)
library(plotly)
library(dplyr)
library(zoo)
library(lubridate)
library(rmarkdown)
library(glue)

options(scipen = 999)
options(encoding = "UTF-8")

# ============================================================
# Cargar funciones (SOLO DEFINICIONES)
# ============================================================
source("3_6b_precios_rangos_desviaciones.R", encoding = "UTF-8")

################################################################################
# SERVER
################################################################################

server <- function(input, output, session) {
  
  # -----------------------------------------------------------
  # Valores por defecto
  # -----------------------------------------------------------
  updateSelectInput(session, "producto", selected = "Aguacate")
  updateSelectInput(session, "anio", selected = "2024")
  
  # -----------------------------------------------------------
  # Datos filtrados
  # -----------------------------------------------------------
  data_filtrada <- reactive({
    req(input$producto, input$anio)
    
    df <- data %>% filter(producto == input$producto)
    
    if (input$anio != "todos") {
      df <- df %>% filter(year(fecha) == as.numeric(input$anio))
    }
    
    df
  })
  
  # -----------------------------------------------------------
  # Gráfico interactivo (PLOTLY)
  # -----------------------------------------------------------
  output$grafico <- renderPlotly({
    df <- data_filtrada()
    
    if (nrow(df) == 0) {
      showNotification(
        "⚠️ No hay datos disponibles para ese producto y año.",
        type = "warning"
      )
      return(NULL)
    }
    
    anio_sel <- if (input$anio == "todos") NULL else as.numeric(input$anio)
    
    visualizar_bandas_plotly(
      df,
      producto_sel = input$producto,
      anio_sel     = anio_sel
    )
  })
  
  # -----------------------------------------------------------
  # Descargar gráfico interactivo PNG
  # -----------------------------------------------------------
  output$descargarGrafico <- downloadHandler(
    filename = function() {
      paste0("bandas_precio_", gsub(" ", "_", input$producto), "_", input$anio, ".png")
    },
    content = function(file) {
      df <- data_filtrada()
      req(nrow(df) > 0)
      
      anio_sel <- if (input$anio == "todos") NULL else as.numeric(input$anio)
      graf <- visualizar_bandas_plotly(df, input$producto, anio_sel)
      
      grafico_plano <- diferencias_precios_estatico(res$datos)
      
      # Usa ggsave para guardar el gráfico
      ggplot2::ggsave(filename = file, plot = grafico_plano, width = 13, height = 7, dpi = 200)
        }
  )
  
  
  output$descargarGrafico <- downloadHandler(
    filename = function() {
      paste("IND2_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      # Forzar la ejecución de la función reactiva
      
      df <- data_filtrada()
      req(nrow(df) > 0)
      anio_sel <- if (input$anio == "todos") NULL else as.numeric(input$anio)
      grafico_plano <- visualizar_bandas_estatico(df, input$producto, anio_sel)
      # Usa ggsave para guardar el gráfico
      ggplot2::ggsave(filename = file, plot = grafico_plano, width = 13, height = 7, dpi = 200)
    }
  )
  
  
  # -----------------------------------------------------------
  # Descargar datos CSV
  # -----------------------------------------------------------
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste0("bandas_datos_", gsub(" ", "_", input$producto), "_", input$anio, ".csv")
    },
    content = function(file) {
      df <- data_filtrada()
      
      if (nrow(df) == 0) {
        write.csv(
          data.frame(Mensaje = "No hay datos disponibles"),
          file,
          row.names = FALSE
        )
      } else {
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
  
  # -----------------------------------------------------------
  # Panel lateral: días atípicos
  # -----------------------------------------------------------
  output$diasAtipicos <- renderUI({
    df <- data_filtrada()
    if (nrow(df) == 0) return(HTML("<p>No hay información disponible.</p>"))
    
    df_proc <- df %>%
      mutate(anio = year(fecha)) %>%
      arrange(fecha) %>%
      group_by(anio) %>%
      mutate(
        media_20 = rollapply(precio, 20, mean, align = "right", fill = NA, na.rm = TRUE),
        sd_20    = rollapply(precio, 20, sd,   align = "right", fill = NA, na.rm = TRUE),
        precio_norm = precio - media_20,
        banda_sup =  2 * sd_20,
        banda_inf = -2 * sd_20,
        estado = case_when(
          is.na(precio_norm) ~ NA_character_,
          precio_norm > banda_sup | precio_norm < banda_inf ~ "Atípico",
          TRUE ~ "Normal"
        )
      ) %>%
      ungroup() %>%
      filter(estado == "Atípico")
    
    if (nrow(df_proc) == 0) {
      HTML("<p>✔️ No se registran días con valores atípicos para este año.</p>")
    } else {
      df_proc <- df_proc %>%
        mutate(
          fecha_label = format(fecha, "%d de %B"),
          valor = paste0("$", format(round(precio, 0), big.mark = ".", decimal.mark = ","))
        )
      
      lista <- paste0(
        "<li>", df_proc$fecha_label, ": ", df_proc$valor, "</li>",
        collapse = ""
      )
      
      HTML(glue("
        <div class='panel-atipicos'>
          <p><b>Se detectaron {nrow(df_proc)} días atípicos:</b></p>
          <ul>{lista}</ul>
        </div>
      "))
    }
  })
  
  # -----------------------------------------------------------
  # Generar informe PDF (GRÁFICO ESTÁTICO)
  # -----------------------------------------------------------
  output$descargarPDF <- downloadHandler(
    filename = function() {
      paste0("informe_bandas_precio_", input$producto, "_", input$anio, ".pdf")
    },
    content = function(file) {
      
      df <- data_filtrada()
      req(nrow(df) > 0)
      
      anio_sel <- if (input$anio == "todos") NULL else as.numeric(input$anio)
      
      # =====================================================
      # GRÁFICO ESTÁTICO
      # =====================================================
      grafico_plano <- visualizar_bandas_estatico(
        df,
        producto_sel = input$producto,
        anio_sel     = anio_sel
      )
      
      # =====================================================
      # DATA PROCESADA (MISMA LÓGICA QUE PANEL)
      # =====================================================
      df_proc <- df %>%
        mutate(anio = year(fecha)) %>%
        arrange(fecha) %>%
        group_by(anio) %>%
        mutate(
          media_20 = rollapply(precio, 20, mean, align = "right", fill = NA, na.rm = TRUE),
          sd_20    = rollapply(precio, 20, sd,   align = "right", fill = NA, na.rm = TRUE),
          precio_norm = precio - media_20,
          banda_sup =  2 * sd_20,
          banda_inf = -2 * sd_20,
          estado = case_when(
            is.na(precio_norm) ~ NA_character_,
            precio_norm > banda_sup | precio_norm < banda_inf ~ "Atípico",
            TRUE ~ "Normal"
          )
        ) %>%
        ungroup()
      
      if (!is.null(anio_sel)) {
        df_proc <- df_proc %>% filter(anio == anio_sel)
      }
      
      # =====================================================
      # MENSAJE RESUMEN
      # =====================================================
      df_atip <- df_proc %>% filter(estado == "Atípico")
      n_atipicos <- nrow(df_atip)
      
      mensaje1 <- if (n_atipicos == 0) {
        glue("No se registraron días con precios atípicos durante {input$anio}.")
      } else {
        glue("Se detectaron {n_atipicos} días con precios atípicos durante {input$anio}.")
      }
      
      # =====================================================
      # LISTA FAO
      # =====================================================
      if (n_atipicos == 0) {
        
        lista_atipicos <- "No se identificaron días atípicos."
        
      } else {
        
        df_atip2 <- df_atip %>%
          mutate(
            dia = format(fecha, "%d de %B"),
            precio_fmt = paste0(
              "\\$", format(round(precio, 0), big.mark=".", decimal.mark=",")
            )
          )
        
        lista_atipicos <- paste0(
          "\\begin{itemize}\n",
          paste0("  \\item ", df_atip2$dia, ": ", df_atip2$precio_fmt, collapse = "\n"),
          "\n\\end{itemize}"
        )
      }
      
      # =====================================================
      # COPIAR INSUMOS
      # =====================================================
      tempReport <- file.path(tempdir(), "informe.Rmd")
      file.copy("informe.Rmd", tempReport, overwrite = TRUE)
      
      file.copy("Prompt-Regular.ttf", file.path(tempdir(), "Prompt-Regular.ttf"), overwrite = TRUE)
      file.copy("Prompt-Black.ttf",   file.path(tempdir(), "Prompt-Black.ttf"),   overwrite = TRUE)
      
      dir.create(file.path(tempdir(), "www"), showWarnings = FALSE)
      file.copy("www/logo_3.png", file.path(tempdir(), "www/logo_3.png"), overwrite = TRUE)
      file.copy("www/logo_4.png", file.path(tempdir(), "www/logo_4.png"), overwrite = TRUE)
      
      # =====================================================
      # RENDER FINAL
      # =====================================================
      rmarkdown::render(
        input = tempReport,
        output_format = "pdf_document",
        output_file = file,
        params = list(
          producto       = input$producto,
          anio           = input$anio,
          plot           = grafico_plano,
          datos          = df_proc,
          mensaje1       = mensaje1,
          lista_atipicos = lista_atipicos
        ),
        envir = new.env(parent = globalenv())
      )
    },
    contentType = "application/pdf"
  )
  
  # -----------------------------------------------------------
  # Reset
  # -----------------------------------------------------------
  observeEvent(input$reset, {
    updateSelectInput(session, "producto", selected = "Aguacate")
    updateSelectInput(session, "anio", selected = "2024")
  })
}
