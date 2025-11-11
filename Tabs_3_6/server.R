################################################################################
# Proyecto FAO - VP - 2025
# SERVER - Bandas de precios normalizados con botones FAO
################################################################################

library(shiny)
library(plotly)
library(dplyr)
library(zoo)
library(lubridate)
library(rmarkdown)
library(webshot2)
library(htmlwidgets)
library(glue)

source("3_6b_precios_rangos_desviaciones.R")  # función visualizar_bandas_plotly

server <- function(input, output, session) {
  
  ##############################################################################
  # --- Valores por defecto ---
  ##############################################################################
  updateSelectInput(session, "producto", selected = "Aguacate")
  updateSelectInput(session, "anio", selected = "2024")
  
  ##############################################################################
  # --- Reactivo: Filtrado de datos ---
  ##############################################################################
  data_filtrada <- reactive({
    req(input$producto, input$anio)
    
    df <- data %>% filter(producto == input$producto)
    if (input$anio != "todos") {
      df <- df %>% filter(year(fecha) == as.numeric(input$anio))
    }
    df
  })
  
  ##############################################################################
  # --- Render del gráfico principal ---
  ##############################################################################
  output$grafico <- renderPlotly({
    df <- data_filtrada()
    if (nrow(df) == 0) {
      showNotification("⚠️ No hay datos disponibles para ese producto y año.", type = "warning")
      return(NULL)
    }
    anio_sel <- if (input$anio == "todos") NULL else as.numeric(input$anio)
    visualizar_bandas_plotly(df, producto_sel = input$producto, anio_sel = anio_sel)
  })
  
  ##############################################################################
  # --- 📉 Descargar gráfica como PNG ---
  ##############################################################################
  output$descargarGrafico <- downloadHandler(
    filename = function() {
      paste0("bandas_precio_", gsub(" ", "_", input$producto), "_", input$anio, ".png")
    },
    content = function(file) {
      df <- data_filtrada()
      if (nrow(df) == 0) stop("No hay datos para exportar.")
      graf <- visualizar_bandas_plotly(df, producto_sel = input$producto, anio_sel = input$anio)
      tmp_html <- tempfile(fileext = ".html")
      tmp_png  <- tempfile(fileext = ".png")
      htmlwidgets::saveWidget(plotly::as_widget(graf), tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, file = tmp_png, vwidth = 1600, vheight = 900, delay = 1)
      file.copy(tmp_png, file, overwrite = TRUE)
    }
  )
  
  ##############################################################################
  # --- 💾 Descargar datos CSV ---
  ##############################################################################
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste0("bandas_datos_", gsub(" ", "_", input$producto), "_", input$anio, ".csv")
    },
    content = function(file) {
      df <- data_filtrada()
      if (nrow(df) == 0) {
        write.csv(data.frame(Mensaje = "No hay datos disponibles"), file, row.names = FALSE)
      } else {
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
  
  ##############################################################################
  # --- Panel lateral: días con precios atípicos ---
  ##############################################################################
  output$diasAtipicos <- renderUI({
    df <- data_filtrada()
    if (nrow(df) == 0) return(HTML("<p>No hay información disponible.</p>"))
    
    # Recalcular bandas y estado
    df_proc <- df %>%
      mutate(anio = year(fecha)) %>%
      arrange(fecha) %>%
      group_by(anio) %>%
      mutate(
        media_20 = zoo::rollapply(precio, width = 20, FUN = mean,
                                  align = "right", fill = NA, na.rm = TRUE),
        sd_20    = zoo::rollapply(precio, width = 20, FUN = sd,
                                  align = "right", fill = NA, na.rm = TRUE),
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
      lista <- paste0("<li>", df_proc$fecha_label, ": ", df_proc$valor, "</li>", collapse = "")
      n_atipicos <- nrow(df_proc)
      
      HTML(glue("
        <div class='panel-atipicos'>
          <p><b>Se detectaron {n_atipicos} días atípicos:</b></p>
          <ul>{lista}</ul>
        </div>
      "))
    }
  })
  
  ##############################################################################
  # --- 📑 Generar informe PDF ---
  ##############################################################################
  output$descargarPDF <- downloadHandler(
    filename = function() {
      paste0("informe_bandas_precio_", input$producto, "_", input$anio, ".pdf")
    },
    content = function(file) {
      df <- data_filtrada()
      if (nrow(df) == 0) stop("No hay datos para generar el informe.")
      anio_sel <- if (input$anio == "todos") NULL else as.numeric(input$anio)
      grafico <- visualizar_bandas_plotly(df, producto_sel = input$producto, anio_sel = anio_sel)
      
      df_proc <- df %>%
        mutate(anio = year(fecha)) %>%
        arrange(fecha) %>%
        group_by(anio) %>%
        mutate(
          media_20 = rollapply(precio, width = 20, FUN = mean,
                               align = "right", fill = NA, na.rm = TRUE),
          sd_20    = rollapply(precio, width = 20, FUN = sd,
                               align = "right", fill = NA, na.rm = TRUE),
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
      
      if (!is.null(anio_sel)) df_proc <- df_proc %>% filter(anio == anio_sel)
      
      tmp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(as_widget(grafico), tmp_html, selfcontained = TRUE)
      
      tempReport <- file.path(tempdir(), "informe.Rmd")
      file.copy("informe.Rmd", tempReport, overwrite = TRUE)
      
      rmarkdown::render(
        input = tempReport,
        output_format = "pdf_document",
        output_file = file,
        params = list(
          datos = df_proc,
          grafico = tmp_html,
          producto = input$producto,
          anio = input$anio
        ),
        envir = new.env(parent = globalenv())
      )
    },
    contentType = "application/pdf"
  )
  
  ##############################################################################
  # --- 🔁 Resetear filtros ---
  ##############################################################################
  observeEvent(input$reset, {
    updateSelectInput(session, "producto", selected = "Aguacate")
    updateSelectInput(session, "anio", selected = "2024")
  })
}
