################################################################################-
# Proyecto FAO - VP - 2025
# SERVER - Huella de Carbono (versión estable sin errores validate)
################################################################################-
# Autores: Luis Miguel García, Juliana Lalinde, Laura Quintero, Germán Angulo
# Fecha: 10/11/2025
################################################################################-

rm(list = ls())

# Librerías
library(shiny)
library(plotly)
library(dplyr)
library(rmarkdown)
library(webshot2)
library(htmlwidgets)
library(glue)
library(knitr)
library(magick)

# Cargar función y datos
source("3_9b_huella_carbono.R")
options(scipen = 999)

################################################################################-
# Servidor
################################################################################-

server <- function(input, output, session) {
  
  # ------------------------------------------------------------
  # 1. Inicialización
  # ------------------------------------------------------------
  observe({
    updateSelectInput(session, "anio", selected = 2024)
    updateSelectInput(session, "mes", selected = "12")
  })
  
  # ------------------------------------------------------------
  # 2. Base filtrada
  # ------------------------------------------------------------
  data_filtrada <- reactive({
    req(input$anio, input$mes)
    df <- data %>%
      mutate(mes = as.numeric(mes)) %>%
      filter(anio == as.numeric(input$anio), mes == as.numeric(input$mes))
    
    if (nrow(df) == 0) return(NULL)
    df
  })
  
  # ------------------------------------------------------------
  # 3. Resultado principal
  # ------------------------------------------------------------
  resultado <- reactive({
    df <- data_filtrada()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    grafico_plotly <- graficar_treemap_producto(df, input$anio, input$mes)
    
    resumen <- df %>%
      group_by(categoria) %>%
      summarise(
        total_ton = sum(total_ton, na.rm = TRUE),
        total_co2 = sum(c02_total, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_co2)) %>%
      mutate(porcentaje = 100 * total_co2 / sum(total_co2, na.rm = TRUE))
    
    if (nrow(resumen) == 0) return(NULL)
    
    top_cat <- resumen$categoria[1]
    top_pct <- round(resumen$porcentaje[1], 1)
    
    mensaje1 <- paste0(
      top_cat, " es la categoría con mayor huella de carbono, ",
      "aportando el ", top_pct, "% del total de emisiones en el periodo seleccionado."
    )
    
    list(
      datos = df,
      grafico_plotly = grafico_plotly,
      resumen = resumen,
      mensaje1 = mensaje1
    )
  })
  
  # ------------------------------------------------------------
  # 4. Gráfico principal
  # ------------------------------------------------------------
  output$grafico <- renderPlotly({
    res <- resultado()
    if (is.null(res) || is.null(res$grafico_plotly)) {
      plotly::plot_ly() %>% layout(title = "Sin datos disponibles")
    } else {
      res$grafico_plotly
    }
  })
  
  # ------------------------------------------------------------
  # 5. Panel lateral (Top emisores)
  # ------------------------------------------------------------
  output$top5_emisores <- renderUI({
    res <- resultado()
    if (is.null(res) || is.null(res$resumen)) return(HTML("<p>No hay datos disponibles.</p>"))
    
    resumen <- res$resumen %>%
      head(5) %>%
      mutate(
        pct_fmt = paste0(round(porcentaje, 1), "%"),
        co2_fmt = format(round(total_co2, 1), big.mark = ".", decimal.mark = ",")
      )
    
    HTML(paste0(
      "<div style='background-color:#f3e8ff; border-left:5px solid #6a1b9a;
                   padding:12px; border-radius:8px;'>",
      "<h4 style='color:#6a1b9a; font-weight:600; text-align:center;'>Top 5 categorías emisoras</h4>",
      paste0("<b>", resumen$categoria, ":</b> ", resumen$co2_fmt, " tCO₂ (", resumen$pct_fmt, ")<br>", collapse = ""),
      "</div>"
    ))
  })
  
  # ------------------------------------------------------------
  # 6. Texto interpretativo
  # ------------------------------------------------------------
  output$mensaje1 <- renderText({
    res <- resultado()
    if (is.null(res) || is.null(res$mensaje1)) return("Sin información disponible.")
    res$mensaje1
  })
  
  # ------------------------------------------------------------
  # 7. Descargas
  # ------------------------------------------------------------
  
  # CSV
  output$descargarDatos <- downloadHandler(
    filename = function() glue("emisiones_{input$anio}_{input$mes}.csv"),
    content = function(file) {
      res <- resultado()
      if (!is.null(res)) write.csv(res$datos, file, row.names = FALSE)
    }
  )
  
  # PNG del gráfico
  output$descargarGraf <- downloadHandler(
    filename = function() glue("grafico_emisiones_{input$anio}_{input$mes}.png"),
    content = function(file) {
      res <- resultado()
      if (is.null(res) || is.null(res$grafico_plotly)) stop("Sin gráfico.")
      tmp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(res$grafico_plotly, tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, file, vwidth = 1600, vheight = 1000)
    }
  )
  
  # PDF con logos y gráfico embebido
  output$report <- downloadHandler(
    filename = function() glue("informe_huella_carbono_{input$anio}_{input$mes}.pdf"),
    contentType = "application/pdf",
    content = function(file) {
      tryCatch({
        tmp_dir <- tempdir()
        tmp_rmd <- file.path(tmp_dir, "informe.Rmd")
        tmp_html <- tempfile(fileext = ".html")
        tmp_png  <- file.path(getwd(), "grafico_tmp.png")
        
        # Copiar el Rmd
        if (!file.exists("informe.Rmd")) stop("No se encuentra informe.Rmd")
        file.copy("informe.Rmd", tmp_rmd, overwrite = TRUE)
        
        # Guardar gráfico en PNG
        res <- resultado()
        if (is.null(res) || is.null(res$grafico_plotly)) stop("Sin gráfico disponible.")
        htmlwidgets::saveWidget(as_widget(res$grafico_plotly), tmp_html, selfcontained = TRUE)
        webshot2::webshot(tmp_html, tmp_png, vwidth = 1600, vheight = 1000)
        
        # Copiar logos
        logo_sup_tmp <- file.path(tmp_dir, "logo_3.png")
        logo_inf_tmp <- file.path(tmp_dir, "logo_2.png")
        file.copy("www/logo_3.png", logo_sup_tmp, overwrite = TRUE)
        file.copy("www/logo_2.png", logo_inf_tmp, overwrite = TRUE)
        
        # Renderizar PDF
        out_pdf <- file.path(tmp_dir, "informe_tmp.pdf")
        rmarkdown::render(
          input = tmp_rmd,
          output_file = out_pdf,
          params = list(
            datos = res$datos,
            resumen = res$resumen,
            mensaje1 = res$mensaje1,
            grafico = tmp_png,
            anio = input$anio,
            mes = input$mes,
            logo_sup = logo_sup_tmp,
            logo_inf = logo_inf_tmp
          ),
          envir = new.env(parent = globalenv())
        )
        
        file.copy(out_pdf, file, overwrite = TRUE)
        
      }, error = function(e) {
        showNotification(paste("Error al generar el informe:", e$message),
                         type = "error", duration = NULL)
      })
    }
  )
  
  # ------------------------------------------------------------
  # 8. Reset filtros
  # ------------------------------------------------------------
  observeEvent(input$reset, {
    updateSelectInput(session, "anio", selected = 2024)
    updateSelectInput(session, "mes", selected = "12")
  })
}
