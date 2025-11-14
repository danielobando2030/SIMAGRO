################################################################################-
# Proyecto FAO - VP - 2025
# SERVER - Huella de Carbono (versión estable funcional, adaptada del antiguo)
################################################################################-
# Autores: Luis Miguel García, Juliana Lalinde, Laura Quintero, Germán Angulo
# Fecha: 12/11/2025
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
  
  # ------------------------------------------------------------------
  # 1. Base filtrada
  # ------------------------------------------------------------------
  data_filtrada <- reactive({
    req(input$anio, input$mes)
    df <- data %>%
      mutate(mes = as.numeric(mes)) %>%
      filter(anio == as.numeric(input$anio),
             mes == as.numeric(input$mes))
    if (nrow(df) == 0) return(NULL)
    df
  })
  
  # ------------------------------------------------------------------
  # 2. Resultado principal
  # ------------------------------------------------------------------
  resultado <- reactive({
    df <- data_filtrada()
    if (is.null(df)) return(NULL)
    
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
  
  # ------------------------------------------------------------------
  # 3. Render del gráfico
  # ------------------------------------------------------------------
  output$grafico <- renderPlotly({
    res <- resultado()
    if (is.null(res) || is.null(res$grafico_plotly))
      return(plotly::plot_ly() %>% layout(title = "Sin datos disponibles"))
    res$grafico_plotly
  })
  
  # ------------------------------------------------------------------
  # 4. Panel lateral (Top emisores)
  # ------------------------------------------------------------------
  output$top5_emisores <- renderUI({
    res <- resultado()
    if (is.null(res)) return(HTML("<p>No hay datos disponibles.</p>"))
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
  
  # ------------------------------------------------------------------
  # 5. Texto interpretativo
  # ------------------------------------------------------------------
  output$mensaje1 <- renderText({
    res <- resultado()
    if (is.null(res)) return("Sin información disponible.")
    res$mensaje1
  })
  
  # ------------------------------------------------------------------
  # 6. Descargas
  # ------------------------------------------------------------------
  
  # Datos CSV
  output$descargarDatos <- downloadHandler(
    filename = function() glue("emisiones_{input$anio}_{input$mes}.csv"),
    content = function(file) {
      res <- resultado()
      if (!is.null(res)) write.csv(res$datos, file, row.names = FALSE)
    }
  )
  
  # Gráfico PNG
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
  
  # ------------------------------------------------------------------
  # 7. Generación de informe PDF (modelo antiguo adaptado)
  # ------------------------------------------------------------------
  output$report <- downloadHandler(
    filename = function() glue("informe_huella_carbono_{input$anio}_{input$mes}.pdf"),
    contentType = "application/pdf",
    content = function(file) {
      res <- resultado()
      if (is.null(res)) stop("No hay datos para generar el informe.")
      
      # Guardar gráfico
      tmp_html <- tempfile(fileext = ".html")
      tmp_png  <- "grafico_tmp.png"
      htmlwidgets::saveWidget(as_widget(res$grafico_plotly), tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, tmp_png, vwidth = 1600, vheight = 1000)
      
      # Renderizar PDF (idéntico al server antiguo)
      rmarkdown::render(
        input = "informe.Rmd",
        output_file = file,
        params = list(
          datos = res$datos,
          resumen = res$resumen,
          mensaje1 = res$mensaje1,
          grafico = tmp_png,
          anio = input$anio,
          mes = input$mes
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # ------------------------------------------------------------------
  # 8. Reset filtros
  # ------------------------------------------------------------------
  observeEvent(input$reset, {
    updateSelectInput(session, "anio", selected = 2024)
    updateSelectInput(session, "mes", selected = "12")
  })
}
