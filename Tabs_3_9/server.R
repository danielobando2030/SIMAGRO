################################################################################-
# Proyecto FAO - VP - 2025
# SERVER - Huella de Carbono (versión estable funcional)
################################################################################-
# Autores: Luis Miguel García, Juliana Lalinde, Laura Quintero, Germán Angulo
# Fecha: 12/11/2025
################################################################################-

rm(list = ls())

library(shiny)
library(plotly)
library(dplyr)
library(rmarkdown)
library(webshot2)
library(htmlwidgets)
library(glue)
library(knitr)
library(magick)

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
  # 2. Resultado principal (mensaje1 + mensaje2)
  # ------------------------------------------------------------------
  resultado <- reactive({
    df <- data_filtrada()
    if (is.null(df)) return(NULL)
    
    grafico_plotly <- graficar_treemap_producto(df, input$anio, input$mes)
    
    # ===================== MENSAJE 1 =====================
    prod_top <- df %>%
      group_by(producto, categoria) %>%
      summarise(
        total_co2 = sum(c02_total, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_co2)) %>%
      slice(1)
    
    prod_name  <- prod_top$producto
    cat_name   <- prod_top$categoria
    prod_co2   <- prod_top$total_co2
    
    grupo_total <- df %>%
      filter(categoria == cat_name) %>%
      summarise(total = sum(c02_total, na.rm = TRUE)) %>%
      pull(total)
    
    pct_within_group <- round(100 * prod_co2 / grupo_total, 1)
    
    mensaje1 <- paste0(
      prod_name, " es el producto con mayor huella de carbono dentro del grupo ",
      cat_name, ", aportando el ", pct_within_group,
      "% del total de emisiones de ese grupo."
    )
    
    
    # ===================== MENSAJE 2 (TOP 3) =====================
    
    ranking_top3 <- df %>%
      group_by(producto) %>%
      summarise(total_co2 = sum(c02_total, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_co2)) %>%
      head(3) %>%
      mutate(
        co2_fmt = paste0(format(round(total_co2, 3), big.mark = ".", decimal.mark = ","), " tCO₂")
      )
    
    mensaje2 <- paste0(
      "Productos que más CO₂ emiten:\n\n",
      paste0(ranking_top3$producto, ": ", ranking_top3$co2_fmt, collapse = "\n")
    )
    
    # ===================== Resumen por categoría =====================
    
    resumen <- df %>%
      group_by(categoria) %>%
      summarise(
        total_ton = sum(total_ton, na.rm = TRUE),
        total_co2 = sum(c02_total, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_co2)) %>%
      mutate(porcentaje = 100 * total_co2 / sum(total_co2, na.rm = TRUE))
    
    
    list(
      datos          = df,
      grafico_plotly = grafico_plotly,
      resumen        = resumen,
      mensaje1       = mensaje1,
      mensaje2       = mensaje2
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
  # 4. Panel derecho: top 3
  # ------------------------------------------------------------------
  output$top5_emisores <- renderUI({
    res <- resultado()
    if (is.null(res)) return(HTML("<p>No hay datos disponibles.</p>"))
    
    df <- res$datos
    
    ranking_prod <- df %>%
      group_by(producto) %>%
      summarise(
        total_co2 = sum(c02_total, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_co2)) %>%
      head(3) %>%
      mutate(
        co2_fmt = paste0(format(round(total_co2, 3), big.mark = ".", decimal.mark = ","), " tCO₂")
      )
    
    HTML(paste0(
      "<p style='margin-top:0; margin-bottom:8px; font-size:15px;'>
          <b>Productos que más CO₂ emiten:</b></p>",
      paste0(
        "<b>", ranking_prod$producto, ":</b> ",
        ranking_prod$co2_fmt, "<br>",
        collapse = ""
      )
    ))
  })
  
  # ------------------------------------------------------------------
  # 5. Panel rojo superior: mensaje1
  # ------------------------------------------------------------------
  output$mensaje1 <- renderUI({
    res <- resultado()
    if (is.null(res)) return(HTML("<p>Sin información disponible.</p>"))
    HTML(paste0("<p>", res$mensaje1, "</p>"))
  })
  
  # ------------------------------------------------------------------
  # 6. Descargas CSV y PNG
  # ------------------------------------------------------------------
  output$descargarDatos <- downloadHandler(
    filename = function() glue("emisiones_{input$anio}_{input$mes}.csv"),
    content = function(file) {
      res <- resultado()
      if (!is.null(res)) write.csv(res$datos, file, row.names = FALSE)
    }
  )
  
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
  # 7. Informe PDF
  # ------------------------------------------------------------------
  output$report <- downloadHandler(
    filename = function() glue("informe_huella_carbono_{input$anio}_{input$mes}.pdf"),
    contentType = "application/pdf",
    content = function(file) {
      res <- resultado()
      if (is.null(res)) stop("No hay datos para generar el informe.")
      
      tmp_html <- tempfile(fileext = ".html")
      tmp_png  <- tempfile(fileext = ".png")
      
      htmlwidgets::saveWidget(as_widget(res$grafico_plotly), tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, tmp_png, vwidth = 1600, vheight = 1000)
      
      mensaje1_txt <- gsub("%", "\\\\%", res$mensaje1)
      
      mensaje2_txt <- res$mensaje2
      mensaje2_txt <- gsub("%", "\\\\%", mensaje2_txt)          # escapar %
      mensaje2_txt <- gsub("\n", " \\\\newline ", mensaje2_txt) # saltos de línea LaTeX
      
      rmarkdown::render(
        input = "informe.Rmd",
        output_file = file,
        params = list(
          producto    = "Huella de Carbono",
          anio        = input$anio,
          mes         = input$mes,
          datos       = res$datos,
          mensaje1    = mensaje1_txt,
          mensaje2    = mensaje2_txt,
          grafico_png = tmp_png,
          mapa_png    = "",
          tabla_datos = NULL,
          logo_sup    = "www/logo_3.png",
          logo_inf    = "www/logo_2.png"
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

