################################################################################-
# Proyecto FAO - VP - 2025
# SERVER - Huella de Carbono (versión estable LATAM)
################################################################################-

rm(list = ls())

library(shiny)
library(plotly)
library(dplyr)
library(rmarkdown)
library(htmlwidgets)
library(glue)
library(knitr)
library(magick)

source("3_9b_huella_carbono.R")
options(scipen = 999)

# ------------------------------------------------------------------
# Función LATAM: miles con punto, decimales con coma
# ------------------------------------------------------------------
formato_latam <- function(x, dec = 0) {
  format(round(x, dec), big.mark=".", decimal.mark=",", nsmall=dec)
}

################################################################################-
# Server
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
      summarise(total_co2 = sum(c02_total, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_co2)) %>%
      slice(1)
    
    prod_name  <- prod_top$producto
    cat_name   <- prod_top$categoria
    prod_co2   <- prod_top$total_co2
    
    # total del grupo
    grupo_total <- df %>%
      filter(categoria == cat_name) %>%
      summarise(total = sum(c02_total, na.rm = TRUE)) %>%
      pull(total)
    
    pct_within_group <- formato_latam(100 * prod_co2 / grupo_total, dec = 1)
    
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
      slice(1:3) %>%
      mutate(
        co2_fmt = paste0(formato_latam(total_co2, 0), " tCO₂")
      )
    
    mensaje2 <- paste0(
      "Productos que más CO₂ emiten:\n\n",
      paste0(ranking_top3$producto, ": ", ranking_top3$co2_fmt, collapse = "\n")
    )
    
    
    # ===================== Resumen categoría =====================
    resumen <- df %>%
      group_by(categoria) %>%
      summarise(
        total_ton = sum(total_ton, na.rm = TRUE),
        total_co2 = sum(c02_total, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_co2)) %>%
      mutate(
        porcentaje = 100 * total_co2 / sum(total_co2, na.rm = TRUE)
      )
    
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
  # 4. Panel derecho: Top emisores (LATAM)
  # ------------------------------------------------------------------
  output$top5_emisores <- renderUI({
    res <- resultado()
    if (is.null(res)) return(HTML("<p>No hay datos disponibles.</p>"))
    
    df <- res$datos
    
    ranking_prod <- df %>%
      group_by(producto) %>%
      summarise(total_co2 = sum(c02_total, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_co2)) %>%
      slice(1:3) %>%
      mutate(
        co2_fmt = paste0(formato_latam(total_co2, 0), " tCO₂")
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
  # 5. Panel rojo superior → mensaje1
  # ------------------------------------------------------------------
  output$mensaje1 <- renderUI({
    res <- resultado()
    if (is.null(res)) return(HTML("<p>Sin información disponible.</p>"))
    HTML(paste0("<p>", res$mensaje1, "</p>"))
  })
  
  
  # ------------------------------------------------------------------
  # 6. Descarga CSV y PNG
  # ------------------------------------------------------------------
  output$descargarDatos <- downloadHandler(
    filename = function() glue("emisiones_{input$anio}_{input$mes}.csv"),
    content = function(file) {
      res <- resultado()
      if (!is.null(res)) write.csv(res$datos, file, row.names = FALSE)
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
  
  
  
  
  





  
  # ------------------------------------------------------------------
  # 7. Informe PDF (mantiene formato correcto + gráfico estático)
  # ------------------------------------------------------------------
  output$report <- downloadHandler(
    filename = function() glue("informe_huella_carbono_{input$anio}_{input$mes}.pdf"),
    contentType = "application/pdf",
    content = function(file) {
      res <- resultado()
      if (is.null(res)) stop("No hay datos para generar el informe.")
      
      # =========================
      # 👉 GRÁFICO ESTÁTICO (NUEVO)
      # =========================
      grafico_estatico <- graficar_treemap_producto_estatico(
        data = res$datos,
        anio = input$anio,
        mes  = input$mes
      )
      
      # -------------------------
      # Escapar mensajes (igual que antes)
      # -------------------------
      mensaje1_txt <- gsub("%", "\\\\%", res$mensaje1)
      
      mensaje2_txt <- res$mensaje2
      mensaje2_txt <- gsub("%", "\\\\%", mensaje2_txt)
      mensaje2_txt <- gsub("\n", " \\\\newline ", mensaje2_txt)
      
      # -------------------------
      # Render PDF
      # -------------------------
      rmarkdown::render(
        input = "informe.Rmd",
        output_file = file,
        params = list(
          producto = "Huella de Carbono",
          anio     = input$anio,
          mes      = input$mes,
          
          # 👇 CLAVE FAO
          plot     = grafico_estatico,
          
          datos    = res$datos,
          mensaje1 = mensaje1_txt,
          mensaje2 = mensaje2_txt,
          
          logo_sup = "www/logo_3.png",
          logo_inf = "www/logo_2.png"
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )

  
  
  
  output$descargarGraf<- downloadHandler(
    filename = function(){
      paste("IND2_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      # Forzar la ejecución de la función reactiva
      res <- resultado()
      #if (is.null(res)) stop("No hay datos para generar el informe.")
      # =========================
      # 👉 GRÁFICO ESTÁTICO (NUEVO)
      # =========================
      grafico_plano <- graficar_treemap_producto_estatico(
        data = res$datos,
        anio = input$anio,
        mes  = input$mes
      )
      # Usa ggsave para guardar el gráfico
      ggplot2::ggsave(filename = file, plot = grafico_plano, width = 13, height = 7, dpi = 200)
      
      
      #ggplot2::ggsave(filename = file, plot = grafico_plano, width = 13, height = 7, dpi = 200)
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
