################################################################################-
# Proyecto FAO - VP - 2025
# Ranking mensual de precios mayoristas por ciudad
################################################################################-

pacman::p_load(shiny, plotly, dplyr, zoo, rmarkdown, webshot2, htmlwidgets)
options(scipen = 999)

source("3_5b_ranking_precios_ciudad.R")

server <- function(input, output, session) {
  
  # ---------------------------------------------------------------
  # VARIABLES REACTIVAS PARA MENSAJES DEL PANEL (Y PARA EL PDF)
  # ---------------------------------------------------------------
  mensaje1_val <- reactiveVal("")   # Mensaje: meses donde Bogotá fue #1
  mensaje2_val <- reactiveVal("")   # Mensaje: escalafón promedio de Bogotá
  
  # ---------------------------------------------------------------
  # Valores por defecto
  # ---------------------------------------------------------------
  updateSelectInput(session, "producto", selected = "Aguacate")
  updateSelectInput(session, "anio", selected = "2024")
  
  # ---------------------------------------------------------------
  # DATOS FILTRADOS REACTIVOS
  # ---------------------------------------------------------------
  data_filtrada <- reactive({
    req(input$producto, input$anio)
    
    df <- data %>% filter(producto == input$producto)
    
    if (input$anio != "todos") {
      df <- df %>% filter(format(as.yearmon(mes_y_ano), "%Y") == input$anio)
    }
    
    df
  })
  
  # ---------------------------------------------------------------
  # GRÁFICO PRINCIPAL
  # ---------------------------------------------------------------
  output$grafico <- renderPlotly({
    df <- data_filtrada()
    
    if (nrow(df) == 0) {
      showNotification("⚠️ No hay datos disponibles para ese producto y año.", type = "warning")
      return(NULL)
    }
    
    visualizar_ranking(df, producto = input$producto, anio = input$anio)
  })
  
  # ---------------------------------------------------------------
  # DESCARGAR GRÁFICO PNG
  # ---------------------------------------------------------------
  output$descargarGrafico <- downloadHandler(
    filename = function() {
      paste0("ranking_precios_", gsub(" ", "_", input$producto), "_", input$anio, ".png")
    },
    content = function(file) {
      df <- data_filtrada()
      if (nrow(df) == 0) stop("No hay datos disponibles para exportar el gráfico.")
      
      graf <- visualizar_ranking(df, producto = input$producto, anio = input$anio)
      tmp_html <- tempfile(fileext = ".html")
      tmp_png  <- tempfile(fileext = ".png")
      
      htmlwidgets::saveWidget(plotly::as_widget(graf), tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, file = tmp_png, vwidth = 1600, vheight = 900, delay = 1)
      file.copy(tmp_png, file, overwrite = TRUE)
    }
  )
  
  # ---------------------------------------------------------------
  # MENSAJE 1 — MESES DONDE BOGOTÁ FUE LA MÁS COSTOSA
  # ---------------------------------------------------------------
  output$mensaje1 <- renderText({
    df <- data_filtrada()
    
    if (nrow(df) == 0) {
      mensaje <- "No hay información disponible para este año."
      mensaje1_val(mensaje)
      return(mensaje)
    }
    
    df_rank <- df %>%
      mutate(mes_y_ano = as.yearmon(mes_y_ano, "%Y-%m")) %>%
      group_by(mes_y_ano) %>%
      mutate(ranking = rank(-precio_prom, ties.method = "min")) %>%
      ungroup()
    
    meses_bog <- df_rank %>%
      filter(ciudad == "Bogotá", ranking == 1) %>%
      mutate(mes = format(mes_y_ano, "%B")) %>% pull(mes)
    
    if (length(meses_bog) == 0) {
      mensaje <- glue::glue(
        "En el año {input$anio}, Bogotá no ocupó el escalafón más alto del precio en ningún mes."
      )
    } else {
      meses_txt <- paste(meses_bog, collapse = ", ")
      mensaje <- glue::glue(
        "En el año {input$anio}, Bogotá ocupó el primer lugar en precio mayorista en los meses: {meses_txt}."
      )
    }
    
    mensaje1_val(mensaje)
    mensaje
  })
  
  # ---------------------------------------------------------------
  # MENSAJE 2 — ESCALAFÓN PROMEDIO DE BOGOTÁ
  # ---------------------------------------------------------------
  output$mensaje2 <- renderText({
    df <- data_filtrada()
    
    if (nrow(df) == 0) {
      mensaje <- ""
      mensaje2_val(mensaje)
      return(mensaje)
    }
    
    df_rank <- df %>%
      mutate(mes_y_ano = as.yearmon(mes_y_ano, "%Y-%m")) %>%
      group_by(mes_y_ano) %>%
      mutate(ranking = rank(-precio_prom, ties.method = "min")) %>%
      ungroup()
    
    bog_rank <- df_rank %>% filter(ciudad == "Bogotá") %>% pull(ranking)
    
    if (length(bog_rank) == 0) {
      mensaje <- ""
      mensaje2_val(mensaje)
      return(mensaje)
    }
    
    prom <- round(mean(bog_rank), 1)
    mensaje <- glue::glue(
      "En promedio, Bogotá ocupó el escalafón {prom} entre todas las ciudades durante {input$anio}."
    )
    
    mensaje2_val(mensaje)
    mensaje
  })
  
  # ---------------------------------------------------------------
  # DESCARGA DE DATOS CSV
  # ---------------------------------------------------------------
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste0("ranking_datos_", gsub(" ", "_", input$producto), "_", input$anio, ".csv")
    },
    content = function(file) {
      df <- data_filtrada()
      
      if (nrow(df) == 0) {
        write.csv(data.frame(Mensaje = "No hay datos disponibles."), file, row.names = FALSE)
      } else {
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
  
  # ---------------------------------------------------------------
  # DESCARGAR INFORME PDF
  # ---------------------------------------------------------------
  output$descargarPDF <- downloadHandler(
    filename = function() {
      paste0("ranking_precios_", gsub(" ", "_", input$producto), "_", input$anio, ".pdf")
    },
    content = function(file) {
      
      df <- data_filtrada()
      if (nrow(df) == 0) stop("No hay datos para generar el informe.")
      
      graf <- visualizar_ranking(df, producto = input$producto, anio = input$anio)
      mensaje1_txt <- mensaje1_val()
      mensaje2_txt <- mensaje2_val()
      
      # Directorio temporal
      tmpdir <- tempdir()
      tempReport <- file.path(tmpdir, "informe.Rmd")
      file.copy("informe.Rmd", tempReport, overwrite = TRUE)
      
      # Copiar logos
      if (dir.exists("www")) file.copy("www", tmpdir, recursive = TRUE)
      
      # Copiar fuente Prompt
      if (file.exists("Prompt-Regular.ttf")) {
        file.copy("Prompt-Regular.ttf", tmpdir, overwrite = TRUE)
      } else if (file.exists(file.path("Prompt", "Prompt-Regular.ttf"))) {
        file.copy(file.path("Prompt", "Prompt-Regular.ttf"), tmpdir, overwrite = TRUE)
      }
      
      # Convertir gráfico → PNG
      tmp_html <- tempfile(fileext = ".html")
      tmp_png  <- file.path(tmpdir, "grafico_ranking.png")
      
      htmlwidgets::saveWidget(plotly::as_widget(graf), tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, file = tmp_png, vwidth = 1600, vheight = 900, delay = 1)
      
      # Render PDF
      out_pdf <- file.path(tmpdir, paste0("ranking_precios_", input$producto, "_", input$anio, ".pdf"))
      
      rmarkdown::render(
        input       = tempReport,
        output_file = out_pdf,
        params = list(
          producto    = input$producto,
          anio        = input$anio,
          datos       = df,
          grafico_png = tmp_png,
          mensaje1    = mensaje1_txt,
          mensaje2    = mensaje2_txt
        ),
        envir         = new.env(parent = globalenv()),
        knit_root_dir = tmpdir
      )
      
      file.copy(out_pdf, file, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )
}
