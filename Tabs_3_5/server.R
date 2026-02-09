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
      paste("IND2_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      # Forzar la ejecución de la función reactiva
      
      
      
      df <- data_filtrada()
      if (nrow(df) == 0) stop("No hay datos para generar el informe.")
      
      # --- Gráfico estático ggplot ---
      grafico_plano <- visualizar_ranking_estatico(df, producto = input$producto, anio = input$anio)
      
      # Usa ggsave para guardar el gráfico
      ggplot2::ggsave(filename = file, plot = grafico_plano, width = 13, height = 7, dpi = 200)
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
    
    prom <- mean(bog_rank)
    rango_inf <- floor(prom)
    rango_sup <- ceiling(prom)
    
    mensaje <- glue::glue(
      "El escalafón que ocupó Bogotá en promedio estuvo entre el {rango_inf} y {rango_sup} durante {input$anio}
      con respecto a las demás ciudades."
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
      paste0("informe_comparativo_", input$producto, "_", input$anio, ".pdf")
    },
    content = function(file) {
      
      df <- data_filtrada()
      if (nrow(df) == 0) stop("No hay datos para generar el informe.")
      
      # --- Gráfico estático ggplot ---
      graf <- visualizar_ranking_estatico(df, producto = input$producto, anio = input$anio)
      
      
      
      
      
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
      
      
      
      # Render PDF
      out_pdf <- file.path(tmpdir, paste0("ranking_precios_", input$producto, "_", input$anio, ".pdf"))
      
      rmarkdown::render(
        input       = tempReport,
        output_file = out_pdf,
        params = list(
          producto    = input$producto,
          anio        = input$anio,
          datos       = df,
          plot        = graf,  
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
