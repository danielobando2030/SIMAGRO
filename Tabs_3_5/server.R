################################################################################-
# Proyecto FAO - VP - 2025
# Ranking mensual de precios mayoristas por ciudad
################################################################################-
# Autores: Luis Miguel García, Laura Quintero, Daniel Obando
# Última modificación: 08/11/2025
################################################################################-

pacman::p_load(shiny, plotly, dplyr, zoo, rmarkdown, webshot2, htmlwidgets)
options(scipen = 999)

source("3_5b_ranking_precios_ciudad.R")

server <- function(input, output, session) {
  
  # --- Valores por defecto ---
  updateSelectInput(session, "producto", selected = "Aguacate")
  updateSelectInput(session, "anio", selected = "2024")
  
  # --- Datos filtrados reactivos ---
  data_filtrada <- reactive({
    req(input$producto, input$anio)
    df <- data %>% filter(producto == input$producto)
    if (input$anio != "todos") {
      df <- df %>% filter(format(as.yearmon(mes_y_ano), "%Y") == input$anio)
    }
    df
  })
  
  # --- Gráfico interactivo ---
  output$grafico <- renderPlotly({
    df <- data_filtrada()
    if (nrow(df) == 0) {
      showNotification("⚠️ No hay datos disponibles para ese producto y año.", type = "warning")
      return(NULL)
    }
    visualizar_ranking(df, producto = input$producto, anio = input$anio)
  })
  
  # --- Descargar gráfica PNG ---
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
  
  # --- 📊 Cálculo resumen para panel lateral ---
  output$mensaje1 <- renderText({
    df <- data_filtrada()
    if (nrow(df) == 0) return("No hay información disponible para este año.")
    
    # Asegurar estructura
    df <- df %>%
      mutate(mes_y_ano = as.yearmon(mes_y_ano, "%Y-%m")) %>%
      group_by(mes_y_ano) %>%
      mutate(ranking = rank(-precio_prom, ties.method = "min")) %>%
      ungroup()
    
    # Meses donde Bogotá fue la más costosa
    meses_bog <- df %>%
      filter(ciudad == "Bogotá", ranking == 1) %>%
      mutate(mes = format(mes_y_ano, "%B")) %>%
      pull(mes)
    
    if (length(meses_bog) == 0) {
      return(glue::glue("En el año {input$anio}, Bogotá no ocupó el escalafón más alto en el precio en ningún mes."))
    } else {
      meses_txt <- paste(meses_bog, collapse = ", ")
      return(glue::glue("En el año {input$anio}, Bogotá ocupó el escalafón más alto en el precio con respecto a las demás ciudades en los meses: {meses_txt}."))
    }
  })
  
  # --- Descargar datos CSV ---
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste0("ranking_datos_", gsub(" ", "_", input$producto), "_", input$anio, ".csv")
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
  
  # --- Generar informe PDF ---
  output$descargarPDF <- downloadHandler(
    filename = function() {
      paste0("ranking_precios_", gsub(" ", "_", input$producto), "_", input$anio, ".pdf")
    },
    content = function(file) {
      df <- data_filtrada()
      if (nrow(df) == 0) stop("No hay datos para generar el informe.")
      graf <- visualizar_ranking(df, producto = input$producto, anio = input$anio)
      
      tempReport <- file.path(tempdir(), "ranking_precios.Rmd")
      file.copy("informe.Rmd", tempReport, overwrite = TRUE)
      out_pdf <- file.path(tempdir(), paste0("ranking_precios_", input$producto, "_", input$anio, ".pdf"))
      
      rmarkdown::render(
        input = tempReport,
        output_format = "pdf_document",
        output_file = out_pdf,
        params = list(
          datos = df,
          grafico = graf,
          producto = input$producto,
          anio = input$anio
        ),
        envir = new.env(parent = globalenv())
      )
      file.copy(out_pdf, file, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )
  
  # --- Reset ---
  observeEvent(input$reset, {
    updateSelectInput(session, "producto", selected = "Aguacate")
    updateSelectInput(session, "anio", selected = "2024")
  })
}
