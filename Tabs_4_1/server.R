################################################################################-
# Proyecto FAO - VP - 2025
# Servidor - Mapa de Rutas por Región (usa función graficar_rutas)
################################################################################-

# ---- Carga ordenada de librerías ----
suppressPackageStartupMessages({
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    dplyr, stringr, scales, readr, purrr, leaflet, glue,
    rmarkdown, htmlwidgets, webshot2, callr, shiny
  )
})

# ---- Forzar que validate y need sean de Shiny ----
validate <- shiny::validate
need <- shiny::need

# ---- Cargar función y datos ----
source("4_1b_rutas_abastecimiento.R")  # contiene graficar_rutas()
data_cierres_final <- data_merged

# ---- Servidor ----
server <- function(input, output, session) {
  
  # -----------------------------------------------------------
  # 1. Inicialización de selectInput
  # -----------------------------------------------------------
  observe({
    updateSelectInput(
      session, "anio",
      choices = sort(unique(data_cierres_final$anio)),
      selected = max(data_cierres_final$anio, na.rm = TRUE)
    )
    
    nombres_meses <- setNames(
      sprintf("%02d", 1:12),
      c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
        "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
    )
    
    updateSelectInput(
      session, "mes",
      choices = nombres_meses,
      selected = sprintf("%02d", max(as.numeric(data_cierres_final$mes), na.rm = TRUE))
    )
    
    updateSelectInput(
      session, "producto",
      choices = sort(unique(data_cierres_final$producto)),
      selected = sort(unique(data_cierres_final$producto))[1]
    )
  })
  
  # -----------------------------------------------------------
  # 2. Datos filtrados reactivos
  # -----------------------------------------------------------
  datos_filtrados <- reactive({
    req(input$anio, input$mes, input$producto)
    
    df <- data_cierres_final %>%
      filter(
        as.numeric(anio) == as.numeric(input$anio),
        as.numeric(mes) == as.numeric(input$mes),
        producto == input$producto
      )
    
    validate(
      need(nrow(df) > 0, "No hay datos disponibles para los filtros seleccionados.")
    )
    
    df
  })
  
  # -----------------------------------------------------------
  # 3. Estado del mapa
  # -----------------------------------------------------------
  output$estado_mapa <- reactive({
    df <- datos_filtrados()
    if (is.null(df) || nrow(df) == 0) "sin_datos" else "ok"
  })
  outputOptions(output, "estado_mapa", suspendWhenHidden = FALSE)
  
  # -----------------------------------------------------------
  # 4. Render del mapa
  # -----------------------------------------------------------
  output$grafico <- leaflet::renderLeaflet({
    df <- datos_filtrados()
    
    validate(
      need(!is.null(df) && nrow(df) > 0, "No hay datos disponibles para los filtros seleccionados.")
    )
    
    df <- df %>%
      mutate(
        mpio_origen = as.character(mpio_origen),
        depto_origen = as.character(depto_origen),
        producto = as.character(producto),
        routes_coords_str = as.character(routes_coords_str)
      )
    
    df[is.na(df)] <- ""
    
    mapa <- graficar_rutas(
      df,
      Año = input$anio,
      Mes = input$mes,
      Producto = input$producto
    )
    
    if (!inherits(mapa, "leaflet")) {
      leaflet() %>%
        addTiles() %>%
        addControl("Mapa no disponible", position = "topright")
    } else {
      mapa
    }
  })
  
  # -----------------------------------------------------------
  # 5. Estadísticas laterales
  # -----------------------------------------------------------
  output$region_mas_importante <- renderText({
    df <- datos_filtrados()
    validate(
      need(!is.null(df) && "mpio_origen" %in% names(df) && "importancia_ruta" %in% names(df),
           "Sin datos disponibles")
    )
    
    top <- df %>%
      group_by(mpio_origen, depto_origen) %>%
      summarise(importancia_total = sum(importancia_ruta, na.rm = TRUE), .groups = "drop") %>%
      slice_max(importancia_total, n = 1)
    
    paste0(
      str_to_title(top$mpio_origen), " (", str_to_title(top$depto_origen), ") — ",
      sprintf("%.2f%%", top$importancia_total * 100)
    )
  })
  
  output$region_menos_importante <- renderText({
    df <- datos_filtrados()
    validate(
      need(!is.null(df) && "mpio_origen" %in% names(df) && "importancia_ruta" %in% names(df),
           "Sin datos disponibles")
    )
    
    low <- df %>%
      group_by(mpio_origen, depto_origen) %>%
      summarise(importancia_total = sum(importancia_ruta, na.rm = TRUE), .groups = "drop") %>%
      slice_min(importancia_total, n = 1)
    
    paste0(
      str_to_title(low$mpio_origen), " (", str_to_title(low$depto_origen), ") — ",
      sprintf("%.2f%%", low$importancia_total * 100)
    )
  })
  
  # -----------------------------------------------------------
  # 6. Botones de acción
  # -----------------------------------------------------------
  observeEvent(input$reset, {
    updateSelectInput(session, "anio", selected = max(data_cierres_final$anio, na.rm = TRUE))
    updateSelectInput(session, "mes", selected = sprintf("%02d", max(as.numeric(data_cierres_final$mes), na.rm = TRUE)))
    updateSelectInput(session, "producto", selected = sort(unique(data_cierres_final$producto))[1])
  })
  
  output$descargarDatos <- downloadHandler(
    filename = function() {
      glue("rutas_regiones_{input$anio}_{input$mes}_{input$producto}.csv")
    },
    content = function(file) {
      df <- datos_filtrados()
      if (!is.null(df)) write_csv(df, file)
    }
  )
  
  # -----------------------------------------------------------
  # 7. Generación del informe PDF con logos FAO
  # -----------------------------------------------------------
  output$descargarPDF <- downloadHandler(
    filename = function() {
      glue("rutas_abastecimiento_{input$anio}_{input$mes}_{input$producto}.pdf")
    },
    content = function(file) {
      tempReport <- file.path(getwd(), "rutas_abas.Rmd")
      
      df <- datos_filtrados()
      grafico <- graficar_rutas(
        df,
        Año = input$anio,
        Mes = input$mes,
        Producto = input$producto
      )
      
      tmp_dir <- tempdir()
      logo_sup <- file.path(tmp_dir, "logo_3.png")
      logo_inf <- file.path(tmp_dir, "logo_2.png")
      file.copy("www/logo_3.png", logo_sup, overwrite = TRUE)
      file.copy("www/logo_2.png", logo_inf, overwrite = TRUE)
      
      params <- list(
        datos = df,
        grafico = grafico,
        anio = input$anio,
        mes = input$mes,
        producto = input$producto,
        logo_sup = logo_sup,
        logo_inf = logo_inf
      )
      
      showNotification("Generando informe PDF...", duration = NULL, id = "pdf_notify")
      
      callr::r(
        func = function(input_file, output_file, params) {
          rmarkdown::render(
            input = input_file,
            output_file = output_file,
            params = params,
            envir = new.env(parent = globalenv())
          )
        },
        args = list(input_file = tempReport, output_file = file, params = params)
      )
      
      removeNotification("pdf_notify")
    }
  )
}

################################################################################-
# Fin del servidor
################################################################################-
