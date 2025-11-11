################################################################################-
# Proyecto FAO - VP - 2025
# Servidor - Mapa de Rutas (usa función graficar_rutas_color_importancia + PDF)
################################################################################-

library(shiny)
library(dplyr)
library(stringr)
library(scales)
library(readr)
library(purrr)
library(leaflet)
library(glue)
library(rmarkdown)
library(htmlwidgets)
library(webshot2)
library(callr)

# Cargar función y datos
source("4_2b_cierres_rutas_abastecimiento.R")  # contiene graficar_rutas_color_importancia

server <- function(input, output, session) {
  
  # -----------------------------------------------------------
  # 1. Inicializar selectInputs con valores predeterminados
  # -----------------------------------------------------------
  observe({
    anios_disp <- sort(unique(data_cierres_final$anio))
    meses_disp <- sort(unique(data_cierres_final$mes))
    productos_disp <- sort(unique(data_cierres_final$producto))
    regiones_disp <- sort(unique(data_cierres_final$region_geo))
    
    updateSelectInput(session, "anio",
                      choices = anios_disp,
                      selected = ifelse(2024 %in% anios_disp, 2024, max(anios_disp)))
    
    updateSelectInput(session, "mes",
                      choices = meses_disp,
                      selected = ifelse("12" %in% meses_disp, "12", tail(meses_disp, 1)))
    
    updateSelectInput(session, "producto",
                      choices = productos_disp,
                      selected = ifelse("Aguacate Hass" %in% productos_disp,
                                        "Aguacate Hass",
                                        productos_disp[1]))
    
    updateCheckboxGroupInput(session, "rutas",
                             choices = regiones_disp,
                             selected = regiones_disp)
  })
  
  # -----------------------------------------------------------
  # 2. Datos filtrados (por año, mes, producto y regiones seleccionadas)
  # -----------------------------------------------------------
  datos_filtrados <- reactive({
    req(input$anio, input$mes, input$producto)
    df <- data_cierres_final %>%
      filter(anio == input$anio,
             mes == input$mes,
             producto == input$producto)
    
    if (!is.null(input$rutas) && length(input$rutas) > 0) {
      df <- df %>% filter(region_geo %in% input$rutas)
    }
    if (nrow(df) == 0) return(NULL)
    df
  })
  
  # -----------------------------------------------------------
  # 3. Render del mapa
  # -----------------------------------------------------------
  output$grafico <- leaflet::renderLeaflet({
    df <- datos_filtrados()
    shiny::validate(shiny::need(!is.null(df) && nrow(df) > 0,
                                "No hay datos disponibles para los filtros seleccionados."))
    
    mapa <- graficar_rutas_color_importancia(
      df, Año = input$anio, Mes = input$mes, Producto = input$producto
    )
    
    if (!inherits(mapa, "leaflet")) {
      leaflet() %>% addTiles() %>% addControl("Mapa no disponible", position = "topright")
    } else mapa
  })
  
  # -----------------------------------------------------------
  # 4. Cuadros de texto
  # -----------------------------------------------------------
  output$municipio_mas_importante <- renderText({
    df <- datos_filtrados()
    if (is.null(df)) return("Sin datos")
    resumen_prop <- df %>%
      group_by(region_geo) %>%
      summarise(
        prop_region = mean(prop_region_producto, na.rm = TRUE),
        kg_region = sum(suma_kg, na.rm = TRUE)
      ) %>%
      ungroup()
    top <- resumen_prop %>% slice_max(prop_region, n = 1)
    ruta_nom <- str_to_title(top$region_geo)
    porc_fmt <- paste0(round(top$prop_region * 100, 1), "%")
    kg_fmt <- format(round(top$kg_region, 0), big.mark = ".", decimal.mark = ",")
    glue("La ruta más importante para el abastecimiento es {ruta_nom}, representando el {porc_fmt} del total de volumen de ingreso ({kg_fmt} kg).")
  })
  
  output$ranking_rutas <- renderText({
    df <- datos_filtrados()
    if (is.null(df)) return("Sin datos")
    ranking <- df %>%
      group_by(region_geo) %>%
      summarise(importancia_total = sum(importancia_ruta, na.rm = TRUE)) %>%
      arrange(desc(importancia_total))
    rutas_texto <- paste(str_to_title(ranking$region_geo), collapse = ", ")
    glue("Las rutas del abastecimiento por orden de importancia son: {rutas_texto}.")
  })
  
  # -----------------------------------------------------------
  # 5. Botones
  # -----------------------------------------------------------
  observeEvent(input$reset, {
    updateSelectInput(session, "anio", selected = 2024)
    updateSelectInput(session, "mes", selected = "12")
    updateSelectInput(session, "producto", selected = "Aguacate Hass")
    updateCheckboxGroupInput(session, "rutas",
                             selected = sort(unique(data_cierres_final$region_geo)))
  })
  
  output$descargarDatos <- downloadHandler(
    filename = function() {
      glue("rutas_abastecimiento_{input$anio}_{input$mes}_{input$producto}.csv")
    },
    content = function(file) {
      df <- datos_filtrados()
      if (!is.null(df)) write_csv(df, file)
    }
  )
  
  # -----------------------------------------------------------
  # 6. Generación del informe PDF con logos
  # -----------------------------------------------------------
  output$descargarPDF <- downloadHandler(
    filename = function() {
      glue("rutas_abastecimiento_cierres_{input$anio}_{input$mes}_{input$producto}.pdf")
    },
    content = function(file) {
      tempReport <- file.path(getwd(), "informe.Rmd")
      df <- datos_filtrados()
      grafico <- graficar_rutas_color_importancia(
        df, Año = input$anio, Mes = input$mes, Producto = input$producto
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
