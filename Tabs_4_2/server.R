################################################################################-
# Proyecto FAO - VP - 2025
# SERVER – Mapa de Rutas (PDF OK, PNG OK, mensajes seguros LaTeX)
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

# Función para escapar caracteres peligrosos en LaTeX
latex_escape <- function(txt) {
  if (is.null(txt) || is.na(txt)) return("")
  txt <- gsub("%", "\\\\%", txt)
  txt <- gsub("&", "\\\\&", txt)
  txt <- gsub("#", "\\\\#", txt)
  txt <- gsub("_", "\\\\_", txt)
  txt <- gsub("\\{", "\\\\{", txt)
  txt <- gsub("\\}", "\\\\}", txt)
  return(txt)
}

# Cargar función y datos
source("4_2b_cierres_rutas_abastecimiento.R")   

server <- function(input, output, session) {
  
  ###########################################################################
  # 0. RUTAS SELECCIONADAS
  ###########################################################################
  rutas_seleccionadas <- reactive({
    rutas <- c()
    if (isTRUE(input$r_Noroccidente)) rutas <- c(rutas, "Noroccidente")
    if (isTRUE(input$r_Nororiente))   rutas <- c(rutas, "Nororiente")
    if (isTRUE(input$r_Norte))        rutas <- c(rutas, "Norte")
    if (isTRUE(input$r_Oriente))      rutas <- c(rutas, "Oriente")
    if (isTRUE(input$r_Suroriente))   rutas <- c(rutas, "Suroriente")
    if (isTRUE(input$r_Sur))          rutas <- c(rutas, "Sur")
    if (isTRUE(input$r_Suroccidente)) rutas <- c(rutas, "Suroccidente")
    if (isTRUE(input$r_Occidente))    rutas <- c(rutas, "Occidente")
    rutas
  })
  
  ###########################################################################
  # 1. INICIALIZAR SELECTINPUTS
  ###########################################################################
  observe({
    
    updateSelectInput(session, "anio",
                      choices  = sort(unique(data_cierres_final$anio)),
                      selected = ifelse(2024 %in% data_cierres_final$anio, 2024,
                                        max(data_cierres_final$anio)))
    
    updateSelectInput(session, "mes",
                      choices = setNames(sprintf("%02d", 1:12),
                                         c("Enero","Febrero","Marzo","Abril","Mayo",
                                           "Junio","Julio","Agosto","Septiembre",
                                           "Octubre","Noviembre","Diciembre")),
                      selected = "12")
    
    updateSelectInput(session, "producto",
                      choices  = sort(unique(data_cierres_final$producto)),
                      selected = ifelse("Aguacate Hass" %in% data_cierres_final$producto,
                                        "Aguacate Hass",
                                        data_cierres_final$producto[1]))
  })
  
  ###########################################################################
  # 2. DATOS FILTRADOS
  ###########################################################################
  datos_filtrados <- reactive({
    req(input$anio, input$mes, input$producto)
    
    df <- data_cierres_final %>%
      filter(
        anio == input$anio,
        mes  == as.integer(input$mes),
        producto == input$producto
      )
    
    sel <- rutas_seleccionadas()
    if (length(sel) > 0) df <- df %>% filter(region_geo %in% sel)
    
    if (nrow(df) == 0) return(NULL)
    df
  })
  
  ###########################################################################
  # 3. MENSAJE 1 (TEXTO DERECHO)
  ###########################################################################
  mensaje1_reactivo <- reactive({
    df <- datos_filtrados()
    if (is.null(df)) return("Sin datos")
    
    resumen <- df %>% 
      group_by(region_geo) %>% 
      summarise(
        prop_region = mean(prop_region_producto, na.rm = TRUE),
        kg_region   = sum(suma_kg, na.rm = TRUE),
        .groups = "drop"
      )
    
    if (nrow(resumen) == 0) return("Sin datos")
    
    top <- resumen %>% slice_max(prop_region, n = 1)
    
    txt <- glue(
      "La ruta más importante es {str_to_title(top$region_geo)}, ",
      "representando el {round(top$prop_region * 100, 1)}% ",
      "del volumen total ({format(top$kg_region, big.mark='.', decimal.mark=',')} kg)."
    )
    
    latex_escape(txt)
  })
  
  ###########################################################################
  # 4. MENSAJE 2 (TEXTO DERECHO)
  ###########################################################################
  mensaje2_reactivo <- reactive({
    df <- datos_filtrados()
    if (is.null(df)) return("Sin datos")
    
    ranking <- df %>%
      group_by(region_geo) %>%
      summarise(importancia_total = sum(importancia_ruta),
                .groups = "drop") %>%
      arrange(desc(importancia_total))
    
    rutas_txt <- paste(str_to_title(ranking$region_geo), collapse = ", ")
    
    txt <- glue("Las rutas por orden de importancia son: {rutas_txt}.")
    
    latex_escape(txt)
  })
  
  ###########################################################################
  # 5. TEXTO PANEL DERECHO
  ###########################################################################
  output$municipio_mas_importante <- renderText(mensaje1_reactivo())
  output$ranking_rutas            <- renderText(mensaje2_reactivo())
  
  ###########################################################################
  # 6. MAPA
  ###########################################################################
  output$grafico <- leaflet::renderLeaflet({
    df <- datos_filtrados()
    
    shiny::validate(
      shiny::need(!is.null(df), "No hay datos disponibles para los filtros seleccionados.")
    )
    
    graficar_rutas_color_importancia(
      df,
      Año = input$anio,
      Mes = as.integer(input$mes),
      Producto = input$producto
    )
  })
  
  ###########################################################################
  # 7. RESET
  ###########################################################################
  observeEvent(input$reset, {
    updateSelectInput(session, "anio", selected = 2024)
    updateSelectInput(session, "mes", selected = "12")
    updateSelectInput(session, "producto", selected = "Aguacate Hass")
    
    rutas <- c("r_Noroccidente","r_Nororiente","r_Norte",
               "r_Oriente","r_Suroriente","r_Sur",
               "r_Suroccidente","r_Occidente")
    
    for (r in rutas) updateCheckboxInput(session, r, TRUE)
  })
  
  ###########################################################################
  # 8. DESCARGAR CSV
  ###########################################################################
  output$descargarDatos <- downloadHandler(
    filename = function() {
      glue("rutas_abastecimiento_{input$anio}_{input$mes}_{input$producto}.csv")
    },
    content = function(file) {
      df <- datos_filtrados()
      if (!is.null(df)) write_csv(df, file)
    }
  )
  
  ###########################################################################
  # 9. GENERACIÓN PDF (PNG + MENSAJES OK)
  ###########################################################################
  output$descargarPDF <- downloadHandler(
    filename = function() {
      glue("rutas_abastecimiento_{input$anio}_{input$mes}_{input$producto}.pdf")
    },
    content = function(file) {
      
      df <- datos_filtrados()
      if (is.null(df)) {
        showNotification("No hay datos para generar el informe.", type="error")
        return(NULL)
      }
      
      tempReport <- file.path(getwd(), "informe.Rmd")
      mes_num    <- as.integer(input$mes)
      
      # ----------- GENERAR PNG DEL MAPA ------------
      mapa <- graficar_rutas_color_importancia(df, Año=input$anio, Mes=mes_num, Producto=input$producto)
      
      tmp_dir  <- tempdir()
      tmp_html <- file.path(tmp_dir, "mapa_tmp.html")
      tmp_png  <- file.path(tmp_dir, "mapa_tmp.png")
      
      htmlwidgets::saveWidget(mapa, tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, tmp_png, vwidth=1600, vheight=900, delay=1)
      
      # ----------- LOGOS ------------
      logo_sup <- file.path(tmp_dir, "logo_3.png")
      logo_inf <- file.path(tmp_dir, "logo_2.png")
      file.copy("www/logo_3.png", logo_sup, overwrite = TRUE)
      file.copy("www/logo_2.png", logo_inf, overwrite = TRUE)
      
      # ----------- PARÁMETROS DEL INFORME ------------
      params <- list(
        anio        = input$anio,
        mes         = input$mes,
        producto    = input$producto,
        datos       = df,
        grafico_png = tmp_png,
        logo_sup    = logo_sup,
        logo_inf    = logo_inf,
        mensaje1    = mensaje1_reactivo(),
        mensaje2    = mensaje2_reactivo()
      )
      
      showNotification("Generando informe PDF...", duration=NULL, id="pdf_n")
      
      callr::r(
        func = function(input_file, output_file, params) {
          rmarkdown::render(input = input_file,
                            output_file = output_file,
                            params = params,
                            envir = new.env(parent = globalenv()))
        },
        args = list(input_file=tempReport, output_file=file, params=params)
      )
      
      removeNotification("pdf_n")
    }
  )
}

################################################################################-
# FIN SERVER
################################################################################-
