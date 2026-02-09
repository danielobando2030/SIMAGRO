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

library(callr)
library(maps)   # ✅ CORRECTO (no 'map')

# -----------------------------------------------------------
# MAPA BASE COLOMBIA (UNA SOLA VEZ)
# -----------------------------------------------------------
colombia_map <- map_data("world", region = "Colombia")

# -----------------------------------------------------------
# Funciones auxiliares LaTeX
# -----------------------------------------------------------
latex_escape <- function(txt) {
  if (is.null(txt) || is.na(txt)) return("")
  txt <- gsub("%", "\\\\%", txt)
  txt <- gsub("&", "\\\\&", txt)
  txt <- gsub("#", "\\\\#", txt)
  txt <- gsub("_", "\\\\_", txt)
  txt <- gsub("\\{", "\\\\{", txt)
  txt <- gsub("\\}", "\\\\}", txt)
  txt
}

latex_safe <- function(x) {
  if (is.null(x) || is.na(x)) return("")
  # NO escapar la barra invertida aquí para no romper \textbf
  x <- gsub("%", "\\\\%", x)
  x <- gsub("&", "\\\\&", x)
  x <- gsub("#", "\\\\#", x)
  x <- gsub("_", "\\\\_", x)
  x <- gsub("\\$", "\\\\$", x)
  x
}

html_to_latex <- function(txt) {
  if (is.null(txt) || is.na(txt)) return("")
  txt <- gsub("%", "\\\\%", txt)
  txt <- gsub("&", "\\\\&", txt)
  txt <- gsub("#", "\\\\#", txt)
  txt <- gsub("_", "\\\\_", txt)
  txt <- gsub("<b>", "\\\\textbf{", txt)
  txt <- gsub("</b>", "}", txt)
  txt
}

formato_pct_es <- function(x, digits = 1) {
  x <- x * 100
  if (abs(x - round(x)) < 1e-8) {
    paste0(formatC(round(x), format = "f", digits = 0), "%")
  } else {
    paste0(formatC(x, format = "f", digits = digits, decimal.mark = ","), "%")
  }
}

################################################################################-
# SERVER
################################################################################-
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
  # 1. SELECTINPUTS
  ###########################################################################
  observe({
    updateSelectInput(session, "anio",
                      choices  = sort(unique(data_cierres_final$anio)),
                      selected = ifelse(2024 %in% data_cierres_final$anio, 2024, max(data_cierres_final$anio))
    )
    
    updateSelectInput(session, "mes",
                      choices = setNames(sprintf("%02d", 1:12),
                                         c("Enero","Febrero","Marzo","Abril","Mayo",
                                           "Junio","Julio","Agosto","Septiembre",
                                           "Octubre","Noviembre","Diciembre")),
                      selected = "12"
    )
    
    updateSelectInput(session, "producto",
                      choices  = sort(unique(data_cierres_final$producto)),
                      selected = ifelse("Aguacate Hass" %in% data_cierres_final$producto,
                                        "Aguacate Hass", data_cierres_final$producto[1])
    )
  })
  
  ###########################################################################
  # 2. DATOS FILTRADOS
  ###########################################################################
  datos_filtrados <- reactive({
    req(input$anio, input$mes, input$producto)
    
    df <- data_cierres_final %>%
      filter(
        anio     == input$anio,
        mes      == as.integer(input$mes),
        producto == input$producto
      )
    
    sel <- rutas_seleccionadas()
    if (length(sel) > 0) df <- df %>% filter(region_geo %in% sel)
    if (nrow(df) == 0) return(NULL)
    df
  })
  
  #####################################################################
  # MENSAJES PANEL DERECHO (DASHBOARD)  ✅ (RESTABLECIDOS)
  #####################################################################
  
  output$municipio_mas_importante <- renderUI({
    df <- datos_filtrados()
    if (is.null(df)) return(HTML("—"))
    
    resumen <- df %>%
      group_by(region_geo) %>%
      summarise(kg = sum(suma_kg, na.rm = TRUE), .groups = "drop")
    
    total <- sum(resumen$kg, na.rm = TRUE)
    top   <- resumen %>% slice_max(kg, n = 1)
    pct   <- formato_pct_es(top$kg / total)
    
    HTML(glue(
      "La ruta externa más importante para el abastecimiento es ",
      "<b>{str_to_title(top$region_geo)}</b>, ",
      "representando el <b>{pct}</b> del total del volumen."
    ))
  })
  
  # 👇 Tu UI usa htmlOutput("ranking_rutas"), por eso esto debe ser renderUI()
  output$ranking_rutas <- renderUI({
    df <- datos_filtrados()
    if (is.null(df)) return(HTML("—"))
    
    ranking <- df %>%
      group_by(region_geo) %>%
      summarise(importancia = sum(importancia_ruta, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(importancia))
    
    HTML(glue(
      "Las rutas de abastecimiento por orden de importancia son: ",
      "<b>{paste(str_to_title(ranking$region_geo), collapse = ', ')}</b>."
    ))
  })
  
  output$impacto_cierre <- renderUI({
    df <- datos_filtrados()
    if (is.null(df)) return(HTML("—"))
    
    total_mes <- sum(df$suma_kg, na.rm = TRUE)
    sel <- rutas_seleccionadas()
    
    todas <- c("Noroccidente","Nororiente","Norte","Oriente",
               "Suroriente","Sur","Suroccidente","Occidente")
    
    if (setequal(sel, todas)) {
      HTML(glue(
        "Un cierre total de las rutas implicaría una reducción del ",
        "<b>100%</b> del abastecimiento mensual ",
        "(<b>{format(total_mes, big.mark='.', decimal.mark=',')}</b> toneladas)."
      ))
    } else {
      perdidas <- df %>% filter(!region_geo %in% sel)
      ton <- sum(perdidas$suma_kg, na.rm = TRUE)
      pct <- round(ton / total_mes * 100, 1)
      
      HTML(glue(
        "Un cierre de las rutas no seleccionadas podría reducir el abastecimiento en ",
        "<b>{pct}%</b> ",
        "(<b>{format(ton, big.mark='.', decimal.mark=',')}</b> toneladas)."
      ))
    }
  })
  
  ###########################################################################
  # 6. MAPA INTERACTIVO
  ###########################################################################
  output$grafico <- leaflet::renderLeaflet({
    df <- datos_filtrados()
    shiny::validate(shiny::need(!is.null(df), "No hay datos disponibles."))
    
    graficar_rutas_color_importancia(
      df,
      Año = input$anio,
      Mes = as.integer(input$mes),
      Producto = input$producto
    )
  })
  
  
  
  output$descargar <- downloadHandler(
    filename = function() {
      paste("IND2_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      # Forzar la ejecución de la función reactiva
      
      
      # Usa ggsave para guardar el gráfico
      ggplot2::ggsave(filename = file, plot = plot_estatico_pdf(), width = 13, height = 7, dpi = 200)
    }
  )  
  
  
  
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste0("datos_comparativo_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <-datos_filtrados()
      
      req(res)
      write.csv(res, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  
  ###########################################################################
  # 8.5 MAPA ESTÁTICO GGplot PARA PDF
  ###########################################################################
  plot_estatico_pdf <- reactive({
    df <- datos_filtrados()
    if (is.null(df)) return(NULL)
    
    graficar_rutas_color_importancia_estatico(
      df,
      Año = input$anio,
      Mes = as.integer(input$mes),
      Producto = input$producto
    )
  })
  
  ###########################################################################
  # 9. PDF  ✅ SIN mensaje*_pdf()  (se calculan aquí adentro)
  ###########################################################################
  output$descargarPDF <- downloadHandler(
    filename = function() {
      glue("rutas_abastecimiento_{input$anio}_{input$mes}_{input$producto}.pdf")
    },
    content = function(file) {
      
      df <- datos_filtrados()
      if (is.null(df)) return(NULL)
      
      # ------- MENSAJES PARA PDF (LaTeX) - calculados localmente -------
      # (no afectan el dashboard)
      resumen <- df %>%
        group_by(region_geo) %>%
        summarise(kg = sum(suma_kg, na.rm = TRUE), .groups = "drop")
      
      total <- sum(resumen$kg, na.rm = TRUE)
      top   <- resumen %>% slice_max(kg, n = 1)
      pct1  <- formato_pct_es(top$kg / total)
      pct1  <- gsub("%", "\\\\%", pct1)   # ← CLAVE
      
      mensaje1 <- paste0(
        "La ruta externa más importante para el abastecimiento es ",
        "\\textbf{", str_to_title(top$region_geo), "}, ",
        "representando el \\textbf{", pct1, "} del total del volumen."
      )
      
      ranking <- df %>%
        group_by(region_geo) %>%
        summarise(importancia = sum(importancia_ruta, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(importancia))
      
      mensaje2 <- glue(
        "Las rutas de abastecimiento por orden de importancia son: ",
        paste(str_to_title(ranking$region_geo), collapse = ", "),
        "."
      )
      
      total_mes <- sum(df$suma_kg, na.rm = TRUE)
      sel <- rutas_seleccionadas()
      todas <- c("Noroccidente","Nororiente","Norte","Oriente",
                 "Suroriente","Sur","Suroccidente","Occidente")
      
      if (setequal(sel, todas)) {
        
        # 👉 CIERRE TOTAL
        mensaje3 <- paste0(
          "Un cierre total de las rutas implicaría una reducción del ",
          "\\textbf{100\\%} del abastecimiento mensual ",
          "(\\textbf{", format(total_mes, big.mark='.', decimal.mark=','), "} toneladas)."
        )
        
      } else {
        
        # 👉 CIERRE PARCIAL
        perdidas <- df %>% filter(!region_geo %in% sel)
        ton  <- sum(perdidas$suma_kg, na.rm = TRUE)
        pct3 <- round(ton / total_mes * 100, 1)
        
        mensaje3 <- paste0(
          "Un cierre de las rutas no seleccionadas podría reducir el abastecimiento en ",
          "\\textbf{", pct3, "\\%} ",
          "(\\textbf{", format(ton, big.mark='.', decimal.mark=','), "} toneladas)."
        )
      }
      
      # ------- logos -------
      tmp_dir  <- tempdir()
      logo_sup <- file.path(tmp_dir, "logo_3.png")
      logo_inf <- file.path(tmp_dir, "logo_2.png")
      file.copy("www/logo_3.png", logo_sup, overwrite = TRUE)
      file.copy("www/logo_2.png", logo_inf, overwrite = TRUE)
      
      # Al pasar a los params, NO uses latex_safe si el mensaje ya tiene LaTeX
      # O usa la versión corregida de arriba
      params <- list(
        anio     = input$anio,
        mes      = input$mes,
        producto = latex_safe(input$producto),
        datos    = df,
        plot     = plot_estatico_pdf(),
        logo_sup = logo_sup,
        logo_inf = logo_inf,
        mensaje1 = mensaje1, # Pásalo directo, ya está construido con \textbf
        mensaje2 = mensaje2,
        mensaje3 = mensaje3
      )
      
      
      callr::r(
        func = function(input_file, output_file, params) {
          rmarkdown::render(
            input       = input_file,
            output_file = output_file,
            params      = params,
            envir       = new.env(parent = globalenv())
          )
        },
        args = list(
          input_file  = file.path(getwd(), "informe.Rmd"),
          output_file = file,
          params      = params
        )
      )
    }
  )
  
} # fin server
################################################################################-
# FIN SERVER
################################################################################-
