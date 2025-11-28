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
  
  html_to_latex <- function(txt){
    txt <- gsub("<b>", "\\\\textbf{", txt)
    txt <- gsub("</b>", "}", txt)
    return(txt)
  }
  
  
  ###########################################################################
  # 1. INICIALIZAR SELECTINPUTS  (CAMBIO MINIMO)
  ###########################################################################
  observe({
    
    # Se ejecuta SOLO AL INICIAR, porque no depende de inputs
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
                      choices = sort(unique(data_cierres_final$producto)),
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
  
  ###########
  
  output$descargar <- downloadHandler(
    filename = function() {
      glue("grafico_rutas_{input$anio}_{input$mes}_{input$producto}.png")
    },
    content = function(file) {
      
      df <- datos_filtrados()
      if (is.null(df)) {
        showNotification("No hay datos para generar el mapa.", type = "error")
        return(NULL)
      }
      
      # 1. Crear mapa leaflet
      mapa <- graficar_rutas_color_importancia(
        df,
        Año      = input$anio,
        Mes      = as.integer(input$mes),
        Producto = input$producto
      )
      
      # 2. Exportar PNG usando tu función robusta (que ya tienes)
      tmp_png <- grafico_plano(mapa)
      
      # 3. Copiar al archivo final
      file.copy(tmp_png, file, overwrite = TRUE)
    }
  )
  
  ###########################################################################
  # 3. MENSAJE 1 (TEXTO DERECHO)
  ###########################################################################
  
  mensaje1_reactivo <- function(raw = FALSE) {
    df <- datos_filtrados()
    if (is.null(df)) return("Sin datos")
    
    resumen_mes <- df %>% 
      group_by(region_geo) %>% 
      summarise(
        kg = sum(suma_kg, na.rm = TRUE),
        .groups = "drop"
      )
    
    total_mes <- sum(resumen_mes$kg, na.rm = TRUE)
    
    top <- resumen_mes %>% slice_max(kg, n = 1)
    pct <- round((top$kg / total_mes) * 100, 1)
    
    txt <- glue(
      "La ruta externa al departamento más importante para el abastecimiento de Cundinamarca es ",
      "<b>{str_to_title(top$region_geo)}</b>, ",
      "representando el <b>{pct}%</b> del total de volumen de ingreso a las principales centrales de abasto."
    )
    
    if (raw) return(txt)
    return(latex_escape(txt))
  }
  
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
    
    txt <- glue("Las rutas de abastecimiento en Cundinamarca por orden de importancia en el periodo y para el
producto seleccionado son: {rutas_txt}.")
    
    latex_escape(txt)
  })
  
  ###########################################################################
  # 4. MENSAJE 3 (TEXTO DERECHO)
  ###########################################################################
  
  mensaje3_reactivo <- function(raw = FALSE) {
    # Data filtrada por año, mes y producto (todas las rutas)
    df_mes <- data_cierres_final %>%
      filter(
        anio == input$anio,
        mes == as.integer(input$mes),
        producto == input$producto
      )
    
    if (nrow(df_mes) == 0) return("Sin datos")
    
    # Total del mes
    total_mes <- sum(df_mes$suma_kg, na.rm = TRUE)
    
    # Rutas seleccionadas por el usuario
    sel <- rutas_seleccionadas()
    
    # ----- CASO 1: TODAS LAS RUTAS ESTÁN SELECCIONADAS → PÉRDIDA = 100% -----
    todas_rutas <- c("Noroccidente","Nororiente","Norte","Oriente",
                     "Suroriente","Sur","Suroccidente","Occidente")
    
    if (setequal(sel, todas_rutas)) {
      
      txt <- glue(
        "Un hipotético cierre de las rutas seleccionadas podría reducir el ",
        "abastecimiento de alimentos en un <b>100%</b>, ",
        "al dejar de ingresar <b>{format(total_mes, big.mark='.', decimal.mark=',')}</b> ",
        "toneladas a las principales centrales de abasto de Bogotá."
      )
      
      if (raw) return(txt)
      return(latex_escape(txt))
    }
    
    # ----- CASO 2: ALGUNA RUTA FUE DESMARCADA → cálculo real -----
    df_cerradas <- df_mes %>% filter(!region_geo %in% sel)
    
    ton_perdidas <- sum(df_cerradas$suma_kg, na.rm = TRUE)
    pct <- round((ton_perdidas / total_mes) * 100, 1)
    
    txt <- glue(
      "Un hipotético cierre de las rutas seleccionadas podría reducir el ",
      "abastecimiento de alimentos en un <b>{pct}%</b>, ",
      "al dejar de ingresar <b>{format(ton_perdidas, big.mark='.', decimal.mark=',')}</b> ",
      "toneladas a las principales centrales de abasto de Bogotá."
    )
    
    if (raw) return(txt)
    return(latex_escape(txt))
  }
  
  ###########################################################################
  # 5. TEXTO PANEL DERECHO
  ###########################################################################
  output$municipio_mas_importante <- renderUI({
    HTML(mensaje1_reactivo(raw = TRUE))
  })
  output$ranking_rutas <- renderText(mensaje2_reactivo())
  
  output$impacto_cierre <- renderUI({
    HTML(mensaje3_reactivo(raw = TRUE))
  })
  
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
  # 7. RESET (con bloqueo de eventos JS para evitar loop)
  ###########################################################################
  observeEvent(input$reset, {
    
    # Bloquear eventos de checkboxes mientras se actualizan
    session$sendCustomMessage("block_checkbox_events", TRUE)
    
    # Asegurar que se re-activen aunque falle algo
    on.exit({
      session$sendCustomMessage("block_checkbox_events", FALSE)
    }, add = TRUE)
    
    # Actualizar selects
    updateSelectInput(session, "anio",     selected = 2024)
    updateSelectInput(session, "mes",      selected = "12")
    updateSelectInput(session, "producto", selected = "Aguacate Hass")
    
    # Actualizar checkboxes
    rutas <- c("r_Noroccidente","r_Nororiente","r_Norte",
               "r_Oriente","r_Suroriente","r_Sur",
               "r_Suroccidente","r_Occidente")
    
    for (r in rutas) {
      updateCheckboxInput(session, r, TRUE)
    }
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
        mensaje1    = html_to_latex(mensaje1_reactivo(raw = TRUE)),
        mensaje2    = html_to_latex(mensaje2_reactivo(raw = TRUE)),
        mensaje3    = html_to_latex(mensaje3_reactivo(raw = TRUE))
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
