################################################################################
# Proyecto FAO - VP - 2025
# Servidor - Módulo 3_1: Precios Mayoristas (Bogotá)
################################################################################
# Autores: Luis Miguel García, Laura Quintero, Daniel Obando
# Última edición: 2025/11/07
################################################################################

library(shiny)
library(plotly)
library(dplyr)
library(stringr)
library(glue)
library(ggplot2)
library(rmarkdown)
library(htmlwidgets)

# -------------------------------------------------------------------------------
# Cargar función principal
# -------------------------------------------------------------------------------
source("3_1b_functions_price.R")
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

# -------------------------------------------------------------------------------
# Servidor Shiny
# -------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- 1. Dataset reactivo según temporalidad ---
  data_sel <- reactive({
    if (input$temporalidad == "mensual") mensual else diaria
  })
  
  # --- 2. Select dinámico de productos ---
  output$productoUI <- renderUI({
    req(data_sel())
    productos <- sort(unique(data_sel()$producto))
    productos <- trimws(productos)
    choices <- c(
      "Todos los productos" = "Todos los productos",
      setNames(as.list(productos), str_to_title(productos))
    )
    selectInput(
      "producto", "Seleccione producto:",
      choices = choices,
      selected = "Todos los productos"
    )
  })
  
  # --- 3. Select dinámico de años ---
  output$anioUI <- renderUI({
    req(data_sel())
    anios <- sort(unique(data_sel()$anio))
    selectInput(
      "anio", "Seleccione el año:",
      choices = c("Todos los años", anios),
      selected = "Todos los años"
    )
  })
  
  # ---------------------------------------------------------------------------
  # 4. Gráfico reactivo principal + métricas + ggplot plano
  # ---------------------------------------------------------------------------
  grafico_reactivo <- reactive({
    req(input$producto, input$anio, input$temporalidad, input$variable)
    
    alimento    <- if (input$producto == "Todos los productos") NULL else input$producto
    anio_filtro <- if (input$anio == "Todos los años") NULL else input$anio
    
    # --- Llamada base a la función FAO ---
    res_fun <- graficar_variable(
      temporalidad = input$temporalidad,
      alimento     = alimento,
      variable     = input$variable,
      anio_filtro  = anio_filtro
    )
    
    # ---- promedio por fecha (línea) ----
    df_linea <- res_fun$datos %>%
      group_by(mes_y_ano) %>%
      summarise(
        valor = mean(.data[[input$variable]], na.rm = TRUE),
        .groups = "drop"
      )
    
    df_linea <- df_linea %>%
      mutate(
        hover_valor = format(round(valor, 2), big.mark=".", decimal.mark=","),
        hover_texto = paste0(
          "<b>Fecha:</b> ", format(mes_y_ano, "%d-%b-%Y"),
          "<br><b>Valor:</b> ", hover_valor
        )
      )
    
    tick_format <- if (input$anio == "Todos los años") "%B %Y" else "%B"
    
    min_y <- floor(min(df_linea$valor, na.rm = TRUE) / 500) * 500
    max_y <- ceiling(max(df_linea$valor, na.rm = TRUE) / 500) * 500
    ticks_y <- seq(min_y, max_y, by = 500)
    ticks_y_text <- format(ticks_y, big.mark=".", decimal.mark=",")
    
    # --------------------------------------------------
    # EJE X: etiquetas en español cada 3 meses
    # --------------------------------------------------
    idx_ticks <- seq(1, nrow(df_linea), by = 3)
    
    tick_vals_x <- df_linea$mes_y_ano[idx_ticks]
    tick_text_x <- str_to_title(
      format(tick_vals_x, "%B %Y")
    )
    
    grafico_interactivo <- plot_ly(
      data = df_linea,
      x = ~mes_y_ano,
      y = ~valor,
      type = "scatter",
      mode = "lines+markers",
      customdata = df_linea$hover_texto,
      hovertemplate = "%{customdata}<extra></extra>",
      line = list(color = col_grafico, width = 2),
      marker = list(color = col_grafico, size = 6)
    ) %>%
      layout(
        xaxis = list(
          title     = "Fecha",
          tickvals  = tick_vals_x,
          ticktext  = tick_text_x,
          tickangle = -90
        ),
        yaxis = list(
          title = "",
          tickvals = ticks_y,
          ticktext = ticks_y_text
        ),
        hoverlabel = list(bgcolor="white", font=list(color="black")),
        hovermode = "x unified",
        margin = list(l=50, r=30, t=30, b=50)
      )
    
    # ---- métricas base ----
    promedio  <- mean(df_linea$valor, na.rm = TRUE)
    fecha_max <- df_linea$mes_y_ano[which.max(df_linea$valor)]
    fecha_min <- df_linea$mes_y_ano[which.min(df_linea$valor)]
    
    # ---- métricas para mensajes laterales ----
    df_vol <- data_sel()
    if (input$anio != "Todos los años") df_vol <- df_vol %>% filter(anio == input$anio)
    
    prod_mas_volatil <- df_vol %>%
      group_by(producto) %>%
      summarise(var_cambio = var(cambio_pct, na.rm = TRUE), .groups="drop") %>%
      slice_max(var_cambio, n = 1) %>%
      pull(producto)
    
    df_cambio <- data_sel()
    if (input$producto != "Todos los productos") df_cambio <- df_cambio %>% filter(producto == input$producto)
    if (input$anio != "Todos los años") df_cambio <- df_cambio %>% filter(anio == input$anio)
    
    promedio_cambio <- mean(df_cambio$cambio_pct, na.rm = TRUE)
    
    df_anual <- df_cambio %>% filter(!is.na(cambio_pct_anual))
    df_max   <- df_anual %>% slice_max(cambio_pct_anual, n = 1)
    
    list(
      grafico        = grafico_interactivo,
      grafico_plano  = res_fun$grafico_plano,   # <- CLAVE FAO
      datos          = df_linea,
      promedio       = promedio,
      fecha_max      = fecha_max,
      fecha_min      = fecha_min,
      prod_mas_volatil = prod_mas_volatil,
      promedio_cambio  = promedio_cambio,
      mes_max          = df_max$mes_y_ano,
      valor_max        = df_max$cambio_pct_anual
    )
  })
  
  # --- 5. Mostrar gráfico ---
  output$grafico <- renderPlotly({
    grafico_reactivo()$grafico
  })
  
  # --- 6. Descargar CSV ---
  output$descargarDatos <- downloadHandler(
    filename = function() glue("datos_precios_{Sys.Date()}.csv"),
    content = function(file) {
      write.csv(grafico_reactivo()$datos, file, row.names = FALSE)
    }
  )
  
  # --- 7. Descargar PDF (ggplot FAO) ---
  output$descargarInforme <- downloadHandler(
    filename = function() glue("Informe_{input$producto}_{Sys.Date()}.pdf"),
    content = function(file) {
      
      res <- grafico_reactivo()
      
      # Mensajes (recalculados aquí, como debe ser)
      mensaje_volatil <- glue(
        "El producto '{res$prod_mas_volatil}' presenta la mayor volatilidad."
      )
      
      mensaje_promedio <- glue(
        "En promedio, los precios variaron un ",
        format(round(res$promedio_cambio, 1), big.mark=".", decimal.mark=","),
        "%."
      )
      
      mensaje_mesanual <- glue(
        "En {format(res$mes_max, '%B de %Y')}, los precios aumentaron un ",
        format(round(res$valor_max, 1), big.mark=".", decimal.mark=","),
        "%."
      )
      
      rmarkdown::render(
        "informe.Rmd",
        output_file = file,
        params = list(
          plot             = res$grafico_plano,
          datos            = res$datos,
          promedio         = res$promedio,
          fecha_max        = res$fecha_max,
          fecha_min        = res$fecha_min,
          producto         = input$producto,
          anio             = input$anio,
          temporalidad     = input$temporalidad,
          variable         = input$variable,
          mensaje_volatil  = mensaje_volatil,
          mensaje_promedio = mensaje_promedio,
          mensaje_mesanual = mensaje_mesanual
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # ------------------------------------------------------------------
  # MENSAJES LATERALES (SIN CAMBIOS)
  # ------------------------------------------------------------------
  output$texto_volatil <- renderUI({
    res <- grafico_reactivo()
    div(
      style = glue("background-color:{col_palette[1]}; color:white;
                    padding:15px; border-radius:5px; margin-top:10px;"),
      glue("El producto '{res$prod_mas_volatil}' presenta la mayor volatilidad.")
    )
  })
  
  output$texto_promedio_cambio <- renderUI({
    res <- grafico_reactivo()
    div(
      style = glue("background-color:{col_palette[2]}; color:white;
                    padding:15px; border-radius:5px; margin-top:10px;"),
      glue(
        "En promedio, los precios variaron un ",
        format(round(res$promedio_cambio,1), big.mark='.', decimal.mark=','),
        "%."
      )
    )
  })
  
  output$texto_mes_max_anual <- renderUI({
    res <- grafico_reactivo()
    div(
      style = glue("background-color:{col_palette[3]}; color:white;
                    padding:15px; border-radius:5px; margin-top:10px;"),
      glue(
        "En {format(res$mes_max, '%B de %Y')}, los precios aumentaron un ",
        format(round(res$valor_max,1), big.mark='.', decimal.mark=','),
        "%."
      )
    )
  })
  
  
  
  output$descargar <- downloadHandler(
    filename = function() {
      paste("IND2_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      # Forzar la ejecución de la función reactiva
      res <- grafico_reactivo()
      
      # Usa ggsave para guardar el gráfico
      ggplot2::ggsave(filename = file, plot = res$grafico_plano, width = 13, height = 7, dpi = 200)
    }
  )
  
  
  
  
  
  
  # --- 8. Reset ---
  observeEvent(input$reset, {
    updateSelectInput(session, "temporalidad", selected = "mensual")
    updateSelectInput(session, "variable", selected = "precio_prom")
    updateSelectInput(session, "anio", selected = "Todos los años")
    updateSelectInput(session, "producto", selected = "Todos los productos")
  })

  
  
  
  
  }
