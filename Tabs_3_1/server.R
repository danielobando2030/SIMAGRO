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
library(webshot2)
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
    choices <- c("Todos los productos" = "Todos los productos",
                 setNames(as.list(productos), str_to_title(productos)))
    selectInput("producto", "Seleccione producto:", choices = choices,
                selected = "Todos los productos")
  })
  
  # --- 3. Select dinámico de años ---
  output$anioUI <- renderUI({
    req(data_sel())
    anios <- sort(unique(data_sel()$anio))
    selectInput("anio", "Seleccione el año:",
                choices = c("Todos los años", anios),
                selected = "Todos los años")
  })
  
  # --- 4. Gráfico reactivo principal ---
  grafico_reactivo <- reactive({
    req(input$producto, input$anio, input$temporalidad, input$variable)
    
    alimento     <- if (input$producto == "Todos los productos") NULL else input$producto
    anio_filtro  <- if (input$anio == "Todos los años") NULL else input$anio
    
    res <- graficar_variable(
      temporalidad = input$temporalidad,
      alimento     = alimento,
      variable     = input$variable,
      anio_filtro  = anio_filtro
    )
    
    # --- Procesamiento agregado por mes ---
    df_linea <- res$datos %>%
      group_by(mes_y_ano) %>%
      summarise(valor = mean(.data[[input$variable]], na.rm = TRUE), .groups = "drop")
    
    # --- Gráfico interactivo plotly ---
    grafico_interactivo <- plot_ly(
      data = df_linea,
      x = ~mes_y_ano,
      y = ~valor,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = col_grafico, width = 2),
      marker = list(color = col_grafico, size = 6)
    ) %>%
      layout(
        xaxis = list(title = "Fecha"),
        yaxis = list(title = ""),
        hovermode = "x unified",
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        margin = list(l = 50, r = 30, t = 30, b = 50)
      )
    
    # --- Cálculos resumen ---
    promedio      <- mean(df_linea$valor, na.rm = TRUE)
    fecha_max     <- df_linea$mes_y_ano[which.max(df_linea$valor)]
    fecha_min     <- df_linea$mes_y_ano[which.min(df_linea$valor)]
    
    # --- Producto más volátil ---
    df_vol <- data_sel()
    if (input$anio != "Todos los años") df_vol <- df_vol %>% filter(anio == input$anio)
    
    prod_mas_volatil <- df_vol %>%
      group_by(producto) %>%
      summarise(var_cambio = var(cambio_pct, na.rm = TRUE)) %>%
      slice_max(var_cambio, n = 1) %>%
      pull(producto)
    
    # --- Promedio de variación porcentual ---
    df_cambio <- data_sel()
    if (input$producto != "Todos los productos") df_cambio <- df_cambio %>% filter(producto == input$producto)
    if (input$anio != "Todos los años") df_cambio <- df_cambio %>% filter(anio == input$anio)
    promedio_cambio <- mean(df_cambio$cambio_pct, na.rm = TRUE)
    
    # --- Mes con mayor aumento anual ---
    df_anual <- df_cambio %>% filter(!is.na(cambio_pct_anual))
    df_max   <- df_anual %>% slice_max(cambio_pct_anual, n = 1)
    mes_max  <- df_max$mes_y_ano
    valor_max <- df_max$cambio_pct_anual
    
    list(
      grafico          = grafico_interactivo,
      datos            = df_linea,
      promedio         = promedio,
      fecha_max        = fecha_max,
      fecha_min        = fecha_min,
      prod_mas_volatil = prod_mas_volatil,
      promedio_cambio  = promedio_cambio,
      mes_max          = mes_max,
      valor_max        = valor_max
    )
  })
  
  # --- 5. Render gráfico ---
  output$grafico <- renderPlotly({
    grafico_reactivo()$grafico
  })
  
  # --- 6. Descargar gráfico visible (PNG exacto) ---
  output$descargar <- downloadHandler(
    filename = function() {
      glue("grafico_precios_{Sys.Date()}.png")
    },
    content = function(file) {
      res <- grafico_reactivo()
      
      # Guardar widget temporal HTML
      temp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(as_widget(res$grafico), temp_html, selfcontained = TRUE)
      
      # Capturar imagen visible del gráfico
      webshot2::webshot(
        temp_html,
        file = grafico_path,
        vwidth = 1200,
        vheight = 800,
        zoom = 2,
        delay = 0.5
      )
      
      if (!file.exists(grafico_path)) {
        stop(glue("No se generó el archivo PNG del gráfico: {grafico_path}"))
      }
    }
  )
  
  # --- 7. Descarga de datos CSV ---
  output$descargarDatos <- downloadHandler(
    filename = function() {
      glue("datos_precios_{Sys.Date()}.csv")
    },
    content = function(file) {
      write.csv(grafico_reactivo()$datos, file, row.names = FALSE)
    }
  )
  
  # --- 8. Descarga de informe PDF ---
  output$descargarInforme <- downloadHandler(
    filename = function() {
      glue("Informe_{input$producto}_{Sys.Date()}.pdf")
    },
    content = function(file) {
      res <- grafico_reactivo()
      temp_dir <- tempdir()
      
      # Guardar gráfico temporal como PNG
      grafico_path <- file.path(temp_dir, "grafico.png")
      temp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(as_widget(res$grafico), temp_html, selfcontained = TRUE)
      webshot2::webshot(temp_html, file = grafico_path, vwidth = 1200, vheight = 800, zoom = 2)
      
      # Renderizar informe PDF
      rmarkdown::render(
        input = "informe.Rmd",
        output_file = file,
        params = list(
          datos            = res$datos,
          grafico          = grafico_path,
          temporalidad     = input$temporalidad,
          variable         = input$variable,
          anio             = input$anio,
          producto         = input$producto,
          promedio         = res$promedio,
          fecha_max        = res$fecha_max,
          fecha_min        = res$fecha_min,
          prod_mas_volatil = res$prod_mas_volatil,
          promedio_cambio  = res$promedio_cambio,
          mes_max          = res$mes_max,
          valor_max        = res$valor_max,
          
          #  ESTOS ERAN LOS QUE FALTABAN 
          mensaje_volatil = glue(
            "El precio de '{res$prod_mas_volatil}' presenta la mayor volatilidad entre los productos analizados."
          ),
          mensaje_promedio = glue(
            "En promedio, los precios variaron un {round(res$promedio_cambio, 1)}%."
          ),
          mensaje_mesanual = glue(
            "En {format(res$mes_max, '%B de %Y')}, los precios aumentaron un {round(res$valor_max,1)}% frente al mismo mes del año anterior."
          )
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  # --- 9. Subtítulo dinámico ---
  output$subtitulo <- renderText({
    res <- grafico_reactivo()
    glue("El precio promedio fue de ${round(res$promedio, 0)}. 
         El valor máximo se registró en {format(res$fecha_max, '%B-%Y')} 
         y el mínimo en {format(res$fecha_min, '%B-%Y')}.")
  })
  
  # --- 10. Cuadros resumen ---
  output$texto_volatil <- renderUI({
    res <- grafico_reactivo()
    div(
      style = glue("
      background-color:{col_palette[1]};
      color:white;
      padding:15px; border-radius:5px; margin-top:10px;
    "),
      glue("El precio de '{res$prod_mas_volatil}' presenta la mayor volatilidad entre los productos analizados.")
    )
  })
  
  output$texto_promedio_cambio <- renderUI({
    res <- grafico_reactivo()
    div(
      style = glue("
      background-color:{col_palette[2]};
      color:white;
      padding:15px; border-radius:5px; margin-top:10px;
    "),
      glue("En promedio, los precios variaron un {round(res$promedio_cambio, 1)}%.")
    )
  })
  
  output$texto_mes_max_anual <- renderUI({
    res <- grafico_reactivo()
    div(
      style = glue("
      background-color:{col_palette[3]};
      color:white;
      padding:15px; border-radius:5px; margin-top:10px;
    "),
      glue("En {format(res$mes_max, '%B de %Y')}, los precios aumentaron un {round(res$valor_max,1)}% frente al mismo mes del año anterior.")
    )
  })
  
  # --- 11. Botón de restablecer filtros ---
  observeEvent(input$reset, {
    updateSelectInput(session, "temporalidad", selected = "mensual")
    updateSelectInput(session, "variable", selected = "precio_prom")
    updateSelectInput(session, "anio", selected = "Todos los años")
    updateSelectInput(session, "producto", selected = "Todos los productos")
  })
}
