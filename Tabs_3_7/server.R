################################################################################
# Proyecto FAO - VP - 2025
# SERVER - Variación porcentual mensual del precio promedio
################################################################################

pacman::p_load(shiny, plotly, dplyr, zoo, lubridate, rmarkdown, webshot2, htmlwidgets)
options(scipen = 999)

# Cargar función del gráfico
source("3_7b_variaciones_precio_presente_precio_pasado.R")

# --- Definir servidor ---
server <- function(input, output, session) {
  
  ##############################################################################
  # --- Filtro reactivo principal ---
  ##############################################################################
  data_filtrada <- reactive({
    req(input$producto, input$anio)
    
    df <- data %>% filter(producto == input$producto)
    
    df <- df %>%
      mutate(mes_y_ano = lubridate::ym(gsub("[^0-9]", "", as.character(mes_y_ano)))) %>%
      filter(year(mes_y_ano) == as.numeric(input$anio)) %>%
      arrange(mes_y_ano)
    
    df
  })
  
  ##############################################################################
  # --- Gráfico principal ---
  ##############################################################################
  grafico_reactivo <- reactive({
    df <- data_filtrada()
    
    if (nrow(df) == 0) {
      showNotification("⚠️ No hay datos suficientes para este producto y año.", type = "warning")
      return(NULL)
    }
    
    df <- df %>%
      mutate(cambio_pct_mensual = (precio_prom - lag(precio_prom)) / lag(precio_prom) * 100) %>%
      filter(!is.na(cambio_pct_mensual))
    
    plot_ly(df,
            x = ~mes_y_ano,
            y = ~cambio_pct_mensual,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = '#DBC21F', width = 2),
            marker = list(color = '#DBC21F', size = 8),
            text = ~paste0(
              "Mes: ", format(mes_y_ano, "%B %Y"),
              "<br>Cambio mensual: ", round(cambio_pct_mensual, 2), "%"
            ),
            hoverinfo = 'text') %>%
      add_lines(y = 0, x = ~mes_y_ano,
                line = list(color = 'rgba(155,48,255,0.3)', dash = 'dash'),
                showlegend = FALSE) %>%
      layout(
        title = NULL,
        xaxis = list(
          title = "Mes",
          tickvals = df$mes_y_ano,
          ticktext = tools::toTitleCase(format(df$mes_y_ano, "%B")),
          tickangle = -90,
          tickfont = list(size = 12)
        ),
        yaxis = list(title = "Cambio % mensual"),
        hovermode = "x unified"
      )
  })
  
  output$grafico <- renderPlotly({
    grafico_reactivo()
  })
  
  ##############################################################################
  # --- Panel lateral: mayor cambio mensual ---
  ##############################################################################
  output$mayorCambio <- renderUI({
    df <- data_filtrada()
    if (nrow(df) == 0) return(NULL)
    
    df <- df %>%
      arrange(mes_y_ano) %>%
      mutate(cambio_pct_mensual = (precio_prom - lag(precio_prom)) / lag(precio_prom) * 100) %>%
      filter(!is.na(cambio_pct_mensual))
    
    max_change <- df[which.max(df$cambio_pct_mensual), ]
    min_change <- df[which.min(df$cambio_pct_mensual), ]
    
    mes_max  <- tools::toTitleCase(format(max_change$mes_y_ano, "%B %Y"))
    mes_min  <- tools::toTitleCase(format(min_change$mes_y_ano, "%B %Y"))
    valor_max <- round(max_change$cambio_pct_mensual, 2)
    valor_min <- round(min_change$cambio_pct_mensual, 2)
    
    HTML(glue::glue("
      <div class='panel-cambio'>
        <p><b> Mayor incremento mensual:</b><br>
        {mes_max} (+{valor_max}%)</p>
        <p><b> Mayor reducción mensual:</b><br>
        {mes_min} ({valor_min}%)</p>
      </div>
    "))
  })
  
  ##############################################################################
  # --- Botón de reset ---
  ##############################################################################
  observeEvent(input$reset, {
    updateSelectInput(session, "producto", selected = "Aguacate")
    updateSelectInput(session, "anio", selected = "2014")
  })
  
  ##############################################################################
  # --- Descarga del informe PDF (con LaTeX) ---
  ##############################################################################
  output$report <- downloadHandler(
    filename = function() {
      paste0("informe_variacion_", input$producto, "_", input$anio, ".pdf")
    },
    
    content = function(file) {
      
      tmp_dir <- tempdir()
      tmp_rmd <- file.path(tmp_dir, "informe.Rmd")
      tmp_html <- file.path(tmp_dir, "grafico_tmp.html")
      tmp_png  <- file.path(tmp_dir, "grafico_tmp.png")
      
      file.copy("informe.Rmd", tmp_rmd, overwrite = TRUE)
      
      # Guardar gráfico HTML → PNG
      g <- grafico_reactivo()
      htmlwidgets::saveWidget(as_widget(g), tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, tmp_png,
                        vwidth = 1200, vheight = 800)
      
      #### Construir mensaje estadístico real (sin tocar los %) ####
      mensaje_panel <- tryCatch({
        df <- data_filtrada() %>%
          arrange(mes_y_ano) %>%
          mutate(cambio_pct_mensual = (precio_prom - lag(precio_prom)) / 
                   lag(precio_prom) * 100) %>%
          filter(!is.na(cambio_pct_mensual))
        
        max_change <- df[which.max(df$cambio_pct_mensual), ]
        min_change <- df[which.min(df$cambio_pct_mensual), ]
        
        mes_max   <- tools::toTitleCase(format(max_change$mes_y_ano, "%B %Y"))
        mes_min   <- tools::toTitleCase(format(min_change$mes_y_ano, "%B %Y"))
        valor_max <- round(max_change$cambio_pct_mensual, 2)
        valor_min <- round(min_change$cambio_pct_mensual, 2)
        
        glue::glue(
          "Mayor incremento mensual: {mes_max} (+{valor_max}%). ",
          "Mayor reducción mensual: {mes_min} ({valor_min}%)."
        )
      }, error = function(e) "")
      
      # Renderizar PDF
      rmarkdown::render(
        input         = tmp_rmd,
        output_format = rmarkdown::pdf_document(latex_engine = "xelatex"),
        output_file   = file,
        params = list(
          producto    = input$producto,
          anio        = input$anio,
          grafico_png = tmp_png,
          mensaje1    = mensaje_panel
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
}
