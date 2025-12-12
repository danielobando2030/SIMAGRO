################################################################################
# Proyecto FAO - VP - 2025
# SERVER - Variación porcentual mensual del precio promedio
################################################################################

pacman::p_load(
  shiny, plotly, dplyr, zoo, lubridate,
  rmarkdown, webshot2, htmlwidgets
)
options(scipen = 999)

# Cargar función del gráfico
source("3_7b_variaciones_precio_presente_precio_pasado.R")

################################################################################
# SERVIDOR
################################################################################
server <- function(input, output, session) {
  
  ##############################################################################
  # --- DATA FILTRADA ---
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
  # --- GRÁFICO REACTIVO ---
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
    
    # Ticks formateados en español
    ticks_y <- pretty(df$cambio_pct_mensual)
    ticks_y_lbl <- format(
      round(ticks_y, 2),
      decimal.mark = ",",
      big.mark = "."
    )
    
    plot_ly(
      df,
      x = ~mes_y_ano,
      y = ~cambio_pct_mensual,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = '#DBC21F', width = 2),
      marker = list(color = '#DBC21F', size = 8),
      text = ~paste0(
        "Mes: ", format(mes_y_ano, "%B %Y"),
        "<br>Cambio mensual: ", format(round(cambio_pct_mensual, 2),
                                       decimal.mark = ",", big.mark = "."), "%"
      ),
      hoverinfo = "text"
    ) %>%
      add_lines(
        y = 0,
        x = ~mes_y_ano,
        line = list(color = "rgba(100, 100, 100, 0.7)", dash = "dot", width = 1.5),
        hoverinfo = "none",
        showlegend = FALSE
      ) %>%
      layout(
        title = NULL,
        xaxis = list(
          title = "Mes",
          tickvals = df$mes_y_ano,
          ticktext = tools::toTitleCase(format(df$mes_y_ano, "%b")),
          tickangle = -45
        ),
        yaxis = list(
          title = "Cambio % mensual",
          
          tickmode = "array",
          tickvals = ticks_y,
          ticktext = formato_es(ticks_y, 2),
          
          # --- LÍNEAS CRÍTICAS PARA QUE SE VEAN TODOS ---
          automargin = TRUE,
          tickson = "boundaries",
          ticks = "outside",
          tickfont = list(size = 14),
          constrain = "range",
          
          # Rango ampliado para más aire visual
          range = c(min(ticks_y) - 5, max(ticks_y) + 5),
          
          zeroline = FALSE
        ),
        hovermode = "x unified"
      )
  })
  
  output$grafico <- renderPlotly({
    grafico_reactivo()
  })
  
  ##############################################################################
  # PANEL LATERAL: MAYOR CAMBIO  (CON FORMATO LATAM)
  ##############################################################################
  output$mayorCambio <- renderUI({
    df <- data_filtrada()
    if (nrow(df) == 0) return(NULL)
    
    df <- df %>%
      arrange(mes_y_ano) %>%
      mutate(cambio_pct_mensual = (precio_prom - lag(precio_prom)) / lag(precio_prom) * 100) %>%
      filter(!is.na(cambio_pct_mensual))
    
    max_c <- df[which.max(df$cambio_pct_mensual), ]
    min_c <- df[which.min(df$cambio_pct_mensual), ]
    
    mes_max <- tools::toTitleCase(format(max_c$mes_y_ano, "%B %Y"))
    mes_min <- tools::toTitleCase(format(min_c$mes_y_ano, "%B %Y"))
    
    # ⭐ FORMATO LATINO — coma decimales, punto miles
    valor_max <- format(
      round(max_c$cambio_pct_mensual, 2),
      decimal.mark = ",",
      big.mark = "."
    )
    
    valor_min <- format(
      round(min_c$cambio_pct_mensual, 2),
      decimal.mark = ",",
      big.mark = "."
    )
    
    HTML(glue::glue("
    <div class='panel-cambio'>
      <p><b>Mayor incremento mensual:</b><br>{mes_max} (+{valor_max}%)</p>
      <p><b>Mayor reducción mensual:</b><br>{mes_min} ({valor_min}%)</p>
    </div>
  "))
  })
  
  ##############################################################################
  # RESET
  ##############################################################################
  observeEvent(input$reset, {
    updateSelectInput(session, "producto", selected = "Aguacate")
    updateSelectInput(session, "anio", selected = "2024")
  })
  
  ##############################################################################
  # DESCARGA DE GRÁFICA PNG
  ##############################################################################
  output$descargarGrafico <- downloadHandler(
    
    filename = function() {
      paste0("variacion_mensual_", gsub(" ", "_", input$producto), "_", input$anio, ".png")
    },
    
    content = function(file) {
      
      g <- grafico_reactivo()
      
      tmp_html <- tempfile(fileext = ".html")
      tmp_png  <- tempfile(fileext = ".png")
      
      # Guardar widget temporal
      htmlwidgets::saveWidget(as_widget(g), tmp_html, selfcontained = TRUE)
      
      # Convertir a PNG
      webshot2::webshot(
        tmp_html, 
        file = tmp_png,
        vwidth = 1600, 
        vheight = 900, 
        delay = 1
      )
      
      # Copiar archivo final
      file.copy(tmp_png, file, overwrite = TRUE)
    }
  )
  
  ##############################################################################
  # DESCARGAR DATOS CSV
  ##############################################################################
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste0("variacion_precios_", input$producto, "_", input$anio, ".csv")
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
  
  
  ##############################################################################
  # INFORME PDF (LaTeX)
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
      
      # Copiar informe
      file.copy("informe.Rmd", tmp_rmd, overwrite = TRUE)
      
      # Guardar gráfico como PNG
      g <- grafico_reactivo()
      htmlwidgets::saveWidget(as_widget(g), tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, tmp_png, vwidth = 1200, vheight = 800)
      
      # Copiar logos (www)
      if (dir.exists("www")) {
        file.copy("www", tmp_dir, recursive = TRUE)
      }
      
      # COPIAR FUENTE PROMPT (CORRECCIÓN CRÍTICA)
      if (file.exists("Prompt-Regular.ttf")) {
        file.copy("Prompt-Regular.ttf", tmp_dir, overwrite = TRUE)
      } else if (file.exists(file.path("Prompt", "Prompt-Regular.ttf"))) {
        file.copy(file.path("Prompt", "Prompt-Regular.ttf"), tmp_dir, overwrite = TRUE)
      } else if (file.exists("www/Prompt-Regular.ttf")) {
        file.copy("www/Prompt-Regular.ttf", tmp_dir, overwrite = TRUE)
      }
      
      print("Fuentes en tmp_dir:")
      print(list.files(tmp_dir))
      
      # Mensaje del panel
      mensaje_panel <- tryCatch({
        df <- data_filtrada() %>%
          arrange(mes_y_ano) %>%
          mutate(cambio_pct_mensual = (precio_prom - lag(precio_prom)) / lag(precio_prom) * 100) %>%
          filter(!is.na(cambio_pct_mensual))
        
        max_c <- df[which.max(df$cambio_pct_mensual), ]
        min_c <- df[which.min(df$cambio_pct_mensual), ]
        
        mes_max <- tools::toTitleCase(format(max_c$mes_y_ano, "%B %Y"))
        mes_min <- tools::toTitleCase(format(min_c$mes_y_ano, "%B %Y"))
        valor_max <- round(max_c$cambio_pct_mensual, 2)
        valor_min <- round(min_c$cambio_pct_mensual, 2)
        
        glue::glue(
          "Mayor incremento mensual: {mes_max} (+{valor_max}%). ",
          "Mayor reducción mensual: {mes_min} ({valor_min}%)."
        )
      }, error = function(e) "")
      
      # RENDERIZAR PDF
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
        envir = new.env(parent = globalenv()),
        knit_root_dir = tmp_dir
      )
    }
  )
}
