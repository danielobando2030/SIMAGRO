################################################################################
# Proyecto FAO - VP - 2025
# SERVER - Variación porcentual mensual del precio promedio
################################################################################

pacman::p_load(
  shiny, plotly, dplyr, zoo, lubridate,
  rmarkdown, webshot2, htmlwidgets, glue
)
options(scipen = 999)

# Cargar función interactiva
source("3_7b_variaciones_precio_presente_precio_pasado.R")

################################################################################
# SERVER
################################################################################
server <- function(input, output, session) {
  
  ##############################################################################
  # DATA FILTRADA (NO TOCAR)
  ##############################################################################
  data_filtrada <- reactive({
    req(input$producto, input$anio)
    
    data %>%
      filter(producto == input$producto) %>%
      mutate(mes_y_ano = lubridate::ym(gsub("[^0-9]", "", mes_y_ano))) %>%
      filter(year(mes_y_ano) == as.numeric(input$anio)) %>%
      arrange(mes_y_ano)
  })
  
  ##############################################################################
  # GRÁFICO INTERACTIVO (NO TOCAR)
  ##############################################################################
  grafico_reactivo <- reactive({
    df <- data_filtrada()
    
    if (nrow(df) < 2) {
      showNotification("⚠️ No hay datos suficientes.", type = "warning")
      return(NULL)
    }
    
    df <- df %>%
      mutate(cambio_pct_mensual = (precio_prom - lag(precio_prom)) / lag(precio_prom) * 100) %>%
      filter(!is.na(cambio_pct_mensual))
    
    ticks_y <- pretty(df$cambio_pct_mensual)
    
    plot_ly(
      df,
      x = ~mes_y_ano,
      y = ~cambio_pct_mensual,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#DBC21F", width = 2),
      marker = list(color = "#DBC21F", size = 8),
      text = ~paste0(
        "Mes: ", tools::toTitleCase(format(mes_y_ano, "%B %Y")),
        "<br>Cambio mensual: ",
        format(round(cambio_pct_mensual, 2),
               decimal.mark = ",", big.mark = "."),
        "%"
      ),
      hoverinfo = "text",
      showlegend = FALSE
    ) %>%
      add_lines(
        x = ~mes_y_ano,
        y = 0,
        line = list(color = "rgba(120,120,120,0.6)", dash = "dot"),
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
          tickvals = ticks_y,
          ticktext = format(
            round(ticks_y, 2),
            decimal.mark = ",",
            big.mark = "."
          ),
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
  # PANEL DERECHO — MAYOR CAMBIO (NO TOCAR)
  ##############################################################################
  output$mayorCambio <- renderUI({
    df <- data_filtrada()
    if (nrow(df) < 2) return(NULL)
    
    df <- df %>%
      arrange(mes_y_ano) %>%
      mutate(cambio_pct_mensual = (precio_prom - lag(precio_prom)) / lag(precio_prom) * 100) %>%
      filter(!is.na(cambio_pct_mensual))
    
    max_c <- df[which.max(df$cambio_pct_mensual), ]
    min_c <- df[which.min(df$cambio_pct_mensual), ]
    
    mes_max <- tools::toTitleCase(format(max_c$mes_y_ano, "%B %Y"))
    mes_min <- tools::toTitleCase(format(min_c$mes_y_ano, "%B %Y"))
    
    valor_max <- format(round(max_c$cambio_pct_mensual, 2),
                        decimal.mark = ",", big.mark = ".")
    valor_min <- format(round(min_c$cambio_pct_mensual, 2),
                        decimal.mark = ",", big.mark = ".")
    
    HTML(glue("
      <div class='panel-cambio'>
        <p><b>Mayor incremento mensual:</b><br>{mes_max} (+{valor_max}%)</p>
        <p><b>Mayor reducción mensual:</b><br>{mes_min} ({valor_min}%)</p>
      </div>
    "))
  })
  
  ##############################################################################
  # INFORME PDF (ÚNICO CAMBIO REAL)
  ##############################################################################
  output$report <- downloadHandler(
    filename = function() {
      paste0("informe_variacion_", input$producto, "_", input$anio, ".pdf")
    },
    content = function(file) {
      
      tmp_dir <- tempdir()
      tmp_rmd <- file.path(tmp_dir, "informe.Rmd")
      
      file.copy("informe.Rmd", tmp_rmd, overwrite = TRUE)
      
      # ========= GRÁFICO ESTÁTICO (SOLO PARA EL INFORME) =========
      df <- data_filtrada() %>%
        arrange(mes_y_ano) %>%
        mutate(cambio_pct_mensual = (precio_prom - lag(precio_prom)) / lag(precio_prom) * 100) %>%
        filter(!is.na(cambio_pct_mensual))
      
      grafico_plano <- ggplot(df, aes(x = mes_y_ano, y = cambio_pct_mensual)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
        geom_line(color = "#DBC21F", linewidth = 0.8) +
        geom_point(color = "#DBC21F", size = 2) +
        scale_y_continuous(
          name = "Cambio % mensual",
          labels = function(x)
            format(round(x, 2), decimal.mark = ",", big.mark = ".")
        ) +
        scale_x_date(
          name = "Mes",
          date_labels = "%b",
          date_breaks = "1 month"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank()
        )
      
      # ========= MENSAJE (MISMO QUE PANEL DERECHO) =========
      max_c <- df[which.max(df$cambio_pct_mensual), ]
      min_c <- df[which.min(df$cambio_pct_mensual), ]
      
      mensaje_panel <- glue(
        "Mayor incremento mensual: {tools::toTitleCase(format(max_c$mes_y_ano, '%B %Y'))} ",
        "(+{round(max_c$cambio_pct_mensual,2)}%). ",
        "Mayor reducción mensual: {tools::toTitleCase(format(min_c$mes_y_ano, '%B %Y'))} ",
        "({round(min_c$cambio_pct_mensual,2)}%)."
      )
      
      rmarkdown::render(
        input       = tmp_rmd,
        output_file = file,
        params = list(
          producto = input$producto,
          anio     = input$anio,
          plot     = grafico_plano,
          mensaje1 = mensaje_panel
        ),
        envir = new.env(parent = globalenv()),
        knit_root_dir = tmp_dir
      )
    }
  )
  
  ##############################################################################
  # RESET
  ##############################################################################
  observeEvent(input$reset, {
    updateSelectInput(session, "producto", selected = "Aguacate")
    updateSelectInput(session, "anio", selected = "2024")
  })
}
