################################################################################-
# Proyecto FAO - VP - 2025
# UI – Mapa de Rutas por Regiones (misma lógica que Tabs_4_1)
################################################################################-

library(shiny)
library(leaflet)

ui <- fluidPage(
  tags$head(
    tags$title("Mapa de rutas por cierres – FAO 2025"),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "https://fonts.googleapis.com/css2?family=Prompt&display=swap"
    ),
    tags$style(HTML("
      body {
        font-family: 'Prompt', sans-serif;
        background-color: #fafafa;
      }
      .main-header {
        font-family: 'Prompt', sans-serif;
        font-size: 34px;
        color: #134174;
        text-align: center;
        font-weight: bold;
        margin-top: 10px;
        margin-bottom: 5px;
      }
      .main-header_2 {
        font-family: 'Prompt', sans-serif;
        font-size: 18px;
        color: #4E4D4D;
        text-align: center;
        margin-top: 0px;
        margin-bottom: 15px;
      }
      .stat-box {
        background-color: #134174;
        color: #FFFFFF;
        padding: 12px;
        border-radius: 8px;
        margin-bottom: 10px;
        font-size: 13px;
      }
      .stat-title {
        font-weight: bold;
        margin-bottom: 4px;
      }
      .stat-text {
        font-size: 13px;
      }
      .btn,
      .btn-primary,
      .btn-danger,
      .btn-default,
      .shiny-download-link,
      .shiny-action-button {
        background-color:#134174 !important;
        border-color:#134174 !important;
        color:white !important;
        font-weight:bold;
      }
      .btn:hover,
      .btn-primary:hover,
      .btn-danger:hover,
      .btn-default:hover,
      .shiny-download-link:hover,
      .shiny-action-button:hover {
        background-color:#0F355F !important;
        border-color:#0F355F !important;
        color:white !important;
      }
    "))
  ),
  
  # Logo superior
  tags$div(
    tags$img(src = "logo_3.png", style = "width: 100%; margin-bottom: 10px;")
  ),
  
  # Títulos
  tags$h1("Rutas de abastecimiento por regiones geográficas", class = "main-header"),
  tags$h2("Participación de las rutas de cierre según región y producto", class = "main-header_2"),
  
  # ------------------- Filtros (misma lógica que Tabs_4_1) -------------------
  fluidRow(
    column(
      3,
      selectInput(
        "anio",
        "Año:",
        choices = sort(unique(data_cierres_final$anio)),
        selected = 2024
      )
    ),
    column(
      3,
      selectInput(
        "mes",
        "Mes:",
        choices = sort(unique(data_cierres_final$mes)),  # "01", "02", ...
        selected = "12"
      )
    ),
    column(
      3,
      selectInput(
        "producto",
        "Producto:",
        choices = sort(unique(data_cierres_final$producto)),
        selected = "Aguacate Hass"
      )
    ),
    column(
      3,
      checkboxGroupInput(
        "rutas", "Regiones a mostrar:",
        choices = c(
          "Noroccidente" = "Noroccidente",
          "Nororiente"   = "Nororiente",
          "Norte"        = "Norte",
          "Oriente"      = "Oriente",
          "Suroriente"   = "Suroriente",
          "Sur"          = "Sur",
          "Suroccidente" = "Suroccidente",
          "Occidente"    = "Occidente"
        ),
        selected = c(
          "Noroccidente","Nororiente","Norte","Oriente",
          "Suroriente","Sur","Suroccidente","Occidente"
        )
      )
    )
  ),
  
  # *** OJO: aquí NO hay br() para no crear espacio extra ***
  
  # ------------------- Mapa + Panel lateral -------------------
  fluidRow(
    # Columna del mapa y botones
    column(
      9,
      div(
        leafletOutput("grafico", height = "450px"),
        style = "margin-top: 5px;"
      ),
      br(),
      # Botones en el orden: Gráfica, Datos, GitHub, Restablecer, Generar informe
      actionButton("descargar", "Gráfica", icon = icon("download")),
      downloadButton("descargarDatos", "Datos"),
      shiny::a("GitHub",
               href = "https://github.com/danielobando2030/SIMAGRO",
               target = "_blank",
               class = "btn btn-default shiny-action-button",
               icon("github")),
      actionButton("reset", "Restablecer filtros", icon = icon("refresh")),
      downloadButton("descargarPDF", "Generar informe")
    ),
    
    # Columna de textos
    column(
      3,
      div(
        class = "stat-box",
        div(class = "stat-title", "Ruta más importante"),
        div(class = "stat-text", textOutput("municipio_mas_importante"))
      ),
      div(
        class = "stat-box",
        div(class = "stat-title", "Ranking de rutas"),
        div(class = "stat-text", textOutput("ranking_rutas"))
      )
    )
  ),
  
  br(),
  
  # Fuente
  fluidRow(
    column(
      12,
      HTML("<b>Fuente:</b> Cálculos propios a partir de datos del SIPSA."),
      style = "font-size:12px; color:#4E4D4D; font-family:'Prompt';"
    )
  ),
  
  # Logo inferior
  fluidRow(
    tags$div(
      tags$img(src = "logo_2.png", style = "width: 100%; margin-top: 10px;"),
      style = "width: 100%; margin: 0;"
    )
  )
)
