################################################################################
# Proyecto FAO - VP - 2025
# UI - Matriz de correlación de precios entre productos (estructura tipo abastecimiento)
################################################################################

pacman::p_load(shiny, plotly, dplyr, lubridate)
options(scipen = 999)

data <- readRDS("precios_bogota_balanceado_3_8.rds")
data$anio <- lubridate::year(data$mes_y_ano)

ui <- fluidPage(
  
  ##############################################################################
  # HEAD — estilos institucionales
  ##############################################################################
  tags$head(
    tags$title("Correlación de precios - FAO VP 2025"),
    tags$link(
      rel = "stylesheet", type = "text/css",
      href = "https://fonts.googleapis.com/css2?family=Prompt:wght@400;600&display=swap"
    ),
    tags$style(HTML("
      body { 
        font-family: 'Prompt', sans-serif; 
        background-color: #fafafa; 
        color: #4E4D4D; 
      }

      .main-header {
        font-size: 40px;
        font-weight: 700;
        color: #6D673E !important;
        text-align: left;
      }

      .main-subheader {
        font-size: 20px;
        color: #6D673E !important;
        text-align: left;
        margin-top: -10px;
        margin-bottom: 15px;
      }

      .sub-header2 {
        font-size: 15px;
        color: #4E4D4D;
        text-align: left;
        margin-bottom: 20px;
      }

      .btn-faoc {
        background-color: #FFFFFF !important;
        border: 1.5px solid #A0A0A0 !important;
        color: #4E4D4D !important;
        font-weight: 500 !important;
        border-radius: 6px !important;
        padding: 6px 14px !important;
        margin-right: 6px !important;
        height: 36px !important;
        display: inline-flex !important;
        align-items: center !important;
      }
      .btn-faoc:hover { background-color: #EAEAEA !important; }

      .panel-dorado {
        background-color: #DBC21F !important;
        color: white !important;
        border-radius: 10px;
        padding: 15px;
        font-size: 14px;
        font-weight: 500;
        margin-bottom: 12px;
      }

      .panel-olivo {
        background-color: #494634 !important;
        color: white !important;
        border-radius: 10px;
        padding: 15px;
        font-size: 14px;
        font-weight: 500;
      }
    "))
  ),
  
  ##############################################################################
  # TÍTULOS — igual que abastecimiento
  ##############################################################################
  tags$h1("Matriz de correlación de precios entre productos", class = "main-header"),
  tags$h1("Identificación de relaciones entre precios mayoristas por producto", class = "main-subheader"),
  
  div(textOutput("subtitulo"), class = "sub-header2"),
  
  ##############################################################################
  # SELECTORES — misma disposición que abastecimiento
  ##############################################################################
  div(
    fluidRow(
      column(3,
             selectInput("anio", "Seleccione año:",
                         choices = sort(unique(data$anio)),
                         selected = max(data$anio)))
    )
  ),
  
  ##############################################################################
  # GRÁFICO + PANELS — distribución 9/3 exacta
  ##############################################################################
  fluidRow(
    column(
      9,
      div(
        plotlyOutput("grafico", height = "600px"),
        br(),
        
        actionButton("descargar", "Gráfica", icon = icon("download"), class = "btn-faoc"),
        downloadButton("descargarDatos", "Datos", class = "btn-faoc"),
        shiny::a(
          tagList(icon("github"), " GitHub"),
          href = "https://github.com/Simonaa-Antioquia/Tableros/tree/main/Tabs_3_8",
          target = "_blank",
          class = "btn-faoc"
        ),
        actionButton("reset", "Restablecer", icon = icon("refresh"), class = "btn-faoc"),
        downloadButton("descargarPDF", "Generar informe PDF", class = "btn-faoc")
      )
    ),
    
    column(
      3,
      div(class = "panel-dorado", uiOutput("topPositivas")),
      div(class = "panel-olivo",  uiOutput("topNegativas"))
    )
  ),
  
  ##############################################################################
  # NOTA METODOLÓGICA
  ##############################################################################
  fluidRow(
    column(
      12,
      HTML("
        <div style='font-size:12px; color:#5A5A5A; text-align:left; line-height:1.4;'>
        <b>Fuente:</b> cálculos propios a partir de datos del Sistema de Información de Precios 
        y Abastecimiento del Sector Agropecuario (SIPSA) – DANE.
        </div>
      ")
    )
  ),
  
  ##############################################################################
  # LOGO INFERIOR
  ##############################################################################
  br(),
  fluidRow(
    div(
      img(src = 'logo_2.png', style = "width: 100%; margin: 0;"),
      style = "width: 100%; margin: 0;"
    )
  )
)
