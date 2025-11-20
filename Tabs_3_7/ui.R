################################################################################
# Proyecto FAO - VP - 2025
# UI - Variación porcentual mensual del precio promedio (estructura FAO)
################################################################################

pacman::p_load(shiny, plotly, dplyr, zoo, lubridate)
options(scipen = 999)

# --- Cargar datos ---
data <- readRDS("base_precios_mayorista_mes_filtrados_3_7.rds")
productos <- sort(unique(data$producto))
anios <- sort(unique(format(as.Date(paste0(data$mes_y_ano, "-01")), "%Y")))

ui <- fluidPage(
  
  tags$head(
    tags$title("Variación mensual del precio promedio - FAO VP 2025"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://fonts.googleapis.com/css2?family=Prompt:wght@400;600&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'Prompt', sans-serif;
        background-color: #fafafa;
        color: #4E4D4D;
      }
      .main-header { font-size: 40px; font-weight: 700; color: #6D673E !important; }
      .main-header_2 { font-size: 20px; font-weight: 500; color: #6D673E !important; margin-top: -10px; }
      .sub-header2 { font-size: 15px; color: #6D673E !important; margin-bottom: 25px; }
      .btn-faoc { background-color: #FFFFFF !important; border: 1.5px solid #A0A0A0 !important;
                  color: #4E4D4D !important; font-weight: 500 !important; border-radius: 6px !important;
                  padding: 6px 14px !important; margin-right: 6px !important; height: 36px !important;
                  display: inline-flex !important; align-items: center !important; }
      .btn-faoc:hover { background-color: #EAEAEA !important; }
      .well-panel-fao { background-color: #DBC21F !important; color: white !important;
                        font-weight: 500 !important; font-size: 14px !important;
                        border-radius: 10px !important; padding: 15px !important; }
    "))
  ),
  
  tags$h1("Variación porcentual mensual del precio promedio", class = "main-header"),
  tags$h1("Identificación de los meses con mayor variación intermensual", class = "main-header_2"),
  
  div(textOutput("subtitulo"), class = "sub-header2"),
  
  fluidRow(
    column(3, selectInput("producto", "Seleccione producto:", choices = productos, selected = "Aguacate")),
    column(3, selectInput("anio", "Seleccione año:", choices = anios, selected = "2014"))
  ),
  
  br(),
  
  fluidRow(
    column(
      9,
      div(
        plotlyOutput("grafico", height = "600px"),
        br(),
        
        actionButton("descargar", "Gráfica", icon = icon("download"), class = "btn-faoc"),
        
        shiny::a(
          tagList(icon("github"), " GitHub"),
          href = "https://github.com/Simonaa-Antioquia/Tableros/tree/main/Tabs_3_7",
          target = "_blank",
          class = "btn-faoc",
          style = "text-decoration:none;"
        ),
        
        actionButton("reset", "Restablecer", icon = icon("refresh"), class = "btn-faoc"),
        
        downloadButton("report", "Generar informe", class = "btn-faoc")   # ← ÚNICO botón
      )
    ),
    
    column(
      3,
      wellPanel(uiOutput("mayorCambio"), class = "well-panel-fao")
    )
  ),
  
  br(), br(),
  
  fluidRow(
    column(
      12, align = "left",
      HTML("
        <div style='font-size:12px; color:#4E4D4D; line-height:1.5;'>
        <b>Fuente:</b> cálculos propios a partir de datos del Sistema de Información
        de Precios y Abastecimiento del Sector Agropecuario (SIPSA).<br><br>
        La variación porcentual mensual se obtiene al comparar el precio promedio del mes
        actual con el del mes anterior, permitiendo identificar aumentos o caídas fuertes
        en los mercados mayoristas.
        </div>
      ")
    )
  ),
  
  br(),
  
  fluidRow(
    tags$div(
      tags$img(src = "logo_2.png", style = "width: 100%; margin: 0;"),
      style = "width: 100%; margin: 0;"
    )
  )
)
