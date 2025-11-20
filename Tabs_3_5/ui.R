################################################################################-
# Proyecto FAO - VP - 2025
# UI - Ranking mensual de precios mayoristas (Estilo Abastecimiento)
################################################################################-

library(shiny)
library(plotly)
library(dplyr)
library(zoo)

# Cargar datos
data <- readRDS("base_precios_mayorista_mes_filtrados_3_5.rds")
productos <- sort(unique(data$producto))
anios <- sort(unique(format(as.yearmon(data$mes_y_ano, "%Y-%m"), "%Y")))

ui <- fluidPage(
  
  tags$head(
    tags$title("Ranking mensual de precios - FAO VP 2025"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://fonts.googleapis.com/css2?family=Prompt&display=swap"),
    
    tags$style(HTML("
      body {
        font-family: 'Prompt', sans-serif;
        background-color: #fafafa;
        color: #4E4D4D;
      }

      /* Títulos FAO */
      .main-header {
        font-size: 40px;
        font-weight: bold;
        color: #6D673E;
        text-align: left;
        margin-left: 5px;
      }
      .main-header_2 {
        font-size: 20px;
        font-weight: normal;
        color: #6D673E;
        text-align: left;
        margin-left: 5px;
      }
      .sub-header2 {
        font-size: 15px;
        color: #4E4D4D;
        margin-left: 5px;
      }

      /* Botones FAO */
      .btn-faoc {
        background-color: #FFFFFF !important;
        border: 1.2px solid #A0A0A0 !important;
        color: #4E4D4D !important;
        font-weight: 500 !important;
        border-radius: 6px !important;
        padding: 6px 14px !important;
        margin-right: 6px !important;
      }
      .btn-faoc:hover {
        background-color: #EAEAEA !important;
        border-color: #7A7A7A !important;
      }

      .btn-faoc-link {
        background-color: #FFFFFF !important;
        border: 1.2px solid #A0A0A0 !important;
        color: #4E4D4D !important;
        font-weight: 500 !important;
        border-radius: 6px !important;
        padding: 6px 14px !important;
        margin-right: 6px !important;
        text-decoration: none !important;
        display: inline-flex !important;
        align-items: center !important;
      }
      .btn-faoc-link:hover {
        background-color: #EAEAEA !important;
        border-color: #7A7A7A !important;
        text-decoration: none !important;
        color: #4E4D4D !important;
      }

      /* PANEL DERECHO — COLOR NUEVO #DBC21F */
      .well-panel-fao {
        background-color: #DBC21F !important;
        color: #FFFFFF !important;
        font-weight: bold !important;
        border-radius: 8px !important;
        padding: 15px !important;
        font-size: 14px !important;
        margin-bottom: 15px !important; /* <-- separación entre cuadros */
      }
    "))
  ),
  
  ##############################################################################
  # TÍTULOS
  ##############################################################################
  tags$h1("Ranking mensual de precios mayoristas por ciudad", class = "main-header"),
  tags$h1("Posición relativa de Bogotá frente a otras ciudades", class = "main-header_2"),
  
  div(
    textOutput("subtitulo"),
    class = "sub-header2",
    style = "margin-bottom: 20px;"
  ),
  
  ##############################################################################
  # SELECTORES
  ##############################################################################
  fluidRow(
    column(3, selectInput("producto", "Producto:", productos, selected = "Aguacate")),
    column(3, selectInput("anio", "Año:", choices = c("Todos"="todos", anios), selected = "2024"))
  ),
  
  br(),
  
  ##############################################################################
  # GRAFICO (9) + PANEL DERECHO (3)
  ##############################################################################
  fluidRow(
    column(
      9,
      div(
        plotlyOutput("grafico", height = "480px"),
        br(),
        downloadButton("descargarGrafico", "Gráfica", class = "btn-faoc"),
        downloadButton("descargarDatos", "Datos", class = "btn-faoc"),
        
        shiny::a(
          tagList(icon("github"), " GitHub"),
          href = "https://github.com/Simonaa-Antioquia/Tableros/tree/main/Tabs_3_5",
          target = "_blank",
          class = "btn-faoc-link"
        ),
        
        actionButton("reset", "Restablecer", icon = icon("refresh"), class = "btn-faoc"),
        downloadButton("descargarPDF", "Generar informe", class = "btn-faoc")
      )
    ),
    
    column(
      3,
      div(
        # Primer cuadro: mensaje 1
        wellPanel(
          textOutput("mensaje1"),
          class = "well-panel-fao"
        ),
        
        # Segundo cuadro: mensaje 2
        wellPanel(
          textOutput("mensaje2"),
          class = "well-panel-fao"
        ),
        
        style = "margin-left: 20px;"  # espacio contra el gráfico
      )
    )
  ),
  
  br(),
  
  ##############################################################################
  # FUENTE
  ##############################################################################
  fluidRow(
    column(
      12, align = "left",
      HTML("
        <b>Fuente:</b> Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA).<br><br>
        Este módulo presenta el escalafón mensual de precios mayoristas por ciudad.
      "),
      style = "font-size:12px; color:#4E4D4D;"
    )
  ),
  
  br(), br(),
  
  ##############################################################################
  # LOGO
  ##############################################################################
  fluidRow(
    tags$div(
      tags$img(src = "logo_4.png", style = "width: 100%; margin: 0;"),
      style = "width: 100%; margin: 0;"
    )
  )
)
