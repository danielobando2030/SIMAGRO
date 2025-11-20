################################################################################
# Proyecto FAO - VP - 2025
# UI - Bandas de precios normalizados (estructura institucional FAO)
# SIN LOGO SUPERIOR
################################################################################

library(shiny)
library(plotly)
library(dplyr)
library(zoo)
library(lubridate)

# --- Cargar datos ---
data <- readRDS("base_diaria_mayoristas_indices_bog_3_6.rds")
productos <- sort(unique(data$producto))
anios <- sort(unique(year(data$fecha)))

# ============================
#      INTERFAZ COMPLETA
# ============================
ui <- fluidPage(
  
  ##############################################################################
  # ENCABEZADO Y ESTILOS
  ##############################################################################
  tags$head(
    tags$title("Bandas de precios normalizados - FAO VP 2025"),
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

      /* TITULOS */
      .main-header {
        font-size: 40px;
        color: #6D673E !important;
        font-weight: 700;
        text-align: left;
      }
      .main-header_2 {
        font-size: 20px;
        color: #6D673E !important;
        font-weight: 500;
        text-align: left;
        margin-top: -10px;
      }
      .sub-header2 {
        font-size: 15px;
        color: #4E4D4D;
        font-weight: 400;
      }

      /* BOTONES FAO */
      .btn-faoc {
        background-color: #FFFFFF !important;
        border: 1.5px solid #A0A0A0 !important;
        color: #4E4D4D !important;
        font-weight: 500 !important;
        border-radius: 6px !important;
        padding: 6px 14px !important;
        margin-right: 4px !important;
        height: 36px !important;
        display: inline-flex !important;
        align-items: center !important;
      }
      .btn-faoc:hover {
        background-color: #EAEAEA !important;
      }

      /* PANEL LATERAL */
      .well-panel-fao {
        background-color: #DBC21F !important;
        color: #FFFFFF !important;
        font-weight: 500;
        border-radius: 8px;
        padding: 12px;
      }
    "))
  ),
  
  ##############################################################################
  # TÍTULOS (MISMA DISPOSICIÓN QUE ABASTECIMIENTO)
  ##############################################################################
  tags$h1("Bandas de precios normalizados por producto", class = "main-header"),
  tags$h1("Detección de días con precios atípicos en los mercados mayoristas", class = "main-header_2"),
  
  div(
    textOutput("subtitulo"),
    class = "sub-header2",
    style = "margin-bottom: 20px;"
  ),
  
  ##############################################################################
  # SELECTORES — MISMA FILA Y ORDEN QUE ABASTECIMIENTO
  ##############################################################################
  div(
    fluidRow(
      column(3,
             selectInput("producto", "Seleccione producto:", productos, selected = "Aguacate")),
      column(3,
             selectInput("anio", "Seleccione año:",
                         choices = c("Todos" = "todos", anios),
                         selected = "2024"))
    )
  ),
  
  ##############################################################################
  # GRÁFICO + PANEL LATERAL (9 - 3 IGUAL AL MÓDULO DE ABASTECIMIENTO)
  ##############################################################################
  fluidRow(
    column(
      9,
      div(
        plotlyOutput("grafico", height = "600px"),
        br(),
        downloadButton("descargarGrafico", "Gráfica", class = "btn-faoc"),
        downloadButton("descargarDatos", "Datos", class = "btn-faoc"),
        shiny::a(
          tagList(icon("github"), " GitHub"),
          href = "https://github.com/Simonaa-Antioquia/Tableros/tree/main/Tabs_3_6",
          target = "_blank",
          class = "btn-faoc"
        ),
        actionButton("reset", "Restablecer", icon = icon("refresh"), class = "btn-faoc"),
        downloadButton("descargarPDF", "Generar informe", class = "btn-faoc")
      )
    ),
    
    column(
      3,
      div(
        wellPanel(uiOutput("diasAtipicos"), class = "well-panel-fao")
      )
    )
  ),
  
  ##############################################################################
  # NOTA METODOLÓGICA — BLOQUE ÚNICO
  ##############################################################################
  fluidRow(
    column(
      12, align = "left",
      HTML("
        <b>Fuente:</b> cálculos propios a partir del SIPSA.<br><br>
        Las bandas representan ±2 desviaciones estándar de la media móvil de 20 días. 
        El 95% de los datos debería ubicarse dentro de estas bandas bajo normalidad. <br>
        Los puntos rojos indican días atípicos.
      "),
      style = "font-size:12px; color:#4E4D4D; font-family: 'Prompt'; margin-top:15px;"
    )
  ),
  
  ##############################################################################
  # LOGO INFERIOR — IGUAL A ABASTECIMIENTO
  ##############################################################################
  fluidRow(
    tags$div(
      tags$img(src = "logo_2.png", style = "width: 100%; margin: 0;"),
      style = "width: 100%; margin: 0;"
    )
  )
)
