################################################################################
# Proyecto FAO - VP - 2025
# UI - Módulo 3_9: Huella de Carbono (estructura FAO como abastecimiento)
################################################################################

library(shiny)
library(plotly)
library(dplyr)
library(lubridate)

options(scipen = 999)
source("3_9b_huella_carbono.R")

ui <- fluidPage(
  
  ###########################################################################
  # HEAD
  ###########################################################################
  tags$head(
    tags$title("Huella de carbono - FAO VP 2025"),
    tags$link(
      rel = "stylesheet", type = "text/css",
      href = "https://fonts.googleapis.com/css2?family=Prompt:wght@400;600&display=swap"
    ),
    tags$style(HTML("
      body { 
        font-family: 'Prompt', sans-serif; 
        background-color: #fafafa;
        color:#4E4D4D;
      }

      /* Estilo para separar ícono de texto en cualquier botón */
      .btn i, .btn-faoc i {
        margin-right: 8px !important;
      }

      .main-header {
        font-size: 40px;
        color: #743639;
        font-weight: 700;
      }
      .main-header_2 {
        font-size: 20px;
        color: #743639;
        font-weight: 500;
      }

      .sub-header2 {
        font-size: 15px;
        color: #4E4D4D;
      }

      .btn-faoc {
        background-color: #f0f0f0 !important;
        border-color: #cccccc !important;
        color: #4E4D4D !important;
        font-weight: 500 !important;
        border-radius: 6px !important;
        margin-right: 6px !important;
        height: 36px !important;
        display: inline-flex !important;
        align-items: center !important;
        padding: 6px 14px !important;
      }
      .btn-faoc:hover {
        background-color: #e0e0e0 !important;
      }

      .panel-rojo-oscuro {
        background-color:#8A171C !important;
        color:white !important;
        font-weight:bold;
        border-radius:8px;
        padding:15px;
        margin-bottom:15px;
      }

      .panel-rojo-claro {
        background-color:#BC222A !important;
        color:white !important;
        font-weight:bold;
        border-radius:8px;
        padding:15px;
      }

      .footer-text { 
        font-size:12px; 
        color:#4E4D4D !important; 
        line-height:1.3; 
      }
    "))
  ),
  
  ###########################################################################
  # TITULOS
  ###########################################################################
  tags$h1("Huella de carbono por grupo y producto", class = "main-header"),
  tags$h1("Análisis de emisiones de CO₂ según grupos alimentarios y productos SIPSA.",
          class = "main-header_2"),
  
  div(
    textOutput("subtitulo"),
    class = "sub-header2",
    style = "margin-bottom: 20px;"
  ),
  
  ###########################################################################
  # FILTROS
  ###########################################################################
  div(
    fluidRow(
      column(3,
             selectInput("anio", "Año:",
                         choices = sort(unique(data$anio)),
                         selected = 2024)
      ),
      column(2,
             selectInput("mes", "Mes:",
                         choices = setNames(
                           sprintf("%02d", 1:12),
                           c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                             "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")),
                         selected = "12")
      ),
      column(7, div())
    )
  ),
  
  ###########################################################################
  # GRAFICO + PANEL DERECHO
  ###########################################################################
  fluidRow(
    # ---------------- GRAFICO + BOTONES ----------------
    column(
      9,
      div(
        plotlyOutput("grafico", height = "500px"),
        br(),
# Usamos la misma estructura de iconos del proyecto de referencia
        downloadButton("descargarGraf", "Gráfica", icon = icon("download"), class = "btn-faoc"),
        downloadButton("descargarDatos", "Datos", class = "btn-faoc"), # El icono se puede inyectar vía CSS o dejarlo limpio
        
        shiny::a("GitHub", 
                 href = "https://github.com/danielobando2030/SIMAGRO/tree/main/Tabs_3_9", 
                 target = "_blank",
                 class = "btn btn-default btn-faoc", 
                 icon("github"),
                 style = "text-decoration:none;"),
        
        actionButton("reset", "Restablecer", icon = icon("refresh"), class = "btn-faoc"),
        downloadButton("report", "Generar informe", class = "btn-faoc")
      )
    ),
    
    # ---------------- PANEL DERECHO ----------------
    column(
      3,
      div(class = "panel-rojo-oscuro",
          htmlOutput("mensaje1")
      ),
      div(class = "panel-rojo-claro",
          uiOutput("top5_emisores")
      )
    )
  ),
    

  
  ###########################################################################
  # NOTA METODOLOGICA
  ###########################################################################
  fluidRow(
    column(
      12,
      HTML("
      <b>Fuente:</b> Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA).<br>
      <br> Emisiones estimadas para distancias y factores según: <br>
      Panel Intergubernamental sobre el Cambio Climático (IPCC) <br>
      Departamento de Medio Ambiente, Alimentación y Asuntos Rurales (DEFRA) <br>
      Organización de las Naciones Unidas para la Alimentación y la Agricultura (FAO)
    "),
      class = "footer-text",
      style = "margin-top: 25px;"
    )
  ),
  
  ###########################################################################
  # LOGO FINAL
  ###########################################################################
  fluidRow(
    tags$div(
      tags$img(src = 'logo_4.png', style = "width: 100%; margin: 0;"),
      style = "width: 100%; margin: 0;"
    )
  )
)