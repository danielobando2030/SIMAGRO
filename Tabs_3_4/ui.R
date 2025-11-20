################################################################################-
# Proyecto FAO - VP - 2025
# Comparativo de Precios (Producto y Año)
# Versión institucional con estructura FAO — Estilo Abastecimiento
################################################################################-

library(shiny)
library(plotly)
library(dplyr)

source("3_4b_precios_diferencias_municipios_funciones.R")

productos <- sort(unique(data_comparacion_producto$producto))
anios     <- sort(unique(as.character(data_comparacion_anual_producto$year)))

################################################################################-
# UI
################################################################################-

ui <- fluidPage(
  
  tags$head(
    tags$title("Comparativo de Precios — FAO VP 2025"),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "https://fonts.googleapis.com/css2?family=Prompt&display=swap"
    ),
    
    tags$style(HTML("
      body {
        font-family: 'Prompt', sans-serif;
        background-color: #fafafa;
        color: #4E4D4D;
      }

      /* Botones FAO */
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

      .btn-faoc:hover {
        background-color: #EAEAEA !important;
        border-color: #808080 !important;
      }

      /* ESTILO PARA ENLACE TIPO BOTÓN */
      .btn-faoc-link {
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
        text-decoration: none !important;
      }

      .btn-faoc-link:hover {
        background-color: #EAEAEA !important;
        border-color: #808080 !important;
        text-decoration: none !important;
        color: #4E4D4D !important;
      }

      .main-header {
        font-size: 40px;
        color: #6D673E;
        font-weight: bold;
      }

      .main-header_2 {
        font-size: 20px;
        color: #6D673E;
        margin-bottom: 10px;
      }

      .sub-header2 {
        font-size: 15px;
        color: #4E4D4D;
      }

      .nota-metodo {
        font-size: 12px;
        color: #4E4D4D;
        text-align: left;
        line-height: 1.5;
      }

      .well-panel-fao {
        background-color: #6D673E;
        color: white;
        font-weight: bold;
        border-radius: 8px;
        font-size: 14px;
      }
    "))
  ),
  
  ##############################################################################
  # TÍTULOS PRINCIPALES — MISMO ESTILO ABASTECIMIENTO
  ##############################################################################
  
  tags$h1("Comparativo de Precios entre Ciudades", class = "main-header"),
  tags$h1("Diferencias promedio respecto a Bogotá", class = "main-header_2"),
  
  div(
    textOutput("subtitulo"),
    class = "sub-header2",
    style = "margin-bottom: 20px;"
  ),
  
  ##############################################################################
  # SELECTORES — DISTRIBUCIÓN TIPO 3–3
  ##############################################################################
  
  fluidRow(
    column(3,
           selectInput("producto", "Seleccione producto:", productos, selected = "Aguacate")
    ),
    column(3,
           selectInput("anio", "Seleccione año:", anios, selected = "2014")
    )
  ),
  
  br(),
  
  ##############################################################################
  # GRÁFICO (9) + PANEL LATERAL (3)
  ##############################################################################
  
  fluidRow(
    column(
      9,
      div(
        plotlyOutput("grafico", height = "420px"),
        br(),
        
        downloadButton("descargar", "Gráfica", class = "btn-faoc"),
        downloadButton("descargarDatos", "Datos", class = "btn-faoc"),
        
        shiny::a(
          tagList(icon("github"), " GitHub"),
          href = "https://github.com/Simonaa-Antioquia/Tableros/tree/main/Tabs_3_4",
          target = "_blank",
          class = "btn-faoc-link"
        ),
        
        actionButton("reset", "Restablecer", icon = icon("refresh"), class = "btn-faoc"),
        downloadButton("report", "Generar informe", class = "btn-faoc")
      )
    ),
    
    column(
      3,
      div(
        wellPanel(
          textOutput("mensaje1"),
          class = "well-panel-fao"
        )
      )
    )
  ),
  
  br(),
  
  ##############################################################################
  # NOTA METODOLÓGICA
  ##############################################################################
  
  fluidRow(
    column(
      12,
      HTML("
        <b>Fuente:</b> Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA). <br>
        Este gráfico muestra la diferencia promedio de precios entre ciudades comparadas con Bogotá.<br>
        El tamaño del círculo representa la variabilidad interna del precio (desviación estándar).<br>
        Valores positivos indican precios mayores que Bogotá; valores negativos, menores.
      "),
      class = "nota-metodo"
    )
  ),
  
  br(), br(),
  
  ##############################################################################
  # LOGO INSTITUCIONAL — MISMO ESTILO
  ##############################################################################
  
  fluidRow(
    tags$div(
      tags$img(src = "logo_4.png", style = "width: 100%; margin: 0;"),
      style = "width: 100%; margin: 0;"
    )
  )
)
