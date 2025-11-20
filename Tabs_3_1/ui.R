################################################################################
# Proyecto FAO - VP - 2025
# Interfaz UI - Módulo 3_1: Precios Mayoristas (Bogotá)
################################################################################

library(shiny)
library(plotly)
library(dplyr)
library(stringr)

ui <- fluidPage(
  
  # ------------------------------------------------------------------
  # Encabezado
  # ------------------------------------------------------------------
  tags$head(
    tags$title("Comportamiento de precios mayoristas - Bogotá"),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "https://fonts.googleapis.com/css2?family=Prompt:wght@400;600&display=swap"
    ),
    tags$style(HTML("
      body { font-family: 'Prompt', sans-serif; background-color: #fafafa; color:#4E4D4D; }

      .main-header {
        font-size: 40px;
        color: #6D673E;
        font-weight: 700;
      }

      .main-header_2 {
        font-size: 20px;
        color: #6D673E;
      }

      .sub-header2 {
        font-size: 15px;
        color:#4E4D4D;
      }

      h5, label, p, .stat-value, .stat-title,
      .shiny-input-container, .control-label {
        color:#4E4D4D !important;
      }

      .btn-faoc {
        background-color: white !important;
        border: 1px solid #CCC !important;
        color: #4E4D4D !important;
        font-weight: 500;
        border-radius: 6px;
      }

      .btn-faoc:hover {
        background-color: #F2F2F2 !important;
        border-color: #AAA !important;
      }
    "))
  ),
  
  # ------------------------------------------------------------------
  # TÍTULOS EXACTAMENTE COMO EL OTRO DASHBOARD
  # ------------------------------------------------------------------
  tags$h1("Comportamiento de los precios en el tiempo", class = "main-header"),
  tags$h1("Análisis histórico de precios de alimentos en las centrales de abasto de Bogotá.", class = "main-header_2"),
  
  div(
    textOutput("subtitulo"),
    class = "sub-header2",
    style = "margin-bottom: 20px;"
  ),
  
  # ------------------------------------------------------------------
  # Filtros
  # ------------------------------------------------------------------
  fluidRow(
    column(3,
           selectInput("temporalidad", "Seleccione temporalidad:",
                       choices = c("Mensual" = "mensual", "Diaria" = "diaria"),
                       selected = "mensual")
    ),
    column(3, uiOutput("productoUI")),
    column(3, uiOutput("anioUI")),
    column(3,
           selectInput("variable", "Variable a graficar:",
                       choices = c("Precio promedio" = "precio_prom",
                                   "Cambio porcentual mensual" = "cambio_pct",
                                   "Cambio porcentual anual" = "cambio_pct_anual"),
                       selected = "precio_prom")
    )
  ),
  
  br(),
  
  # ------------------------------------------------------------------
  # Subtítulo dinámico
  # ------------------------------------------------------------------
  fluidRow(
    column(12, align = "center",
           h5(textOutput("subtitulo2"),
              style = "margin-bottom:10px; font-weight:500; color:#4E4D4D;")
    )
  ),
  
  # ------------------------------------------------------------------
  # Gráfico + Panel lateral
  # ------------------------------------------------------------------
  fluidRow(
    column(9, align = "center",
           plotlyOutput("grafico", height = "450px")
    ),
    
    column(3,
           uiOutput("texto_volatil"),
           uiOutput("texto_promedio_cambio"),
           uiOutput("texto_mes_max_anual"),
           style = "padding:0px; margin-top:20px;"
    )
  ),
  
  br(),
  
  # ------------------------------------------------------------------
  # Botones
  # ------------------------------------------------------------------
  fluidRow(
    column(12, align = "center",
           downloadButton("descargar", "Gráfica", class = "btn btn-faoc"),
           downloadButton("descargarDatos", "Datos", class = "btn btn-faoc"),
           a("GitHub",
             href = "https://github.com/Simonaa-Antioquia/Tableros/tree/main/Tabs_3_1",
             target = "_blank", class = "btn btn-faoc", icon("github")),
           actionButton("reset", "Restablecer", icon = icon("refresh"), class = "btn-faoc"),
           downloadButton("descargarInforme", "Generar informe", class = "btn-faoc")
    )
  ),
  
  br(),
  
  # ------------------------------------------------------------------
  # Nota metodológica
  # ------------------------------------------------------------------
  fluidRow(
    column(12,
           align = "left",
           HTML("Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA)."),
           style = "font-size:12px; text-align:left;"
    )
  ),
  
  # ------------------------------------------------------------------
  # Logo
  # ------------------------------------------------------------------
  fluidRow(
    tags$div(
      tags$img(src = 'logo_2.png', style = "width: 100%; margin: 0;"),
      style = "width: 100%; margin: 0;"
    )
  )
)
