################################################################################-
# Proyecto FAO - VP - 2025
# UI - Elasticidad, precios y abastecimiento de alimentos
################################################################################-

library(shiny)
library(plotly)
library(dplyr)
library(lubridate)

source("3_3b_funcion_elasticidad.R", encoding = "UTF-8")

productos <- sort(unique(data$producto))
anios <- sort(unique(year(data$mes_y_ano)))

ui <- fluidPage(
  
  tags$head(
    tags$title("Elasticidad, precios y abastecimiento de alimentos - FAO VP 2025"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Prompt&display=swap"
    ),
    
    tags$style(HTML("
      body {
        font-family: 'Prompt', sans-serif;
        background-color: #fafafa;
        padding: 0 20px;
      }

      /* ======== TÍTULOS ======== */
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
        color: #4E4D4D;
      }

      /* ======== BOTONES NEUTRALES ======== */
      .btn-faoc, .btn-faoc-link {
        background-color: #FFFFFF !important;
        border: 1.5px solid #A0A0A0 !important;
        color: #333 !important;
        font-weight: 500 !important;
        border-radius: 6px !important;
        padding: 6px 12px !important;
        margin-right: 6px !important;
      }
      .btn-faoc:hover, .btn-faoc-link:hover {
        background-color: #EAEAEA !important;
        border-color: #808080 !important;
      }

      /* ======== PANEL LATERAL (CUADRO DERECHO) ======== */
      .well-panel-fao {
        background-color: #DBC21F;   /* color solicitado */
        border: none;                /* ← QUITAR BORDE */
        color: #FFFFFF;              /* ← TEXTO BLANCO */
        font-weight: 500;
        font-size: 14px;
        border-radius: 10px;
        padding: 12px;
      }

      /* ======== NOTA METODOLÓGICA ======== */
      .nota-metodo {
        font-size: 12px;
        color: #4E4D4D;
        line-height: 1.55;
        font-family: 'Prompt', sans-serif;
        margin-top: 15px;
      }
    "))
  ),
  
  tags$h1("Elasticidad, precios y abastecimiento de alimentos", class = "main-header"),
  tags$h1("Evolución mensual del precio, cantidad y elasticidad", class = "main-header_2"),
  
  div(
    textOutput("subtitulo"),
    class = "sub-header2",
    style = "margin-bottom: 20px;"
  ),
  
  div(
    fluidRow(
      column(3,
             selectInput("producto", "Seleccione producto:", productos, selected = "Aguacate")
      ),
      column(2,
             selectInput("anio", "Año:", c("Todos" = "todos", anios))
      )
    )
  ),
  
  fluidRow(
    column(
      9,
      div(
        plotlyOutput("grafico", height = "400px"),
        actionButton("descargar", "Gráfica", icon = icon("download"), class = "btn-faoc"),
        downloadButton("descargarDatos", "Datos", class = "btn-faoc"),
        shiny::a(
          tagList(icon("github"), " GitHub"),
          href = "https://github.com/Simonaa-Antioquia/Tableros/tree/main/Tabs_3_3",
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
  
  fluidRow(
    column(
      12,
      HTML("<b>Fuente:</b> Cálculos propios a partir del SIPSA.<br>
           La elasticidad se calcula como la variación porcentual del precio dividida por la variación porcentual de la cantidad ofertada."),
      class = "nota-metodo"
    )
  ),
  
  fluidRow(
    tags$div(
      tags$img(src = "logo_4.png", style = "width: 100%; margin: 0;"),
      style = "width: 100%; margin: 0;"
    )
  )
)
