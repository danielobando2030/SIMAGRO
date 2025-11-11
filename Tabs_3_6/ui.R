################################################################################
# Proyecto FAO - VP - 2025
# UI - Bandas de precios normalizados con botones FAO y nota de probabilidad
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

# --- Interfaz UI ---
ui <- fluidPage(
  tags$head(
    tags$title("Bandas de precios normalizados - FAO VP 2025"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://fonts.googleapis.com/css2?family=Prompt:wght@400;600&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'Prompt', sans-serif;
        background-color: #fafafa;
      }
      h1 {
        color: #6A1B9A;
        font-weight: bold;
        font-size: 34px;
        text-align: center;
      }
      h2 {
        color: #8E24AA;
        font-size: 20px;
        text-align: center;
        margin-top: -5px;
        margin-bottom: 20px;
      }
      .btn-faoc {
        background-color: #6A1B9A;
        border-color: #6A1B9A;
        color: white;
        font-weight: 500;
        border-radius: 6px;
        margin-right: 5px;
      }
      .btn-faoc:hover {
        background-color: #500985;
        border-color: #500985;
        color: white;
      }
      .well-panel-fao {
        background-color: #6A1B9A;
        color: white;
        font-weight: 500;
        font-size: 14px;
        border-radius: 8px;
        padding: 15px;
      }
      .panel-atipicos ul {
        margin-left: -15px;
        padding-left: 20px;
      }
      .panel-atipicos li {
        margin-bottom: 4px;
      }
    "))
  ),
  
  # --- Logo superior ---
  div(class = "logo-header", img(src = "logo_3.png", height = "90px")),
  
  # --- Título principal ---
  h1("Bandas de precios normalizados por producto"),
  h2("Detección de días con precios atípicos en los mercados mayoristas"),
  
  # --- Filtros ---
  fluidRow(
    column(6,
           selectInput("producto", "Seleccione producto:",
                       choices = productos, selected = "Aguacate")),
    column(6,
           selectInput("anio", "Seleccione año:",
                       choices = c("Todos" = "todos", sort(unique(anios))),
                       selected = "2024"))
  ),
  
  br(),
  
  # --- Gráfico principal y botones FAO ---
  fluidRow(
    column(
      9,
      plotlyOutput("grafico", height = "600px"),
      br(),
      downloadButton("descargarGrafico", "Gráfica", class = "btn-faoc"),
      downloadButton("descargarDatos", "Datos", class = "btn-faoc"),
      shiny::a("GitHub",
               href = "https://github.com/Simonaa-Antioquia/Tableros/tree/main/Tabs_3_6",
               target = "_blank", class = "btn btn-faoc", icon("github")),
      actionButton("reset", "Restablecer", icon = icon("refresh"), class = "btn-faoc"),
      downloadButton("descargarPDF", "Generar informe", class = "btn-faoc")
    ),
    
    # --- Panel lateral de días atípicos ---
    column(
      3,
      wellPanel(uiOutput("diasAtipicos"), class = "well-panel-fao")
    )
  ),
  
  br(), br(),
  
  # --- Nota metodológica ampliada ---
  fluidRow(
    column(
      12, align = "left",
      HTML("
        <div style='font-size:12px; color:#5A5A5A; text-align:left; line-height:1.5;'>
        Fuente: cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA).<br>
        Las bandas representan ±2 desviaciones estándar de una media móvil de 20 días sobre los precios mayoristas normalizados.<br>
        Bajo una distribución normal, aproximadamente el <b>95% de las observaciones</b> se espera que se encuentren dentro de estas dos desviaciones estándar.<br>
        Los puntos rojos indican observaciones atípicas en el comportamiento de precios para el producto y año seleccionados.<br>
        </div>
      ")
    )
  ),
  
  br(),
  
  # --- Logo inferior ---
  div(class = "logo-footer", img(src = "logo_2.png", height = "80px"))
)
