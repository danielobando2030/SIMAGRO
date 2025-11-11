################################################################################-
# Proyecto FAO - VP - 2025
# Comparativo de Precios (Producto y Año)
# Versión institucional FAO con botones y panel lateral
################################################################################-
# Autores: Luis Miguel García, Laura Quintero, Juliana Lalinde
# Última modificación: 08/11/2025
################################################################################-

library(shiny)
library(plotly)
library(dplyr)

source("3_4b_precios_diferencias_municipios_funciones.R")

productos <- sort(unique(data_comparacion_producto$producto))
anios     <- sort(unique(as.character(data_comparacion_anual_producto$year)))

################################################################################-
# Interfaz de usuario
################################################################################-
ui <- fluidPage(
  tags$head(
    tags$title("Comparativo de Precios - FAO VP 2025"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://fonts.googleapis.com/css2?family=Prompt:wght@400;600&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'Prompt', sans-serif;
        background-color: #fafafa;
        margin: 0;
        padding: 0;
      }
      h1 {
        color: #6A1B9A;
        font-weight: bold;
        font-size: 36px;
      }
      h2 {
        color: #8E24AA;
        font-size: 20px;
        margin-top: -5px;
        margin-bottom: 20px;
      }
      .sub-header {
        font-size: 15px;
        color: #333333;
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
      .nota-metodo {
        font-size: 12px;
        color: #5A5A5A;
        text-align: left;
        line-height: 1.5;
      }
      .well-panel-fao {
        background-color: #6A1B9A;
        color: white;
        font-weight: bold;
        font-size: 14px;
        border-radius: 8px;
      }
    "))
  ),
  
  # --- Títulos principales ---
  h1("Comparativo de Precios entre Ciudades"),
  h2("Diferencias promedio respecto a Bogotá"),
  
  div(textOutput("subtitulo"), class = "sub-header"),
  
  # --- Controles ---
  fluidRow(
    column(6, selectInput("producto", "Seleccione producto:", productos, selected = "Aguacate")),
    column(6, selectInput("anio", "Seleccione año:", anios, selected = "2014"))
  ),
  
  br(),
  
  # --- Gráfico + Panel lateral + Botones ---
  fluidRow(
    column(
      9,
      plotlyOutput("grafico", height = "420px"),
      br(),
      downloadButton("descargar", "Gráfica", class = "btn-faoc"),
      downloadButton("descargarDatos", "Datos", class = "btn-faoc"),
      shiny::a("GitHub",
               href = "https://github.com/Simonaa-Antioquia/Tableros/tree/main/Tabs_3_4",
               target = "_blank",
               class = "btn btn-faoc",
               icon("github")),
      actionButton("reset", "Restablecer", icon = icon("refresh"), class = "btn-faoc"),
      downloadButton("report", "Generar informe", class = "btn-faoc")
    ),
    column(
      3,
      wellPanel(textOutput("mensaje1"), class = "well-panel-fao")
    )
  ),
  
  br(),
  
  # --- Nota metodológica extendida ---
  fluidRow(
    column(
      12,
      HTML("
        <div class='nota-metodo'>
        Fuente: Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA).<br>
        Este gráfico muestra la diferencia promedio de precios entre varias ciudades comparadas con Bogotá.<br>
        El tamaño del círculo refleja cuánto cambia el precio dentro de cada ciudad (desviación estándar).<br>
        Valores positivos indican precios mayores que Bogotá; negativos, menores.<br>
        Los productos analizados corresponden a la variedad predominante reportada por el DANE en Corabastos.<br><br>
        </div>
      ")
    )
  ),
  
  br(), br(),
  
  # --- Logo institucional FAO ---
  tags$div(
    tags$img(
      src = "logo_2.png",
      style = "
        width: 100%;
        display: block;
        margin: 30px auto 0 auto;
        max-height: 180px;
        object-fit: contain;
      "
    ),
    style = "
      width: 100%;
      background-color: white;
      padding-top: 20px;
      padding-bottom: 20px;
    "
  )
)
