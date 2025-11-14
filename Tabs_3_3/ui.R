################################################################################-
# Proyecto FAO - VP - 2025
# Elasticidad de precios y abastecimiento de alimentos
################################################################################-
# Autores: Luis Miguel García, Laura Quintero, Juliana Lalinde
# Última modificación: 08/11/2025
################################################################################-

library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
source("3_3b_funcion_elasticidad.R") 
# Datos cargados desde la función
productos <- sort(unique(data$producto))
anios <- sort(unique(year(data$mes_y_ano)))

################################################################################-
# Interfaz de usuario
################################################################################-
ui <- fluidPage(
  tags$head(
    tags$title("Elasticidad, precios y abastecimiento de alimentos - FAO VP 2025"),
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
  h1("Elasticidad, precios y abastecimiento de alimentos"),
  h2("Evolución mensual de la elasticidad en función del precio y la cantidad"),
  
  div(textOutput("subtitulo"), class = "sub-header"),
  
  # --- Controles principales ---
  fluidRow(
    column(6, selectInput("producto", "Seleccione producto:", productos, selected = "Aguacate")),
    column(6, selectInput("anio", "Seleccione año:", c("Todos" = "todos", anios), selected = "todos"))
  ),
  
  br(),
  
  # --- Gráfico y botones de acción ---
  fluidRow(
    column(
      9,
      plotlyOutput("grafico", height = "420px"),
      br(),
      downloadButton("descargar", "Gráfica", class = "btn btn-faoc"),
      downloadButton("descargarDatos", "Datos", class = "btn btn-faoc"),
      shiny::a("GitHub",
               href = "https://github.com/Simonaa-Antioquia/Tableros/tree/main/Tabs_3_3",
               target = "_blank",
               class = "btn btn-faoc",
               icon("github")),
      actionButton("reset", "Restablecer", icon = icon("refresh"), class = "btn btn-faoc"),
      downloadButton("report", "Generar informe", class = "btn btn-faoc")
    ),
    column(
      3,
      wellPanel(textOutput("mensaje1"), class = "well-panel-fao")
    )
  ),
  
  br(),
  
  # --- Nota metodológica extendida FAO-SIPSA ---
  fluidRow(
    column(
      12, align = "left",
      HTML("
        <div class='nota-metodo'>
        Fuente: Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA).<br>
        La información solo se muestra para los precios en el centro de acopio de Bogotá.<br>
        Para los productos fríjol verde, tomate, aguacate, banano, guayaba, mandarina, naranja, piña, arracacha, papa negra y yuca,
        los precios reportados corresponden a la variedad predominante en el mercado al momento de la recolección de la información.<br>
        De acuerdo con el SIPSA, el valor reportado corresponde al precio mayorista por kilogramo de producto de primera calidad en la Central Mayorista de Corabastos.<br><br>
        La elasticidad se calcula como la variación porcentual del precio dividida por la variación porcentual de la cantidad ofertada.
        Valores cercanos a 0 indican rigidez de precios; valores más altos en valor absoluto indican mayor sensibilidad.
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
