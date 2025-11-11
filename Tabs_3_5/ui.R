################################################################################-
# Proyecto FAO - VP - 2025
# UI - Ranking mensual de precios mayoristas
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
        font-weight: bold;
        font-size: 14px;
        border-radius: 8px;
        padding: 15px;
      }
      .nota-metodo {
        font-size: 12px;
        color: #5A5A5A;
        text-align: left;
        line-height: 1.5;
      }
    "))
  ),
  
  div(class = "logo-header", img(src = "logo_3.png", height = "90px")),
  
  h1("Ranking mensual de precios mayoristas por ciudad"),
  h2("Posición relativa de Bogotá frente a otras ciudades"),
  
  fluidRow(
    column(6, selectInput("producto", "Seleccione producto:", productos, selected = "Aguacate")),
    column(6, selectInput("anio", "Seleccione año:", choices = c("Todos" = "todos", anios), selected = "2024"))
  ),
  
  br(),
  
  fluidRow(
    column(
      9,
      plotlyOutput("grafico", height = "480px"),
      br(),
      downloadButton("descargarGrafico", "Gráfica", class = "btn-faoc"),
      downloadButton("descargarDatos", "Datos", class = "btn-faoc"),
      shiny::a("GitHub",
               href = "https://github.com/Simonaa-Antioquia/Tableros/tree/main/Tabs_3_5",
               target = "_blank", class = "btn btn-faoc", icon("github")),
      actionButton("reset", "Restablecer", icon = icon("refresh"), class = "btn-faoc"),
      downloadButton("descargarPDF", "Generar informe", class = "btn-faoc")
    ),
    column(
      3,
      wellPanel(textOutput("mensaje1"), class = "well-panel-fao")
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      12, align = "left",
      HTML("
        <div class='nota-metodo'>
        Fuente: cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA). 
        Este módulo presenta el escalafón mensual de precios mayoristas por ciudad, construido con base en el precio promedio registrado para cada producto y mes. 
        El ranking o posición se obtiene ordenando los precios de mayor a menor dentro del grupo de ciudades con información disponible en cada mes. 
        Una posición igual a 1 indica que la ciudad registró el precio más alto del conjunto, mientras que valores mayores reflejan precios relativamente más bajos. 
        La información corresponde a los precios en los principales centros de acopio urbanos del SIPSA, incluyendo la Central Mayorista de Corabastos (Bogotá). 
        Para productos como fríjol verde, tomate, aguacate, banano, guayaba, mandarina, naranja, piña, arracacha, papa negra y yuca, los valores se refieren a la variedad predominante en el mercado al momento de la recolección. 
        De acuerdo con el SIPSA, los precios corresponden a valores mayoristas por kilogramo de productos de primera calidad.
        </div>
      ")
    )
  ),
  
  br(), br(),
  
  div(class = "logo-footer", img(src = "logo_2.png", height = "80px"))
)
