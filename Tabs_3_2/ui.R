################################################################################-
# Proyecto FAO - VP - 2025
# Visualización de datos - Comparación con Bogotá (AJUSTADO)
################################################################################-

library(shiny)
library(leaflet)
library(dplyr)
library(sf)

# Cargar datos y funciones
source("3_2b_precios_diferencias_mapa_funciones.R")

productos_filtrados <- data_global %>%
  count(producto, name = "Freq") %>%
  filter(Freq > 11) %>%
  arrange(producto) %>%
  pull(producto)

################################################################################-
# UI
################################################################################-

ui <- fluidPage(
  
  # ====================== HEAD ======================
  tags$head(
    tags$title("Comparación de precios con Bogotá"),
    
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "https://fonts.googleapis.com/css2?family=Prompt:wght@400;600&display=swap"
    ),
    
    tags$style(HTML("
      body { 
        font-family: 'Prompt', sans-serif; 
        background-color: #fafafa;
        color:#4E4D4D;
      }

      /* TITULOS */
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

      /* BOTONES FAO */
      .btn-faoc {
        background-color: white !important;
        border: 1px solid #CCC !important;
        color: #4E4D4D !important;
        font-weight: 500;
        border-radius: 6px;
      }
      .btn-faoc:hover {
        background-color: #E2E2E2 !important;
      }

      /* PANELS DERECHA */
      .panel-dorado {
        background-color:#DBC21F;
        color:white;
        font-weight:bold;
        border-radius:8px;
        padding:12px;
      }
      .panel-verde {
        background-color:#B6A534;
        color:white;
        font-weight:bold;
        border-radius:8px;
        padding:12px;
      }

      .nota-metodo {
        font-size:12px; 
        text-align:left;
        margin-top:15px;
        color:#4E4D4D;
      }
    "))
  ),
  
  # ====================== TITULOS ======================
  tags$h1("Diferencia de precios de alimentos por departamento", class = "main-header"),
  tags$h1("Comparación de precios promedio con respecto a Bogotá D.C.", class = "main-header_2"),
  
  div(
    textOutput("subtitulo"),
    class = "sub-header2",
    style = "margin-bottom: 20px;"
  ),
  
  # ====================== FILTROS (MISMA DISPOSICIÓN) ======================
  div(
    fluidRow(
      column(3,
             selectInput("anio", "Año:",
                         choices = c("Todos los años" = "todo", sort(unique(data_global$year))),
                         selected = 2024))
      ,
      column(2,
             selectInput("mes", "Mes:",
                         choices = c("Todos los meses" = "todo",
                                     "Enero"=1,"Febrero"=2,"Marzo"=3,"Abril"=4,
                                     "Mayo"=5,"Junio"=6,"Julio"=7,"Agosto"=8,
                                     "Septiembre"=9,"Octubre"=10,"Noviembre"=11,"Diciembre"=12),
                         selected = 1))
      ,
      column(2,
             selectInput("producto", "Producto:",
                         choices = c("Todos los productos"="todo",
                                     setNames(productos_filtrados, productos_filtrados)),
                         selected = "Aguacate"))
      ,
      column(2, div())  # espacio vacío
      ,
      column(3, div())  # espacio vacío
    )
  ),
  
  # ====================== MAPA + PANEL DERECHA ======================
  fluidRow(
    column(9,
           div(
             leafletOutput("grafico", height = "450px"),
             actionButton("descargar", "Gráfica",  icon=icon("download"), class="btn-faoc"),
             downloadButton("descargarDatos", "Datos", class="btn-faoc"),
             a(
               tagList(icon("github"), " GitHub"),
               href = "https://github.com/Simonaa-Antioquia/Tableros/tree/main/Tabs_3_2",
               target = "_blank",
               class = "btn btn-faoc",
               style = "display:inline-block; padding:6px 12px; border-radius:6px; text-decoration:none;"
             ),
             actionButton("reset", "Restablecer", icon=icon("refresh"), class="btn-faoc"),
             downloadButton("descargarInforme", "Generar informe", class="btn-faoc")
           )
    ),
    column(3,
           
           wellPanel(
             textOutput("mensaje1"),
             style = "
      background-color:#DBC21F; 
      color:white; 
      font-weight:bold; 
      border-radius:8px; 
      padding:12px; 
      margin-bottom:15px;   /* ← ESPACIO ENTRE LOS CUADROS */
    "
           ),
           
           wellPanel(
             textOutput("mensaje2"),
             style = "
      background-color:#B6A534; 
      color:white; 
      font-weight:bold; 
      border-radius:8px; 
      padding:12px;
    "
           )
    )
  ),
  
  # ====================== FUENTE ======================
  fluidRow(
    column(12,
           HTML("<b>Fuente:</b> Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA)."),
           class="nota-metodo")
  ),
  
  # ====================== LOGO ======================
  fluidRow(
    tags$div(
      tags$img(src='logo_2.png', style="width: 100%; margin: 0;"),
      style="width: 100%; margin: 0;"
    )
  )
)
