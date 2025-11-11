################################################################################
# Proyecto FAO - VP - 2025
# Interfaz UI - Módulo 3_10: Precios de Insumos Agrícolas
################################################################################
# Autores: Luis Miguel García, Laura Quintero, Daniel Obando
# Última edición: 2025/11/11
################################################################################

library(shiny)
library(plotly)
library(dplyr)

# Cargar base para los selectInput iniciales
data <- readRDS("data_precios_insumos_3_10.rds")

ui <- fluidPage(
  
  # ------------------------------------------------------------------
  # Estilos institucionales
  # ------------------------------------------------------------------
  tags$head(
    tags$title("Precios de Insumos Agrícolas - FAO VP 2025"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Prompt:wght@400;600&display=swap"
    ),
    tags$style(HTML("
      body { font-family: 'Prompt', sans-serif; background-color: #fafafa; }
      h2, h4, h5 { color: #3D3D6B; }
      .btn-faoc {
        background-color: #6A0DAD;
        border-color: #6A0DAD;
        color: white;
        font-weight: 500;
      }
      .btn-faoc:hover {
        background-color: #500985;
        border-color: #500985;
        color: white;
      }
    "))
  ),
  
  # ------------------------------------------------------------------
  # Encabezado principal
  # ------------------------------------------------------------------
  div(
    h2("Distribución de precios de insumos agrícolas en el tiempo",
       style = "font-weight:600; color:#3D3D6B; text-align:center;"),
    h4("Análisis histórico por subgrupo y presentación de insumos agrícolas (SIPSA - DANE).",
       style = "color:#5A5A5A; font-weight:400; text-align:center; margin-top:-10px; margin-bottom:5px;")
  ),
  
  br(),
  
  # ------------------------------------------------------------------
  # Filtros principales
  # ------------------------------------------------------------------
  fluidRow(
    column(4,
           selectInput(
             "subgrupo_sel", "Seleccione subgrupo:",
             choices = sort(unique(data$subgrupos)),
             selected = "Fungicidas"
           )
    ),
    column(4,
           selectInput(
             "presentacion_sel", "Seleccione presentación:",
             choices = NULL
           )
    ),
    column(4,
           actionButton("reset", "Restablecer selección",
                        icon = icon("rotate-left"),
                        class = "btn btn-faoc",
                        style = "width:100%; margin-top:25px;")
    )
  ),
  
  br(),
  
  # ------------------------------------------------------------------
  # Gráfico principal
  # ------------------------------------------------------------------
  fluidRow(
    column(12, align = "center",
           plotlyOutput("grafico_boxplot", height = "500px")
    )
  ),
  
  br(),
  
  # ------------------------------------------------------------------
  # Botones institucionales FAO (orden: Gráfica, Datos, GitHub, Restablecer, Informe)
  # ------------------------------------------------------------------
  fluidRow(
    column(12, align = "center",
           downloadButton("descargarGrafico", "Gráfica", class = "btn btn-faoc"),
           downloadButton("descargarDatos", "Datos", class = "btn btn-faoc"),
           shiny::a("GitHub",
                    href = "https://github.com/Simonaa-Antioquia/Tableros/tree/main/Tabs_3_10",
                    target = "_blank",
                    class = "btn btn-faoc",
                    icon("github")),
           actionButton("reset2", "Restablecer", icon = icon("refresh"), class = "btn btn-faoc"),
           downloadButton("descargarPDF", "Generar informe", class = "btn btn-faoc")
    )
  ),
  
  br(),
  
  # ------------------------------------------------------------------
  # Fuente de datos y aclaraciones
  # ------------------------------------------------------------------
  fluidRow(
    column(12, align = "left",
           HTML("Fuente: Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA) – DANE.<br>
                 Los valores corresponden a precios promedio reportados para insumos agrícolas por presentación y subgrupo.<br>
                 Los precios reflejan el costo por unidad estándar según la presentación (litro, kilogramo, etc.)."),
           style = "font-size:12px; color:#5A5A5A; text-align:left;"
    )
  ),
  
  # ------------------------------------------------------------------
  # Logo institucional (pie de página)
  # ------------------------------------------------------------------
  fluidRow(
    tags$div(
      tags$img(src = "logo_2.png", style = "width: 100%; margin: 0;"),
      style = "width: 100%; margin:0;"
    )
  )
)
