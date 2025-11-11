################################################################################
# Proyecto FAO - VP - 2025
# Interfaz UI - Módulo 3_9: Huella de Carbono (estructura institucional unificada)
################################################################################
# Autores: Luis Miguel García, Juliana Lalinde, Laura Quintero, Germán Angulo
# Última edición: 2025/11/10
################################################################################

library(shiny)
library(plotly)
library(dplyr)
library(lubridate)

options(scipen = 999)
source("3_9b_huella_carbono.R")

ui <- fluidPage(
  
  # ------------------------------------------------------------------
  # Encabezado
  # ------------------------------------------------------------------
  tags$head(
    tags$title("Huella de carbono - FAO VP 2025"),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "https://fonts.googleapis.com/css2?family=Prompt:wght@400;600&display=swap"
    ),
    tags$style(HTML("
      body { font-family: 'Prompt', sans-serif; background-color: #fafafa; }
      h2, h4, h5 { color: #2E7D32; }
      .btn-faoc {
        background-color: #6A0DAD;
        border-color: #6A0DAD;
        color: white;
        font-weight: 500;
        border-radius: 6px;
      }
      .btn-faoc:hover {
        background-color: #500985;
        border-color: #500985;
        color: white;
      }
      .footer-text { font-size:12px; color:#5A5A5A; text-align:left; line-height:1.3; }
    "))
  ),
  
  # ------------------------------------------------------------------
  # Título principal
  # ------------------------------------------------------------------
  div(
    h2("Huella de carbono por grupo y producto",
       style = "font-weight:600; color:#2E7D32; text-align:center;"),
    h4("Análisis de emisiones de CO₂ según grupos alimentarios y productos representativos del SIPSA.",
       style = "color:#5A5A5A; font-weight:400; text-align:center; margin-top:-10px; margin-bottom:5px;")
  ),
  
  br(),
  
  # ------------------------------------------------------------------
  # Filtros
  # ------------------------------------------------------------------
  fluidRow(
    column(3,
           selectInput("anio", "Año:",
                       choices = sort(unique(data$anio)),
                       selected = 2024)
    ),
    column(3,
           selectInput("mes", "Mes:",
                       choices = setNames(
                         sprintf("%02d", 1:12),
                         c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                           "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")),
                       selected = "12")
    )
  ),
  
  br(),
  
  # ------------------------------------------------------------------
  # Gráfico principal + panel lateral
  # ------------------------------------------------------------------
  fluidRow(
    column(
      width = 9,
      align = "center",
      plotlyOutput("grafico", height = "500px")
    ),
    column(
      width = 3,
      uiOutput("top5_emisores"),
      br(),
      htmlOutput("mensaje1"),
      style = "padding:0px; margin-top:20px;"
    )
  ),
  
  br(),
  
  # ------------------------------------------------------------------
  # Botones institucionales FAO (alineados al centro)
  # ------------------------------------------------------------------
  fluidRow(
    column(12, align = "center",
           downloadButton("descargarGraf", "Gráfica", class = "btn btn-faoc"),
           downloadButton("descargarDatos", "Datos", class = "btn btn-faoc"),
           tags$a("GitHub",
                  href = "https://github.com/FAO-Cundinamarca/VP2025_Tabs_3_9",
                  target = "_blank",
                  class = "btn btn-faoc",
                  icon("github")),
           actionButton("reset", "Restablecer", icon = icon("refresh"), class = "btn btn-faoc"),
           downloadButton("report", "Generar informe PDF", class = "btn btn-faoc")
    )
  ),
  
  br(),
  
  # ------------------------------------------------------------------
  # Fuente de datos
  # ------------------------------------------------------------------
  fluidRow(
    column(12, align = "left",
           HTML("
           <b>Fuente:</b> cálculos propios a partir de datos del 
           <i>Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA) – DANE</i>.<br>
           Las emisiones se estiman a partir de factores de conversión por producto y distancia, expresadas en toneladas de CO₂ equivalentes.<br>
           Los factores de emisión provienen de las siguientes fuentes técnicas:<br>
           • <i>IPCC (2006). Guidelines for National Greenhouse Gas Inventories.</i><br>
           • <i>IPCC (2019). 2019 Refinement to the 2006 IPCC Guidelines for National Greenhouse Gas Inventories.</i><br>
           • <i>DEFRA (2023). UK Government GHG Conversion Factors for Company Reporting.</i><br>
           • <i>FAO (2022). Global Livestock Environmental Assessment Model (GLEAM) 2.0 – Transport and Supply Chains Module.</i><br><br>
           Elaboración propia, FAO VP-2025 – Cundinamarca.
           "),
           class = "footer-text")
  ),
  
  br(),
  
  # ------------------------------------------------------------------
  # Logo institucional
  # ------------------------------------------------------------------
  fluidRow(
    tags$div(
      tags$img(src = "logo_2.png", style = "width: 100%; margin: 0;"),
      style = "width: 100%; margin:0;"
    )
  )
)
