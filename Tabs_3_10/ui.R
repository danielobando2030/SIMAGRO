################################################################################-
# Proyecto FAO
# Visualización de DATOS – Precios de Insumos Agrícolas
################################################################################-
#Autores: Cristian Daniel Obando, Luis Miguel Garcia, Juan Carlos Muñoz Mora
#Fecha de creacion: 09/11/2025
#Fecha de ultima modificacion: 28/11/2025
################################################################################-

# Limpiar entorno
rm(list = ls())

# Paquetes
################################################################################-
library(readr); library(lubridate); library(dplyr); library(ggplot2); library(zoo)
library(readxl); library(glue); library(tidyverse); library(gridExtra)
library(corrplot); library(shiny); library(shinydashboard); library(plotly);library(scales)
options(scipen = 999)
################################################################################-
source("3_10b_boxplot_insumos.R")



################################################################################-
# UI
################################################################################-

ui <- fluidPage(
  
  # ------------------------------------------------------------------
  # Estilos institucionales FAO – Prompt + colores
  # ------------------------------------------------------------------
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "https://fonts.googleapis.com/css2?family=Prompt&display=swap"
    ),
    
    tags$style(HTML("
      .main-header {
        font-family: 'Prompt', sans-serif;
        font-size: 40px;
        color: #743639;
      }
      .sub-header {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
        color: #743639;
      }
      .main-header_2 {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
        color: #743639;
      }
      .sub-header2 {
        font-family: 'Prompt', sans-serif;
        font-size: 15px;
        color: #4E4D4D;
      }
      .sub-header3 {
        font-family: 'Prompt', sans-serif;
        font-size: 15px;
        color: #4E4D4D;
      }
      .center {
        display: flex;
        justify-content: center;
      }
      .scrollable-content {
        overflow-y: auto;
        overflow-x: hidden;
        height: auto;
      }
      
    "))
  ),
  
  # ------------------------------------------------------------------
  # TÍTULO PRINCIPAL
  # ------------------------------------------------------------------
  tags$h1(
    "Distribución de precios de insumos agrícolas en el tiempo",
    class = "main-header"
  ),
  tags$h1(
    "Análisis histórico de precios de los insumos agrícolas en las centrales de abasto de Bogotá.",
    class = "main-header_2"
  ),
  
  div(
    textOutput("subtitulo"),
    class = "sub-header2",
    style = "margin-bottom: 20px;"
  ),
  
  # ------------------------------------------------------------------
  # FILTROS
  # ------------------------------------------------------------------
  fluidRow(
    column(
      width = 3,
      selectInput(
        "subgrupo_sel",
        "Subgrupo:",
        choices = sort(unique(base_precios$subgrupos)),
        selected = "Fungicidas"
      )
    ),
    column(
      width = 3,
      uiOutput("ui1")
      
    )
  ),
  
  # ------------------------------------------------------------------
  # GRÁFICO + RECUADROS LATERALES
  # ------------------------------------------------------------------
  fluidRow(
    column(
      width = 9,
      div(
        plotlyOutput("grafico", height = "420px"),
        # Botones — MISMO ORDEN QUE MÓDULO 1_10b
        actionButton("descargar", "Gráfica", icon = icon("download")),
        downloadButton("descargarDatos", "Datos"),
        shiny::a(
          "GitHub",
          href = "https://github.com/Simonaa-Antioquia/Tableros/tree/main/Tabs_3_10",
          target = "_blank",
          class = "btn btn-default shiny-action-button",
          icon("github")
        ),
        actionButton("reset", "Restablecer", icon = icon("refresh")),
        downloadButton("report", "Generar informe")
      )
    ),
    
    # ------------------------------------------------------------------
    # RECUADROS: mantengo mismo estilo y colores del módulo 1_10b
    # ------------------------------------------------------------------
    column(
      width = 3,
      div(
        wellPanel(
          textOutput("mensaje1"),
          style = "background-color: #8A171C; color: #FFFFFF;"
        )
        
        # Si deseas más recuadros descomentamos:
        # wellPanel(textOutput("mensaje2"),
        #           style = "background-color:#005A45; color:#FFFFFF;")
      )
    )
  ),
  
  # ------------------------------------------------------------------
  # TEXTO ACADÉMICO / ACLARACIONES
  # ------------------------------------------------------------------
  fluidRow(
    column(
      width = 12,
      align = "left",
      HTML("
        <b>Fuente:</b> Cálculos propios con base en el Sistema de Información de 
        Precios y Abastecimiento del Sector Agropecuario – SIPSA (DANE).<br><br>

        <p>Esta visualización muestra cómo varían los precios de los insumos agrícolas 
        a lo largo del tiempo, discriminados por subgrupo y tipo de presentación.</p>

        <ul>
          <li><strong>Cajas del boxplot:</strong> representan la dispersión de precios dentro del subgrupo.</li>
          <li><strong>Mediana:</strong> refleja el precio típico del subgrupo para la presentación seleccionada.</li>
          <li><strong>Rango intercuartílico:</strong> indica qué tan estables o volátiles fueron los precios.</li>
        </ul>

        <p>Comparar entre subgrupos permite identificar insumos con mayor variabilidad de precios 
        y posibles presiones de mercado.</p>
      "),
      style = "font-size:12px; color:#4E4D4D; text-align:left; 
               font-family:'Prompt', sans-serif; margin-top:20px;"
    )
  ),
  
  # ------------------------------------------------------------------
  # LOGO INSTITUCIONAL (PIE DE PÁGINA)
  # ------------------------------------------------------------------
  fluidRow(
    tags$div(
      tags$img(src = "logo_2.png", style = "width: 100%; margin: 0;"),
      style = "width: 100%; margin: 0;"
    )
  )
)

