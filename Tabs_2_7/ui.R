#Proyecto FAO
#INDICE Herfindahl–Hirschman - shiny 2 - De donde viene la comida (municipios)
################################################################################
#Autores: Juan Carlos, Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 14/03/2024
#Fecha de ultima modificacion: 21/02/2024
################################################################################
# Paquetes 
################################################################################
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse); library(shiny); library(lubridate);library(shinythemes);
options(scipen = 999)
################################################################################
rm(list = ls())

source("2_7b_funciones_participacion_destino.R")
productos <- unique(GINI_anual_producto$producto)

ui <- fluidPage(
  #theme = shinythemes::shinytheme("default"),
  tags$head(
    tags$title("Índice Gini de concentración del destino de los alimentos - Índice"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Prompt&display=swap"),
    tags$style(HTML("
      .main-header {
        font-family: 'Prompt', sans-serif;
        font-size: 40px;
        color: #743639;
      }
      .sub-header {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
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
    ")),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML")
  ),
  tags$h1("Índice Gini de diversidad de destino de los alimentos", class = "main-header"),
  tags$h1("Análisis de la variedad de territorios conectados por el flujo de alimentos desde Cundinamarca hacia otras plazas en SIPSA.", class = "main-header_2"),
  div(
    textOutput("subtitulo"),
    class = "sub-header2",
    style = "margin-bottom: 20px;"
  ),
    div(
        fluidRow(
          column(4,
                 selectInput("tipo", "Seleccione el tipo:", 
                             choices = list("Anual" = 1, 
                                            "Anual por producto" = 2, 
                                            "Mensual" = 3, 
                                            "Mensual por producto" = 4))
        ),
        column(4,
               conditionalPanel(
                 condition = "input.tipo == 2 || input.tipo == 4",
                 selectInput("producto", "Seleccione los productos:", 
                             choices = c("Todos los productos" = NULL , unique(GINI_anual_producto$producto)), multiple = TRUE)
               )
        ),
        column(4,
               conditionalPanel(
                 condition = "input.tipo == 3 || input.tipo == 4",
                 selectInput("anio", "Seleccione el año:", 
                             choices = c("Todos los años" = "todo", unique(GINI_mensual_producto$year)))
               )
        )
      )
  ),
 
    fluidRow(
        column(9,
               plotlyOutput("grafico"),
               downloadButton("descargar_", "Gráfica", icon = icon("download")),
               downloadButton("descargarDatos", "Datos"),
               shiny::a("GitHub", href="https://github.com/danielobando2030/SIMAGRO/tree/main/Tabs_2_7", target="_blank",
                        class = "btn btn-default shiny-action-button", icon("github")),
               actionButton("reset", "Restablecer", icon = icon("refresh")),
               downloadButton("report", "Generar informe")
        ),
        column(3, 
               div(
                 wellPanel(textOutput("mensaje1"),
                           style = "background-color: #BC222A; color: #FFFFFF;"),
                 wellPanel(uiOutput("mensaje2"),
                           style = "background-color: #983136; color: #FFFFFF;")
               ))
    ),
  fluidRow(
    column(
      12,
      align = "left",
      HTML("
      <b>Fuente:</b> Elaboración propia con base en datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario – SIPSA (DANE).<br><br>

      Este gráfico se calcula con base en el índice de Gini.<br><br>

      <b>La fórmula del índice de Gini es:</b><br><br>

      $$G = 1 - \\sum_{i=1}^{n} (Y_i + Y_{i-1})(X_i - X_{i-1})$$

      <br>

      Donde <em>X<sub>i</sub></em> representa la proporción acumulada de municipios destino, 
      y <em>Y<sub>i</sub></em> la proporción acumulada del volumen total de alimentos que reciben desde Cundinamarca.<br><br>

      El índice de Gini mide el grado de desigualdad en la distribución. 
      Toma valores entre 0 y 1, donde 0 indica una distribución completamente equitativa 
      (todos los municipios destino reciben alimentos en igual proporción) 
      y 1 indica la máxima concentración 
      (un solo municipio concentra la totalidad del volumen recibido desde Cundinamarca).<br><br>

      Cuanto más cercana esté la curva de Lorenz a la línea de 45°, menor será la concentración 
      del abastecimiento proveniente de Cundinamarca. Por el contrario, cuanto más alejada esté, 
      mayor será la dependencia de un grupo reducido de municipios destino.

      <script>
        MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);
      </script>
    "),
      style = "font-size:12px; color:#4E4D4D;
             text-align:left; font-family:'Prompt', sans-serif; 
             margin-top:20px;"
    )
  ),fluidRow(
      tags$div(
        tags$img(src = 'logo_2.png', style = "width: 100%; margin: 0;"),  
        style = "width: 100%; margin:0;"  
      )
    ) 
    
  )

