# Proyecto FAO
# Visualizacion de DATOS   - abastecimeinto en resto 
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 20/03/2024
#Fecha de ultima modificacion: 23/04/2024
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################-
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse);library(gridExtra);library(corrplot);library(shiny); library(shinydashboard)
options(scipen = 999)
################################################################################-

# Corremos el codigo "002_Indices_abastecimiento_Cundinamarca.R"
source("1_9b_Indices_Distribución_de las_Cantidades_entran.R")


ui <- fluidPage(
  tags$head(
    tags$title("Distribución de la cantidad mensual de productos que entran a Bogotá por año"),  
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Prompt&display=swap"), 
    tags$style(HTML("
      .main-header {
        font-family: 'Prompt', sans-serif;
        font-size: 40px;
         color: #266486;
      }
      .sub-header {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
        color: #4E4D4D;
      }
      .main-header_2 {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
        color: #266486;
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
    tags$h1("Cantidad de productos que entran a Bogotá por mes", class = "main-header"),
    tags$h1("Descubre cómo se comporta la cantidad mensual de productos que entrán a Bogotá según el SIPSA.", class = "main-header_2"),  
  div(
    textOutput("subtitulo"),
    class = "sub-header2",
    style = "margin-bottom: 20px;"
  ),  
  div(
      fluidRow(
         column(3,
               selectInput("producto", "Producto:",c("Todos los productos" = "todo", as.character(sort(unique(abastecimiento_bogota_ANO_mes_alimento_completo$producto)))))),
      
    ),
  fluidRow(
    column(9,
           div(
             plotly::plotlyOutput("grafico",height = "400px"),
             actionButton("descargar", "Gráfica", icon = icon("download")),
             downloadButton("descargarDatos", "Datos"),
             shiny::a("GitHub", href="https://github.com/danielobando2030/SIMAGRO/tree/main/Tabs_1_9", target="_blank",
                      class = "btn btn-default shiny-action-button", icon("github")),
             actionButton("reset", "Restablecer",icon = icon("refresh")),
             downloadButton("report", "Generar informe")
             #,
             #tableOutput("vistaTabla") 
           )),
    column(3, 
           div(
             wellPanel(textOutput("mensaje1"),
                       style = "background-color: #0087CF; color: #FFFFFF;")#,
             #wellPanel(textOutput("mensaje2"),
              #         style = "background-color: #005A45; color: #FFFFFF;"),
             #wellPanel(textOutput("mensaje3"),
              #         style = "background-color: #094735; color: #FFFFFF;")
           ))
  ),
  fluidRow(
    column(
      12,
      align = "left",
      HTML("
      <b>Fuente:</b> Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA).<br><br>

      <span style='font-size:20px; font-weight:600;'>Distribución de la cantidad de productos que entran a Bogotá</span><br><br>

      Esta visualización muestra la distribución mensual de la cantidad de alimentos que ingresan a la ciudad de Bogotá, desagregada por año.<br><br>

      Cada diagrama de caja y bigotes resume la variabilidad de los volúmenes mensuales de abastecimiento para cada año, permitiendo identificar diferencias 
      en la mediana, la dispersión y la presencia de valores atípicos.<br><br>

      El cuerpo de la caja representa el rango intercuartílico (del 25% al 75% de los valores observados), la línea central indica la mediana del volumen mensual, 
      y los bigotes muestran la extensión de los valores típicos. Los puntos fuera de los bigotes corresponden a meses con niveles de abastecimiento 
      inusualmente altos o bajos, que podrían estar asociados con variaciones estacionales, coyunturas de mercado o condiciones climáticas.<br><br>

      Comparar los diagramas entre años permite analizar:<br>
      <ul>
        <li>Cambios en la mediana anual de abastecimiento.</li>
        <li>Aumentos o disminuciones en la variabilidad mensual.</li>
        <li>Presencia de valores extremos asociados con choques específicos en la oferta o la demanda.</li>
      </ul>

      En conjunto, el gráfico facilita una lectura rápida de la estabilidad y tendencia del abastecimiento alimentario hacia Bogotá a lo largo del tiempo.
    "),
      style = "font-size:12px; color:#4E4D4D; text-align:left; font-family:'Prompt', sans-serif; margin-top:20px;"
    )
  ),
  fluidRow(
    tags$div(
      tags$img(src = 'logo_2.png', style = "width: 100%; margin: 0;"),
      style = "width: 100%; margin: 0;"
    )
  )
))