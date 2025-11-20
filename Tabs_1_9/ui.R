# Proyecto FAO
# Visualizacion de DATOS   - abastecimeinto en resto 
################################################################################-
#Autores: Cristian Daniel Obando,m Luis Miguel Garcia
#Fecha de creacion: 09/11/2025
#Fecha de ultima modificacion: 10/11/2025
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
    tags$h1("Distribución del volumen mensual de alimentos que entran a Bogotá por año", class = "main-header"),
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
      <b>Fuente:</b> Elaboración propia con base en datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario – SIPSA (DANE).<br><br>
     <p>Esta visualización muestra cómo varía mes a mes el volumen de alimentos que llegan a Bogotá, separada por años.</p>
<ul>
  <li><strong>Mediana:</strong> el valor típico de cada año (la línea en el centro).</li>
  <li><strong>Percentil 25% y 75%:</strong> los bordes de la caja muestran el punto por debajo del cual está el 25% de los meses y el punto por debajo del cual está el 75% de los meses. Es decir, la caja agrupa al 50% central de los meses.</li>
  <li><strong>Variación mensual:</strong> el tamaño de la caja indica qué tan parejos o dispersos fueron los volúmenes dentro del año.</li>
</ul>

<p>Comparando las cajas entre años puedes ver si los niveles y la variación del abastecimiento cambiaron con el tiempo.</p>"),
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