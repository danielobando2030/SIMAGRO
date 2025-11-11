# Proyecto FAO
# Visualizacion de DATOS   - abastecimeinto en bogota 
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
source("2_1b_Indices_Curva_Lorentz_Gini_Cantidades_entran.R")


ui <- fluidPage(
  tags$head(
    tags$title("Curva de lorentz para la diversidad de origenes"),  
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Prompt&display=swap"), 
    tags$style(HTML("
      .main-header {
        font-family: 'Prompt', sans-serif;
        font-size: 40px;
        color: #0D8D38;
      }
      .sub-header {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
      }
      .main-header_2 {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
        color: #0D8D38;
      }
      .sub-header2 {
        font-family: 'Prompt', sans-serif;
        font-size: 15px;
      }
      .sub-header3 {
        font-family: 'Prompt', sans-serif;
        font-size: 15px;
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
    tags$h1("Diversidad de proveedores de alimentos para Cundinamarca", class = "main-header"),
    tags$h1("Descubre el nivel de dependencia de los municipios proveedores de alimentos que ingresan a Cundinamarca según el SIPSA.", class = "main-header_2"),  
  div(
    textOutput("subtitulo"),
    class = "sub-header2",
    style = "margin-bottom: 20px;"
  ),  
  div(
      fluidRow(
        
        column(2,
               selectInput("anio", "Año:", c("Todos los años" = "todo", sort(as.character(unique(abastecimiento_bogota_ANO_alimento$anio)))))),
        
       
          column(2,
                 selectInput("mes", "Mes:",
                             c("Todos los meses" = "todo",
                               "Enero" = 1, "Febrero" = 2, "Marzo" = 3, "Abril" = 4, "Mayo" = 5, 
                               "Junio" = 6, "Julio" = 7, "Agosto" = 8, "Septiembre" = 9,
                               "Octubre" = 10, "Noviembre" = 11, "Diciembre" = 12),
                             selected = "todo")),
        column(3,
               selectInput("producto", "Producto:",c("Todos los productos" = "todo", as.character(sort(unique(abastecimiento_bogota_alimento$producto)))))),
      
    ),
  fluidRow(
    column(9,
           div(
             plotly::plotlyOutput("grafico",height = "400px"),
             actionButton("descargar", "Gráfica", icon = icon("download")),
             downloadButton("descargarDatos", "Datos"),
             shiny::a("GitHub", href="https://github.com/Simonaa-Antioquia/Tableros/tree/83b0c073b699faab18e927642e2919f2aa0a1dd5/Abs1", target="_blank",
                      class = "btn btn-default shiny-action-button", icon("github")),
             actionButton("reset", "Restablecer",icon = icon("refresh")),
             downloadButton("report", "Generar informe")
             #,
             #tableOutput("vistaTabla") 
           )),
    column(3, 
           div(
             wellPanel(textOutput("mensaje1"),
                       style = "background-color: #0D8D38; color: #FFFFFF;")#,
             #wellPanel(textOutput("mensaje2"),
              #         style = "background-color: #005A45; color: #FFFFFF;"),
             #wellPanel(textOutput("mensaje3"),
              #         style = "background-color: #094735; color: #FFFFFF;")
           ))
  ),
  fluidRow(
    column(12,
           style = "margin-top: 2px;",
           tags$div(
             tags$p("Fuente: Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA).", class = "sub-header2", style = "margin-top: 3px;"),
             tags$p(
               "Esta visualización muestra el porcentaje acumulado de municipios que envían sus productos a Cundinamarca. 
  Cuanto más cercana esté la curva a la línea de 45°, menor será la dependencia de Cundinamarca respecto a un grupo reducido de municipios.",
               class = "sub-header2",
               style = "margin-top: 3px;"
             )
             
           )
    )
  ),
    fluidRow(
    tags$div(
      tags$img(src = 'logo_2.png', style = "width: 100%; margin: 0;"),  
      style = "width: 100%; margin:0;"  
    )
  )
))