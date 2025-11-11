# Proyecto FAO
# Visualizacion de DATOS   - abastecimeinto en bogota 
################################################################################-
#Autores: Cristian Daniel Obando, Luis Miguel Garcia
#Fecha de creacion: 20/03/2024
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
source("1_1b_Indices_abastecimiento_Cundinamarca_funciones.R")


ui <- fluidPage(
  tags$head(
    tags$title("Municipios_proveedores"),  
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Prompt&display=swap"), 
    tags$style(HTML("
      .main-header {
        font-family: 'Prompt', sans-serif;
        font-size: 40px;
        color: #134174;
      }
      .sub-header {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
      }
      .main-header_2 {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
        color: #134174;
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
        color: #4E4D4D;
      }
      .scrollable-content {
        overflow-y: auto;
        overflow-x: hidden;
        height: auto;
        color: #4E4D4D;
      }
      
    "))
  ),
    tags$h1("Municipios proveedores de alimentos para Cundinamarca", class = "main-header"),
    tags$h1("Descubre los principales municipios proveedores de alimentos que ingresan a Cundinamarca según el SIPSA.", class = "main-header_2"),  
  div(
    textOutput("subtitulo"),
    class = "sub-header2",
    style = "margin-bottom: 20px;"
  ),  
  div(
      fluidRow(
        column(3,
               selectInput("variable", "Seleccione la variable:", c("Total"=1,
                                                                    "Local"=2,
                                                                    "Externo"=3))),
        column(2,
               selectInput("anio", "Año:", c("Todos los años" = "todo", sort(as.character(unique(abastecimiento_bogota$anio)))))),
        column(2,
               selectInput("mes", "Mes:", c("Todos los meses" = "todo", "Enero" = 1, "Febrero" = 2, "Marzo" = 3, "Abril" = 4, "Mayo" = 5, 
                                           "Junio" = 6, "Julio" = 7, "Agosto" = 8, "Septiembre" = 9, "Octubre" = 10, "Noviembre" = 11, 
                                           "Diciembre" = 12), selected = "")),
        column(2,
               numericInput("municipios", "Número de municipios:", value = 10, min = 1, max = 18)),
        column(3,
               selectInput("producto", "Producto:",c("Todos los productos" = "todo", as.character(sort(unique(abastecimiento_bogota$producto))))))
      )),
  fluidRow(
    column(9,
           div(
             plotly::plotlyOutput("grafico",height = "400px"),
             actionButton("descargar", "Gráfica", icon = icon("download")),
             downloadButton("descargarDatos", "Datos"),
             shiny::a("GitHub", href="https://github.com/Simonaa-Antioquia/Tableros/tree/83b0c073b699faab18e927642e2919f2aa0a1dd5/Abs1", target="_blank",
                      class = "btn btn-default shiny-action-button", icon("github")),
             actionButton("reset", "Restrablecer",icon = icon("refresh")),
             downloadButton("report", "Generar informe")
             #,
             #tableOutput("vistaTabla") 
           )),
    column(3, 
           div(
             wellPanel(textOutput("mensaje1"),
                       style = "background-color: #134174; color: #FFFFFF;")#,
             #wellPanel(textOutput("mensaje2"),
              #         style = "background-color: #005A45; color: #FFFFFF;"),
             #wellPanel(textOutput("mensaje3"),
              #         style = "background-color: #094735; color: #FFFFFF;")
           ))
  ),
  fluidRow(
    column(12, align = "left",
           HTML("<b>Fuente:</b> Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA).<br>
           <br>
                Esta visualización muestra el ranking de los municipios de orígen de los alimentos que llegan a las principales centrales
                de abasto de Bogotá, resaltando su participación porcentual en el volumen total de ingresos.<br>
                <b>Local:</b> Productos reportados con origen de territorios del departamento de Cundinamarca.<br>
                <b>Externo:</b> Productos reportados con origen fuera del departamento de Cundinamarca.
                "),style = "font-size:12px; color:#4E4D4D; text-align:left;font-family: 'Prompt', sans-serif;"
    )
  ),
  
  # Logo institucional
  fluidRow(
    tags$div(
      tags$img(src = 'logo_2.png', style = "width: 100%; margin: 0;"),  
      style = "width: 100%; margin:0; "  
    )
  )
)