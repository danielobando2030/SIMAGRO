#Proyecto FAO
################################################################################
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 03/04/2024
#Modificado: Cristian Daniel Obando,m Luis Miguel Garcia
#Fecha de ultima modificacion: 10/11/2025
################################################################################
# Paquetes 
################################################################################
library(shiny); library(lubridate);library(shinythemes);library(shinyWidgets)
options(scipen = 999)
################################################################################
rm(list = ls())

source("1_8b_pareto_productos_funcion.R")

ui <- fluidPage(
  tags$head(
    tags$title("Acumulado_Productos"),  
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Prompt&display=swap"),
    tags$style(HTML("
       .main-header {
        font-family: 'Prompt', sans-serif;
        font-size: 40px;
        color: #266486;
       }
        .main-header_2 {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
        color: #266486;
      }
      .sub-header {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
        color: #4E4D4D;
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
  tags$h1("Principales productos por volumen en el abastecimiento desde y hacia Cundinamarca", class = "main-header"),
  tags$h2("Vizualiza los productos con mayor participación en el abastecimiento de alimentos desde y hacia Cundinamarca", class = "main-header_2"),
  div(
    textOutput("subtitulo"),
    class = "sub-header2",
    style = "margin-bottom: 20px;"
  ),  
  div(
    fluidRow(
      column(3,
             selectInput("algo", "Seleccione la variable:", 
                         choices = list("Entradas" = "Neto_entra", 
                                        "Salidas" = "Neto_sale", 
                                        "Entradas locales" = "Neto_entra_local", 
                                        "Entradas externas" = "Neto_entra_exter"))),
      column(3,
             selectInput("año", "Selecciones el año:", c("Todos los años" = "todo", sort(unique(acum_total_anio$anio))))),
      column(3,
             selectInput("mes", "Selecciones el mes:", c("Todos los meses" = "todo", "Enero" = 1, "Febrero" = 2, "Marzo" = 3, "Abril" = 4, "Mayo" = 5,
                                                         "Junio" = 6, "Julio" = 7, "Agosto" = 8, "Septiembre" = 9, "Octubre" = 10, "Noviembre" = 11,
                                                         "Diciembre" = 12), selected="")),
      column(3,
             uiOutput("ubicacionInput"))
    )),
  fluidRow(
    column(9,
           div(
             plotly::plotlyOutput("grafico",height = "400px"),
             actionButton("descargar", "Gráfica", icon = icon("download")),
             downloadButton("descargarDatos", "Datos"),
             shiny::a("GitHub", href="https://github.com/danielobando2030/SIMAGRO/tree/main/Tabs_1_8", target="_blank",
                      class = "btn btn-default shiny-action-button", icon("github")),
             actionButton("reset", "Restrablecer",icon = icon("refresh")),
              downloadButton("report", "Generar informe")
           )),
    
    column(3, 
           div(
             wellPanel(textOutput("mensaje1"),
                       style = "background-color: #0087CF; color: #FFFFFF;"),
             wellPanel(textOutput("mensaje3"),
                       style = "background-color: #2A4E61; color: #FFFFFF;")
           ))
  ),
  
  fluidRow(
    column(
      12,
      align = "left",
      HTML("
      <b>Fuente:</b> Elaboración propia con base en datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario – SIPSA (DANE).<br><br>
      Entradas locales: Productos reportados con origen de territorios del departamento de Cundinamarca.<br>
      Entradas externas: Productos reportados con origen fuera del departamento de Cundinamarca.
    "),
      style ="font-size:12px; color:#4E4D4D; text-align:left; font-family:'Prompt', sans-serif; margin-top:20px;"
  )
),
  
  fluidRow(
    tags$div(
      tags$img(src = 'logo_2.png', style = "width: 100%; margin: 0;"),
      style = "width: 100%; margin: 0;"
    )
  )
)