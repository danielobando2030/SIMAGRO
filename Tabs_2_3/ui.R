#Proyecto FAO
#INDICE Herfindahl–Hirschman - Abastecimiento shiny abasteciemiento
################################################################################
#Autores: Juan Carlos, Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 14/03/2024
#Fecha de ultima modificacion: 14/03/2024
################################################################################
# Paquetes 
################################################################################
library(shiny); library(lubridate);library(shinythemes);library(plotly);
library(shinydashboard)
options(scipen = 999)
################################################################################
rm(list = ls())
source("2_3b_HHindex_abastecimiento_funciones.R")

ui <- fluidPage(
  tags$head(
    tags$title("Índice de concentración alimentaria"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Prompt&display=swap"),
    tags$style(HTML("
    
       .main-header {
        font-family: 'Prompt', sans-serif;
        font-size: 40px;
        color: #0D8D38;
       }
       .main-header_2 {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
        color: #0D8D38;
      }
      .sub-header {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
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
    ")),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML")
  ),
  tags$h1("Índice de diversidad de alimentos", class = "main-header"),
  tags$h1("Análisis de variedad de los alimentos que ingresan a las principales centrales de abasto de Cundinamarca", class = "main-header_2"),
  div(
    textOutput("subtitulo"),
    class = "sub-header2",
    style = "margin-bottom: 20px;"
  ),
  div(
    fluidRow(
      column(6,
             selectInput("tipo", "Seleccione el tipo:", 
                         choices = list("Anual" = 1, 
                                        "Mensual" = 0))
      ),
      column(4,
             conditionalPanel(
               condition = "input.tipo == 0 ",
               selectInput("anio", "Seleccione el año:", 
                           choices = c("Todos los años" = "todo", unique(IHH_anual$year))))
      )
    )
  ),
  
  fluidRow(
    column(9,
           div(
             plotly::plotlyOutput("grafico1",height = "400px"),
             downloadButton("descargar_", "Gráfica", icon = icon("download")),
             downloadButton("descargarDatos", "Datos"),
             shiny::a("GitHub", href="https://github.com/Simonaa-Antioquia/Tableros/tree/main/Pre1", target="_blank",
                      class = "btn btn-default shiny-action-button", icon("github")),
             actionButton("reset", "Restablecer", icon = icon("refresh")),
             downloadButton("report", "Generar informe")
           )),
    
    column(3, 
           div(
             wellPanel(textOutput("mensaje1"),
                       style = "background-color: #0D8D38; color: #FFFFFF;"),
             wellPanel(uiOutput("mensaje2"),
                       style = "background-color: #005A45; color: #FFFFFF;"))
  )),
  
  fluidRow(
    column(12,
           style = "margin-top: 2px;",
           tags$div(
             tags$p("Fuente: Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA)", 
                    tags$br(),"Este gráfico se calcula con base en el índice de Herfindahl-Hirschman"
                    , class = "sub-header2", style = "margin-top: 3px;"),
             tags$div(style = "text-align: left;", 
                      tags$p("La fórmula del índice de Herfindahl-Hirschman es:", class = "sub-header2", style = "margin-top: 3px;"),
                      tags$script(HTML('MathJax.Hub.Queue(["Typeset", MathJax.Hub, "mathjax-output"])')),
                      tags$div(id = "mathjax-output", HTML("$$IHH = \\sum_{i=1}^{n} s_i^2$$"))
             ),
             tags$p(HTML("Donde S<sub>i</sub> es la participación que tiene cada producto en el volumen total de alimentos que ingresan a las principales centrales de abasto de Cundinamarca."), class = "sub-header2", style = "margin-top: 3px;"),
           ) 
    )
  ),
  
  fluidRow(
    tags$div(
      tags$img(src = 'logo_2.png', style = "width: 100%; margin: 0;"),  
      style = "width: 100%; margin:0;"  
    )
  )
) 