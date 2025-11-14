#Proyecto FAO
################################################################################
#Autores: Juan Carlos, Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 28/03/2024
#Fecha de ultima modificacion: 28/03/2024
################################################################################
# Paquetes 
################################################################################
library(shiny); library(lubridate);library(shinythemes);library(shinyWidgets)
options(scipen = 999)
################################################################################
rm(list = ls())

source("1_7b_netos_funciones.R")
productos <- unique(data_anual_producto$producto)


ui <- fluidPage(
  tags$head(
    tags$title("Netos"),  
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
  tags$h1("Balance de Alimentos: Productos de origen local vs. externo", class = "main-header"),
  tags$h1("Compara volúmenes de alimentos locales y externos que ingresan a las principales centrales de abasto de Bogotá.", class = "main-header_2"),
  div(
    textOutput("subtitulo"),
    class = "sub-header2",
    style = "margin-bottom: 20px;"
  ),  
  div(
      fluidRow(
        column(4,
               selectInput("tipo", "Seleccione el tipo de funcion:", 
                           choices = list("Balances generales anuales" = 1, 
                                          "Balances anuales por producto" = 2, 
                                          "Balances generales mensuales" = 3, 
                                          "Balances generales mensuales por producto" = 4))),
        column(4,
               conditionalPanel(
                 condition = "input.tipo == 2 || input.tipo ==4",
                 selectInput("producto_seleccionado", "Producto seleccionado:", choices = c(productos,  Ninguno =""), multiple = TRUE)
               ))
      )),
  fluidRow(
    div(
      column(9,
             plotly::plotlyOutput("grafico",height = "400px"),
             downloadButton("descargar_", "Gráfica", icon = icon("download")),
             downloadButton("descargarDatos", "Datos"),
             shiny::a("GitHub", href="https://github.com/danielobando2030/SIMAGRO/tree/main/Tabs_1_7", target="_blank",
                      class = "btn btn-default shiny-action-button", icon("github")),
             actionButton("reset", "Restablecer", icon = icon("refresh")),
             downloadButton("report", "Generar informe")
      ),
           
    column(3,
           wellPanel(textOutput("mensaje1"),
                     style = "background-color: #0087CF; color: #FFFFFF;"))
    )
    
  ),
    
  fluidRow(
    column(
      12,
      align = "left",
      HTML("
      <b>Fuente:</b> Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA).<br><br>

      El balance de alimentos se define como la diferencia en volumen entre los alimentos que ingresan a las principales centrales de abasto de Bogotá con origen en Cundinamarca y el total de alimentos reportados en las centrales. 
      La fórmula para calcular el balance de alimentos es 
      <i>N<sub>it</sub> = P<sub>A</sub> - D<sub>A</sub></i>, donde 
      <i>N<sub>it</sub></i> es el balance de alimentos, 
      <i>P<sub>A</sub></i> es el volumen de alimentos que se originan en Cundinamarca y 
      <i>D<sub>A</sub></i> es el volumen total de alimentos que llegan al centro de acopio de Bogotá.<br><br>

      Un balance negativo es sinónimo de dependencia hacia otros departamentos del país.
    "),
      style = "font-size:12px; color:#4E4D4D; text-align:left; font-family:'Prompt', sans-serif; margin-top:15px;"
    )
  ),
  fluidRow(
    tags$div(
      tags$img(src = 'logo_2.png', style = "width: 100%; margin: 0;"),
      style = "width: 100%; margin: 0;"
    )
  )
)
