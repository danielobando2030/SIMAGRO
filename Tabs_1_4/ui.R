#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 02/04/2024
#Fecha de ultima modificacion: 02/04/2024
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################-
options(scipen = 999)
source("1_4b_Indices_saca_Cundinamarca_funcion.R")
################################################################################-

# Cargar los paquetes necesarios
library(shiny)
library(dplyr)
library(ggplot2)

# Definir la interfaz de usuario
ui <- fluidPage(
  tags$div(
    style = "position: relative; min-height: 100vh; padding-bottom: 100px;",  # Añade un margen inferior
    tags$head(
      tags$title("Destino_alimentos"),  # Añade esta línea
      tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Prompt&display=swap"),  # Importa la fuente Prompt
      tags$style(HTML("
      #grafico {
             display: block;
             margin: auto;
           }
      .selectize-dropdown {
      z-index: 10000 !important;
    }
      body {
        overflow-x: hidden;
      }
      .main-header {
        font-family: 'Prompt', sans-serif;
        font-size: 40px;
        color: #134174;
      }
       .main-header_2 {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
        color: #134174;
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
    "))
  ),
  tags$h1("Destino de los alimentos de origen cundinamarques", class = "main-header"),
  tags$h1("Descubre las principales ciudades receptores de alimentos con origen Cundinamarca según el SIPSA.", class = "main-header_2"),
  #div(
  #  textOutput("subtitulo"),
  #  class = "sub-header2",
  #  style = "margin-bottom: 20px;"
  #),  
  div(class = "scrollable-content",
      fluidRow(
        column(3,
               selectInput("anio", "Año", c("Todos los años" = "todo", sort(as.character(unique(proviene_cundinamarca$anio)))))),
        column(3,
               selectInput("mes", "Mes", c("Todos los meses" = "todo", "Enero" = 1, "Febrero" = 2, "Marzo" = 3, "Abril" = 4, "Mayo" = 5,
                                           "Junio" = 6, "Julio" = 7, "Agosto" = 8, "Septiembre" = 9, "Octubre" = 10, "Noviembre" = 11,
                                           "Diciembre" = 12), selected="")),
        column(3,
               numericInput("municipios", "Número de municipios", value = 10, min = 1, max = 18)),
        column(3,
               selectInput("producto", "Producto",c("Todos los productos" = "todo", sort(as.character(unique(proviene_cundinamarca$producto))))))
      )),
  div(
    fluidRow(
      column(9,
             plotlyOutput("grafico",height = "400px"),
             downloadButton("descargar", "Gráfica"),
             downloadButton("descargarDatos", "Datos"),
             shiny::a("GitHub", href="https://github.com/danielobando2030/SIMAGRO/tree/main/Tabs_1_4", target="_blank",
                      class = "btn btn-default shiny-action-button", icon("github")),
             actionButton("reset", "Restrablecer",icon = icon("refresh")),
             downloadButton("report", "Generar informe")
      ),
      column(3, 
             wellPanel(textOutput("mensaje1"),
                       style = "background-color: #134174; color: #FFFFFF;")
      
      )
    ),
    fluidRow(
      column(12, align = "left",
             HTML("<b>Fuente:</b> Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA).<br>
               <br>
               Solo se tienen en cuenta las principales ciudades con centros de acopio en las que se recolecta información para el SIPSA.<br>
               Esta visualización muestra el ranking de destinos de los alimentos con origen en Cundinamarca, 
               resaltando el porcentaje del volumen de productos que llegan a cada centro de abasto según el SIPSA."),
             style = "font-size:12px; color:#4E4D4D; text-align:left; font-family: 'Prompt', sans-serif; margin-top:15px;"
      )
    )
  ),
fluidRow(
  tags$div(
    tags$img(src = 'logo_2.png', style = "width: 100%; margin: 0;"),
    style = "width: 100%; margin: 0;"
  )
  
  ) 
))