#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 03/04/2024
#Fecha de ultima modificacion: 03/04/2024
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################-
options(scipen = 999)
source("1_3b_Indices_recibe_de_cundinamarca_cadena_funcion.R")
################################################################################-

# Cargar los paquetes necesarios
library(shiny)
library(dplyr)
library(ggplot2)
library(htmlwidgets);library(webshot);library(magick);library(shinyscreenshot);library(webshot2)

# Definir la interfaz de usuario

ui <- fluidPage(
  tags$div(
    style = "position: relative; min-height: 100vh; padding-bottom: 100px;", 
    tags$head(
      tags$title("Importancia de los productos cundinamarqués en los departamentos destinos"),  
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
    "))
  ),
  tags$h1("Contribución de Cundinamarca al Abastecimiento Nacional", class = "main-header"),
  tags$h1("Análisis de la participación de Cundinamarca en el abastecimiento de alimentos en otros departamentos del país", class = "main-header_2"),
  #div(
  # textOutput("subtitulo"),
  #  class = "sub-header2",
  #  style = "margin-bottom: 20px;"
  #),
  div(class = "scrollable-content",
      fluidRow(
        column(4,
               selectInput("anio", "Año", c("Todos los años" = "todo", sort(as.character(unique(cundinamarca$anio)))))),
        column(4,
               selectInput("mes", "Mes", c("Todos los meses" = "todo", "Enero" = 1, "Febrero" = 2, "Marzo" = 3, "Abril" = 4, "Mayo" = 5, "Junio" = 6, "Julio" = 7, "Agosto" = 8, "Septiembre" = 9, "Octubre" = 10, "Noviembre" = 11, "Diciembre" = 12), selected="")),
        column(4,
               selectInput("producto", "Producto",c("Todos los productos" = "todo", sort(as.character(unique(cundinamarca$producto))))))
      )),
  div(
    fluidRow(
      column(9,
             leafletOutput("grafico"),# width = "500px", height = "500px"),
             downloadButton("descargar_", "Gráfica", icon = icon("download")),
             downloadButton("descargarDatos", "Datos"),
             shiny::a("GitHub", href="https://github.com/PlasaColombia-cundinamarca/Tableros/tree/8d5220f3bec2898e21495993520e1d8637e6b5d4/Abs3", target="_blank",
                      class = "btn btn-default shiny-action-button", icon("github")),
             actionButton("reset", "Restablecer", icon = icon("refresh")),
             downloadButton("report", "Generar informe")

      ),
      column(3, 
             wellPanel(textOutput("mensaje2"),
                       style = "background-color: #0D8D38; color: #FFFFFF;"),
           
             
      )
    ),
    tags$div(tags$p(#"Cada porcentaje es lo que cada departamento recibe de Cundinamarca del total de productos que ingresan a este.",
                    #tags$br(),
                    "Fuente: Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA).",
                    tags$br(),"Esta visualización muestra la contribución de Cundinamarca al abastecimiento de otros departamentos. Los porcentajes representan la proporción del volumen que se registra en las principales centrales de abasto de cada departamento con origen Cundinamarca en relación con el total del volumen recibido.",
                    tags$br(),"La comparación se realiza entre ciudades, para una mejor comprensión visual se considera todo el departamento.",
                    tags$br(),"Los departamentos en color gris indican la ausencia de reportes de ingresos de productos provenientes de Cundinamarca.",
                    class = "sub-header2"), style = "margin-top: 20px;")
  ),
  br(),
  br(),
  br(),
  tags$div(
    tags$img(src = 'logo_2.png', style = "width: 100vw;"),
    style = "position: absolute; bottom: 0; width: 100%;"
    )
  ) 
)